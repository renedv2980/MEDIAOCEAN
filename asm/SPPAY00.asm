*          DATA SET SPPAY00    AT LEVEL 130 AS OF 07/01/19                      
*PHASE T21300A                                                                  
*INCLUDE ACPAYXFR                                                               
*INCLUDE ACPAYSPT                                                               
*INCLUDE CONVMOS                                                                
                                                                                
*==============================================================*                
* 17SEP14  AKAT CHANGE PST ACCUMULATOR FROM PL4 TO XL4         *                
* 28SEP11  AKAT SUPPORT PAYING REP FOR XAUTOPAY RECORDS        *                
* 18AUG11  BPLA SAVE PID AND ACC AGENCY INFO                   *                
* 24MAR11  AKAT SUPPORT NEW AUTOPAY RECORD FORMAT ON XSPFIL    *                
* 01AUG06  MHER 13 INVOICE ENTRY LINES                         *                
* 07JUL06  MHER ADD CLRST ELEMENTS FOR EACH INVOICE            *                
* 15JUN06  MHER SEND ACTUAL MARKET FOR ID=MKTRGP CLIENTS       *                
* 03FEB10  FLAG UNCLEARANCES AND INSERT EST IN CLRST RECS      *                
* 07MAR00  SUPPORT ERRTEXT AND ALWAYS CALL SPPAY15 FOR CAN NET *                
* DEC10/98 FOR WT, SUPPORT NON-TBS TRADE                       *                
* FEB27/96 FOR COKE - ADD Z8 REQUEST WHEN AN INVOICE IS PAID   *                
* 13JUN95  SUPPORT CTA                                         *                
* 05MAY93  SUPPORT PST                                         *                
* 20MAR92  SUPPORT CLEARANCE STATUS RECORDS                    *                
*==============================================================*                
                                                                                
         TITLE 'T21300 - SPOTPAK PAY PROGRAM - BASE'                            
T21300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GENOLDX-GENOLD,T21300,RR=R8                                      
         ST    R8,RELO                                                          
*                                                                               
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21300+4096,R9                                                   
*                                                                               
         BAS   RE,INITL                                                         
*                                                                               
         L     R3,VTWA                                                          
         USING T213FFD,R3                                                       
         OI    PAYSERVH+1,X'01'    SERVICE FIELD ALWAYS MODIFIED                
         OI    PAYSERVH+6,X'80'                                                 
*                                                                               
         L     RF,VCOMFACS                                                      
         MVC   GLOBBER,CGLOBBER-COMFACSD(RF)                                    
         GOTO1 =A(GLOBINVS),RR=RELO     GLOBBER INVOICE LINES                   
                                                                                
* TEST MEDIA PAYMENTS CURRENTLY AVAILABLE                                       
                                                                                
         XC    DMCB,DMCB                                                        
         L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
*                                                                               
         L     RE,VTWA                                                          
         USING TWAD,RE                                                          
**>      CLC   TWAUSRID,=X'0ABF'    SJB                                         
         CLC   TWAUSRID,=X'330A'    MENYPWB                                     
         BNE   PAYSTART                                                         
         DROP  RE                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(SCRTYERR)                                              
         B     PAYERR                                                           
*                                                                               
PAYSTART GOTO1 (RF),DMCB,(X'80',DUB),F#SSTAT3                                   
         TM    DUB,SSBNOPAY        TEST PAYMENTS INHIBITED                      
         BZ    PAY0                NO - CARRY ON                                
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOMEDPAY)                                              
         B     PAYERR                                                           
*                                                                               
PAY0     XC    SVPASSWD,SVPASSWD                                                
         GOTO1 (RF),DMCB,(2,0)           REGULAR GETFACT CALL                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'                                                    
         BZ    PAY00                                                            
         MVC   SVPASSWD,FAPASSWD                                                
         B     PAY00                                                            
         DROP  R1                                                               
*                                                                               
RELO     DS    A                                                                
*                                                                               
PAY00    BRAS  RE,INIT                                                          
*                                                                               
PAY2     DS    0H                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',=C'PAY',3,GLVPGM                           
*                                                                               
         CLI   SVSCRN,X'FE'        TEST 'TTEST' SCREEN LOADED                   
         BNE   PAY4                                                             
         BRAS  RE,RSTRTWA          ALWAYS RESTORE INVOICE SCREEN                
         MVI   RESTORED,C'Y'       SET FLAG THAT SCREEN RESTORED                
*                                                                               
PAY4     TM    PAYERH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    PAYOPH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    PAYMDH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    PAYCLH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    PAYPRH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    PAYSTH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    PAYEEH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    PAYDTH+4,X'20'                                                   
         BZ    EDITHL                                                           
         B     EDITDATA                                                         
*                                                                               
EXIT     OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
EXXMOD   DS    0H                                                               
         CLI   INCHSW,C'Y'         IF TRANSFERED FROM $INCH                     
         BNE   EXXMODX                                                          
         GOTO1 =A(XFRETN),RR=RELO                                               
*                                                                               
EXXMODX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
EDITHL   XC    PAYMSG,PAYMSG                                                    
         FOUT  PAYMSGH                                                          
         MVC   PRVKEY,SVKEY        SAVE HL KEY THRU STA                         
         MVC   PRVREP,QREP          AND REP                                     
         MVC   PRVTEST,SVTST        AND TEST STATUS                             
* LOAD HEADLINE EDIT                                                            
         GOTO1 VCALLOV,DMCB,(1,0),(R3)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
* VALIDATE HEADLINE DATA                                                        
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
         CLI   ERRAREA,0                                                        
         BNE   EXIT                                                             
*                                                                               
         CLI   SVPPROF+10,C'C'     TEST CCUSA INTERFACE ACTIVE                  
         BNE   EDITHL20                                                         
         CLI   SVSCRN,X'FD'        TEST HAVE RIGHT SCREEN                       
         BE    EDITHL2             YES                                          
         MVI   SVSCRN,X'FD'                                                     
         GOTO1 VCALLOV,DMCB,(X'FD',PAYLAST),((R3))                              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
EDITHL2  DS    0H                  DISPLAY INTERFACE DATA                       
         FOUT  PAYXLINH                                                         
         MVC   PAYXLIN,SPACES                                                   
         LA    R4,PAYXLIN                                                       
         LA    R5,SVXFRDTA                                                      
         USING PAYXFRD,R5                                                       
*                                                                               
         CLI   SVXFRSW,C'N'        TEST TRANSFER SUPPRESSED                     
         BNE   *+14                                                             
         MVC   PAYXLIN(30),=C'** NO EXPENDITURE INTERFACE **'                   
         B     EDITHL30                                                         
*                                                                               
         MVC   0(4,R4),=C'ACN='                                                 
         MVC   4(5,R4),XFRACN                                                   
         MVI   9(R4),C','                                                       
         LA    R4,10(R4)                                                        
*                                                                               
         MVC   0(7,R4),=C'BUDGET='                                              
         LA    R4,7(R4)                                                         
         EDIT  XFRBUDG,(8,(R4)),ALIGN=LEFT                                      
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         MVC   0(6,R4),=C'Y-T-D='                                               
         LA    R4,6(R4)                                                         
         EDIT  XFRYTD,(8,(R4)),ALIGN=LEFT                                       
         B     EDITHL30                                                         
*                                                                               
EDITHL20 CLI   SVSCRN,0            IS NORMAL SCREEN ACTIVE                      
         BE    EDITHL30            YES                                          
         MVI   SVSCRN,0            INDICATE NORMAL LOADED                       
         MVC   PAYLAST(3),=X'000001'  AND CLEAR REMAINDER                       
*                                                                               
EDITHL30 CLI   INCHSW,C'Y'         IF CALLED FROM $INCH                         
         BE    EDITDATA            SKIP PAUSE                                   
* TEST TO SEE IF THEY ASKED FOR INVOICE DATA TO BE READ                         
         LA    R2,PAYINV1H                                                      
         CLI   5(R2),1                                                          
         BNE   EDITHL32                                                         
         CLI   8(R2),C'='                                                       
         BE    EDIT1X              HONOR REQUEST FOR INVOICES                   
*                                                                               
EDITHL32 CLI   SVAUTPAY,C'Y'       DOING AUTOPAY SCRIPT?                        
         BE    EDITDATA            YES - PROCESS INVOICES ON SCREEN             
         CLC   PRVKEY(9),SVKEY     SAME A-M/C/PRD/MKT/STA                       
         BNE   *+14                                                             
         CLC   PRVREP,QREP         SAME REP                                     
         BE    EDITDATA                                                         
         BRAS  RE,ENTERINV         GET MESSAGE ADDRESS                          
         MVC   PAYMSG(60),0(R1)                                                 
         B     EXIT                                                             
         EJECT                                                                  
EDITDATA DS    0H                                                               
         LA    R2,PAYINV1H                                                      
         CLI   RESTORED,C'Y'       TEST JUST RESTORED INVOICE SCREEN            
         BE    EXIT                                                             
*                                                                               
         XC    PAYMSG,PAYMSG       CLEAR PREVIOUS MESSAGE                       
         FOUT  PAYMSGH                                                          
                                                                                
*=========================================================*                     
* FOR AUTO ACC POSTINGS MAKE SURE ACC SYSTEM IS OP AND    *                     
* COMPANY RECORD CAN BE READ.                             *                     
*=========================================================*                     
                                                                                
         CLI   SVPPROFA+3,C'Y'                                                  
         BNE   EDIT1                                                            
         MVI   PAYMODE,C'I'        SET FOR INIT MODE                            
         GOTO1 =A(GOTOACC),RR=RELO                                              
         MVI   PAYMODE,0           RESET                                        
                                                                                
*=========================================================*                     
* BUILD TABLE OF INV/AMT/CMT FIELDS PRESENT               *                     
*=========================================================*                     
                                                                                
EDIT1    MVI   SVTRADE,C'N'                                                     
         TM    SVAFLAG1,X'02'      TEST NON-TBS TRADE AGY                       
         BZ    EDIT1A                                                           
         CLI   QPRD+2,C'#'         TEST TRADE CLEARANCE                         
         BNE   EDIT1A                                                           
         MVI   SVTRADE,C'Y'                                                     
*                                                                               
EDIT1A   L     R0,ASVAMTS                                                       
         LHI   R1,SVAMTX-SVAMTS                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R8,ASVAMTS                                                       
         USING AMTD,R8                                                          
*                                                                               
         CLI   SVPPROFA+7,C'Y'     TEST FIRST COMMENT = ID OPTION               
         BNE   EDIT1X                                                           
         MVI   ERRCD,NOCONTID                                                   
*                                                                               
EDIT1B   LA    R2,PAYOPH                                                        
         OC    SVID,SVID           TEST PAYING BY ID                            
         BZ    PAYERR              ID REQUIRED                                  
         LA    R2,PAYCMT1H                                                      
         MVI   ERRCD,NOCOM1                                                     
         CLC   PAYCMT1(8),=C'CONTRACT'   ALLOW DATA THAT I PUT THERE            
         BE    *+12                                                             
         CLI   PAYCMT1H+5,0              BUT USER CAN'T ENTER                   
         BNE   PAYERR                                                           
         MVC   PAYCMT1(8),=C'CONTRACT'                                          
         MVC   PAYCMT1+9(12),SVID                                               
         MVI   PAYCMT1H+5,21                                                    
         OI    PAYCMT1H+6,X'80'                                                 
*                                                                               
EDIT1X   LA    R2,PAYINV1H                                                      
         CLI   5(R2),1             TEST 1 CHARACTER INPUT                       
         BNE   EDIT1Z                                                           
         CLI   8(R2),C'='          IS IT A REQUEST TO GET INVOICES              
         BNE   EDIT1Z                                                           
         BRAS  RE,GETINV                                                        
         CLI   ERRAREA,X'FF'                                                    
         BE    EXIT                                                             
         XC    PAYMSG,PAYMSG                                                    
         BRAS  RE,VERAMTS                                                       
         MVC   PAYMSG(34),0(R1)                                                 
         FOUT  PAYMSGH                                                          
         B     EXIT                                                             
*                                                                               
EDIT1Z   DS    0H                                                               
         CLC   =C'B$',AGYALPHA                                                  
         BE    EDIT1ZZ                                                          
         CLC   =C'MC',AGYALPHA                                                  
         BNE   EDIT2                                                            
EDIT1ZZ  CLI   5(R2),0                                                          
         BE    EDIT4                                                            
         BRAS  RE,GETPADR                                                       
*                                                                               
EDIT2    CLI   5(R2),0             TEST INVOICE PRESENT                         
         BE    EDIT4                                                            
         OI    AMTFLAGS,X'80'      SET INVOICE PRESENT                          
         MVC   AMTINV,8(R2)        SAVE INVOICE                                 
         OC    AMTINV,SPACES                                                    
*                                                                               
EDIT4    LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST AMOUNT PRESENT                          
         BE    *+8                                                              
         OI    AMTFLAGS,X'40'      SET AMOUNT PRESENT                           
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST COMMENT PRESENT                         
         BE    *+14                NO                                           
         OI    AMTFLAGS,X'20'      SET COMMENT PRESENT                          
         MVC   AMTCOM,8(R2)        SAVE THE COMMENT                             
*                                                                               
         AHI   R8,AMTLEN           NEXT LINE IN TABLE                           
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               NEXT INVOICE FIELD                           
         LA    R0,PAYCMTXH                                                      
         CR    R2,R0               HAVE WE REACHED THE END YET                  
         BNH   EDIT2               NO - CONTINUE                                
*                                                                               
         L     R8,ASVAMTS          MAKE SURE NO BLANK LINES                     
         LA    R0,MAXAMTS                                                       
         LA    R2,PAYINV1H                                                      
*                                                                               
EDIT5A   CLI   AMTFLAGS,0                                                       
         BE    EDIT5B                                                           
         AHI   R2,PAYINV2-PAYINV1  NEXT INVOICE ENTRY                           
         AHI   R8,AMTLEN                                                        
         BCT   R0,EDIT5A                                                        
         B     EDIT5X                                                           
*                                                                               
EDIT5B   CLI   AMTFLAGS,0          ALL FIELDS BELOW SHOULD BE BLANK             
         BE    EDIT5C                                                           
         MVI   ERRCD,MSSNGERR                                                   
         AHI   R2,-(PAYINV2-PAYINV1) BACK UP TO BLANK LINE                      
         B     PAYERR                                                           
*                                                                               
EDIT5C   AHI   R8,AMTLEN                                                        
         AHI   R2,PAYINV2-PAYINV1                                               
         BCT   R0,EDIT5B                                                        
*                                                                               
EDIT5X   L     R8,ASVAMTS                                                       
         LA    R2,PAYINV1H                                                      
         MVI   ERRCD,MSSNGERR                                                   
         TM    AMTFLAGS,X'80'         LINE 1 MUST HAVE INV                      
         BZ    PAYERR                                                           
         LA    R2,PAYAMT1H                                                      
         TM    AMTFLAGS,X'40'         AND AMOUNT                                
         BZ    PAYERR                                                           
                                                                                
*================================================================               
* NEED TO MAKE SURE NO MORE THAN 5 COMMENTS FOR EACH INVOICE                    
*================================================================               
                                                                                
         LA    R2,PAYINV2H                                                      
         LHI   R0,MAXAMTS-1                                                     
*                                                                               
EDIT6A   LHI   R4,5                SET MAX 4 MORE COMMENTS                      
*                                                                               
EDIT6B   AHI   R8,AMTLEN                                                        
         AHI   R2,PAYINV2H-PAYINV1H  NEXT INVOICE FIELD                         
         BCT   R0,*+8                                                           
         B     EDIT8                 REACHED EOS                                
*                                                                               
         CLI   AMTFLAGS,0            TEST NO MORE INPUT                         
         BE    EDIT8                                                            
         TM    AMTFLAGS,X'80'        TEST INVOICE THIS LINE                     
         BO    EDIT6A                                                           
         BCT   R4,EDIT6B                                                        
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(MAX5CMTS)                                              
         B     PAYERR                                                           
                                                                                
*===============================================================*               
* NOW NEED TO MAKE SURE THERE ARE NO DUPLICATE INVOICES         *               
*===============================================================*               
                                                                                
EDIT8    LA    R2,PAYINV1H                                                      
*                                                                               
EDIT8A   BAS   RE,CHKINV                                                        
*                                                                               
EDIT8B   AHI   R2,PAYINV2H-PAYINV1H                                             
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    EDIT8B              NO                                           
         LA    R0,PAYINVXH                                                      
         CR    R2,R0               TEST REACHED LAST INVOICE LINE               
         BL    EDIT8A              NO - CHECK AGAIN                             
         B     EDIT10              ELSE DONE                                    
*                                                                               
CHKINV   LA    R4,PAYINV2H-PAYINV1H(R2) POINT TO NEXT INVOICE FIELD             
*                                                                               
CHKINV2  CLI   5(R2),0             TEST INPUT                                   
         BE    CHKINV4             NO                                           
         CLC   8(11,R2),8(R4)      SAME INVOICE NUMBER                          
         BE    CHKINVER                                                         
*                                                                               
CHKINV4  AHI   R4,PAYINV2H-PAYINV1H  NEXT INVOICE                               
         LA    R0,PAYINVXH                                                      
         CR    R4,R0                                                            
         BNH   CHKINV2                                                          
         BR    RE                                                               
*                                                                               
CHKINVER MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DUPINVER)                                              
         LR    R2,R4                                                            
         B     PAYERR                                                           
         EJECT                                                                  
*==============================================================*                
* EDIT AMOUNTS                                                 *                
*==============================================================*                
                                                                                
EDIT10   LA    R2,PAYAMT1H         POINT TO FIRST INVOICE                       
         L     R8,ASVAMTS                                                       
*                                                                               
EDIT11   TM    AMTFLAGS,X'40'      TEST AMOUNT PRESENT                          
         BZ    EDIT22              NO - DONT EDIT                               
         MVI   AMTTYPE,C'1'        SET AMT TYPE                                 
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         CHI   R0,2                                                             
         BNH   EDIT14                                                           
* CHECK FOR CR OR CK AT END OF FIELD                                            
         LA    RE,8(R2)                                                         
         AR    RE,R0                                                            
         AHI   RE,-2                                                            
         CLC   =C'CR',0(RE)                                                     
         BNE   *+12                                                             
         MVI   AMTTYPE,C'2'                                                     
         B     EDIT12                                                           
         CLC   =C'CK',0(RE)                                                     
         BNE   EDIT14                                                           
         MVI   AMTTYPE,C'3'                                                     
*                                                                               
EDIT12   AHI   R0,-2               ADJUST LEN FOR CR/CK                         
*                                                                               
EDIT14   MVI   ERRCD,INVERR                                                     
         CLI   REVSW,C'Y'          REVERSE OPTION                               
         BE    REVERSE                                                          
         LA    R4,8(R2)                                                         
         TM    SVAFLAG1,X'20'      TEST CTA ACTIVE                              
         BZ    EDIT16                                                           
         CLI   8(R2),C'T'          TEST 'TRADE' PAYMENT                         
         BNE   EDIT16                                                           
         MVI   SVTRADE,C'Y'                                                     
         LA    R4,1(R4)                                                         
         BCTR  R0,0                                                             
*                                                                               
EDIT16   GOTO1 VCASHVAL,DMCB,(2,(R4)),(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    PAYERR                                                           
         ICM   R0,15,4(R1)                                                      
         BM    PAYERR                                                           
         ST    R0,AMT              SAVE AMOUNT                                  
         ST    R0,AMTUNADJ         AND HERE TOO!                                
*                                                                               
         CLI   SVPPROFB+3,C'Y'     ALLOW $0 CLEARANCES                          
         BNE   EDIT17                                                           
*                                                                               
*--------------------------------------------------------------------           
*        THIS CODE ALLOWS $0 CLEARANCES                                         
*--------------------------------------------------------------------           
         L     R0,ASVAMTS                                                       
         CR    R0,R8               TEST LINE 1                                  
         BE    EDIT22A                                                          
*                                                                               
         CLI   SVTRADE,C'Y'        TEST CTA TRADE PAYMENT                       
         BNE   EDIT22A                                                          
         MVI   ERRCD,NEWERRS       YES - ALLOW ONLY ONE INPUT AMOUNT            
         MVC   NERRCD,=AL2(ONETRINV)                                            
         B     PAYERR                                                           
*                                                                               
EDIT22A  AHI   R8,AMTLEN              NEXT LINE IN TABLE                        
         AHI   R2,PAYAMT2H-PAYAMT1H   NEXT AMOUNT FIELD                         
         LA    R0,PAYAMTXH                                                      
         CR    R2,R0                  TEST PAST LAST FIELD                      
         BNH   EDIT11                                                           
         B     EDIT30                                                           
*--------------------------------------------------------------------           
*        THIS CODE PREVENTS $0 CLEARANCES                                       
*--------------------------------------------------------------------           
EDIT17   L     R0,ASVAMTS                                                       
         CR    R0,R8               TEST LINE 1                                  
         BNE   EDIT18              NO                                           
         OC    AMT,AMT             TEST AMOUNT = 0                              
         BNZ   EDIT22                                                           
         B     EDIT25              MAKE SURE NO MORE DATA                       
*                                                                               
EDIT18   CLI   SVTRADE,C'Y'        TEST CTA TRADE PAYMENT                       
         BNE   EDIT20                                                           
         MVI   ERRCD,NEWERRS       YES - ALLOW ONLY ONE INPUT AMOUNT            
         MVC   NERRCD,=AL2(ONETRINV)                                            
         B     PAYERR                                                           
*                                                                               
EDIT20   OC    AMT,AMT             ZERO AMOUNT NOT ON LINE 1 IS ERROR           
         BZ    PAYERR                                                           
*                                                                               
EDIT22   AHI   R8,AMTLEN              NEXT LINE IN TABLE                        
         AHI   R2,PAYAMT2H-PAYAMT1H   NEXT AMOUNT FIELD                         
         LA    R0,PAYAMTXH                                                      
         CR    R2,R0                  TEST PAST LAST FIELD                      
         BNH   EDIT11                                                           
         B     EDIT30                                                           
*                                                                               
*============================================================                   
* NO DATA THIS LINE - SHOULD HAVE NO MORE DATA                                  
*============================================================                   
                                                                                
EDIT25   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,PAYCMTXH                                                      
         CR    R2,R0                                                            
         BH    EDIT30                                                           
         CLI   5(R2),0                                                          
         BE    EDIT25                                                           
         MVI   ERRCD,INVERR                                                     
         B     PAYERR                                                           
         EJECT                                                                  
* SUM AMOUNTS AND SAVE TOTAL                                                    
*                                                                               
EDIT30   LHI   R0,MAXAMTS                                                       
         L     R8,ASVAMTS                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
EDIT40   L     RF,AMT              GET AMOUNT IN PENNIES                        
         A     RF,AMTGST           ADD GST                                      
         LA    R1,AMTPST           ADD IN PST AMOUNTS                           
         LA    R2,10                                                            
*                                                                               
EDIT41   A     RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R2,EDIT41                                                        
*                                                                               
         CLI   AMTTYPE,C'1'                                                     
         BE    *+6                                                              
         LCR   RF,RF                                                            
         AR    RE,RF                                                            
         LA    R8,AMTLEN(R8)                                                    
         BCT   R0,EDIT40                                                        
*                                                                               
         ST    RE,TOTAMT                                                        
         LA    RF,50                                                            
         CLI   SVPPROFA+10,1       TEST PROFILE OVERRIDE                        
         BL    EDIT42                                                           
         IC    RF,SVPPROFA+10                                                   
         N     RF,=X'0000000F'                                                  
         MHI   RF,100                                                           
*                                                                               
EDIT42   SR    RE,RF                                                            
         ST    RE,TOTAMTLO                                                      
         AR    RF,RF               GET LEEWAY X 2                               
         AR    RE,RF               AND ADD TO LOW                               
         ST    RE,TOTAMTHI                                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAYP)                                    
         CLC   =C'FJWT$$',PAYER                                                 
         BNE   *+10                                                             
         MVC   TODAYP,SVPAYDT      OVERRIDE PAY DATE                            
         GOTO1 VDATCON,DMCB,(5,0),TODAY                                         
*                                                                               
         OC    SVTST,SVTST         IS TEST OPTION ACTIVE                        
         BZ    EDIT44              NO                                           
         CLI   SVSCRN,X'FE'        TEST HAVE RIGHT SCREEN                       
         BE    EDIT44                                                           
                                                                                
         XC    DMCB(24),DMCB                                                    
         L     RF,VTWA             SAVE CURRENT SCREEN IN TWA1                  
         MVC   DMCB+10(2),2(RF)    2 BYTE TERM NO.                              
         MVI   DMCB+8,1            PAGE 1                                       
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,VTWA                        
*                                                                               
         MVI   SVSCRN,X'FE'        NOW LOAD TEST SCREEN                         
         GOTO1 VCALLOV,DMCB,(X'FE',PAYHLH),(R3)                                 
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
EDIT44   LA    R0,4                ALL CLEARANCES BUT CAN NET USE 04            
         CLI   PAYMD,C'N'          TEST NTWK                                    
         BNE   EDIT46                                                           
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   EDIT46                                                           
*                                                                               
         LA    R0,X'15'                                                         
         CLI   SVPPROFB+7,C'Y'     TEST SUPERPAY ACTIVE                         
         BNE   EDIT46                                                           
*                                                                               
         CLC   SVENDB,=X'6E061C'   TEST PER STARTS AFTER JUN27/10               
         BL    EDIT46                                                           
         LA    R0,X'16'                                                         
*                                                                               
EDIT46   DS    0H                                                               
         SLL   R0,24                                                            
         GOTO1 VCALLOV,DMCB,(R0),(R3)                                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         B     EXXMOD                                                           
*                                                                               
PAYERR   GOTO1 ERROR                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
****     REVERSE OPTION                                                         
         SPACE 2                                                                
REVERSE  DS    0H                                                               
         CLI   AMTTYPE,C'1'        MUST BE CR OR CK                             
         BE    PAYERR                                                           
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         BE    PAYERR                                                           
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BM    PAYERR                                                           
         ST    R0,AMT              SAVE AMOUNT                                  
*                                                                               
         CLI   SVPPROFB+3,C'Y'     ALLOW $0 CLEARANCES                          
         BE    *+14                                                             
         OC    AMT,AMT                                                          
         BZ    PAYERR              AMOUNT CAN'T BE 0                            
*                                                                               
REV15    DS    0H                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0               R2 POINTS TO COMMENT                         
         IC    R0,0(R2)                                                         
         AR    R2,R0               NOW POINTS TO NEXT INVOICE                   
         LA    R0,PAYINVXH                                                      
         CR    R2,R0               ARE WE AT END                                
         BH    REV30               YES                                          
         SPACE 1                                                                
         TM    AMTFLAGS,X'20'      DOES THIS LINE HAVE COMMENT                  
         BO    REV20                                                            
         SPACE 1                                                                
*              NO COMMENT THIS LINE                                             
         LA    R8,AMTLEN(R8)                                                    
         CLI   AMTFLAGS,0          TEST DATA NEXT LINE                          
         BE    REV25                                                            
         MVI   ERRCD,INVERR                                                     
         B     PAYERR                                                           
         SPACE 1                                                                
REV20    LA    R8,AMTLEN(R8)                                                    
         CLI   AMTFLAGS,0          TEST DATA NEXT LINE                          
         BE    REV25                                                            
         MVI   ERRCD,INVERR                                                     
         TM    AMTFLAGS,X'C0'      INV OR AMT NEXT LINE                         
         BNZ   PAYERR              NOT ALLOWED                                  
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     REV15                                                            
         SPACE 1                                                                
*        NO DATA THIS LINE, SHOULD BE NO MORE DATA                              
REV25    LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,PAYCMTXH                                                      
         CR    R2,R0               ARE WE AT END                                
         BH    REV30               YES                                          
         SPACE 1                                                                
         CLI   5(R2),0                                                          
         BE    REV25                                                            
         MVI   ERRCD,INVERR                                                     
         B     PAYERR                                                           
         SPACE 1                                                                
REV30    DS    0H                  NET DOWN GROSS AMT IF NECESSARY              
         L     R8,ASVAMTS                                                       
         CLI   SVPPROF+0,C'G'                                                   
         BNE   REV110                                                           
         L     R5,AMT                                                           
         M     R4,=F'85'                                                        
         AHI   R5,50                                                            
         D     R4,=F'100'                                                       
         ST    R5,AMT                                                           
         SPACE 1                                                                
REV110   MVI   PASS,0                                                           
         CLI   SVPPROFA+3,C'Y'     TEST AUTO ACC POSTING                        
         BNE   *+8                                                              
         BAS   RE,INITACC                                                       
*                                                                               
REV115   GOTO1 =A(BLDREQ),RR=RELO                                               
         MVI   ZSTATUS,0           YOU IDIOT                                    
         MVI   ZSEQNUM,0           FORCE STATUS SEQ                             
         CLI   PASS,C'S'           TEST SPOT PASS                               
         BNE   *+8                                                              
         OI    ZSTATUS,X'80'       SET STATUS TO XFR'D                          
         CLI   SVAUTPAY,C'Y'                                                    
         BNE   *+8                                                              
         OI    ZSTATUS,ZSTATUS_AUTPAY                                           
*                                                                               
         CLC   SVPPROF+14(2),=C'*A'  TEST ALT ACC AGENCY                        
         BNH   REV116                                                           
         CLC   SVPPROF+14(2),=C'00'  00 DOESN'T COUNT                           
         BE    REV116                                                           
         MVC   ZAGYCODE,SVPPROF+14                                              
         B     REV117                                                           
*                                                                               
REV116   CLC   SVCACCAG,=C'  '       IS THERE AN AGY CODE IN CLTHDR             
         BNH   REV117                                                           
         MVC   ZAGYCODE,SVCACCAG     YUP - USE IT                               
*                                                                               
REV117   CLI   SVKEY+9,0           TEST PAYING BY EST                           
         BE    *+10                NO                                           
         MVC   ZACCEST,ZEST        YES - MOVE ESTIMATE NUMBER                   
         MVC   ZTYPE,=C'30'                                                     
         GOTO1 VDATCON,DMCB,(5,0),ZDATE    TODAY'S DATE                         
***      MVC   ZAREA+66(1),SVOFFC                                               
         MVC   ZOFFICE(1),SVOFFC   DEFAULT TO MEDIA OFFICE                      
         MVI   ZOFFICE+1,C' '                                                   
* IF ANY ACC OFFICE CODE, USE IT INSTEAD                                        
         CLI   SVCACCOF,C' '         TEST ACC OFFICE CODE PRESENT               
         BNH   *+10                  NO                                         
         MVC   ZOFFICE(2),SVCACCOF   YES - THEN USE IT                          
*                                                                               
         CLI   SVOFFCSW,C'Y'       TEST SPECIAL OFFICE ACTIVE                   
         BNE   *+10                                                             
         MVC   ZAREA+66(1),SVPPROF+4     **SET SPEC. OFF. CODE**                
         SPACE 1                                                                
*                                                                               
*              WHEN AMTTYPE =1, IT'S NORMAL                                     
*                           =2, IT'S A CR                                       
*                           =3, IT'S A CK                                       
*                                                                               
         L     R0,AMT                                                           
         CLI   AMTTYPE,C'2'        TEST CR                                      
         BE    REV120                                                           
         LCR   R0,R0               MAKE CR & CK MINUS                           
         MVI   ZAMTTYPE,C'2'       DO NEW ENTRY FIRST                           
         B     *+8                                                              
         SPACE 1                                                                
REV120   MVI   ZAMTTYPE,C'3'                                                    
         CVD   R0,DUB                                                           
         ZAP   ZAMT,DUB            PL5 NET FIELD                                
         ZAP   Z2PNET2,DUB         PL6 NET FIELD                                
*                                                                               
         L     R0,TOTG             ALWAYS SEND GROSS AMOUNT                     
         LCR   R0,R0               COMPLEMENTED FOR REVERSE                     
         CVD   R0,DUB              AMOUNT IS PACK-ED                            
         ZAP   Z2GRSAMT,DUB                                                     
*                                                                               
         MVC   Z2PID,SVPASSWD      SEND PID NUMBER                              
*                                                                               
         LA    R2,PAYINV1H                                                      
         MVC   ZINV(10),8(R2)                                                   
         OC    ZINV(12),SPACES                                                  
*                                                                               
         GOTO1 AADDREQ                                                          
*                                                                               
* NOW CORRECT THE OLD ENTRY                                                     
         SPACE 1                                                                
         MVC   ZAMTTYPE,AMTTYPE                                                 
         LA    R2,PAYCMT1H                                                      
         TM    AMTFLAGS,X'20'      COMMENT                                      
         BZ    REV190                                                           
*                                                                               
REV130   MVI   ZCONT,0                                                          
         LA    R5,ZCOM1                                                         
*                                                                               
REV132   MVC   0(40,R5),8(R2)                                                   
         OC    0(40,R5),SPACES                                                  
         LA    R5,40(R5)                                                        
         SPACE 1                                                                
         LA    R8,AMTLEN(R8)       NEXT AMOUNT                                  
         CLI   AMTFLAGS,0          MORE COMMENT                                 
         BE    REV190              NO                                           
*                                                                               
         LLC   R0,0(R2)            NEXT LINE                                    
         AR    R2,R0                                                            
         CLI   AMTFLAGS,X'20'      NEXT LINE COMMENT ONLY                       
         BE    REV132                                                           
*                                                                               
REV190   GOTO1 AADDREQ                                                          
*                                                                               
         CLI   PASS,C'A'           TEST ACC POSTING PASS                        
         BNE   REV195              NO                                           
         GOTO1 =A(GOTOACC),RR=RELO SWITCH TO ACC                                
         MVI   PASS,C'S'           SET FOR SPOT PASS                            
         L     R8,ASVAMTS                                                       
         B     REV115              AND PROCESS REQUESTS AGAIN                   
         SPACE 1                                                                
REV195   NI    PAYOPH+4,X'DF'        FORCE HEADLINE EDIT                        
         MVC   PAYOP(3),=C'***'                                                 
         FOUT  PAYOPH                                                           
*                                                                               
         L     R8,ASVAMTS                                                       
         LA    R2,PAYOPH                                                        
         OI    6(R2),X'40'         POSTION CURSOR                               
         MVC   PAYMSG,SPACES                                                    
         MVC   PAYMSG(16),=C'CR CHANGED TO CK'                                  
         CLI   AMTTYPE,C'2'        CR                                           
         BE    *+10                                                             
         MVC   PAYMSG(16),=C'CK CHANGED TO CR'                                  
         FOUT  PAYMSGH                                                          
         MVI   REVSW,C'N'                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
INITL    DS    0H                                                               
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
* CLEAR WORK AREA                                                               
         LA    R4,8(RC)                                                         
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LA    R0,256                                                           
INIT2    CR    R5,R0                                                            
         BL    INIT4                                                            
         XC    0(256,R4),0(R4)                                                  
         AR    R4,R0                                                            
         SR    R5,R0                                                            
         B     INIT2                                                            
INIT4    BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)  * EXECUTED *                                      
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         LM    R2,R4,0(R1)                                                      
         ST    R2,VTIOB            A(TRANSLATOR IO BLOCK)                       
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD          A(FIRST INPUT FIELD HDR)                     
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD          A(LAST INPUT FIELD HDR)                      
         MVC   NUMFLD,4(R2)        NUMBER OF INPUT FIELDS                       
*                                                                               
         ST    R3,VTWA                                                          
         MVC   VDATAMGR(80),0(R4)  FACILITY LIST                                
*                                                                               
         L     RF,12(R1)           A(TIA)                                       
         ST    RF,VTIA                                                          
*                                                                               
         MVC   VCOMFACS,16(R1)     SET COMFACS ADDRESS                          
*                                                                               
         L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         LLC   R0,TIOBAID                                                       
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY                          
*                                                                               
         LR    RA,R3                                                            
         MVC   AGYALPHA,14(RA)                                                  
         LA    R3,64(R3)           PRESET ERROR MSG ADDRESS                     
         ST    R3,ERRAREA                                                       
         MVI   DMINBTS,X'C0'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVC   AREC,AREC1                                                       
* SET UP COMMON FACILITY LINKAGES                                               
         LA    R6,SPCOMMON                                                      
         SR    R1,R1                                                            
         LA    R8,ERROR                                                         
         LA    R0,SPCOMCNT                                                      
*                                                                               
INIT10   ST    R6,0(R8)                                                         
         STC   R1,0(R8)                                                         
         LA    R1,4(R1)                                                         
         LA    R8,4(R8)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         L     R0,=V(ACPAYSPT)                                                  
         A     R0,RELO                                                          
         ST    R0,VACPAYSP                                                      
*                                                                               
         L     R0,=A(ADDREQ)                                                    
         A     R0,RELO                                                          
         ST    R0,AADDREQ                                                       
*                                                                               
         LHI   R0,SVAMTS-T213FFD                                                
         AR    R0,R3                                                            
         ST    R0,ASVAMTS                                                       
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
SPCOMMON NTR1  BASE=BASERB,LABEL=NO                                             
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         L     RE,4(RD)                                                         
         MVC   0(4,RE),=C'+SPC'    FZSA                                         
*                                                                               
         SRL   RF,24                                                            
         B     SPCOMTAB(RF)                                                     
*                                                                               
SPCOMTAB B     SPERROR             X'00'                                        
         B     SPANY               X'04'                                        
         B     SPMOVE              X'08'                                        
         B     SPPACK              X'0C'                                        
         B     SPREAD              X'10'                                        
         B     SPSEQ               X'14'                                        
         B     SPHIGH              X'18'                                        
         B     SPADD               X'1C'                                        
         B     SPDIR               X'20'                                        
         B     SPRDSTA             X'24'                                        
         B     SPSTA               X'28'                                        
         B     SPGETREC            X'2C'                                        
         B     SPPUTREC            X'30'                                        
         B     SPADDREC            X'34'                                        
         B     SPFIL               X'3C'                                        
         DC    5AL4(0)             X'40/44/48/4C/50' RESERVED                   
SPCOMUSR DC    9AL4(0)   ** USER ROUTINES ORIGIN HERE WITH X'54' **             
SPCOMCNT EQU   (*-SPCOMTAB)/4      NUMBER OF ENTRIES                            
         SPACE 2                                                                
SPCOMXIT XIT1                                                                   
         EJECT                                                                  
SPERROR  DS    0H                                                               
         CLI   ERRAREA,X'FF'       TEST MESSAGE ALREADY PRESENT                 
         BE    SPERRX                                                           
*                                                                               
         CLI   ERRCD,NEWERRS       IS THIS A 2 CHAR ERROR                       
         BNE   SPERROR1                                                         
*                                                                               
         CLI   ERRAREA,X'FE'                                                    
         BE    *+8                                                              
         MVI   ERRAREA,X'FF'                                                    
         XC    WORK,WORK           DEFINE CONTROL BLOCK                         
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         MVI   GTMTYP,GTMERR       SET MESSAGE TYPE TO ERRO                     
         MVC   GTMSGNO,NERRCD      AND MESSAGE NUMBER                           
         MVI   GTMSYS,2            AND MESSAGE SYSTEM                           
         LA    RE,ERRTEXT          AREA FOR APPENDED ERR TEXT                   
         CLI   0(RE),C' '          TEST IF THERE IS ANY TEXT                    
         BL    *+12                                                             
         ST    RE,GTATXT-1                                                      
         MVI   GTLTXT,L'ERRTEXT                                                 
         DROP  R1                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),WORK           PUT OUT SYSTEM MESSAGE                       
*                                                                               
         CLI   ERRAREA,X'FE'       TEST TO DO $ABEND                            
         BNE   SPERRX                                                           
         DC    H'0',C'$ABEND'                                                   
         EJECT                                                                  
SPERROR1 MVC   DMCB+20(4),VDATAMGR                                              
         L     R4,ERRAREA                                                       
         GOTO1 VGETMSG,DMCB+12,(ERRCD,8(R4)),(2,DMCB)                           
*                                                                               
SPERRX   OI    6(R2),X'40'         POSITION CURSOR                              
         MVI   ERRAREA,X'FF'                                                    
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         L     RD,BASERD           RETURN TO * BASE *                           
         LM    RE,RC,12(RD)        ** RETURN TO BASE **                         
         BR    RE                                                               
         SPACE 2                                                                
SPANY    CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         MVI   ERRCD,1                                                          
         B     SPERROR                                                          
*                                                                               
ANY2     TM    4(R2),X'10'                                                      
         BZ    SPCOMXIT                                                         
         MVI   ERRCD,3                                                          
         B     SPERROR                                                          
         SPACE 2                                                                
SPPACK   SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1               EXIT ON ZERO LENGTH                          
         BZ    PACKX                                                            
         TM    4(R2),X'08'         OR NON-NUMERIC                               
         BZ    PACKX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+12                                                          
         CVB   R0,DUB                                                           
         B     PACKX                                                            
         PACK  DUB,8(0,R2)   * EXECUTED *                                       
*                                                                               
PACKX    XIT1  REGS=(R0,R1)                                                     
         SPACE 2                                                                
SPMOVE   MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    SPCOMXIT                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PACKX                                                            
*                                                                               
         MVC   WORK(0),8(R2) * EXECUTED *                                       
         EJECT                                                                  
SPREAD   MVC   COMMAND,=C'DMREAD'                                               
         MVI   GBYACT,C'R'                                                      
         B     SPDIR                                                            
SPSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         MVI   GBYACT,C'S'                                                      
         B     SPDIR                                                            
SPHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         MVI   GBYACT,C'H'                                                      
         MVC   KEYSAVE,KEY                                                      
         B     SPDIR                                                            
SPADD    MVC   COMMAND,=C'DMADD'                                                
         MVI   GBYACT,C'B'                                                      
         B     SPDIR                                                            
*                                                                               
SPWRITE  MVC   COMMAND,=C'DMWRT'                                                
*                                                                               
SPDIR    OI    DMINBTS,X'80'       SET READ FOR UPDATE                          
         CLI   RDUPDATE,C'Y'                                                    
         BNE   *+8                                                              
         NI    DMINBTS,X'7F'                                                    
*                                                                               
         CLI   KEY,X'10'           TEST BUYREC KEY                              
         BH    SPDIR2              YES                                          
         MVI   GBYACT,0            ELSE SET NO GETBUY CALL NEEDED               
         B     SPDIR6                                                           
*                                                                               
SPDIR2   XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBYDMOUT,DMOUTBTS                                                
         LA    RE,KEY                                                           
         ST    RE,GBYKEYIN                                                      
         ST    RE,GBYKEYOT                                                      
         MVC   GBYCOMF,VCOMFACS                                                 
         MVC   GBY1OR2,SV1OR2                                                   
*                                                                               
         GOTO1 VGETBUY,GETBLK                                                   
         B     SPDIRX                                                           
*                                                                               
SPDIR6   GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTDIR',KEY,KEY               
*                                                                               
SPDIRX   TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    SPDIRX2             NO ERROR                                     
         CLI   COMMAND+2,C'R'      TEST READ COMMAND                            
         BE    *+6                                                              
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERROR            
*                                                                               
SPDIRX2  MVI   RDUPDATE,C'N'                                                    
         MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,8(R1)                                                       
         BZ    SPCOMXIT                                                         
* DATAMGR ERROR HAS OCCURRED                                                    
         MVI   ERRCD,0                                                          
         B     SPERROR                                                          
         SPACE 2                                                                
SPRDSTA  MVC   COMMAND,=C'DMREAD'                                               
*                                                                               
SPSTA    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AREC             
*                                                                               
         B     SPDIRX                                                           
         SPACE 2                                                                
SPGETREC MVC   COMMAND,=C'GETREC'                                               
         MVI   GBYACT,C'G'                                                      
         CLI   KEY,X'10'           TEST BUYREC (KEY MUST BE VALID)              
         BH    *+8                                                              
         MVI   GBYACT,0                                                         
         B     SPFIL                                                            
*                                                                               
SPPUTREC MVC   COMMAND,=C'PUTREC'                                               
         MVI   GBYACT,C'P'                                                      
         L     RE,AREC                                                          
         CLI   0(RE),X'10'         TEST BUYREC                                  
         BL    SPPUT2                                                           
         BRAS  RE,TSTLOCK                                                       
         BE    SPPUT2                                                           
         MVI   ERRAREA,X'FE'       FORCE DC H'0',C'$ABEND' TO UNWIND            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DATALOCK)                                              
         B     SPERROR                                                          
*                                                                               
SPPUT2   XC    DMCB+20(4),DMCB+20  MAKE SURE DM6 CLEAR                          
         L     RE,AREC                                                          
         CLI   0(RE),X'10'                                                      
         BH    *+8                                                              
         MVI   GBYACT,0                                                         
         B     SPFIL                                                            
*                                                                               
SPADDREC MVC   COMMAND,=C'ADDREC'                                               
         MVI   GBYACT,0                                                         
         L     RE,AREC                                                          
         CLI   0(RE),X'10'         TEST BUYREC                                  
         BL    *+8                                                              
         MVI   GBYACT,C'A'                                                      
*                                                                               
SPFIL    OI    DMINBTS,X'80'       SET READ FOR UPDATE                          
         CLI   RDUPDATE,C'Y'                                                    
         BE    *+8                                                              
         NI    DMINBTS,X'7F'                                                    
*                                                                               
         CLI   GBYACT,0            TEST BUYREC I/O                              
         BE    SPFIL2                                                           
         XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBYDMOUT,DMOUTBTS                                                
         LA    RE,KEY+14                                                        
         ST    RE,GBYDA                                                         
         MVC   GBYIOA,AREC                                                      
         LA    RE,DMWORK                                                        
         ST    RE,GBYDMWRK                                                      
         MVC   GBYPRDL,DMCB+20                                                  
         MVC   GBYCOMF,VCOMFACS                                                 
         MVC   GBY1OR2,SV1OR2                                                   
*                                                                               
         GOTO1 VGETBUY,GETBLK                                                   
         B     SPFIL4                                                           
*                                                                               
SPFIL2   GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTFILE',KEY+14,AREC,X        
               DMWORK                                                           
         MVI   RDUPDATE,C'N'       ALWAYS RESET!                                
*                                                                               
SPFIL4   CLI   COMMAND,C'G'        TEST GETREC                                  
         BE    SPDIRX2                                                          
         TM    8(R1),X'D0'         TEST EOF OR ERROR                            
         BZ    SPDIRX2                                                          
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERRORS           
         EJECT                                                                  
         ORG   SPCOMUSR                                                         
         B     ICL                 USER 1                                       
         B     CHK                      2                                       
         B     TEST                     3                                       
         B     GETRT                    4                                       
         B     GOPAYXFR                 5                                       
         B     GOBLDST                  6                                       
         B     GOBLDPST                 7                                       
         ORG                                                                    
         SPACE 2                                                                
ICL      DS    0H                  ** USER 1 **                                 
         CLI   SVREQOPT,C'N'       TEST TO SUPPRESS T/A                         
         BE    ICLX                YES -EXIT                                    
         CLI   PAYMD,C'N'                                                       
         BNE   ICL1                                                             
         CLI   SVAPROF+7,C'C'                                                   
         BE    DN                                                               
ICL1     CLI   SVNETPAK,C'Y'                                                    
         BE    ICLX                                                             
         TM    SVTSTOPT,X'10'      TEST PAID OPTION ACTIVE                      
         BO    ICLX                                                             
*                                                                               
         GOTO1 =A(BLDREQ),RR=RELO                                               
*                                                                               
         MVC   ZTYPE,=C'21'        DISABLED BELOW  MHER 20OCT09                 
* SUM INVOICE AMOUNTS                                                           
         LHI   R0,MAXAMTS                                                       
         L     R8,ASVAMTS                                                       
         USING AMTD,R8                                                          
         SR    RE,RE                                                            
ICL2     L     RF,AMT                                                           
         CLI   AMTTYPE,C'1'                                                     
         BE    *+6                                                              
         LCR   RF,RF                                                            
         AR    RE,RF                                                            
         L     RF,AMTGST                                                        
         LA    R2,10                                                            
         LA    R1,AMTPST           ADD IN PST                                   
*                                                                               
ICL1A    A     RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R2,ICL1A                                                         
         CLI   AMTTYPE,C'1'                                                     
         BE    *+6                                                              
         LCR   RF,RF                                                            
         AR    RE,RF                                                            
         LA    R8,AMTLEN(R8)                                                    
         BCT   R0,ICL2                                                          
         LR    R0,RE                                                            
         CVD   R0,DUB                                                           
         UNPK  ZICLAMT,DUB                                                      
         MVC   ZUESTOR,PAYER                                                    
         OC    ZUESTOR,SPACES                                                   
*                                                                               
         CLI   SVREQOPT,C'M'       WANT INVOICE MATCHING REPORT                 
         BE    IMR                                                              
*&&DO*&& GOTO1 AADDREQ             DO NOT ADD 21 REQUEST                        
ICLX     DS    0H                                                               
         LA    R2,PAYMDH                                                        
         OC    SVID,SVID                                                        
         BZ    *+8                                                              
         LA    R2,PAYOPH                                                        
         OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
         BRAS  RE,DISCMSG                                                       
         MVC   PAYMSG(49),0(R1)                                                 
         CLI   SVREQOPT,C'M'                                                    
         BE    *+10                                                             
         XC    PAYMSG+20(29),PAYMSG+20  CLEAR OUT DISC RPT REQSTD               
*                                                                               
         CLI   SVAUTPAY,C'Y'       AUTOPAY - GOOF REAL ERROR                    
         BNE   ICLXX                                                            
         CLI   ERRCD,MSSNGAFD      IF ERROR ALREADY SET                         
         BE    SPCOMXIT            USE THAT INSTEAD                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DISCREQD)                                              
         GOTO1 ERROR                                                            
*                                                                               
ICLXX    TM    SVTSTOPT,X'10'      TEST PAID OPTION ACTIVE                      
         BZ    *+14                                                             
         BRAS  RE,TOTSDSPL                                                      
         MVC   PAYMSG(20),0(R1)                                                 
         OC    SVTST,SVTST                                                      
         BZ    SPCOMXIT                                                         
         SPACE 1                                                                
* DISPLAY TOTALS FOR TEST OPTION                                                
         SPACE 1                                                                
         MVC   PAYMSG+20(29),SPACES                                             
         MVC   PAYMSG+22(2),=C'G='                                              
         LA    R4,PAYMSG+24                                                     
         L     R0,TOTG                                                          
         A     R0,TOTGST                                                        
         LA    R1,TOTPST                                                        
         LA    R5,10                                                            
*                                                                               
TST1     A     R0,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R5,TST1                                                          
*                                                                               
         BAS   RE,PAYEDT                                                        
         AR    R4,R0                                                            
         MVC   2(2,R4),=C'N='                                                   
         LA    R4,4(R4)                                                         
         L     R0,TOTN                                                          
         A     R0,TOTGST                                                        
         LA    R1,TOTPST                                                        
         LA    R5,10                                                            
*                                                                               
TST2     A     R0,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R5,TST2                                                          
*                                                                               
         BAS   RE,PAYEDT                                                        
         B     SPCOMXIT                                                         
         EJECT                                                                  
IMR      DS    0H                                                               
         MVC   ZAMT(10),SPACES                                                  
         ST    R0,DUB                                                           
         MVC   ZAMT(4),DUB         SET AMOUNT IN BINARY                         
         MVC   ZTYPE,=C'U1'                                                     
         MVC   ZPGR(2),SPACES                                                   
         MVC   ZMODE,SPACES                                                     
         MVC   ZAREA+34(3),ZPRD2   MOVE PRD2 FOR GRANT                          
         MVC   ZPRD2(3),SPACES     AND BLANK OLD AREA                           
         GOTO1 AADDREQ                                                          
         B     ICLX                                                             
         SPACE 2                                                                
DN       GOTO1 =A(BLDREQ),RR=RELO                                               
*                                                                               
         CLI   SVKEY+9,0           CONVERT 'NO' EST TO SERIES                   
         BNE   *+10                                                             
         MVC   ZEST(6),=C'001255'                                               
*                                                                               
         MVC   ZTYPE,=C'DN'                                                     
         MVC   ZREP(4),SPACES                                                   
         SR    R0,R0                                                            
         ICM   R0,3,SVKEY+11                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ZAREA+61(3),DUB                                                  
         MVI   ZAREA+64,C'U'                                                    
         MVC   ZUESTOR,PAYER                                                    
         OC    ZUESTOR,SPACES                                                   
         GOTO1 AADDREQ                                                          
         B     ICLX                                                             
         DROP  R8                                                               
         EJECT                                                                  
CHK      DS    0H                  ** USER 2 **                                 
         XC    ASVADJ,ASVADJ                                                    
         CLI   SVPPROFB+5,0        TEST PAY INVOICE AMOUNTS                     
         BE    CHK1X               NO                                           
*                                                                               
         L     R8,ASVAMTS                                                       
         USING AMTD,R8                                                          
         LA    R0,MAXAMTS                                                       
         L     RF,FILEDIFF                                                      
         LPR   RF,RF                                                            
*                                                                               
CHK1A    L     RE,AMTNET           THEY ARE PAYING NET                          
         LPR   RE,RE                                                            
         CR    RE,RF               INPUT AMOUNT TO ADJUSTMENT AMOUNT            
         BH    CHK1B               BIG ENOUGH TO ADJUST                         
         AHI   R8,AMTLEN           ELSE TRY NEXT AMOUNT                         
         BCT   R0,CHK1A                                                         
*                                                                               
         L     R8,ASVAMTS          IF NO AMOUNT WORKS, POINT TO FIRST           
*                                                                               
CHK1B    ST    R8,ASVADJ           AND SAVE ADDR OF AMT TO ADJUST               
*                                                                               
CHK1X    MVI   PASS,0                                                           
         CLI   SVPPROFA+3,C'Y'     TEST AUTO ACC POSTING                        
         BNE   CHK2                                                             
         BAS   RE,INITACC          INITIALIZE FOR ACC PASS                      
*                                                                               
CHK2     XC    PSRDATA,PSRDATA     CLEAR PAYMENT SPLIT DATA                     
         LA    R4,PSRDATA          INITIALIZE POINTER TO FIRST REP              
         ST    R4,PSRPTR             AND SAVE IT                                
*                                                                               
         OC    SVSPREP,SVSPREP     TEST SPECIAL REP PAYMENT                     
         BZ    CHK3                NO                                           
         CLI   SVPPROFA+1,C'Y'     TEST FEATURE ENABLED                         
         BNE   CHK3                NO                                           
         BRAS  RE,GETPSR           YES - TRY TO GET SPLIT DATA                  
*                                                                               
CHK3     DS    0H                                                               
         OC    TOTGST,TOTGST       TEST ANY GST INVOLVED                        
         BZ    CHK4                NO                                           
         BAS   RE,CALCGST          GO SPLIT PAYMENTS/GST AMOUNTS                
         SPACE 1                                                                
*======================================================*                        
* NET DOWN GROSS AMOUNTS (WHICH NO LONGER INCLUDE GST) *                        
*======================================================*                        
         SPACE 1                                                                
CHK4     BAS   RE,CALCPST                                                       
*                                                                               
         CLI   SVPPROF+0,C'G'                                                   
         BNE   CHK10                                                            
         OC    TOTG,TOTG                                                        
         BZ    CHK10                                                            
*                                                                               
         LA    R0,MAXAMTS          SAVE GROSS INV AMOUNTS                       
         L     R8,ASVAMTS          BEFORE NETTING THEM DOWN                     
         USING AMTD,R8                                                          
         MVC   AMTGRS,AMT                                                       
         AHI   R8,AMTLEN                                                        
         BCT   R0,*-10                                                          
*                                                                               
         LA    R0,MAXAMTS          NET DOWN GROSS INV AMOUNTS                   
         L     R8,ASVAMTS                                                       
*                                                                               
CHK6     L     R5,AMT                                                           
         AR    R5,R5               X 2                                          
         M     R4,TOTN                                                          
         D     R4,TOTG                                                          
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AHI   R5,1                                                             
         SRA   R5,1                                                             
         ST    R5,AMT                                                           
         AHI   R8,AMTLEN                                                        
         BCT   R0,CHK6                                                          
* SUM NET INVOICES                                                              
         LA    R0,MAXAMTS                                                       
         L     R8,ASVAMTS                                                       
         SR    RE,RE                                                            
*                                                                               
CHK7     L     RF,AMT                                                           
         CLI   AMTTYPE,C'1'                                                     
         BE    *+6                                                              
         LCR   RF,RF                                                            
         AR    RE,RF                                                            
         AHI   R8,AMTLEN                                                        
         BCT   R0,CHK7                                                          
         A     RE,TOTGST           ADD IN GST                                   
         LA    R0,10                                                            
         LA    R8,TOTPST           ADD IN PST                                   
*                                                                               
CHK8     A     RE,0(R8)                                                         
         LA    R8,4(R8)                                                         
         BCT   R0,CHK8                                                          
         ST    RE,TOTAMT                                                        
*                                                                               
CHK10    L     RE,TOTN                                                          
         A     RE,TOTGST                                                        
         LA    R0,10                                                            
         LA    R8,TOTPST           ADD IN PST                                   
*                                                                               
CHK11    A     RE,0(R8)                                                         
         LA    R8,4(R8)                                                         
         BCT   R0,CHK11                                                         
         ST    RE,DUB              SAVE FILE TOTAL INCL TAX                     
*                                                                               
         S     RE,TOTAMT                                                        
         BZ    CHK20                                                            
         LR    RF,RE               SAVE DISCREPANCY AMOUNT STUPIDO!             
                                                                                
* ADJUST FIRST NON CK AMT BY DIFFERENCE                                         
         LA    R0,MAXAMTS                                                       
         L     R8,ASVAMTS                                                       
*                                                                               
CHK12    OC    AMT,AMT             TEST AMOUNT=0                                
         JZ    CHK13               YES - NEVER ADJUST IT                        
         CLI   AMTTYPE,C'1'                                                     
         BE    CHK14                                                            
         CLI   AMTTYPE,C'2'                                                     
         BE    CHK16                                                            
*                                                                               
CHK13    AHI   R8,AMTLEN                                                        
         BCT   R0,CHK12                                                         
         B     CHK20                                                            
*                                                                               
CHK14    OC    ASVADJ,ASVADJ       TEST ACTUAL AMT TO ADJ PRESENT               
         BZ    CHK14X              NO                                           
         C     R8,ASVADJ           TEST THIS IS IT                              
         BNE   CHK13               NO - TRY AGAIN                               
*                                                                               
CHK14X   LR    RE,RF               RESTORE DISCREPANCY AMOUNT                   
         A     RE,AMT              ADJUST THIS AMOUNT                           
         BP    *+6                                                              
         SR    RE,RE               DO NOT ALLOW NEGATIVE AMOUNTS HERE!          
         ST    RE,DUB+4            ALWAYS SAVE ADJUSTED AMOUNT HERE             
         BNZ   CHK15               AMOUNT IS NOT 0                              
         CLI   SVPPROFB+3,C'Y'     ALLOW $0 CLEARANCES                          
         BNE   CHK13               DO NOT SET AMOUNT = 0                        
CHK15    ST    RE,AMT              UNLESS WE ALLOW $0 CLEARANCES                
         B     CHK20                                                            
*                                                                               
CHK16    LR    RE,RF               RESTORE DISCREPANCY AMOUNT                   
         S     RE,AMT              ADJUST CR AMT BY SUBTRACTION                 
         BM    *+6                                                              
         SR    RE,RE               DO NOT ALLOW NEGATIVE AMOUNTS HERE!          
         ST    RE,DUB+4            ALWAYS SAVE ADJUSTED AMOUNT HERE             
         BNZ   CHK17                                                            
         CLI   SVPPROFB+3,C'Y'     ALLOW $0 CLEARANCES                          
         BNE   CHK13               DO NOT SET AMOUNT = 0                        
CHK17    LCR   RE,RE               RESTORE PROPER SIGN                          
         ST    RE,AMT                                                           
         EJECT                                                                  
CHK20    CLI   SVPPROFB+3,C'Y'     ALLOW $0 CLEARANCES                          
         BE    CHK20X                                                           
                                                                                
* TRY NOT TO PUT ZERO CLEARANCES WHEN FILE TOTAL IS 0                           
                                                                                
         OC    DUB(4),DUB          TEST FILE TOTAL IS ZERO                      
         BNZ   CHK20X              NO                                           
*                                                                               
         OC    DUB+4(4),DUB+4      TEST FIRST AMOUNT NOW ZERO                   
         BNZ   CHK20X              NO                                           
*                                                                               
         L     R8,ASVAMTS                                                       
         AHI   R8,AMTLEN           POINT TO SECOND AMOUNT                       
         CLI   AMTTYPE,C'1'        TEST MORE THAN ONE AMOUNT                    
         BL    CHKX                NO                                           
*                                                                               
CHK20X   L     R8,ASVAMTS                                                       
         CLI   SVTRADE,C'Y'                                                     
         BE    CHK21               ALWAYS DO ONE TRADE CHECK                    
*                                                                               
         CLI   SVPPROFB+3,C'Y'     ALLOW $0 CLEARANCES                          
         BE    *+14                                                             
         OC    AMT,AMT             FIRST AMOUNT FIELD = 0                       
         BZ    CHKX                YES - EXIT                                   
*                                                                               
CHK21    GOTO1 =A(BLDREQ),RR=RELO                                               
         MVC   ZSTATUS(4),SPACES   CLEAR MARKET FIELD                           
         MVI   ZSTATUS,0           ORG'D OVER ZMKT                              
         MVI   ZSTATUS+1,0                                                      
         CLI   SVCXTRA+2,C'A'      TEST ID=MKTGRP                               
         BL    CHK22                                                            
         CLI   SVCXTRA+2,C'Z'                                                   
         BH    CHK22                                                            
         CLI   SVCXTRA+2,C'N'       N MEANS NO !                                
         BE    CHK22                                                            
         OC    SVID,SVID           IF PAYING BY ID                              
         BZ    CHK22                                                            
         OC    QMKT,QMKT                                                        
         BZ    CHK22                                                            
         PACK  DUB,QMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,ZBMKT          PASS MARKET IN BINARY                        
*                                                                               
CHK22    CLI   PASS,C'S'           TEST SPOT PASS                               
         BNE   *+8                                                              
         OI    ZSTATUS,X'80'       SET STATUS TO XFR'D                          
         CLI   SVAUTPAY,C'Y'                                                    
         BNE   *+8                                                              
         OI    ZSTATUS,ZSTATUS_AUTPAY                                           
         CLI   SVKEY+9,0           TEST PAYING BY ESTIMATE                      
         BE    *+10                                                             
         MVC   ZACCEST,ZEST        MOVE ESTIMATE FOR ACC                        
         SPACE 1                                                                
CHK22A   CLC   SVPPROF+14(2),=C'*A'  TEST ALT ACC AGENCY                        
         BNH   CHK22B                                                           
         CLC   SVPPROF+14(2),=C'00'  00 DOESN'T COUNT                           
         BE    CHK22B                                                           
         MVC   ZAGYCODE,SVPPROF+14                                              
         B     CHK22C                                                           
*                                                                               
CHK22B   CLC   SVCACCAG,=C'  '       IS THERE AN AGY CODE IN CLTHDR             
         BNH   CHK22C                                                           
         MVC   ZAGYCODE,SVCACCAG     YUP - USE IT                               
*                                                                               
CHK22C   MVC   ZTYPE,=C'30'                                                     
         MVC   ZDATE,TODAY                                                      
         MVC   ZOFFICE(1),SVOFFC   DEFAULT TO MEDIA OFFICE                      
         MVI   ZOFFICE+1,C' '                                                   
* IF ANY ACC OFFICE CODE, USE IT INSTEAD                                        
         CLI   SVCACCOF,C' '         TEST ACC OFFICE CODE PRESENT               
         BNH   CHK22D                NO                                         
         MVC   ZOFFICE(2),SVCACCOF   YES - THEN USE IT                          
*                                                                               
CHK22D   CLI   SVOFFCSW,C'Y'         TEST SPECIAL OFFC ACTIVE                   
         BNE   *+10                                                             
         MVC   ZOFFICE(1),SVPPROF+4  ** SET SPECIAL OFFICE CODE **              
         MVC   ZSEQNUM,STATSEQ     SET STATUS SEQUENCE NUMBER                   
*                                                                               
         CLI   SVPPROFA+1,C'Y'     TEST SPLIT PAYEE FEATURE                     
         BNE   CHK24A              NO                                           
         L     R4,PSRPTR                                                        
         OC    0(2,R4),0(R4)       TEST SPLIT IN EFFECT                         
         BZ    CHK24A              NO                                           
         SR    R0,R0                                                            
         ICM   R0,3,0(R4)          GET REP CODE                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ZREP,DUB                                                         
*                                                                               
         BAS   RE,GETPSRAD         UPDATE ADDRESS FOR THIS PAYEE                
*                                                                               
         L     R0,AMT              GET AMOUNT                                   
         XC    FULL,FULL           CLEAR ADJUSTMENT AMOUNT                      
         LA    RE,PSRDATA                                                       
         CR    R4,RE               TEST FIRST PSR                               
         BNE   *+8                                                              
         BRAS  RE,ADJPSR           ADJUST FIRST PSR AMOUNT                      
*                                                                               
         AR    R0,R0               X 2                                          
         ICM   R1,15,2(R4)         GET PERCENTAGE                               
         MR    R0,R0                                                            
         D     R0,=F'100000'                                                    
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRL   R1,1                ROUND                                        
         LR    R0,R1               PASS CLEARANCE AMOUNT                        
         S     R0,FULL             ADJUST FIRST REP AMOUNT                      
         XC    FULL,FULL           ONLY DO IT FOR FIRST REP                     
         SR    R1,R1               CLEAR GST AMOUNT STUPID                      
         B     CHK24B                                                           
*                                                                               
CHK24A   L     R0,AMT                                                           
         L     R1,AMTGST                                                        
         SPACE 1                                                                
CHK24B   CLI   AMTTYPE,C'2'        TEST CR                                      
         BNE   *+8                                                              
         LCR   R0,R0                                                            
         LCR   R1,R1                                                            
*                                                                               
         CLI   SVUNPAY,C'Y'                                                     
         BNE   CHK24D                                                           
         CLC   =C'UNCLEAR',PAYER                                                
         BE    CHK24C                                                           
         CLC   =C'REVCHECK',PAYER                                               
         BE    CHK24C                                                           
         B     CHK24D                                                           
*                                                                               
CHK24C   LCR   R0,R0                                                            
         LCR   R1,R1                                                            
*                                                                               
CHK24D   CVD   R0,DUB              AMOUNT IS PACK-ED                            
         ZAP   ZAMT,DUB                                                         
         ZAP   Z2PNET2,DUB         PL6 NET AMOUNT FIELD                         
         STCM  R1,15,ZGST         * NOW ITS IN BINARY 21JUN99 *                 
         MVC   ZGSTCD,GSTCODE                                                   
         MVC   ZAMTTYPE,AMTTYPE                                                 
         MVC   ZINV(10),AMTINV                                                  
         OC    ZINV,SPACES                                                      
*                                                                               
         BRAS  RE,SETPST           SET PST IN REQUEST                           
*                                                                               
         CLI   SVPPROFB+5,0        TEST PAYING INVOICE AMTS                     
         BE    CHK24C1             NO                                           
*                                                                               
         L     R0,AMTUNADJ         GET UNADJUSTED DOLLARS                       
         CLI   AMTTYPE,C'2'        TEST CR                                      
         BNE   *+6                                                              
         LNR   R0,R0               MAKE IT A NEGATIVE NUMBER                    
*                                                                               
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+6                                                              
         LCR   R0,R0                                                            
*                                                                               
         CVD   R0,DUB                                                           
         ZAP   Z2INVAMT,DUB        SET ACTUAL INVOICE AMOUNT                    
*                                                                               
         C     R8,ASVADJ           TEST ADJUST THIS INVOICE                     
         BNE   CHK24C1                                                          
         L     R0,FILEDIFF         POST WHOLE DIFF TO THIS INVOICE              
*                                                                               
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+6                                                              
         LCR   R0,R0                                                            
*                                                                               
         CVD   R0,DUB                                                           
         ZAP   Z2DIFF,DUB                                                       
*                                                                               
CHK24C1  L     R0,AMTGRS           ACTUAL INVOICE GRS AMOUNT                    
         CLI   SVPPROF+0,C'G'      IF PAYING GROSS                              
         BNE   CHK24C2             NO - GO CALCULATE IT                         
*                                                                               
         CLI   AMTTYPE,C'2'        TEST CR                                      
         BNE   *+6                                                              
         LCR   R0,R0               COMPLEMENT                                   
         B     CHK24C7             OK                                           
*                                                                               
CHK24C2  L     R1,ASVAMTS                                                       
         CR    R1,R8               IS THIS THE FIRST INVOICE                    
         BNE   CHK24C3             NO THEN CALC GROSS                           
*                                                                               
         LA    R1,AMTLEN(R1)       IS THERE MORE THAN 1 INVOICE                 
         CLI   AMTFLAGS-AMTD(R1),0                                              
         BNE   CHK24C3             YES THEN CALC GROSS                          
         L     R0,TOTG             IF ONLY 1 INVOICE USE GRAND TOTAL            
         B     CHK24C6                                                          
*                                                                               
CHK24C3  DS    0H                  CALCULATE GROSS FROM NET*(TOTN/TOTG)         
         L     R0,AMT                                                           
         OC    TOTN,TOTN           CAN'T DIVIDE BY ZERO                         
         BZ    CHK24C6                                                          
         L     R5,AMT                                                           
         AR    R5,R5               X 2                                          
         M     R4,TOTG                                                          
         D     R4,TOTN                                                          
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AHI   R5,1                                                             
         SRA   R5,1                                                             
         LR    R0,R5                                                            
         OI    Z2FLAG,Z2FGRCAL     GROSS HAS BEEN CALCULATED                    
*                                                                               
         CLI   AMTTYPE,C'2'        TEST CR  <- IF CALCULATED X2                 
         BNE   *+6                             REMOVED SIGN                     
         LCR   R0,R0               COMPLEMENT                                   
         B     CHK24C7                                                          
*                                                                               
CHK24C6  CLI   AMTTYPE,C'3'        IF YOU DIDN'T CALCULATE THE GROSS            
         BNE   *+6                                                              
         LCR   R0,R0               COMPLEMENT CKS GROSS MEMO                    
*                                                                               
CHK24C7  CLI   SVUNPAY,C'Y'                                                     
         BNE   CHK24C9                                                          
         CLC   =C'UNCLEAR',PAYER                                                
         BE    CHK24C8                                                          
         CLC   =C'REVCHECK',PAYER                                               
         BE    CHK24C8                                                          
         B     CHK24C9                                                          
*                                                                               
CHK24C8  LCR   R0,R0               COMPLEMENT IF UNPAY                          
*                                                                               
CHK24C9  CVD   R0,DUB              AMOUNT IS PACK-ED                            
         ZAP   Z2GRSAMT,DUB                                                     
*                                                                               
         MVC   Z2PID,SVPASSWD      SEND PID NUMBER                              
*&&DO                                                                           
         CLI   SVTRADE,C'Y'                                                     
         BNE   CHK24E                                                           
         CLI   PASS,C'A'           CTA FOR PASS=S,0                             
         BE    CHK24E                                                           
         GOTO1 =A(SETCTA),RR=RELO  SET CTA CONTRACT AMOUNTS                     
*&&                                                                             
CHK24E   TM    AMTFLAGS,X'20'      TEST COMMENT THIS LINE                       
         BO    CHK26               YES                                          
* NO COMMENT                                                                    
         GOTO1 AADDREQ                                                          
         AHI   R8,AMTLEN                                                        
         CLI   AMTFLAGS,0                                                       
         BE    CHKX                                                             
         B     CHK21                                                            
*                                                                               
* COMMENT THIS LINE                                                             
*                                                                               
CHK26    LA    R5,ZCOM1            POINT TO FIRST COMMENT POSN                  
*                                                                               
CHK27    MVC   0(40,R5),AMTCOM     MOVE TO REQUEST                              
         CLC   =C'//',0(R5)                                                     
         BNE   CHK28                                                            
         MVC   0(38,R5),AMTCOM+2                                                
         MVI   38(R5),C' '                                                      
         MVI   39(R5),C' '                                                      
*                                                                               
CHK28    OC    0(40,R5),SPACES                                                  
         LA    R5,40(R5)                                                        
*                                                                               
         AHI   R8,AMTLEN           NEXT AMOUNT                                  
         CLI   AMTFLAGS,X'20'      TEST NEXT LINE COMMENT ONLY                  
         BE    CHK27               NO                                           
*                                                                               
CHK29    GOTO1 AADDREQ                                                          
*                                                                               
         L     R0,ASVAMTS          START OF AMOUNTS                             
         AHI   R0,SVAMTX-SVAMTS    LENGTH TO END                                
         CR    R8,R0               TEST REACHED LAST AMOUNT FIELD               
         BNL   CHKX                YES- STOP                                    
         CLI   AMTFLAGS,0          TEST ANY MORE INPUT                          
         BNE   CHK21                                                            
*                                                                               
CHKX     DS    0H                                                               
         CLI   PASS,C'A'           TEST ACCOUNTING PASS                         
         BNE   CHKX2               NO                                           
         GOTO1 =A(GOTOACC),RR=RELO DO ACC POSTINGS                              
         MVI   PASS,C'S'           SET FOR SPOTPAK PASS                         
         B     CHK20                                                            
*                                                                               
CHKX2    CLI   SVPPROFA+1,C'Y'     TEST SPLIT PAYEE FEATURE                     
         BNE   CHKX4               NO                                           
         L     R4,PSRPTR                                                        
         LA    R4,6(R4)            NEXT SPLIT PAYEE                             
         ST    R4,PSRPTR                                                        
         OC    0(2,R4),0(R4)                                                    
         BZ    CHKX4                                                            
*                                                                               
         CLI   SVPPROFA+3,C'Y'     TEST AUTO ACC POSTING                        
         BNE   *+8                 NO                                           
         BAS   RE,INITACC          INITIALIZE FOR NEXT PAYEE                    
         B     CHK20               GO PROCESS NEXT PAYEE                        
                                                                                
*===============================================================                
* DISPLAY PAID AMOUNTS                                                          
*===============================================================                
                                                                                
CHKX4    LA    R2,PAYMDH                                                        
         OC    SVID,SVID                                                        
         BZ    *+8                                                              
         LA    R2,PAYOPH                                                        
         OI    6(R2),X'40'         POSITION CURSOR                              
         MVC   PAYMSG,SPACES                                                    
         MVC   PAYMSG(10),=C'** PAID **'                                        
*                                                                               
         CLI   SVAUTPAY,C'Y'                                                    
         BNE   CHKX5                                                            
         GOTO1 =A(SETAPY),RR=RELO  UPDATE AUTOPAY RECORDS                       
         B     CHKX20              AND EXIT                                     
*                                                                               
CHKX5    CLC   =C'FJWT$$',PAYER                                                 
         BNE   *+10                                                             
         MVC   PAYMSG+1(6),PAYER                                                
*                                                                               
         LA    R4,SVPKGEL                                                       
         XC    0(80,R4),0(R4)      CLEAR A WORK AREA                            
*                                                                               
         MVC   0(6,R4),=C'GROSS='                                               
         LA    R4,6(R4)                                                         
*                                                                               
         L     R0,TOTG                                                          
         A     R0,TOTGST                                                        
         LA    R1,TOTPST                                                        
         LA    R5,10                                                            
*                                                                               
CHKX6    A     R0,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R5,CHKX6                                                         
*                                                                               
         BAS   RE,PAYEDT                                                        
         AR    R4,R0                                                            
         MVC   1(4,R4),=C'NET='                                                 
         LA    R4,5(R4)                                                         
         L     R0,TOTN                                                          
         A     R0,TOTGST                                                        
         LA    R1,TOTPST                                                        
         LA    R5,10                                                            
*                                                                               
CHKX8    A     R0,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R5,CHKX8                                                         
*                                                                               
         BAS   RE,PAYEDT                                                        
         AR    R4,R0                                                            
*                                                                               
         CLI   SVPPROFB+5,0        TEST PAYING ACTUAL INV AMT                   
         BE    CHKX9               NO                                           
         MVC   1(4,R4),=C'DIF='                                                 
         LA    R4,5(R4)                                                         
         L     R0,FILEDIFF                                                      
         BAS   RE,PAYEDT                                                        
*                                                                               
CHKX9    MVC   PAYMSG+12(L'PAYMSG-12),SVPKGEL  MOVE MESSAGE TO SCREEN           
         LA    RE,PAYMSGH                                                       
         LLC   R0,0(RE)                                                         
         AR    RE,R0               POINT TO S/R FIELD                           
         XC    8(17,RE),8(RE)                                                   
         MVC   8(7,RE),=C'$AUTOSW'                                              
         CLI   SVAUTOSW,0                                                       
         BE    CHKX10                                                           
         MVC   8(10,RE),SVAUTOSW                                                
         B     CHKX16                                                           
*                                                                               
CHKX10   LA    R1,=C'+INV'                                                      
         CLI   SVPPROFA+0,C'I'    TEST RETURN TO INVOICE                        
         BE    CHKX12                                                           
         LA    R1,=C'+NIN'                                                      
         CLI   SVPPROFA+0,C'N'    TEST RETURN TO NEWINV                         
         BNE   CHKX16                                                           
*                                                                               
CHKX12   XC    8(17,RE),8(RE)                                                   
         MVC   8(4,RE),0(R1)                                                    
*                                                                               
CHKX16   CLC   AGYALPHA,=C'AP'     MAKE Z8 REQ IF AGY IS APPLE/LIBERTY          
         BE    CHKX18                                                           
         CLC   AGYALPHA,=C'LI'                                                  
         BNE   CHKX20                                                           
*                                                                               
CHKX18   GOTO1 =A(BLDZ8),RR=RELO                                                
*                                                                               
CHKX20   B     SPCOMXIT                                                         
         EJECT                                                                  
* INITIALIZE FOR ACC POSTING PASS *                                             
         SPACE 1                                                                
INITACC  DS    0H                                                               
         MVI   PASS,C'A'           SET FOR ACCPAK PASS                          
         L     R1,AREC2                                                         
         ST    R1,SVACCPTR         POINT TO START OF FIRST REQUEST              
         LA    R0,8                                                             
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
         L     R1,AREC2                                                         
         LA    R0,2                                                             
         STH   R0,0(R1)            SET INITIAL LENGTH                           
         BR    RE                                                               
         SPACE 2                                                                
PAYEDT   DS    0H                                                               
         EDIT  (R0),(13,(R4)),2,COMMAS=YES,MINUS=YES,ALIGN=LEFT,       X        
               ZERO=NOBLANK                                                     
         BR    RE                                                               
         EJECT                                                                  
*===================================================================*           
* FOR ON-LINE CLEARANCES ONLY, UPDATE SPLIT PAYEE ADDRESS DATA      *           
*===================================================================*           
         SPACE 1                                                                
GETPSRAD NTR1                                                                   
         CLI   SVPPROFA+3,C'Y'     TEST AUTO ACC POSTING                        
         BNE   GTPSRADX                                                         
         CLI   PASS,C'A'           TEST ACCOUNTING PASS                         
         BNE   GTPSRADX                                                         
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),PAYMD                                                   
         MVC   KEY+2(3),ZREP                                                    
         MVC   KEY+5(2),AGYALPHA                                                
*                                                                               
         MVC   COMMAND,=C'DMRDHI'                                               
         L     R6,AREC1                                                         
         ST    R6,AREC                                                          
         USING REPRECD,R6                                                       
         GOTO1 STA                                                              
         CLC   KEY(15),0(R6)                                                    
         BNE   GTPSRADX                                                         
*                                                                               
         MVC   PAYNME,RNAME                                                     
         LA    R4,PAYSTADD                                                      
         MVC   0(26,R4),SPACES                                                  
         MVC   0(24,R4),R1LINE                                                  
         LA    R4,26(R4)                                                        
         MVC   0(78,R4),SPACES                                                  
         MVC   0(24,R4),R2LINE                                                  
         MVC   26(2,R4),R3LINE                                                  
         MVC   52(10,R4),RBIGZIP                                                
         MVI   PAYCNTRY,C'U'                                                    
         CLC   =C'CANAD',RZIP                                                   
         BNE   *+8                                                              
         MVI   PAYCNTRY,C'C'                                                    
GTPSRADX XIT1                                                                   
         EJECT                                                                  
*=========================================================*                     
* SUBROUTINE SPLITS INVOICE AMOUNTS INTO STATION DOLLARS  *                     
* AND PST AMOUNTS. PST FOR EACH INVOICE IS CALCULATED     *                     
* BY   INVAMT*TOTAL_GST/TOTAL DOLLARS                     *                     
* THE TOTAL DOLLARS ARE GROSS OR NET BY AGENCY OPTION     *                     
*=========================================================*                     
         SPACE 1                                                                
CALCPST  NTR1                                                                   
         MVI   BYTE,0                                                           
         XC    DUB,DUB                                                          
         XC    WORK,WORK                                                        
         XC    WORK2,WORK2                                                      
         LA    R1,TOTPST                                                        
         SR    R2,R2                                                            
         LA    R4,10                                                            
*                                                                               
CPST5    A     R2,0(R1)            ACCUMULATE TOTAL PST FOR THIS INV            
         LA    R1,4(R1)                                                         
         BCT   R4,CPST5                                                         
         ST    R2,DUB                                                           
         L     R8,ASVAMTS                                                       
*                                                                               
CPST10   LA    R4,TOTPST                                                        
         LA    R5,10               10 PROVINCES                                 
*                                                                               
CPST20   L     R2,0(R4)            TOTAL PST AMOUNT FOR THIS PROVINCE           
         LTR   R2,R2               IF NO PST - CONTINUE                         
         BZ    CPST30                                                           
*                                                                               
         SR    R0,R0                                                            
         L     R1,AMT                                                           
         AR    R1,R1               X 2                                          
         MR    R0,R2               TIMES TOTAL PST FOR PROVINCE                 
         LTR   R1,R1                                                            
         BZ    CPST30                                                           
*                                                                               
         L     R6,TOTG                                                          
         CLI   SVPPROF+0,C'G'                                                   
         BE    *+8                                                              
         L     R6,TOTN                                                          
         A     R6,DUB              ADD IN TOTAL PST FOR THIS INVOICE            
         LTR   R6,R6                                                            
         BZ    CPST30                                                           
*                                                                               
         DR    R0,R6               DIVIDE BY FILE TOTAL                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
         ST    R1,FULL             PST FOR THIS INV/PROVINCE                    
         LA    R2,AMTPST                                                        
         LA    R1,10                                                            
         SR    R1,R5               INDEX INTO PROVINCE                          
         MHI   R1,4                                                             
         AR    R2,R1                                                            
         ST    R2,WORK2                                                         
         MVC   0(4,R2),FULL                                                     
         L     R1,FULL             KEEP A RUNNING TOTAL OF                      
         A     R1,DUB+4                                                         
         ST    R1,DUB+4            TOTAL PST FOR THIS INVOICE                   
*                                                                               
CPST30   LA    R4,4(R4)            NEXT PROVINCE                                
         BCT   R5,CPST20                                                        
*                                                                               
         L     R1,AMT              GET INVOICE AMOUNT                           
         L     R0,DUB+4            GET PST AMOUNT                               
         SR    R1,R0                                                            
         ST    R1,AMT              SET INVOICE WITHOUT PST                      
*                                                                               
         CLI   AMTTYPE,C'1'        WHEN ADDING IT UP, NEED SIGN                 
         BE    *+6                                                              
         LNR   R0,R0                                                            
         A     R0,WORK                                                          
         ST    R0,WORK                                                          
         XC    DUB+4(4),DUB+4                                                   
*                                                                               
CPST40   LA    R8,AMTLEN(R8)                                                    
         LLC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         CLI   BYTE,MAXAMTS                                                     
         BL    CPST10                                                           
*                                                                               
*        NOW MAKE SURE THAT TOTAL PST = ACCUMULATED PST OF ALL INVOICES         
*        DUB  = TOTAL PST                                                       
*        WORK = PST AMOUNT WRITTEN TO CLEARANCE                                 
*                                                                               
         CLC   DUB(4),WORK                                                      
         BE    CPSTX                                                            
         L     R8,ASVAMTS                                                       
         LA    R0,MAXAMTS                                                       
         SR    RF,RF               CLEAR ACCUM                                  
         ICM   R1,15,WORK2         SET POINTER TO BIGGEST PST                   
         BZ    CPSTX               IF NONE, EXIT                                
         LR    R4,R8               AND AMOUNT                                   
*                                                                               
CPST50   LA    R5,10                                                            
         LA    R2,AMTPST                                                        
*                                                                               
CPST60   L     RE,0(R2)                                                         
         CLI   AMTTYPE,C'1'                                                     
         BE    *+6                                                              
         LNR   RE,RE                                                            
         CR    RE,RF                                                            
         BNH   CPST70                                                           
         LR    R1,R2                                                            
         LR    RF,RE                                                            
         LR    R4,R8                                                            
*                                                                               
CPST70   LA    R2,4(R2)                                                         
         BCT   R5,CPST60                                                        
         LA    R8,AMTLEN(R8)                                                    
         BCT   R0,CPST50                                                        
*                                                                               
         LR    R8,R4               POINT TO BIGGEST                             
         L     R0,DUB              PST TOT PUT TO FILE                          
         LPR   R0,R0               SO GET THE ABSOLUTE VALUE                    
         L     RF,WORK             AND GET ABSOLUTE VALUE OF TOTAL PST          
         LPR   RF,RF                                                            
         SR    RF,R0               THEN GET THE DIFFERENCE                      
*                                                                               
         L     R0,0(R1)                                                         
         SR    R0,RF               ADJUST THE LARGEST PST AMOUNT                
         ST    R0,0(R1)            AND STORE IT BACK                            
*                                                                               
         L     R0,AMT                                                           
         AR    R0,RF                                                            
         ST    R0,AMT                                                           
*                                                                               
CPSTX    XIT1                                                                   
         EJECT                                                                  
*=========================================================*                     
* SUBROUTINE SPLITS INVOICE AMOUNTS INTO STATION DOLLARS  *                     
* AND GST AMOUNTS. GST FOR EACH INVOICE IS CALCULATED     *                     
* BY   INVAMT TOTAL/TOTAL DOLLARS * TOTAL GST             *                     
* THE TOTAL DOLLARS ARE GROSS OR NET BY AGENCY OPTION     *                     
*=========================================================*                     
         SPACE 1                                                                
CALCGST  NTR1                                                                   
         L     R1,TOTG             PICK UP THE DIVISOR                          
         CLI   SVPPROF+0,C'G'                                                   
         BE    *+8                                                              
         L     R1,TOTN             TOTAL AMOUNT                                 
         A     R1,TOTGST            PLUS TOTAL GST                              
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
*                                                                               
         LA    R8,TOTPST           NEED TO ADD QST TOTAL TOO SINCE              
         SR    R5,R5                INVOICE AMOUNTS HAVE GST AND QST            
         LA    R4,10                                                            
CALCGST1 A     R5,0(R8)            ACCUMULATE TOTAL QST                         
         LA    R8,4(R8)                                                         
         BCT   R4,CALCGST1                                                      
         AR    R1,R5               ADD TOT QST TO INV TOTAL + TOT GST           
         LTR   R1,R1                                                            
         BZ    CALCGST3                                                         
*                                                                               
         LA    R0,MAXAMTS                                                       
         L     R8,ASVAMTS                                                       
         SR    R4,R4                                                            
*                                                                               
CALCGST2 L     R5,AMT              GET INVOICE TOTAL DOLLARS                    
         AR    R5,R5               X 2                                          
         M     R4,TOTGST           X TOTAL GST                                  
         DR    R4,R1               DIVIDE BY TOTAL DOLLARS                      
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AHI   R5,1                                                             
         SRA   R5,1                                                             
         ST    R5,AMTGST           SAVE THE GST AMOUNT                          
         L     R4,AMT                                                           
         SR    R4,R5                                                            
         ST    R4,AMT              AND THE NEW INVOICE AMOUNT                   
*                                                                               
         LA    R8,AMTLEN(R8)                                                    
         BCT   R0,CALCGST2                                                      
         SPACE 1                                                                
*========================================================*                      
* NOW ADD THE GST AMOUNTS AND CHECK SUM TO TOTAL GST     *                      
* IF THEY ARE NOT EQUAL ADJUST THE LARGEST GST AMOUNT    *                      
* BY THE DIFFERENCE                                      *                      
* IT ALSO SEEMS OBVIOUS (NOW) THAT I SHOULD ALSO ADJUST  *                      
*   THE AMOUNT ITSELF AS WELL !!!                        *                      
* REMEMBER THAT ALL AMOUNTS IN TABLE ARE POSITIVE !      *                      
*========================================================*                      
                                                                                
CALCGST3 LA    R0,MAXAMTS                                                       
         L     R8,ASVAMTS                                                       
         LR    R1,R8               SET POINTER TO BIGGEST                       
         SR    RF,RF               CLEAR ACCUM                                  
*                                                                               
CALCGST4 L     RE,AMTGST                                                        
         CLI   AMTTYPE,C'1'                                                     
         BE    *+6                                                              
         LNR   RE,RE                                                            
         AR    RF,RE                                                            
         CLC   AMTGST,AMTGST-AMTD(R1)                                           
         BNH   *+6                                                              
         LR    R1,R8               SAVE POINTER TO LARGER                       
         LA    R8,AMTLEN(R8)                                                    
         BCT   R0,CALCGST4                                                      
*                                                                               
         LR    R8,R1               POINT TO BIGGEST                             
         L     R0,TOTGST           GST TOT FROM FILE IS SIGNED                  
         LPR   R0,R0               SO GET THE ABSOLUTE VALUE                    
         LPR   RF,RF               AND GET ABSOLUTE VALUE OF TOTAL GST          
         SR    RF,R0               THEN GET THE DIFFERENCE                      
*                                                                               
         L     R0,AMTGST                                                        
         SR    R0,RF               ADJUST THE LARGEST GST AMOUNT                
         ST    R0,AMTGST           AND STORE IT BACK                            
*                                                                               
         L     R0,AMT                                                           
         AR    R0,RF                                                            
         ST    R0,AMT                                                           
         B     EXIT                                                             
         EJECT                                                                  
GETRT    DS    0H                  ** USER 4 **                                 
         GOTO1 VGETRATE,(R1)                                                    
         B     SPCOMXIT                                                         
*                                                                               
GOPAYXFR DS    0H                  ** USER 5 **                                 
         GOTO1 =V(ACPAYXFR),(R1),RR=RB                                          
         CLI   SVAUTPAY,C'Y'       AUTOPAY?                                     
         BNE   SPCOMXIT            NO                                           
         CLI   8(R1),0                                                          
         BE    SPCOMXIT                                                         
         MVI   ERRCD,NEWERRS       YES MUST USE STANDARD ERROR                  
         MVC   NERRCD,=Y(PAYPGERR)     SO THE SCRIPT PICKS IT UP                
         GOTO1 ERROR                                                            
         B     SPCOMXIT                                                         
         EJECT                                                                  
TEST     CLI   TESTLN,X'FF'        ** USER 3 **                                 
         BE    SPCOMXIT                                                         
         CLI   SVSCRN,X'FE'        TEST 'TEST' SCREEN LOADED                    
         BNE   SPCOMXIT                                                         
         ICM   R2,15,TESTLN                                                     
         BNZ   TEST4                                                            
         LA    R2,PAYHLH                                                        
         LLC   R0,0(R2)            SKIP PAST TITLES                             
         AR    R2,R0                                                            
*                                                                               
TEST2    ST    R2,TESTLN                                                        
         FOUT  (R2)                                                             
*                                                                               
TEST4    L     R4,TESTADDR                                                      
         LTR   R4,R4                                                            
         BNZ   *+12                                                             
         LA    R4,8(R2)                                                         
         B     *+8                                                              
         LA    R4,20(R4)                                                        
         ST    R4,TESTADDR                                                      
         LA    R0,75+8(R2)                                                      
         CR    R4,R0               TEST ROOM THIS LINE                          
         BNH   TEST10              YES                                          
         XC    TESTADDR,TESTADDR                                                
         LLC   R0,0(R2)            NO - TRY NEXT LINE                           
         AR    R2,R0                                                            
         CLI   0(R2),86                                                         
         BE    TEST2                                                            
         MVI   TESTLN,X'FF'                                                     
         B     SPCOMXIT                                                         
*                                                                               
TEST10   MVC   0(7,R4),WORK                                                     
*                                                                               
         L     R0,BUYTOTG                                                       
         TM    SVTSTOPT,X'40'      TEST EXPLICIT REQ TO DISPLAY GROSS           
         BO    TEST12                                                           
*                                                                               
         L     R0,BUYTOTN                                                       
         TM    SVTSTOPT,X'20'      TEST REQUEST TO DISPLAY NET                  
         BO    TEST12                                                           
*                                                                               
         L     R0,BUYTOTG          NO OVERRIDE - DISPLAY PER PROFILE            
         CLI   SVPPROF+0,C'G'                                                   
         BE    TEST12                                                           
         L     R0,BUYTOTN                                                       
*                                                                               
TEST12   DS    0H                                                               
         CLI   SVGSTOPT,C'Y'                                                    
         BNE   *+12                                                             
         A     R0,BUYTGST                                                       
         A     R0,BUYTPST                                                       
         EDIT  (R0),(11,7(R4)),2,MINUS=YES,ZERO=NOBLANK                         
         EJECT                                                                  
* FLOAT COST CHAR TO LEFT OF COST                                               
         L     R8,AREC                                                          
         USING BUYRECD,R8                                                       
*                                                                               
         TM    BDCIND2,X'04'       TEST BDCIND IS A CHARACTER                   
         BZ    TEST12A                                                          
*                                                                               
         CLI   BDCIND,BDCGRS       GROSS                                        
         BE    TEST14                                                           
         B     TEST12B                                                          
*                                                                               
TEST12A  TM    BDCIND,X'20'                                                     
         BO    TEST14                                                           
                                                                                
* FIND SPOT FOR INSERTION                                                       
                                                                                
TEST12B  LA    R1,16(R4)           POINT TO LAST COST CHAR                      
         LA    R0,11                                                            
         CLI   0(R1),C' '                                                       
         BE    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         MVC   0(1,R1),BDCIND                                                   
         TM    BDCIND2,X'04'       TEST BDCIND IS A CHARACTER                   
         BNZ   TEST14                                                           
*                                                                               
         MVI   0(R1),C'F'                                                       
         TM    BDCIND,X'80'                                                     
         BO    TEST14                                                           
         MVI   0(R1),C'Q'                                                       
         TM    BDCIND,X'40'                                                     
         BO    TEST14                                                           
         MVI   0(R1),C'N'                                                       
         TM    BDCIND,X'10'                                                     
         BO    TEST14                                                           
         MVI   0(R1),C'V'                                                       
         TM    BDCIND,X'08'                                                     
         BO    TEST14                                                           
         MVI   0(R1),C'S'                                                       
         TM    BDCIND,X'04'                                                     
         BO    TEST14                                                           
         MVI   0(R1),C'X'                                                       
         TM    BDCIND,X'02'                                                     
         BO    TEST14                                                           
         MVI   0(R1),C'P'                                                       
TEST14   DS    0H                                                               
         B     SPCOMXIT                                                         
         DROP  R8                                                               
         EJECT                                                                  
GOBLDST  BRAS  RE,BLDST                                                         
         B     SPCOMXIT                                                         
*                                                                               
GOBLDPST BRAS  RE,BLDPST                                                        
         B     SPCOMXIT                                                         
         LTORG                                                                  
         EJECT                                                                  
BLDZ8    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ZCTL,ZCTL                                                        
         LA    R0,5                                                             
         LA    R1,ZAREA                                                         
         MVC   0(80,R1),SPACES                                                  
         LA    R1,80(R1)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,ZAREA                                                         
         MVC   0(2,R1),=C'Z8'                                                   
         MVC   2(2,R1),AGYALPHA                                                 
         MVC   4(14,R1),=C'*.INV.GEN..OV,'                                      
         MVC   18(3,R1),PAYER                                                   
         MVC   21(4,R1),=C'....'                                                
         MVC   25(1,R1),PAYMD                                                   
         MVI   26(R1),C'.'                                                      
         MVC   27(3,R1),QCLT                                                    
         MVI   30(R1),C'.'                                                      
         MVC   31(3,R1),QPRD                                                    
         MVI   34(R1),C'.'                                                      
*                                                                               
         LLC   R0,SVKEY+9          ESTIMATE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  35(3,R1),DUB                                                     
         MVI   38(R1),C'.'                                                      
*                                                                               
         MVC   39(2,R1),SVEND+2                                                 
         MVI   41(R1),C'/'                                                      
         MVC   42(2,R1),SVSTART                                                 
         MVI   44(R1),C'.'                                                      
         MVC   45(3,R1),=C'CK*'                                                 
         MVI   48(R1),C'.'                                                      
         MVC   49(5,R1),QSTA                                                    
         MVC   54(2,R1),=C'.*'                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',ZCTL,ZCTL                    
         XIT1                                                                   
         EJECT                                                                  
*================================================================               
* RESTORE INVOICE ENTRY TWA FROM TEMPSTR PAGE 1                                 
* AND MOVE CURRENT INPUT FIELDS TO SCREEN                                       
*================================================================               
                                                                                
RSTRTWA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AREC1            DATA SAVE AREA                               
*                                                                               
         LA    R2,PAYMSGH                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,PAYERH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,PAYOPH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,PAYMDH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,PAYCLH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,PAYPRH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,PAYSTH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,PAYEEH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,PAYDTH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     RF,VTWA             SAVE CURRENT SCREEN IN TWA1                  
         MVC   DMCB+10(2),2(RF)    2 BYTE TERM NO.                              
         MVI   DMCB+8,1                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,VTWA                       
         MVC   PAYMSG,SVPKGEL      MOVE MESSAGE TO RESTORED TWA                 
         MVC   PAYLAST(3),=X'000101'  FORCE XMT ALL                             
                                                                                
*===========================================================                    
* NOW MOVE CHANGED INPUT FIELDS                                                 
*===========================================================                    
                                                                                
         L     R4,AREC1            DATA SAVE AREA                               
*                                                                               
         LA    R2,PAYMSGH                                                       
         BAS   RE,MOVEBACK                                                      
*                                                                               
         LA    R2,PAYERH                                                        
         BAS   RE,MOVEBACK                                                      
*                                                                               
         LA    R2,PAYOPH                                                        
         BAS   RE,MOVEBACK                                                      
*                                                                               
         LA    R2,PAYMDH                                                        
         BAS   RE,MOVEBACK                                                      
*                                                                               
         LA    R2,PAYCLH                                                        
         BAS   RE,MOVEBACK                                                      
*                                                                               
         LA    R2,PAYPRH                                                        
         BAS   RE,MOVEBACK                                                      
*                                                                               
         LA    R2,PAYSTH                                                        
         BAS   RE,MOVEBACK                                                      
*                                                                               
         LA    R2,PAYEEH                                                        
         BAS   RE,MOVEBACK                                                      
*                                                                               
         LA    R2,PAYDTH                                                        
         BAS   RE,MOVEBACK                                                      
*                                                                               
         LA    R2,PAYERH           CURSOR POSITION                              
         MVI   SVSCRN,0            INDICATE FF SCREEN NOW LOADED                
*                                                                               
         XIT1                                                                   
*                                                                               
MOVEFLD  SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,MOVEDATA                                                      
         LA    R4,1(RF,R4)                                                      
         BR    RE                                                               
MOVEDATA MVC   0(0,R4),0(R2)       MOVE HEADER AND FIELD                        
*                                                                               
MOVEBACK SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,MOVEDTAB                                                      
         LA    R4,1(RF,R4)                                                      
         BR    RE                                                               
MOVEDTAB MVC   0(0,R2),0(R4)       MOVE HEADER AND FIELD                        
*                                                                               
         EJECT                                                                  
BLDREQ   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*BLDREQ*'                                                    
*                                                                               
         XC    ZCTL,ZCTL                                                        
         LA    R0,5                                                             
         LA    R1,ZAREA                                                         
         MVC   0(80,R1),SPACES                                                  
         LA    R1,80(R1)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         MVC   ZAGY,AGYALPHA                                                    
         MVC   ZMED,PAYMD                                                       
         MVC   ZCLT,QCLT                                                        
         MVI   ZPGR,C'N'                                                        
         MVI   ZMGR,C'N'                                                        
         MVC   ZPRD,QPRD                                                        
         MVC   ZEST(3),=C'NO '                                                  
         CLI   SVKEY+9,0                                                        
         BE    BLDREQ2                                                          
         LLC   R0,SVKEY+9                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ZEST(3),DUB                                                      
BLDREQ2  DS    0H                                                               
         MVC   ZMKT,QMKT                                                        
         MVC   ZSTA,QSTA                                                        
         CLI   ZSTA+4,C'T'                                                      
         BNE   *+8                                                              
         MVI   ZSTA+4,C' '                                                      
*                                                                               
         OC    SVSPREP,SVSPREP    TEST SPECIAL REP CLEARANCE                    
         BZ    BLDREQ4                                                          
         CLI   SVREPTYP,C'S'      TEST A SYNDICATION REP                        
         BNE   BLDREQ4                                                          
         CLI   SVPPROFA+9,C'Y'    TEST PAY STATION DIRECT                       
         BE    BLDREQ6                                                          
*                                                                               
BLDREQ4  MVC   ZREP,QREP                                                        
         CLC   ZREP,SPACES                                                      
         BH    *+10                                                             
         MVC   ZREP,=C'000'        DICK TURNER WANTS NUMBERS                    
         OC    SVSPREP,SVSPREP                                                  
         BZ    *+8                                                              
         MVI   ZREPTYPE,C'S'                                                    
*                                                                               
BLDREQ6  MVC   ZSTART(12),SVSTART                                               
         OC    QPRD2,QPRD2                                                      
         BZ    *+10                                                             
         MVC   ZPRD2,QPRD2                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* INITIALIZE                                                                    
*====================================================================           
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         MVI   DMCB+7,QSQUASH      GET A(SQUASHER)                              
         BAS   RE,PCALLOV                                                       
         MVC   SQUASHER,0(R1)                                                   
         MVI   DMCB+7,QMSPACK      GET A(MSPACK)                                
         BAS   RE,PCALLOV                                                       
         MVC   MSPACK,0(R1)                                                     
         MVI   DMCB+7,QMSUNPK      GET A(MSUNPK)                                
         BAS   RE,PCALLOV                                                       
         MVC   MSUNPK,0(R1)                                                     
         MVI   DMCB+7,QSTAPACK     GET A(STAPACK)                               
         BAS   RE,PCALLOV                                                       
         MVC   STAPACK,0(R1)                                                    
         MVI   DMCB+7,QGETRATE     GET A(GETRATE)                               
         BAS   RE,PCALLOV                                                       
         MVC   VGETRATE,0(R1)                                                   
         MVI   DMCB+7,QGETBRD                                                   
         BAS   RE,PCALLOV                                                       
         MVC   VGETBRD,0(R1)                                                    
         MVI   DMCB+7,QRCPACK                                                   
         BAS   RE,PCALLOV                                                       
         MVC   VRCPACK,0(R1)                                                    
         MVI   DMCB+7,QGETBUY                                                   
         BAS   RE,PCALLOV                                                       
         MVC   VGETBUY,0(R1)                                                    
* SET UP REC ADDRESSABILITY                                                     
         LA    RE,REC1                                                          
         ST    RE,AREC1                                                         
         A     RE,=A(REC2-REC1)                                                 
         ST    RE,AREC2                                                         
         A     RE,=A(REC3-REC2)                                                 
         ST    RE,AREC3                                                         
*                                                                               
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYINIT                                                   
         MVC   GBYAGY,AGYALPHA                                                  
         MVC   GBYCOMF,VCOMFACS                                                 
*                                                                               
         GOTO1 VGETBUY,GETBLK                                                   
         MVC   SV1OR2,GBY1OR2                                                   
         CLC   AGYALPHA,=C'T1'                                                  
         BE    *+14                                                             
         CLC   AGYALPHA,=C'SJ'                                                  
         BNE   INITX                                                            
         CLC   QCLT,=C'TBL'                                                     
         BNE   INITX                                                            
         MVI   SV1OR2,2                                                         
*                                                                               
INITX    XIT1                                                                   
*                                                                               
PCALLOV  LR    R0,RE                                                            
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* CALL SPGETCTA TO UPDATE CONTRACT PAID DOLLARS AND            *                
* PUT CTA CONTRACT DOLLARS IN REQUEST                          *                
*==============================================================*                
         SPACE 1                                                                
*&&DO                                                                           
SETCTA   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCALLOV,DMCB,0,X'D9000A7C'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            GET SPGETCTA ADDRESS                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING CIBBLKD,R1                                                       
*                                                                               
         LA    R4,SVCTADTA                                                      
         LA    R5,NSVCTA                                                        
*                                                                               
         MVI   CIBIAM,CIBPAYQ      INDICATE PAY PROGRAM CALL                    
         MVC   CIBAGYA,AGYALPHA                                                 
         TM    SVAFLAG1,X'02'      TEST NON-TBS AGY                             
         BZ    *+10                                                             
         MVC   CIBTRDPR,SVKEY+3    TRADE PRODUCT CODE                           
         MVC   CIBACOMF,VCOMFACS                                                
         MVC   CIBARCUP,VRECUP                                                  
         MVC   CIBAIO,AREC3                                                     
         L     RE,AREC1            POINT TO LAST REC PROCESSED                  
         ST    RE,CIBABUY                                                       
*                                                                               
SETCTA2  ZAP   DUB(4),=P'9999999'                                               
         SP    DUB(4),1(3,R4)                                                   
         L     R0,DUB                                                           
         SRL   R0,4                                                             
         STCM  R0,7,CIBNCON        SET PWOS CONTRACT NUMBER                     
         MVC   CIBNPAID,4(R4)      MOVE GROSS PAID DOLLARS                      
         GOTO1 (RF),(R1)                                                        
         CLI   CIBERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,L'SVCTADTA(R4)                                                
         OC    0(4,R4),0(R4)       TEST ANY MORE CONTRACTS                      
         BZ    SETCTA10                                                         
         BCT   R5,SETCTA2                                                       
         EJECT                                                                  
SETCTA10 MVI   Z2CTA,C'T'          INDICATE TRADE PAYMENT                       
*                                                                               
         LA    R8,ZCTADATA                                                      
         LA    R4,SVCTADTA                                                      
         LA    R5,NSVCTA                                                        
*                                                                               
SETCTA12 OC    0(4,R4),0(R4)       TEST MORE ENTRIES                            
         BZ    SETCTAX                                                          
         MVC   ZCTACON-ZCTADATA(4,R8),0(R4)   MOVE CONTRACT                     
         L     R0,4(R4)            GET PAID GROSS                               
         CVD   R0,DUB                                                           
         LTR   R0,R0                                                            
         BM    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         ZAP   ZCTAGRS-ZCTADATA(6,R8),DUB                                       
*                                                                               
         LA    R8,L'ZCTADATA(R8)                                                
         LA    R4,L'SVCTADTA(R4)                                                
         BCT   R5,SETCTA12                                                      
*                                                                               
* NEED TO SET NUMBER OF ENTRIES                                                 
*                                                                               
SETCTAX  LA    R0,NSVCTA                                                        
         SR    R0,R5                                                            
         STC   R0,Z2CTA#                                                        
*                                                                               
         XIT1                                                                   
*&&                                                                             
         EJECT                                                                  
*==============================================================*                
* UPDATE SPOT AUTOPAY RECORDS                                  *                
*==============================================================*                
         SPACE 1                                                                
SETAPY   NTR1  BASE=*,LABEL=*                                                   
         B     *+12                                                             
         DC    CL8'*SETAPY*'                                                    
*                                                                               
         CLI   SVXATPAY,C'Y'       CHECK AUTOPAY RECS ON XSPFIL?                
         BE    SETAP10             YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0D'                                                        
         MVI   KEY+1,X'3A'                                                      
         MVI   KEY+3,1             WELL, NOT EXACTLY THE FIRST !                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                RETRIEVE FIRST RECORD TO GET DATE            
         CLC   KEY(2),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY+4(L'KEY-4),KEY+4                                             
         MVC   KEY+4(1),SVKEY      A-M                                          
         MVC   KEY+5(2),SVKEY+1    CLT                                          
         MVC   KEY+7(1),QPRD+3     PRD                                          
         MVC   KEY+8(1),QPRD2+3    PRD2                                         
         MVC   KEY+9(1),SVKEY+9    EST                                          
         MVC   KEY+10(3),SVKEY+6   STATION                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    SETAP4                                                           
* NOT THERE TODAY, TRY PREVIOUS DAY (DATES ARE COMPLEMENTED)                    
* UP TO 7 TIMES - THEN GIVE UP                                                  
         LA    R6,7                SET COUNTER                                  
*                                                                               
SETAP2   MVC   WORK(13),KEYSAVE    SAVE PREVIOUS KEYARG                         
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+4(4),=F'-1'                                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                GET NEXT DATE                                
         ICM   R0,3,KEY+2          SAVE THE DATE                                
         MVC   KEY(13),WORK                                                     
         STCM  R0,3,KEY+2                                                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    SETAP4                                                           
         BCT   R6,SETAP2                                                        
         DC    H'0'                WHERE DID AUTOPAY RECORD GO                  
*                                                                               
SETAP4   L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         USING APYRECD,R6                                                       
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                  NEED TO FIND ELEM FOR MONTH                  
         GOTO1 VDATCON,DMCB,(2,SVENDP),(9,WORK)     GET MMM/YY                  
*                                                                               
         USING APYEL,R6                                                         
         LA    R6,24(R6)                                                        
SETAP6   CLI   0(R6),0                                                          
         BE    SETAPX                                                           
         CLI   0(R6),X'01'         ELEM                                         
         BE    SETAP8                                                           
SETAP7   LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     SETAP6                                                           
SETAP8   CLC   APYMONTH,WORK       MATCH MONTH                                  
         BNE   SETAP7                                                           
*                                                                               
         MVC   APYPAID,TODAYP      SET PAID DATE IN RECORD                      
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 PUTREC                                                           
         B     SETAPX                                                           
*                                                                               
SETAP10  XC    BIGKEY,BIGKEY       CLEAR THE KEY                                
         LA    R4,BIGKEY           R6 = KEY                                     
         USING APXRECD,R4          AUTOPAY RECORD DSECT                         
         MVI   APXKTYP,APXKTYPQ    X'0D'                                        
         MVI   APXKSUB,APXKSUBQ    X'3A'                                        
         MVI   APXKDATE+1,1        DON'T READ VERY FIRST AUTOPAY REC!           
         MVC   BIGKEYSV,BIGKEY     SAVE OFF THE KEY                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',BIGKEYSV,BIGKEY              
         B     SETAP10B                                                         
*                                                                               
SETAP10A GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',BIGKEY,BIGKEY                
*                                                                               
SETAP10B CLC   APXKEY(2),BIGKEYSV  FOUND AN AUTOPAY KEY?                        
         BE    *+6                 YES                                          
         DC    H'0'                NO - WHERE DID AUTOPAY RECORD GO?            
*                                                                               
         CLC   APXKAGMD,SVKEY      A-M WE ARE LOOKING FOR?                      
         BNE   SETAP10A            NO - READ SEQ                                
*                                                                               
         XC    APXKAGMD(28),APXKAGMD CLEAR A/M AND BEYOND                       
         MVC   APXKAGMD,SVKEY      A-M                                          
         MVC   APXKCLT,SVKEY+1     CLT                                          
         MVC   APXKPRD,QPRD        PRD                                          
         MVC   APXKPTN,QPRD2       PRD2                                         
         MVC   APXKEST,SVKEY+9     EST                                          
         MVC   APXKMKT,SVKEY+4     MARKET                                       
         MVC   APXKSTA,SVKEY+6     STATION                                      
         MVC   APXKMON,SVENDP      MOS                                          
         CLI   PAYREP,C'Y'         INPUT PAYING REP FOR LOCAL CABLE?            
         BNE   SETAP10C            NO - TRY SPECIAL REP                         
         CLC   QREP,=C'000'        PAYING REP 000?                              
         BE    SETAP10D            YES - THAT MEANS NO REP!                     
         CLC   QREP,SPACES         REP = SPACES?                                
         BE    SETAP10D            YES - THAT MEANS NO REP!                     
         MVC   APXKREP,QREP        PAYING REP                                   
         B     SETAP10D            SKIP SPECIAL REP                             
                                                                                
SETAP10C OC    SVSPREP,SVSPREP     HAVE A SPECIAL REP?                          
         BZ    SETAP10D            NO                                           
         MVC   APXKREP,QREP        YES - EBCIDIC SPECIAL REP                    
*                                                                               
SETAP10D MVC   APXKOFC,SVOFFC      CLIENT OFFICE                                
         MVC   BIGKEYSV,BIGKEY     SAVE OFF THE KEY                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',BIGKEYSV,BIGKEY              
*                                                                               
         CLC   APXKEY,BIGKEYSV     FOUND AUTOPAY KEY?                           
         BE    SETAP15             YES                                          
***                                                                             
* NOT THERE FOR LATEST DATE. TRY PREVIOUS 7 DATES BEFORE GIVING UP              
***                                                                             
         LA    R6,7                SET COUNTER                                  
*                                                                               
SETAP11  MVC   WORK(32),BIGKEYSV   SAVE PREVIOUS KEYARG                         
         MVI   BIGKEYSV+4,X'FF'    GET NEXT AVAILABLE DATE                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',BIGKEYSV,BIGKEY              
*                                                                               
         CLC   APXKEY(2),BIGKEYSV  FOUND AN AUTOPAY KEY?                        
         BE    *+6                 YES                                          
         DC    H'0'                NO - WENT PAST THEM                          
*                                                                               
         MVC   WORK+2(2),APXKDATE  NEXT AVAILABLE DATE                          
         MVC   BIGKEYSV(32),WORK   RESTORE KEY WITH NEW DATE                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',BIGKEYSV,BIGKEY              
*                                                                               
         CLC   APXKEY(2),BIGKEYSV  FOUND AN AUTOPAY KEY?                        
         BE    *+6                 YES                                          
         DC    H'0'                NO - WENT PAST THEM                          
*                                                                               
         CLC   BIGKEY(32),BIGKEYSV FOUND AUTOPAY KEY?                           
         BE    SETAP15             YES                                          
         BCT   R6,SETAP11          NO - LOOK FOR NEXT AVAILABLE DATE            
         DC    H'0'                WHERE DID AUTOPAY RECORD GO?                 
*                                                                               
SETAP15  GOTO1 VDATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL',APXKDA,     X        
               AREC3,DMWORK                                                     
*                                                                               
         L     R6,AREC3            A(AUTOPAY RECORD)                            
         MVI   ELCODE,APXELQ       X'01' ALPHA ELEMENT                          
         BRAS  RE,GETEL            HAVE THE X'01' ELEMENT?                      
         BE    *+6                 YES                                          
         DC    H'0'                NO - ELEM SHOULD ALWAYS BE PRESENT           
         USING APXEL,R6            X'01' ELEMENT DSECT                          
         MVC   APXPAID,TODAYP      SET PAID DATE IN RECORD                      
         DROP  R6                  DROP X'01' ELEMENT USING                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',APXKDA,AREC3,DMWORK          
*                                                                               
SETAPX   XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*=====================================================*                         
* GLOBINVS - SET INVOICE LINES FROM GLOBBER           *                         
*=====================================================*                         
         SPACE 1                                                                
GLOBINVS NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*GLOBIN*'                                                    
*                                TRY FOR TRANSFER CONTROL ELEM                  
         MVI   INCHSW,C'N'                                                      
         GOTO1 GLOBBER,DMCB,=C'GETD',ELEM,24,GLVXCTL                            
         CLI   DMCB+8,0            IF NONE                                      
         BNE   GLOBINX             FORGET IT                                    
         GOTO1 (RF),(R1),=C'DELE'                                               
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRPR,=C'INC'    OR IF NOT FROM INCH                          
         BNE   GLOBINX                                                          
         MVI   INCHSW,C'Y'         SET IS TRANSFER FROM INCH                    
         DROP  R1                                                               
*                                  NOW DO INVOICE LINES                         
*                                  NOTE THERE SHOULD BE A FULL SET OF 5         
GLOBIN3  DS    0H                                                               
         LA    R4,5                5 INVOICE LINES                              
         LA    R2,PAYINV1H                                                      
         LA    R5,GLVSPIL1         CODE FOR LINE 1                              
*                                                                               
GLOBIN4  DS    0H                                                               
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(63),ELEM                                                  
         GOTO1 GLOBBER,DMCB,=C'GETD',ELEM,64,(R5)                               
         CLI   DMCB+8,GLEGNF       IF NOT FOUND                                 
         BE    GLOBINX             DONE (DON'T WIPE OUT WHATS THERE)            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                GLOBBER ERROR                                
         GOTO1 GLOBBER,DMCB,=C'DELE'                                            
*                                                                               
         MVC   8(11,R2),ELEM       INVOICE NUMBER                               
         BAS   RE,GBSETLN          SET FIELD LENGTH IN HEADER                   
         FOUT  (R2)                                                             
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(13,R2),ELEM+11    AMOUNT                                       
         BAS   RE,GBSETLN          SET FIELD LENGTH IN HEADER                   
         FOUT  (R2)                                                             
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(40,R2),ELEM+24    REMITTANCE COMMENT                           
         BAS   RE,GBSETLN          SET FIELD LENGTH IN HEADER                   
         FOUT  (R2)                                                             
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R5,1(R5)            BUMP LINE CODE                               
         BCT   R4,GLOBIN4                                                       
*                                                                               
GLOBINX  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                  SET FIELD LENGTH                             
GBSETLN  DS    0H                                                               
         LLC   RF,0(R2)                                                         
         AR    RF,R2                                                            
         BCTR  RF,0                RF AT LAST BYTE                              
         LA    R0,8(R2)                                                         
*                                                                               
GLS04    DS    0H                                                               
         CR    RF,R0                                                            
         BL    GLS06                                                            
         CLI   0(RF),C' '                                                       
         BH    GLS06                                                            
         BCT   RF,GLS04                                                         
*                                                                               
GLS06    DS    0H                                                               
         SR    RF,R0                                                            
         LA    RF,1(RF)                                                         
         STC   RF,5(R2)                                                         
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
*==================================================================*            
* GET INVOICES FROM XSPFILE                                        *            
*==================================================================*            
         SPACE 1                                                                
GETINV   NTR1  BASE=*,LABEL=*                                                   
*                                  CLEAR THE INVS, AMTS, COMMENTS               
         XC    BIGZERO,BIGZERO     SET NO ZERO INVOICE FOUND                    
         LA    RE,PAYINV1H                                                      
         SR    RF,RF                                                            
GETINV2  IC    RF,0(RE)                                                         
         SH    RF,=H'9'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)                                                    
         OI    6(RE),X'80'         SET TO XMT                                   
         LA    RE,9(RE,RF)                                                      
         TM    1(RE),X'20'         TEST PROTECTED                               
         BO    GETINV4             YES - REACHED TEST DATA                      
         CLI   0(RE),9                                                          
         BH    GETINV2                                                          
*                                                                               
GETINV4  LA    R2,PAYINV1H                                                      
*                                                                               
         XC    BIGKEY,BIGKEY       NOTE USES ** ELEM **                         
         LA    R5,BIGKEY                                                        
         USING SNVKEYD,R5                                                       
         MVI   SNVKTYPE,X'0E'                                                   
         MVI   SNVKSUB,X'03'                                                    
         MVC   SNVKAM,SVKEY        A/M                                          
         MVC   SNVKCLT,SVKEY+1     CLT                                          
         MVC   SNVKSTA,SVKEY+6     STATION                                      
* NOW GET END YMD                                                               
         GOTO1 VDATCON,DMCB,(2,SVENDP),(3,WORK)                                 
         MVI   WORK+2,1            SET TO 1ST OF MONTH                          
         GOTO1 (RF),(R1),(3,WORK),(2,SNVKMOS)                                   
         XC    SNVKMOS,=X'FFFF'                                                 
*                                                                               
GETINV10 MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',BIGKEYSV,BIGKEY              
*                                                                               
         CLC   BIGKEY(10),BIGKEYSV  COMPARE TYPE/A-M/CLT/STA/MOS                
         BNE   GETINVX                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL',SNVDDA,     X        
               AREC2,DMWORK                                                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(10),SNVKINV    SAVE INVOICE NUMBER !                        
*                                                                               
         L     R5,AREC2                                                         
         LA    R5,SNVELS                                                        
         CLI   0(R5),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SNVHDELD,R5                                                      
         TM    SNVHDCTL,SNVHDMCQ   TEST MCT INVOICE                             
         BO    GETINV40            NEVER PAY THESE                              
*                                                                               
         CLC   SVKEY+3(1),SNVHDPRD TEST PRODUCTS MATCH                          
         BNE   GETINV12                                                         
         CLC   QPRD2+3(1),SNVHDPR2 TEST PRD2 MATCHES AS WELL                    
         BE    GETINV13            NO                                           
*                                                                               
GETINV12 CLI   SVKEY+3,X'FF'       TEST PAYING POL                              
         BNE   GETINV40                                                         
         CLI   SNVHDPRD,0          TEST INVOICE FOR VARIOUS                     
         BNE   GETINV40                                                         
*                                                                               
GETINV13 CLC   SVKEY+9(1),SNVHDEST TEST ESTIMATES MATCH                         
         BE    GETINV14                                                         
         CLI   SVKEY+9,0           TEST PAYING ALL ESTIMATES                    
         BNE   GETINV40                                                         
*                                                                               
GETINV14 CLI   SVPPROFB+4,C'Y'     FILTER INVOICES ON REP CODE                  
         BNE   GETINV15                                                         
         OC    SVSPREP,SVSPREP     TEST SPECIAL REP PAYMENT                     
*        CLC   QREP,=C'000'        REP 000 IS NO REP                            
         BNZ   *+14                                                             
         CLC   SNVHDREP,SPACES                                                  
         BNH   GETINV17                                                         
*                                                                               
         OC    SVSPREP,SVSPREP     TEST SPECIAL REP PAYMENT                     
         BZ    GETINV40                                                         
         CLC   SNVHDREP,QREP       SPECIAL REP ONLY                             
         BNE   GETINV40                                                         
         B     GETINV17                                                         
*                                                                               
GETINV15 CLI   SVPPROFB+4,C'T'     FILTER INV ON TRADE REP ONLY                 
         BNE   GETINV17                                                         
         TM    SVCOPT4,COP4TRD     TRADE CLIENT                                 
         BNO   GETINV17                                                         
         OC    SVSPREP,SVSPREP     REP ENTERED ON SCREEN                        
         BZ    GETINV16                                                         
         CLC   QREP,SVTRDREP       YES, IS IT THE TRADE REP                     
         BNE   GETINV16                                                         
         CLC   SNVHDREP,SVTRDREP   TRADE REP ENTERED                            
         BNE   GETINV40            ONLY GET TRADE INVS                          
         B     GETINV17                                                         
*                                                                               
GETINV16 CLC   SNVHDREP,SVTRDREP   NO TRADE REP ENTERED                         
         BE    GETINV40            SKIP TRADE INVOICES                          
*                                                                               
GETINV17 CLI   SVAUTPAY,C'Y'       TEST AUTOPAY                                 
         BE    GETINV18                                                         
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   GETINV20                                                         
GETINV18 CLC   =C'ID=',PAYOP       ID=ACN OR ID IN OPTIONS                      
         BNE   GETINV20                                                         
         CLC   SVID(5),SNVHDCON    MATCH ON INVOICE CONTRACT                    
         BNE   GETINV40                                                         
*                                                                               
* USE THIS INVOICE ??                                                           
*                                                                               
GETINV20 TM    SNVHDCTL,SNVHDPDQ   TEST INVOICE PAID FLAG                       
         BO    GETINV22            YES                                          
         TM    SVTSTOPT,X'10'      NO - TEST PAID OPTION ACTIVE                 
         BO    GETINV40            SKIP UNPAID IF PAID OPT ACTV                 
         CLI   SVAUTPAY,C'Y'       TEST AUTOPAY                                 
         BNE   GETINV30                                                         
*                                                                               
         CLI   SVPPROFB+3,C'Y'     ALLOW $0 CLEARANCES                          
         BE    GETINV30                                                         
         CP    SNVHDTCS,=P'0'      TEST ZERO INVOICE                            
         BNE   GETINV30                                                         
         MVC   BIGZERO(10),WORK    SAVE THE INVOICE NUMBER                      
         B     GETINV40            YES - IGNORE                                 
*                                                                               
GETINV22 TM    SVTSTOPT,X'10'      TEST PAID OPTION ACTIVE                      
         BZ    GETINV40                                                         
                                                                                
GETINV30 LA    R0,PAYINVXH         TEST ANY MORE FIELDS ON SCREEN               
         CR    R2,R0                                                            
         BH    GETINVE2                                                         
*                                                                               
         MVC   8(10,R2),WORK       MOVE SAVED INVOICE NUMBER                    
         MVI   5(R2),10            SET LENGTH                                   
         OI    6(R2),X'80'         SET XMT                                      
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   GETINV31                                                         
*                                                                               
*        CLC   SNVHDREP,SPACES     PAYING REP                                   
*        BNH   GI30A                                                            
*        MVI   PAYEE,C'P'                                                       
*        MVC   PAYEE+1(3),SNVHDREP                                              
*        OI    PAYEEH+6,X'80'                                                   
*        NI    PAYEEH+4,X'FF'-X'20'                                             
*                                                                               
*I30A    CLC   SNVHDCON,SPACES     CONTRACT                                     
*        BNH   GI30X                                                            
*        MVC   PAYOP,SPACES                                                     
*        MVC   PAYOP(3),=C'ID='                                                 
*        MVC   PAYOP+3(12),SNVHDCON                                             
*        OI    PAYOPH+6,X'80'                                                   
*        NI    PAYOPH+4,X'FF'-X'20'                                             
*                                                                               
GI30X    TM    SNVHDCT2,SNVHDTXQ   X'01' MEANS TAX IS NET+TAX                   
         BNO   GETINV31                                                         
         ICM   R1,15,SNVHDTAX      THEN JUST USE NET+TAX                        
         B     GETINV38                                                         
*                                                                               
GETINV31 ZAP   DUB,SNVHDTCS        GET COST                                     
         CVB   R1,DUB                                                           
*                                                                               
         CP    SNVHDTCS,=P'0'                                                   
         BNL   GETINV3A                                                         
         L     R0,=X'FFFFFFFF'                                                  
*                                                                               
GETINV3A CLI   SVPPROF+0,C'G'      TEST AGENCY PAYS GROSS                       
         BNE   GETINV32            NO                                           
         TM    SNVHDCTL,X'88'      TEST NET OR NETTED DOWN                      
         BZ    GETINV36            NO                                           
* NEED TO GROSS UP INVOICE AMOUNT                                               
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'85'                                                        
         B     GETINV34                                                         
*                                                                               
GETINV32 TM    SNVHDCTL,X'88'      TEST NET OR NETTED DOWN                      
         BNZ   GETINV36                                                         
* NEED TO NET INVOICE DOWN                                                      
         M     R0,=F'170'           X 85 X 2                                    
         D     R0,=F'100'                                                       
GETINV34 LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
GETINV36 DS    0H                                                               
         ICM   R0,15,SNVHDTAX                                                   
         AR    R1,R0                                                            
*                                                                               
         CLI   SVAUTPAY,C'Y'       AUTOPAY RUN?                                 
         JNE   GETINV38            NO - SO CONTINUE                             
*                                                                               
         CVD   R1,DUB              MAKE AMOUNT PACKED                           
         ZAP   WORK(16),DUB        STORE IT                                     
         SR    R0,R0                                                            
         ICM   R0,3,SVTAX          GET TAX RATE (10%=10000)                     
         CVD   R0,DUB              MAKE IT PACKED                               
         MP    WORK(16),DUB        AMOUNT * TAX RATE                            
         SRP   WORK(16),64-5,5     ROUND OUT THE ANSWER                         
         ZAP   DUB,WORK(16)                                                     
         CVB   R0,DUB                                                           
         AR    R1,R0               ADD THE TAX                                  
*                                                                               
GETINV38 EQU   *                                                                
*                                                                               
         EDIT  (R1),(10,8(R2)),2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                
         STC   R0,5(R2)            SET LENGTH                                   
         OI    6(R2),X'81'         SET XMT AND MODIFIED                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO COMMENT FIELD                       
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT INVOICE FIELD                  
*                                                                               
GETINV40 LA    R5,BIGKEY           FORCE NEXT INVOICE                           
         USING SNVKEYD,R5                                                       
         MVC   SNVKMINK(8),=8X'FF'                                              
         B     GETINV10                                                         
*                                                                               
GETINVX  LA    R0,PAYINV1H                                                      
         CR    R2,R0               DID WE FIND ANY INVOICES                     
         BNE   GETINVXX                                                         
         CLI   SVAUTPAY,C'Y'                                                    
         BNE   GETINVE1            NO                                           
         OC    BIGZERO,BIGZERO     TEST FOUND A ZERO INVOICE                    
         BZ    GETINVE1            NO                                           
         MVC   8(10,R2),BIGZERO    MOVE SAVED INVOICE NUMBER                    
         MVI   5(R2),10            SET LENGTH                                   
         OI    6(R2),X'81'         SET XMT                                      
         LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO AMOUNT FIELD                        
         MVI   8(R2),C'0'                                                       
         MVI   5(R2),1                                                          
         OI    6(R2),X'81'                                                      
GETINVXX XIT1                                                                   
         DROP  R5                                                               
*                                                                               
GETINVE1 MVC   NERRCD,=AL2(NOINVS)                                              
         B     GETINVEX                                                         
*                                                                               
GETINVE2 MVC   NERRCD,=AL2(MANYINVS)                                            
         B     GETINVEX                                                         
*                                                                               
GETINVEX MVI   ERRCD,NEWERRS                                                    
         LA    R2,PAYINV1H                                                      
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*==================================================================*            
* SWITCH TO ACC SYSTEM TO POST CHECKS                              *            
*==================================================================*            
         SPACE 1                                                                
GOTOACC  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*GOTOACC'                                                    
*                                                                               
         L     RE,AREC2                                                         
         OC    2(2,RE),2(RE)       TEST ANY CHECKS BUILT                        
         BZ    GOTOACCX            NO - EXIT                                    
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVXFRACC                                                 
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    GOTOACC2                                                         
         MVC   PAYMSG(26),=C'ACC SYSTEM NOT OPERATIONAL'                        
         B     GOTOACC6                                                         
*                                                                               
GOTOACC2 DS    0H                                                               
         MVC   PAYFACS,VCOMFACS                                                 
         L     R0,AREC2                                                         
         ST    R0,PAYREQ                                                        
         MVC   PAYCOMP,SVXFRCOM    SET COMPANY CODE                             
         MVC   PAYALPHA,AGYALPHA                                                
         CLC   SVPPROF+14(2),=C'AA'  TEST ALT ACC AGENCY                        
         BNH   GOTOACC4                                                         
         CLC   SVPPROF+14(2),=C'00'  00 DOESN'T COUNT                           
         BE    GOTOACC4                                                         
         MVC   PAYALPHA,SVPPROF+14                                              
GOTOACC4 L     RE,VTWA                                                          
         USING TWAD,RE                                                          
         MVC   PAYUSRID,TWAUSRID   SET USER ID NUMBER                           
         DROP  RE                                                               
*                                                                               
         GOTO1 VACPAYSP,PAYFACS                                                 
         OC    PAYERROR,PAYERROR                                                
         BZ    GOTOACC8                                                         
*                                                                               
         CLI   SVAUTPAY,C'Y'       AUTOPAY?                                     
         BNE   GOTOACC5            NO                                           
         MVI   ERRCD,NEWERRS       YES MUST USE STANDARD ERROR                  
         MVC   NERRCD,=Y(PAYPGERR)     SO THE SCRIPT PICKS IT UP                
         GOTO1 ERROR                                                            
*                                                                               
GOTOACC5 L     RE,PAYERROR                                                      
         MVC   PAYMSG(60),0(RE)    MOVE ERROR TO HEADLINE                       
*                                                                               
GOTOACC6 LA    R2,PAYERH                                                        
         OI    6(R2),X'40'         POSITION CURSOR                              
         DC    H'0',C'$ABEND'      NEED TO UNWIND CLEARANCE                     
*                                                                               
GOTOACC8 DS    0H                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOTOACCX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================*               
* SUBROUTINE TO CHECK FOR PSR RECORD AND SAVE SPLIT DATA        *               
*===============================================================*               
         SPACE 1                                                                
GETPSR   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D48'                                                  
         MVC   KEY+2(3),SVKEY      A-M/CLT                                      
         MVC   KEY+5(2),SVSPREP                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETPSRX                                                          
         MVC   AREC,AREC1                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         USING PSREL5,R6                                                        
*                                                                               
GETPSR2  CLI   0(R6),5                                                          
         BNE   GETPSR4                                                          
         GOTO1 VDATCON,DMCB,SVEND,(3,DUB)                                       
         CLC   PSR5DATE,DUB         ELEM DATE TO PERIOD END                     
         BH    GETPSRX                                                          
         MVC   PSRDATA(32),PSR5DATA SAVE REPS AND SPLITS + 2X'00'               
         DROP  R6                                                               
*                                                                               
GETPSR4  DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETPSR2                                                          
GETPSRX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* APPLY PSR PCTS TO AMOUNT IN R0 AND RETURN DIFFERENCE IN FULL                  
* R4 POINTS TO FIRST REP IN PSRDATA                                             
*===============================================================                
                                                                                
ADJPSR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    R0,FULL             SAVE AMOUNT                                  
         SR    RF,RF               CLEAR TOTAL                                  
*                                                                               
ADJPSR2  L     R0,FULL                                                          
         AR    R0,R0               X 2                                          
         ICM   R1,15,2(R4)         GET PERCENTAGE                               
         MR    R0,R0                                                            
         D     R0,=F'100000'                                                    
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRL   R1,1                ROUND                                        
         AR    RF,R1               ADD TO TOTAL                                 
*                                                                               
         LA    R4,6(R4)            NEXT REP                                     
         OC    0(2,R4),0(R4)                                                    
         BNZ   ADJPSR2                                                          
*                                                                               
         S     RF,FULL             GET DIFF BETWEEN REP TOT AND AMT             
         ST    RF,FULL             AND RETURN IT                                
         J     XFRX                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================*                         
* XFRETN  - RETURN FROM TRANSFER                      *                         
*=====================================================*                         
         SPACE 1                                                                
XFRETN   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*XFRETN*'                                                    
*                                  SET UP FOR RETURN TO $INCH                   
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING GLVXFRSY,R5                                                      
         MVC   GLVXFRSY,=C'SPO'    FROM THE SPOT SYSTEM                         
         MVC   GLVXFRPR,=C'PAY'    PAY PROGRAM                                  
         MVC   GLVXTOSY,=C'SPO'    TO THE SPOT SYSTEM                           
         MVC   GLVXTOPR,=C'INC'    INCH PROGRAM                                 
         OI    GLVXFLG1,GLV1RETN+GLV1RETG                                       
         DROP  R5                                                               
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',ELEM,24,GLVXCTL                            
         GOTO1 (RF),(R1),=C'PUTD',PAYMSG,60,GLVSMSG                             
*                                                                               
XFRX     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
ADDREQ   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'30',ZTYPE                                                     
         BNE   ADDREQ20                                                         
         CLC   =C'FJWT$$',PAYER                                                 
         BE    ADDREQX                                                          
* COUNT NUMBER OF COMMENTS AND PASS IN ZCONT                                    
         LA    R0,5                                                             
         LA    R1,ZCOM1                                                         
*                                                                               
ADDREQ11 CLC   0(40,R1),SPACES                                                  
         BE    ADDREQ12                                                         
         LA    R1,40(R1)                                                        
         BCT   R0,ADDREQ11                                                      
*                                                                               
ADDREQ12 LA    R1,5                                                             
         SR    R1,R0               GET NUMBER OF COMMENTS                       
         STC   R1,ZCONT                                                         
*                                                                               
         LA    R1,1(R1)            SET TO ROUND UP                              
         SRL   R1,1                GIVES NUMBER OF 80 BYTE COMMENTS             
         LA    R1,2(R1)            ADD 2 FOR 160 BYTES ALWAYS PRESENT           
         BCTR  R1,0                THEN PASS N'CARDS - 1                        
         SLL   R1,4                SHIFT TO LEFT NIBBLE                         
         STC   R1,REQFLAG          SET NUMBER OF CARDS                          
*                                                                               
         CLI   PASS,C'A'           TEST ACC PASS                                
         BNE   ADDREQ20                                                         
*======================================================*                        
* MOVE REQUEST TO SAVE AREA FOR ACC POSTINGS           *                        
*======================================================*                        
         L     R4,SVACCPTR         POINT TO REQUEST BUILD AREA                  
         LLC   RE,ZCONT            GET NUMBER OF COMMENTS                       
         MH    RE,=H'40'           X 40                                         
         LA    RE,162(RE)          ADD FIXED REQ + 2 FOR LENGTH                 
         STH   RE,0(R4)                                                         
*                                                                               
         MVC   2(160,R4),ZAREA     MOVE CHECK                                   
         MVC   162(240,R4),ZCOM1   MOVE COMMENTS                                
*                                                                               
         LLC   RE,ZCONT                                                         
         MHI   RE,40                                                            
         LA    RE,162(R4,RE)       POINT TO BYTE AFTER LAST COMMENT             
         XC    0(2,RE),0(RE)                                                    
         ST    RE,SVACCPTR         POINT TO NEXT REQUEST AREA                   
         B     ADDREQX                                                          
*                                                                               
ADDREQ20 DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',ZCTL,ZCTL                    
         LR    RE,R0                                                            
         CLI   8(R1),0                                                          
         BE    ADDREQX                                                          
         CLC   =C'30',ZAREA                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
ADDREQX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPPAY00MSG                                                     
         EJECT                                                                  
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BU'    LOCK BUYS                                    
         L     RE,AREC                                                          
         MVC   L.LOCKMED,PAYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         MVC   L.LOCKSTA,QSTA                                                   
         CLI   L.LOCKSTA,C'0'                                                   
         BL    *+8                                                              
         MVI   L.LOCKSTA+4,C'/'                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
* TEST ALLOCATION KEYS                                                          
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BA'    LOCK BUYS                                    
         MVC   L.LOCKMED,PAYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         CLI   SVKEY+9,0           NO EST = LEAVE AS NULLS                      
         BE    TSTLOCK1            LOCKED IF ANY EST IS LOCKED                  
         SR    R0,R0                                                            
         IC    R0,SVKEY+9                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LOCKEST,DUB                                                    
         MVC   DUB(3),L.LOCKEST                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
TSTLOCK1 BAS   RE,TSTIT                                                         
*                                                                               
         CLI   SVAPROF+7,C'C'      FOR CAN, MAKE SURE MED C NOT LOCKED          
         BNE   TSTLKEQ                                                          
         CLI   PAYMD,C'T'                                                       
         BE    *+12                                                             
         CLI   PAYMD,C'N'                                                       
         BNE   TSTLKEQ                                                          
         MVI   L.LOCKMED,C'C'                                                   
         CLI   SVKEY+9,0           NO EST = LEAVE AS NULLS                      
         BE    TSTLOCK3            LOCKED IF ANY EST IS LOCKED                  
         MVC   L.LOCKEST,DUB                                                    
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
TSTLOCK3 BAS   RE,TSTIT                                                         
         B     TSTLKEQ                                                          
         DROP  L                                                                
*                                                                               
TSTIT    LR    R0,RE                                                            
*                                                                               
TSTIT2   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTIT2                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* PUT PST AMOUNTS IN REQUEST                                     *              
*================================================================*              
         SPACE 1                                                                
         USING AMTD,R8                                                          
SETPST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,PRVTAB           TABLE OF PROVINCES                           
         LA    R2,ZPST                                                          
         LA    R4,AMTPST                                                        
         LA    R5,PSTCODE                                                       
         LA    R6,10                                                            
*                                                                               
SPST10   CLI   0(R5),C' '          IF NO PST CODE - SKIP                        
         BNH   SPST20                                                           
         ICM   R0,15,0(R4)                                                      
         CLI   AMTTYPE,C'2'        TEST CR                                      
         BNE   *+6                                                              
         LCR   R0,R0                                                            
*                                                                               
         CLI   SVUNPAY,C'Y'                                                     
         BNE   SPST15                                                           
         CLC   =C'REVCHECK',PAYER                                               
         BNE   SPST15                                                           
         LCR   R0,R0                                                            
*                                                                               
SPST15   MVC   0(2,R2),0(R1)       PROVINCE                                     
         MVC   2(1,R2),0(R5)       PST CODE                                     
         STCM  R0,15,3(R2)         PST AMOUNT                                   
         LA    R2,7(R2)                                                         
*                                                                               
SPST20   LA    R1,2(R1)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,SPST10                                                        
*                                                                               
         OI    Z2FLAG,Z2PSTHEX     PST AMOUNT IS IN  HEX                        
         XIT1                                                                   
         DROP  R8                                                               
*                                                                               
SPSTERR  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(PST2BIG)                                               
         LA    R2,PAYINV1H                                                      
         MVI   ERRAREA,X'FE'                                                    
         GOTO1 ERROR                                                            
*                                                                               
PRVTAB   DS    0CL2                                                             
         DC    C'BC'                                                            
         DC    C'AL'                                                            
         DC    C'SA'                                                            
         DC    C'MA'                                                            
         DC    C'ON'                                                            
         DC    C'PQ'                                                            
         DC    C'NB'                                                            
         DC    C'NS'                                                            
         DC    C'PE'                                                            
         DC    C'NF'                                                            
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* CLRTEST -                                                                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
CLRTEST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,PAYLAST                                                       
         CLI   0(R2),0                                                          
         BE    CLRTESTX                                                         
         LLC   R0,0(R2)                                                         
         AR    R2,R0               SKIP PAST TITLES                             
CLRTEST2 CLI   0(R2),0                                                          
         BE    CLRTESTX                                                         
         LLC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CLROC                                                         
         BE    CLRTEST4                                                         
         EX    RE,CLRSP                                                         
         BE    CLRTEST4                                                         
         EX    RE,CLRXC                                                         
         FOUT  (R2)                                                             
CLRTEST4 LA    R2,9(RE,R2)                                                      
         B     CLRTEST2                                                         
CLRTESTX XIT1                                                                   
*                                                                               
CLROC    OC    8(0,R2),8(R2)                                                    
CLRSP    CLC   8(0,R2),SPACES                                                   
CLRXC    XC    8(0,R2),8(R2)                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* GETPADR - SUBROUTINE TO DISPLAY PAYEE ADDRESS                                 
*           ADDRESS IS OBTAINED FROM '23' RECORD IN EASI WORKER FILE            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
GETPADR  NTR1  BASE=*,LABEL=*                                                   
         LR    RA,R3               A(TWA)                                       
         AHI   RA,SVSTA-T213FFD                                                 
         USING SVSTA,RA                                                         
         L     R5,VTIA                                                          
         USING VTIAD,R5                                                         
*                                                                               
         CLC   SVMED,PAYMD         SAME MEDIA?                                  
         BNE   GETP05              NO - BUILD EQV STA TABLE                     
*                                                                               
         CLC   QSTA,SVSTA          SAME STATION?                                
         BNE   GETP05              NO - BUILD EQV STA TABLE                     
*                                                                               
         LLC   R1,PAYINV1H+5                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PAYINV1(0),SVINV       SAME INVOICE                              
         BNE   GETP08       NO, GET ADDR, BUT DON'T BUILD STA EQV TABLE         
*                                                                               
         MVC   PAYPADR,SVPADDR                                                  
         OI    PAYPADRH+6,X'80'                                                 
         B     GETPX               EVERYTHING IS THE SAME - JUST EXIT           
*                                                                               
GETP05   DS    0H                                                               
         MVC   SVMED,PAYMD         SAVE MEDIA                                   
         MVC   SVSTA,QSTA          SAVE STATION                                 
*                                                                               
         LA    RE,EQVSTATB         CLEAR STA EQUIVALENCY TABLE                  
         LHI   RF,EQVSTATL                                                      
         XCEF                                                                   
*                                                                               
         BRAS  RE,EQSTA                                                         
*                                                                               
GETP08   DS    0H                                                               
         MVC   SVINV,PAYINV1       SAVE INVOICE                                 
*                                                                               
         XC    ZWKRIND,ZWKRIND                                                  
         LA    RE,WRKRBUFF                                                      
         ST    RE,ZWKRBUF                                                       
         LHI   RF,WRKRBEND-WRKRBUFF                                             
         XCEF                                                                   
*                                                                               
         L     RE,AREC2                                                         
         ST    RE,ZWKRREC                                                       
         LHI   RF,4000                                                          
         XCEF                                                                   
*                                                                               
         XC    ZWKRIND,ZWKRIND                                                  
         MVC   ZWKRIND(2),T213FFD+10         UID                                
         GOTO1 VDATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',ZWKRIND,         C        
               ZWKRREC,ZWKRBUF                                                  
         LA    R1,ZWKRIND                                                       
         USING UKRECD,R1                                                        
         MVC   ZWKRFIL,UKUSRINF                                                 
         DROP  R1                                                               
         XC    ZWKRIND,ZWKRIND                                                  
*                                                                               
GETP10   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'INDEX',ZWKRFIL,ZWKRIND,ZWKRREC,        C        
               ZWKRBUF                                                          
         TM    DMCB+8,X'80'        TEST EOF                                     
         BNZ   GETP100                                                          
*                                                                               
         CLI   ZWKRIND+6,X'99'                                                  
         BNE   GETP10                                                           
         CLC   ZWKRIND(2),T213FFD+10         UID                                
         BNE   GETP10                                                           
*                                                                               
         LA    RF,EQVSTA                                                        
         MVC   WORK(4),ZWKRIND+2   STATION IN INDEX                             
         CLI   WORK+3,X'4B'                                                     
         BNE   *+8                                                              
         MVI   WORK+3,C' '                                                      
*                                                                               
GETP40   DS    0H                                                               
         CLC   WORK(4),0(RF)                                                    
         BNE   GETP45                                                           
*                                                                               
         CLC   ZWKRIND+7(1),4(RF)                                               
         BE    GETP50                                                           
*                                                                               
GETP45   DS    0H                                                               
         LA    RF,5(RF)            ADVANCE IN EQV STA TABLE                     
         CLI   0(RF),X'00'         END OF IT?                                   
         BE    GETP10              NEXT INDEX                                   
         CLI   0(RF),X'FF'                                                      
         BE    GETP10                                                           
         B     GETP40              ANOTHER EQV STA HERE, COMPARE AGAIN          
*                                                                               
GETP50   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'READ',ZWKRFIL,ZWKRIND,ZWKRREC,ZWKRBUF           
         TM    DMCB+8,X'80'        IF EOF ON FIRST READ                         
         BNZ   GETP10              SKIP- GET NEXT INDEX                         
*                                                                               
         L     R6,ZWKRREC                                                       
         MVC   BYTE2,1(R6)         REC LEN FOR PARSE                            
         LA    R6,4(R6)            SKIP REC LEN                                 
         MVC   GETPRTYP,0(R6)      SAVE RECORD TYPE                             
         CLC   GETPRTYP,=C'23'      PAYEE RECORD?                               
         BE    GETP55              YES - PARSE IT!                              
         CLC   GETPRTYP,=C'31'      INVOICE RECORD?                             
         BNE   GETP50              NO - READ NEXT                               
         OC    0(L'SPACES,R6),SPACES                                            
*                                                                               
GETP55   DS    0H                                                               
         L     R4,AREC3                                                         
         XCEFL (R4),1000          CLEAR PARSE BLOCK                             
         LR    R1,R6              A(TEXT STRING)                                
         ICM   R1,8,BYTE2         L(TEXT STRING)                                
         L     R2,AREC3           PARSE BLOCK                                   
         BRAS  RE,PARSE                                                         
         L     R4,AREC3                                                         
*                                                                               
* SKIP FIRST FIELDS                                                             
         LHI   RF,9                8 IN INVOICE RECORD                          
         CLC   GETPRTYP,=C'23'     PAYEE RECORD?                                
         BNE   *+8                                                              
         LHI   RF,2                2 IN PAYEE RECORD                            
*                                                                               
GETP65   DS    0H                                                               
         OC    0(5,R4),0(R4)        END OF PARSE BLOCK?                         
         BZ    GETP50               READ NEXT RECORD                            
         LA    R4,5(R4)             SKIP THE FIELD                              
         BCT   RF,GETP65                                                        
*                                                                               
         CLC   GETPRTYP,=C'23'      PAYEE RECORD?                               
         BE    GETP68               GET THE ADDRESS                             
*                                                                               
* INVOICE HEADER HERE                                                           
         OC    0(5,R4),0(R4)        END OF PARSE BLOCK?                         
         BZ    GETP50               READ NEXT RECORD                            
         LLC   R1,0(R4)             LENGTH                                      
         CHI   R1,1                 ANYTHING IN INV. NO. FIELD?                 
         BL    GETP50               NO - READ NEXT RECORD                       
         ICM   RF,15,1(R4)          A(DATA)                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PAYINV1(0),0(RF)     MAKE SURE SAME INVOICE                      
         BNE   GETP50                                                           
*                                                                               
* SAME INVOICE AS SCREEN INPUT                                                  
*                                                                               
         L     R6,AREC3            SQUASHER BUILD AREA                          
         LA    R6,1004(R6)         LEAVE 4 FOR LENGTH                           
         CLC   0(55,R6),SPACES     ANYTHING THERE?                              
         BH    GETP90              YES - PRINT ADDRESS                          
         B     GETP110             NO - PRINT ERROR                             
*                                                                               
* PAYEE ADDRESS RECORD HERE                                                     
GETP68   DS    0H                                                               
         L     R6,AREC3            SQUASHER BUILD AREA                          
         LA    R6,1004(R6)                                                      
         MVI   0(R6),X'40'                                                      
         MVC   1(250,R6),0(R6)                                                  
         SR    RF,RF               SQUASHER INPUT LENGTH                        
*                                                                               
GETP70   DS    0H                  SQUASHER INPUT BUILD LOOP                    
         OC    0(5,R4),0(R4)       END OF TABLE?                                
         BZ    GETP80              YES - SQUASH IT                              
         LLC   RE,0(R4)            DATA LENGTH                                  
         CHI   RE,0                                                             
         BNH   GETP75              EMPTY FIELD                                  
         AR    RF,RE               TOTAL LENGTH FOR SQUASHER                    
         AHI   RF,1                LEAVE SPACE BETWEEN TABLE ENTRIES            
         BCTR  RE,0                                                             
         ICM   R1,15,1(R4)         A(DATA)                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R1)       COPY IT TO SQUASHER BUILD AREA               
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,R6),SPACES      GET RID OF BINARY DATA                       
*                                                                               
GETP75   DS    0H                                                               
         AHI   RE,2             COMPENSATE FOR PREVIOUS BCTR, ADD SPACE         
         AR    R6,RE                                                            
         LA    R4,5(R4)                                                         
         B     GETP70                                                           
*                                                                               
GETP80   DS    0H                                                               
         L     R6,AREC3            SQUASHER BUILD AREA                          
         LA    R6,1004(R6)                                                      
         GOTO1 SQUASHER,DMCB,(R6),(RF)                                          
         L     R6,AREC3            SQUASHER BUILD AREA                          
         LA    R6,1000(R6)                                                      
         MVC   0(4,R6),DMCB+4      SAVE NEW STRING LENGTH                       
         OC    0(4,R6),0(R6)       ANYTHING THERE?                              
         BZ    GETP110             NO - SAY ADDRESS NOT FOUND                   
         B     GETP50              READ NEXT RECORD                             
*                                                                               
GETP90   DS    0H                                                               
         MVC   PAYPADR(L'PAYPADR),SPACES                                        
*                                                                               
         SR    R0,R0                                                            
         LA    R1,PAYPADR                                                       
         CLC   ZWKRIND+2(4),QSTA                                                
         BNE   *+14                                                             
         CLC   ZWKRIND+7(1),QSTA+4                                              
         BE    GETP95                                                           
*                                                                               
         MVI   0(R1),C'('                                                       
         MVI   6(R1),C')'                                                       
         MVC   1(4,R1),ZWKRIND+2                                                
         MVC   5(1,R1),ZWKRIND+7                                                
         LHI   R0,7                                                             
         LA    R1,7(R1)                                                         
*                                                                               
GETP95   DS    0H                                                               
         L     RE,AREC3            SQUASHER AREAA                               
         LA    RE,1000(RE)         A(NEW STRING LENGTH)                         
         SR    RF,RF                                                            
         ICM   RF,15,0(RE)         RF = STRING LENGTH AFTER SQUASHER            
         LA    RE,4(RE)            ADVANCE TO THE STRING ITSELF                 
         BCTR  RF,0                                                             
*                                                                               
         LR    R2,R0               EQV STATION LENGTH                           
         AR    R2,RF               PLUS ADDRESS STRING LENGTH                   
         CHI   R2,L'PAYPADR-1      DO WE FIT IN THE PAYEE ADDR FIELD?           
         BNH   *+10                YES - PROCEED                                
         LHI   RF,L'PAYPADR-1   NO - TRUNCATE ADDR TO FIELD'S LENGTH            
         SR    RF,R0               MINUS LENGTH OF EQV STATION                  
*                                                                               
         CHI   RF,0                LENGTH MUST BE SOMEWHAT POSITIVE             
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
         OI    PAYPADRH+6,X'80'                                                 
         MVC   SVPADDR,PAYPADR                                                  
         B     GETPX                                                            
*                                                                               
GETP100  DS    0H                  PRINT ERROR                                  
         MVC   PAYPADR(L'PAYPADR),=CL55'EASI INVOICE NOT FOUND'                 
         MVC   SVPADDR,PAYPADR                                                  
         OI    PAYPADRH+6,X'80'                                                 
         B     GETPX                                                            
*                                                                               
GETP110  DS    0H                  PRINT ERROR                                  
         MVC   PAYPADR(L'PAYPADR),=CL55'PAYEE ADDRESS NOT FOUND'                
         MVC   SVPADDR,PAYPADR                                                  
         OI    PAYPADRH+6,X'80'                                                 
*                                                                               
GETPX    XIT1                                                                   
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
*                                                                               
EQSTA    NTR1  BASE=*,LABEL=*                                                   
         USING GENOLD,RC,R7                                                     
         USING T213FFD,R3                                                       
         L     R4,VTIA                                                          
         USING VTIAD,R4                                                         
*                                                                               
         LA    RF,EQVSTATB                                                      
         CLI   0(RF),0             HAS TABLE BEEN BUILT ALREADY?                
         BNE   EQVSTA60             YES                                         
*                                                                               
EQVSTA00 XC    KEY2,KEY2                                                        
         LA    R2,EQVSTATL/10                                                   
         LA    R5,EQVSTATB                                                      
         LA    R1,KEY2                                                          
         USING EZSKEY,R1                                                        
         MVC   EZSKTYP,=C'ZS'                                                   
         MVC   EZSKAGY,AGYALPHA                                                 
         DROP  R1                                                               
         MVC   KEY2SAVE(40),KEY2                                                
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'GENDIR',KEY2,KEY2          
         B     EQVSTA14                                                         
*                                                                               
EQVSTA10 GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'DMRSEQ'),=C'GENDIR',KEY2,KEY2          
*                                                                               
EQVSTA14 CLC   KEY2(4),KEY2SAVE                                                 
         BNE   EQVSTA20                                                         
         L     R6,AREC2                                                         
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY2+36,(R6),DMWORK          
         CLI   DMCB+8,0            ANY ERROR                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   EQVSTA10                                                         
*                                                                               
         CLI   0(R6),X'02'                                                      
         BNE   EQVSTA10                                                         
*                                                                               
         USING EZSIDEL,R6                                                       
         MVC   0(5,R5),KEY2+EZSKSTA-EZSKEY                                      
         MVC   5(5,R5),EZSCALL                                                  
         LA    R5,10(,R5)                                                       
         BCT   R2,EQVSTA10                                                      
         DC    H'0'                MAKE TABLE LARGER                            
*                                                                               
EQVSTA20 MVI   0(R5),X'FF'                                                      
*                                                                               
* NOW LOOK UP STATION IN TABLE, IF FIND, PUT REPLACEMENT IN RSTA                
*                                                                               
EQVSTA60 XC    EQVSTA,EQVSTA                                                    
         MVI   EQVSTA+(L'EQVSTA),X'FF'                                          
         MVC   EQVSTA(5),QSTA     ORIGINAL STATION                              
         CLI   EQVSTA+3,C'.'                                                    
         BNE   *+8                                                              
         MVI   EQVSTA+3,C' '                                                    
*                                                                               
         OC    EQVSTA(5),SPACES                                                 
*                                                                               
         CLI   QSTA+4,C'C'        FORCE CABLE                                   
         BE    EQVSTA64                                                         
         CLI   QSTA+4,C'S'        AND SYNDICATION                               
         BNE   EQVSTA66                                                         
*                                                                               
EQVSTA64 MVI   EQVSTA+4,C'N'      TO NETWORK                                    
*                                                                               
EQVSTA66 LA    R1,EQVSTA+5        SKIP ORIGINAL STATION                         
         LA    RE,EQVSTATL/10                                                   
         LA    RF,EQVSTATB                                                      
*                                                                               
EQVSTA70 CLI   0(RF),X'FF'         END OF TABLE                                 
         BE    EQVSTAX                                                          
         CLC   EQVSTA(5),5(RF)                                                  
         BE    EQVSTA80                                                         
         LA    RF,10(,RF)                                                       
         BCT   RE,EQVSTA70                                                      
         DC    H'0'                                                             
*                                                                               
EQVSTA80 DS    0H                                                               
         MVC   0(5,R1),0(RF)                                                    
         LA    R1,5(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    EQVSTAX             NO MORE ROOM FOR EQV STATIONS                
         LA    RF,10(,RF)                                                       
         BCT   RE,EQVSTA70                                                      
         DC    H'0'                                                             
*                                                                               
EQVSTAX  J     EXXMODX                                                          
*                                                                               
         GETEL (R6),42,ELCODE                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* R1 - HIGH ORDER BIT - LENGTH, REST - A(DATA)                                  
* R2 - A(PARSE BLOCK)                                                           
* PARSE BLOCK ENTRY IS DEFINED AS FOLLOWS: 1 BYTE - LENGTH                      
*                                          4 BYTES - A(DATA)                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
                                                                                
PARSE    NTR1  BASE=*,LABEL=*                                                   
         LR    R0,R1                                                            
         SRL   R0,24               R0=LENGTH                                    
         LA    R1,0(R1)            GET RID OF LENGTH BYTE                       
         CHI   R0,0                                                             
         BNH   PARSEX                                                           
*                                                                               
PARSE05  DS    0H                                                               
         LR    RE,R1               SAVE START OF DATA                           
         SR    RF,RF               BYTE COUNTER                                 
*                                                                               
PARSE10  DS    0H                  PROCESS CHARACTER                            
         CLI   0(R1),X'15'         END OF DATA?                                 
         BNH   PARSE20             YES - STOP                                   
         CLI   0(R1),X'5E'         DELIMITER? (SEMICOLON)                       
         BE    PARSE20             YES - STOP                                   
*                                                                               
* UPDATE BYTE COUNTER AND LOOK AT NEXT CHARACTER                                
         AHI   RF,1                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,PARSE10          NO MORE CHARACTERS = EOD                     
*                                                                               
PARSE20  DS    0H                  STOP - DELIMITER OR EOD                      
         STCM  RF,1,0(R2)          STORE LENGTH                                 
         STCM  RE,15,1(R2)         STORE A(DATA)                                
*                                                                               
         CHI   R0,0                RAN OUT OF CHARACTERS?                       
         BNH   PARSEX                                                           
         CLI   0(R1),X'15'         ARE WE AT EOD?                               
         BNH   PARSEX              YES - SIMPLY EXIT                            
*                                                                               
* FOUND DELIMITER                                                               
         LA    R1,1(R1)            MOVE PAST IT                                 
         LA    R2,5(R2)            ADVANCE PARSE BLOCK POINTER                  
         B     PARSE05                                                          
*                                                                               
PARSEX   J     EXXMODX                                                          
         LTORG                                                                  
         EJECT                                                                  
*=========================================================*                     
* MAINTAIN CLEARANCE STATUS RECORD                        *                     
* AND RETURN SEQUENCE NUMBER IN STATSEQ                   *                     
*=========================================================*                     
                                                                                
BLDST    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   STATSEQ,0           RESET CURRENT SEQUENCE NUMBER                
         XC    ELEM,ELEM                                                        
         LA    R8,ELEM                                                          
         USING CLSTEL01,R8                                                      
*                                                                               
         MVI   CLSTEL01,X'01'                                                   
**OLD    MVI   CLSTEL01+1,CL01ELLN                                              
         MVI   CLSTEL01+1,CL01ELN3                                              
         MVC   CLSTCLRD,TODAYP                                                  
         MVC   CLSTPID,SVPASSWD      STORE PID                                  
*                                                                               
         MVC   CLSTACC,AGYALPHA      DEFAULT AGENCY                             
         CLC   SVPPROF+14(2),=C'*A'  TEST ALT ACC AGENCY                        
         BNH   BLDST1                                                           
         CLC   SVPPROF+14(2),=C'00'  IGNORE 00                                  
         BE    *+10                                                             
         MVC   CLSTACC,SVPPROF+14    ACC AGENCY                                 
*                                                                               
BLDST1   DS    0H                                                               
         CLC   QREP,SPACES                                                      
         BE    BLDST2                                                           
         MVC   CLSTPYEE,QREP       REP NUMBER                                   
         MVI   CLSTREPT,C'P'       SET REP TYPE = PAYING                        
         OC    SVSPREP,SVSPREP                                                  
         BZ    *+8                                                              
         MVI   CLSTREPT,C'S'       UNLESS IT IS A SPECIAL REP                   
*                                                                               
BLDST2   MVC   CLSTPRD,QPRD+3      PRODUCT NUMBERS                              
         MVC   CLSTPRD2,QPRD2+3                                                 
         MVC   CLSTSTDT(4),SVSTARTP START/END DATES                             
         MVC   CLSTEST,SVKEY+9     ESTIMATE NUMBER (IF ANY)                     
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+14                                                             
         OI    CLSTSTAT,X'01'      SET UNCLEARANCE                              
         MVC   CLSTOCLR,SVPAYDT                                                 
         MVC   CLSTGRS,TOTG                                                     
         MVC   CLSTNET,TOTN                                                     
*                                                                               
         CLI   SVPPROF+0,C'G'                                                   
         BE    *+8                                                              
         OI    CLSTSTAT,X'20'      SET CLEARED NET                              
         OI    CLSTSTAT,X'02'      SET NEW CLEARANCE (03/05 FOLLOW)             
         DROP  R8                                                               
*                                                                               
BLDST10  LA    R8,KEY                                                           
         USING CLRSTATD,R8                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   CLSKTYPE,=X'0D76'                                                
         MVC   CLSKAGMD,SVKEY      A-M                                          
         MVC   CLSKCLT,SVKEY+1     CLT                                          
         MVC   CLSKMKT(5),SVKEY+4  MKT/STA                                      
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BE    BLDST12                                                          
         CLI   CLSKMKT+2,X'E8'     CABLE IS IT ?                                
         BL    *+8                 NEIN                                         
         NI    CLSKMKT+4,X'80'     YAH - DROP NETWORK FOR LISA                  
         B     BLDST14                                                          
*                                                                               
BLDST12  CLI   PAYMD,C'N'          TEST NETWORK                                 
         BNE   BLDST14                                                          
         MVI   CLSKMKT+4,0         DROP LAST BYTE                               
         DROP  R8                                                               
*                                                                               
BLDST14  MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     SAME TYPE/CLT/MKT/STAT                       
         BE    BLDST20             YES - UPDATE LAST RECORD                     
*                                                                               
BLDST16  L     R8,AREC2                                                         
         ST    R8,AREC                                                          
         XC    0(256,R8),0(R8)                                                  
         USING CLRSTATD,R8                                                      
*                                                                               
         MVC   CLSKEY,KEYSAVE      USE THE KEY FROM READ HIGH                   
         XC    KEYSAVE,KEYSAVE     THEN CLEAR SO WILL ADD LATER                 
         MVC   13(2,R8),=H'24'     SET REC LEN WITH NO ELEMENTS                 
         MVC   CLSAGYA,AGYALPHA                                                 
         LA    R8,24(R8)           POINT TO FIRST ELEMENT POSITION              
         ST    R8,FULL             SET FOR NEXT INST                            
         B     BLDST36                                                          
         DROP  R8                                                               
*                                                                               
BLDST20  MVC   KEYSAVE,KEY         SAVE PREVIOUS KEY                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(10),KEYSAVE                                                  
         BE    BLDST20                                                          
* UPDATE LAST RECORD FOR THIS STATION                                           
         MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
         L     R8,AREC2                                                         
         ST    R8,AREC                                                          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
*=================================================================              
* COUNT NUMBER OF INVOICES TO SEE HOW MANY BYTES NEEDED                         
*=================================================================              
                                                                                
         LA    R0,MAXAMTS                                                       
         L     R8,ASVAMTS                                                       
         USING AMTD,R8                                                          
**OLD    LHI   RE,CL01ELLN         ALWAYS ADD ONE 01 ELEM                       
         LHI   RE,CL01ELN3         ALWAYS ADD ONE 01 ELEM                       
*                                                                               
BLDST22  CLI   AMTFLAGS,0          TEST AMOUNT PRESENT                          
         BE    BLDST24             NO - DONE                                    
         CLI   AMTFLAGS,X'20'      TEST COMMENT ONLY                            
         BE    BLDST24                                                          
*                                                                               
         AHI   RE,CL03ELLN         EACH INVOICE ADDS AN 03 ELEM                 
         AHI   RE,CL05ELLN         AND AN 05 ELEM                               
*                                                                               
BLDST24  LA    R8,AMTLEN(R8)                                                    
         BCT   R0,BLDST22                                                       
         DROP  R8                                                               
*                                                                               
BLDST26  L     R8,AREC                                                          
         SR    R0,R0                                                            
         ICM   R0,3,13(R8)                                                      
         AR    R0,RE                                                            
         CHI   R0,3976             TEST EXCEEDS MAX RECSIZE                     
         BL    BLDST30                                                          
                                                                                
* WILL NOT FIT - SET TO ADD NEW RECORD                                          
* BUT FIRST SAVE LAST CLEARANCE SEQUENCE NUMBER FOR TODAY!!!!                   
                                                                                
BLDST28  XC    KEYSAVE,KEYSAVE     SET FLAG TO ADD NEW RECORD                   
*                                                                               
         USING CLSTEL01,R8                                                      
BLDST30  LA    R8,CLSELEMS-CLRSTATD(R8)  POINT TO FIRST ELEMENT                 
         ST    R8,FULL             STORE JUST IN CASE                           
         B     BLDST34                                                          
*                                                                               
BLDST32  SR    R0,R0                                                            
         ICM   R0,1,1(R8)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R8,R0                                                            
*                                                                               
BLDST34  DS    0H                                                               
         CLI   0(R8),0                                                          
         BE    BLDST36                                                          
         CLI   0(R8),X'01'                                                      
         BNE   BLDST32                                                          
         ST    R8,FULL             SAVE ELEMENT ADDRESS                         
         B     BLDST32                                                          
*                                                                               
BLDST36  LR    R0,R8               SAVE LAST ELEMENT ADDRESS                    
         L     R8,FULL             GET ADDRESS OF LAST 01 ELEM                  
         ST    R0,FULL             AND NOW SAVE EOR ADDRESS                     
         SR    RE,RE               CLEAR SEQNUM                                 
         CLI   0(R8),0                                                          
         BE    BLDST37                                                          
         CLC   CLSTCLRD,TODAYP     TEST PREVIOUS CLEARANCE TODAY                
         BNE   *+8                 NO                                           
         IC    RE,CLSTCLSQ         YES GET SEQUENCE NUMBER                      
         CHI   RE,255              TEST REACHED LIMIT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDST37  AHI   RE,1                BUMP SEQUENCE                                
         STC   RE,STATSEQ          SET FOR USER                                 
         STC   RE,ELEM+CLSTCLSQ-CLSTEL01   AND SET IN NEW ELEMENT               
*                                                                               
         OC    KEYSAVE,KEYSAVE     TEST ADDING NEW RECORD                       
         BNZ   BLDST37A            NO                                           
         L     R8,AREC                                                          
         MVC   13(2,R8),=H'24'     SET REC LEN WITH NO ELEMENTS                 
         XC    24(256,R8),24(R8)   CLEAR ELEMENTS                               
         LA    R8,24(R8)           POINT TO FIRST ELEMENT POSITION              
         ST    R8,FULL             SET FOR NEXT INST                            
*                                                                               
BLDST37A L     R8,FULL             POINT TO E-O-R                               
*                                                                               
BLDST38  DS    0H                                                               
         GOTO1 VRECUP,DMCB,AREC,ELEM,(C'R',(R8))                                
         CLI   8(R1),C'R'                                                       
         BE    BLDST40                                                          
         L     R8,AREC                                                          
         XC    13(256,R8),13(R8)   CLEAR PART OF IT (LEAVE KEY)                 
         MVC   13(2,R8),=H'24'     SET LENGTH WITH NO ELEMENTS                  
         LA    R8,24(R8)           SET TO ADD FIRST ELEMENT                     
         XC    KEYSAVE,KEYSAVE     THEN CLEAR SO WILL ADD LATER                 
         B     BLDST38             ADD THE ELEMENT                              
*                                                                               
BLDST40  DS    0H                                                               
         LHI   R5,MAXAMTS          SET TO ADD 03/05 ELEMENTS                    
         L     R6,ASVAMTS                                                       
*                                                                               
         USING AMTD,R6                                                          
BLDST42  CLI   AMTFLAGS,0          TEST AMOUNT PRESENT                          
         BE    BLDST44             NO - SKIP                                    
         CLI   AMTFLAGS,X'20'      TEST COMMENT ONLY                            
         BE    BLDST44                                                          
                                                                                
E03      USING CLSTEL03,ELEM                                                    
                                                                                
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'03'                                                       
         MVI   ELEM+1,CL03ELLN                                                  
         MVC   ELEM+2(11),AMTINV                                                
         OC    ELEM+2(12),SPACES                                                
*                                                                               
         SR    R0,R0               ADD 03 ELEM AFTER 01 ELEM                    
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         GOTO1 VRECUP,DMCB,AREC,ELEM,(R8)                                       
                                                                                
E05      USING CLSTEL05,ELEM                                                    
                                                                                
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'05'                                                       
         MVI   ELEM+1,CL05ELLN                                                  
*                                                                               
         LA    R1,E05.CLS5GRS                                                   
         CLI   SVPPROF+0,C'G'                                                   
         BE    *+8                                                              
         LA    R1,E05.CLS5NET                                                   
*                                                                               
         L     R0,AMT                                                           
         CLI   AMTTYPE,C'1'        TEST NORMAL DR                               
         BE    *+6                                                              
         LCR   R0,R0                                                            
         ST    R0,0(R1)            STORE EITHER GROSS OR NET DOLLARS            
*                                                                               
         CLI   AMTTYPE,C'2'        TEST CR                                      
         BNE   *+8                                                              
         OI    E05.CLS5STAT,CLS5STAT_CR                                         
*                                                                               
         CLI   AMTTYPE,C'3'        TEST CK                                      
         BNE   *+8                                                              
         OI    E05.CLS5STAT,CLS5STAT_CK                                         
*                                                                               
         SR    R0,R0               ADD AFTER PREVIOUS                           
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         GOTO1 VRECUP,DMCB,AREC,ELEM,(R8)                                       
*                                                                               
BLDST44  AHI   R6,AMTLEN           NEXT AMOUNT ENTRY                            
         BCT   R5,BLDST42                                                       
         DROP  R6,R8,E03,E05                                                    
*                                                                               
BLDST50  L     R8,AREC                                                          
         CLC   0(13,R8),KEYSAVE    WRITING SAME RECORD READ                     
         BE    BLDST52             YES                                          
         LA    RE,24(R8)                                                        
         USING CLSTEL01,RE                                                      
         MVC   10(2,R8),CLSTCLRD   SET LOW PAY DATE AND SEQNUM                  
         MVC   12(1,R8),CLSTCLSQ                                                
         DROP  RE                                                               
         GOTO1 ADDREC                                                           
         B     BLDSTX                                                           
*                                                                               
BLDST52  MVI   RDUPDATE,C'Y'                                                    
         GOTO1 PUTREC                                                           
*                                                                               
BLDSTX   CLI   SVUNPAY,C'Y'        TEST UNCLEAR                                 
         BNE   BLDSTX2                                                          
* GENERATE PASSIVE POINTER FOR UNCLEARANCE                                      
         L     R8,AREC                                                          
         MVC   KEY,0(R8)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+1,X'80'                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                       
*                                                                               
BLDSTX2  MVC   AREC,AREC1          *** EXIT WITH AREC = REC1 ***                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================*               
* OVERRIDE PST VALUES IN BUY PST ELEMENT WITH INPUT             *               
* CHANGED FEB10 TO OVERRIDE GST VALUES AS WELL                  *               
*===============================================================*               
                                                                                
BLDPST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVAPROF+7,C'C'      FOR CANADIAN NETWORK                         
         BNE   BLDPST2                                                          
         CLI   PAYMD,C'N'                                                       
         BNE   BLDPST2                                                          
*                                                                               
         OC    SVOVRPST,SVOVRPST     TEST ANY VALUES INPUT                      
         BNZ   *+14                                                             
         OC    SVNETPST,SVNETPST     OR ANY NETWORK VALUES                      
         BZ    BLDPST2                                                          
*                                                                               
         MVI   SVPSTEL,X'6B'       BUILD NETWORK PST ELEMENT                    
         MVI   SVPSTEL+1,12                                                     
         MVC   SVPSTEL+2(10),SVNETPST   MOVE NETWORK VALUES                     
*                                                                               
         OC    SVOVRPST,SVOVRPST        TEST FOR OVERRIDES                      
         BZ    *+10                                                             
         MVC   SVPSTEL+2(10),SVOVRPST   OVERRIDES ALWAYS WIN                    
*                                                                               
BLDPST2  L     R8,AREC                                                          
         USING BUYRECD,R8                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
                                                                                
* SEE IF ANY UNPAID SPOTS THIS BUYLINE                                          
                                                                                
BLDPST4  ICM   R0,1,1(R6)                                                       
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BLDPSTX             ALL PAID - DO NOTHING                        
         CLI   0(R6),6                                                          
         BL    BLDPST4                                                          
         CLI   0(R6),13                                                         
         BH    BLDPST4                                                          
         OC    4(2,R6),4(R6)                                                    
         BNZ   BLDPST4                                                          
*                                                                               
* NOT ALL PAID - MAKE SURE ALL UNPAID                                           
*                                                                               
BLDPST10 LA    R6,BDELEM                                                        
*                                                                               
BLDPST12 ICM   R0,1,1(R6)                                                       
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BLDPST20                                                         
         CLI   0(R6),6                                                          
         BL    BLDPST12                                                         
         CLI   0(R6),13                                                         
         BH    BLDPST12                                                         
         OC    4(2,R6),4(R6)                                                    
         BZ    BLDPST12                                                         
                                                                                
*===================================================================*           
* UNFORTUNATELY, NEED TO ALLOW OVERRIDE IF IT IS THE SAME AS EXISTS *           
* THIS HAPPENS IN CASES WHERE SOME BUYLINES HAVE BEEN PAID FOR      *           
* PREVIOUS MONTHS AND OTHERS HAVE NOT BEEN PAID AT ALL, SO THEY     *           
* HAVE NO EXISTING OVERRIDES AND USER MUST INPUT THEM               *           
*===================================================================*           
                                                                                
         CLI   SVGSTVAL,0          TEST ANY GST OVERRIDE                        
         BE    BLDPST14            NO                                           
         MVI   BYTE,X'6A'          GET GST ELEMENT                              
         BAS   RE,GETPSTEL         TEST FOR EXISTING GST ELEMENT                
         BNE   BLDPSTE1            IF NOT THERE, NO MATCH --> ERROR             
         CLC   2(1,R6),SVGSTVAL    TEST SAME VALUES                             
         BNE   BLDPSTE1                                                         
*                                                                               
BLDPST14 OC    SVPSTEL+2(10),SVPSTEL+2  ANY PST OVERRIDES?                      
         BZ    BLDPSTX                  NO - EXIT                               
         MVI   BYTE,X'6B'               GET PST ELEMENT                         
         BAS   RE,GETPSTEL              TEST FOR EXISTING PST ELEMENT           
         BNE   BLDPSTE1                 IF NO EL, NO MATCH --> ERROR            
         CLC   2(10,R6),SVPSTEL+2       TEST SAME OVERRIDES                     
         BE    BLDPSTX                                                          
*                                                                               
BLDPSTE1 CLI   SVUNPAY,C'Y'            BUT IF UNPAYING                          
         BE    BLDPSTX                  FORGET THE ERROR                        
         LA    R2,PAYOPH                                                        
         MVI   ERRCD,BADPSTPD                                                   
         MVI   ERRAREA,X'FE'            UNWIND TRANSACTION!                     
         GOTO1 ERROR                                                            
                                                                                
*================================================================               
* NO PAID SPOTS - INSERT NEW OR CHANGE EXISTING ELEMENTS                        
*================================================================               
                                                                                
BLDPST20 DS    0H                                                               
         CLI   SVGSTVAL,0          TEST GST OVERRIDE                            
         BE    BLDPST30            NO                                           
*                                                                               
         MVI   BYTE,X'6A'          GET GST ELEMENT                              
         BAS   RE,GETPSTEL         FIND GSTEL                                   
         BE    BLDPST22                                                         
         MVI   DUB,X'6A'           INSERT NEW ELEMENT                           
         MVI   DUB+1,3                                                          
         MVC   DUB+2(1),SVGSTVAL                                                
         GOTO1 VRECUP,DMCB,(C'S',BUYREC),DUB,(R6)                               
         B     BLDPST30                                                         
*                                                                               
BLDPST22 MVC   2(1,R6),SVGSTVAL    MOVE NEW GST VALUE TO ELEMENT                
         B     BLDPST30                                                         
*                                                                               
BLDPST30 OC    SVPSTEL+2(10),SVPSTEL+2 TEST ANY PST OVERRIDES                   
         BZ    BLDPSTX                 NO                                       
*                                                                               
         MVI   BYTE,X'6B'          GET PST ELEMENT                              
         BAS   RE,GETPSTEL         FIND PSTEL                                   
         BNE   BLDPST24                                                         
         GOTO1 VRECUP,DMCB,(C'S',BUYREC),(R6)                                   
*                                                                               
BLDPST24 GOTO1 VRECUP,DMCB,(C'S',BUYREC),SVPSTEL,(R6)                           
         B     BLDPSTX                                                          
*                                                                               
BLDPSTX  DS    0H                                                               
         XIT1                                                                   
                                                                                
* FIND GST OR PST ELEMENT IN BUY RECORD - ELCODE IS IN BYTE                     
                                                                                
GETPSTEL LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
GETPST2  ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GETPSTNX                                                         
         CLC   0(1,R6),BYTE                                                     
         BNE   GETPST2                                                          
         BR    RE                  RETURN WITH CC EQ                            
*                                                                               
GETPSTNX LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
         LTORG                                                                  
         DROP  R8                                                               
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    CL1                                                              
LOCKCLT  DS    CL3                                                              
LOCKSTA  DS    CL5                                                              
         ORG   LOCKSTA                                                          
LOCKEST  DS    CL3                                                              
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE SPPAYWORK                                                      
         PRINT OFF                                                              
         EJECT                                                                  
GENOLD   DSECT                                                                  
         ORG   REC1+1024                                                        
REQHDR   DS    0D                                                               
       ++INCLUDE DMREQHDR                                                       
         ORG   REQHDR                                                           
ZCTL     DS    CL26                                                             
ZAREA    DS    0CL160                                                           
ZTYPE    DS    CL2                                                              
ZAGY     DS    CL2                                                              
ZMED     DS    CL1                                                              
ZCLT     DS    CL3                                                              
ZPGR     DS    CL1                                                              
ZMGR     DS    CL1                                                              
         DS    CL1                                                              
         ORG   ZPGR                                                             
ZACCEST  DS    CL3                 ESTIMATE NUMBER FOR ACC CLEARANCE            
ZPRD     DS    CL3                                                              
ZMKT     DS    CL4                                                              
         ORG   ZMKT                FOR CHECK REQUESTS ONLY                      
ZSTATUS  DS    CL1                                                              
ZSTATUS_ONLINE EQU X'80'           ON-LINE POSTING TO ACC                       
ZSTATUS_AUTPAY EQU X'40'           GENERATED BY AUTOPAY (BEALS)                 
         DS    CL1                                                              
ZBMKT    DS    XL2                 MKT FOR CHECK REQUESTS BY MGRPID             
*                                                                               
ZSTA     DS    CL5                                                              
ZDATE    DS    CL6                                                              
ZEST     EQU   ZDATE                                                            
ZREP     DS    CL3                                                              
ZREPTYPE DS    CL1                                                              
ZAGYCODE DS    CL2                 OVERRIDE AGENCY CODE FOR ACC                 
ZCONT    DS    CL1                                                              
ZSEQNUM  DS    XL1                 CLEARANCE SEQUENCE NUMBER                    
ZSTART   DS    CL6                                                              
ZEND     DS    CL6                                                              
ZPRD2    DS    CL3                                                              
ZPRD3    DS    CL3                                                              
ZMODE    EQU   ZPRD3                                                            
*                                                                               
ZAMTTYPE DS    CL1                                                              
ZAMT     DS    PL5                                                              
*                                                                               
ZGSTCD   DS    CL1                                                              
ZGST     DS    XL4                                                              
*                                                                               
         ORG   ZAMT                                                             
ZICLAMT  DS    CL10                ICL STILL GETS EBCDIC                        
*                                                                               
ZOFFICE  DS    CL2                 SPECIAL OFFICE CODE                          
ZUESTOR  DS    CL12                                                             
ZINV     EQU   ZUESTOR                                                          
*                                                                               
Z2AREA   DS    0CL80               REQUEST CARD EXTENSION                       
ZPST     DS    CL42                6 7-BYTE PST FIELDS                          
         ORG   ZPST                                                             
ZPSTPROV DS    CL2                 PST PROVINCE CODE                            
ZPSTCD   DS    CL1                 PST CODE                                     
ZPSTAMT  DS    XL4                 PST AMOUNT                                   
*                                                                               
         ORG   Z2AREA                                                           
ZCTADATA DS    5XL10               5 10-BYTE CTA FIELDS                         
         ORG   ZCTADATA                                                         
ZCTACON  DS    PL4                                                              
ZCTAGRS  DS    PL6                                                              
         ORG   Z2AREA+50                                                        
Z2GRSAMT DS    PL6                 GROSS AMOUNT                                 
Z2PID    DS    XL2                 PID NUMBER                                   
Z2PNET2  DS    PL6                                                              
Z2FLAG   DS    XL1                                                              
Z2FGRCAL EQU   X'80'               Z2GRSAMT IS CALCULATED FROM NET              
Z2NOUSE  EQU   X'40'               DON'T USE THE 40 BIT - SPACE PADDED          
Z2PSTHEX EQU   X'20'               PST IS IN HEX                                
*                                                                               
Z2DIFF   DS    PL3                 DIFFERENCE BETWEEN FILE/INVOICES             
Z2INVAMT DS    PL5                 ACTUAL INVOICE AMOUNT                        
Z2SPARE  DS    CL5                                                              
Z2CTA#   DS    CL1                                                              
Z2CTA    DS    CL1                 C'T' = CTA TRADE BUY                         
*                                                                               
ZCOM1    DS    CL40                                                             
ZCOM2    DS    CL40                                                             
ZCOM3    DS    CL40                                                             
ZCOM4    DS    CL40                                                             
ZCOM5    DS    CL40                                                             
ZSPARE   DS    CL40                                                             
         EJECT                                                                  
       ++INCLUDE SPGENPSR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLRST                                                     
         EJECT                                                                  
       ++INCLUDE SPCIBBLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAPY                                                       
       ++INCLUDE SPGENXAPY                                                      
       ++INCLUDE SPGENCLT                                                       
         SPACE 2                                                                
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
* DDCOREQUS                                                                     
* FATWA                                                                         
         PRINT OFF                                                              
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FATWA                                                          
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATIOB                                                         
*                                                                               
       ++INCLUDE EZBLOCK                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE SPGENEZ                                                        
*                                                                               
         PRINT ON                                                               
*                                                                               
VTIAD    DSECT                                                                  
KEY2     DS    XL40                                                             
KEY2SAVE DS    XL40                                                             
ZWKRREC  DS    A                                                                
ZWKRBUF  DS    A                                                                
ZWKRIND  DS    XL44                                                             
ZWKRFIL  DS    CL8                                                              
*                                                                               
EQVSTATB DS    3500X                                                            
EQVSTATL EQU   *-EQVSTATB                                                       
*                                                                               
WRKRBUFF DS    14336X                                                           
WRKRBEND EQU   *                                                                
*                                                                               
VTIADLQ  EQU   *-VTIAD                                                          
*                                                                               
*                                                                               
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'130SPPAY00   07/01/19'                                      
         END                                                                    
