*          DATA SET PPBUY17    AT LEVEL 076 AS OF 01/25/21                      
*PHASE T41117A                                                                  
*INCLUDE WRKIO                                                                  
*INCLUDE PPGETCU                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY17 - PROCESSING WORKER FILE FOR ADBUYER'                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 09/19/14 PRISMA/RADIA ORIGIN                                             
*                                                                               
* KWAN 04/26/13 BUY MOVE FOR PRISMA - ADJUST SPACE DECRIPTION                   
*                                                                               
* KWAN 01/04/12 DRAFT INSERTION FIX FOR 100% AGY COMMISSION                     
*                                                                               
* KWAN 04/07/11 REPLY ERROR FOR BUY MOVE ON IDESK ESTIMATES                     
*                                                                               
* KWAN 02/02/11 CORRECT ELEM LENGTH FOR OUTDOOR SPACE DESCRIPTION               
*                                                                               
* KWAN 10/27/10 DRAFT REPLY - COS2 $, VERSION 4.0.0.4+                          
*                                                                               
* KWAN 09/28/10 NEGATIVE TEN THOUSAND RATE CHANGE - DIFFERENT USERS ERR         
*                                                                               
* KWAN 07/16/10 ADJUST RATE INPUT FROM IDESK (REMOVE COMMAS)                    
*                                                                               
* KWAN 06/01/10 TOKEN FOR DRAFT ADD, 'IMPORT' BUY ORIGIN                        
*                                                                               
* KWAN 02/18/10 FMT_COST FOR ADBUYER, NEGATIVE RATE ADJUSTMENTS                 
*                                                                               
* KWAN 08/19/09 LARGE NEWSPAPER RATE CHANGES FOR LIMIT ACCESS USERS             
*                                                                               
* KWAN 02/06/09 BUY MOVE FOR IDESK (BILLED/PAID INSERTION DATE CHANGE)          
*                                                                               
* KWAN 11/19/08 INSERTION DATE W/ REF# FOR IDESK RESPONSE                       
*                                                                               
* KWAN 07/22/08 IDESK HEX SECURITY CONTROL                                      
*                                                                               
* KWAN 06/24/08 CHANGE IDESK PLACEMENTS FROM MONTHLY TO DAILY                   
*                                                                               
* KWAN 03/20/08 FOREIGN EXCHANGE CALCULATIONS                                   
*                                                                               
* KWAN 03/04/08 HEX SECURITY FIX FOR DRAFT CHANGE UPLOAD                        
*                                                                               
* KWAN 10/01/07 IDESK INSERTION UPLOAD                                          
*                                                                               
* KWAN 05/08/07 CONTRACT UNIT VALUE DRAFT REPLY                                 
*                                                                               
* KWAN 11/17/06 PLANNED COST DRAFT REPLY                                        
*                                                                               
* KWAN 09/07/06 REVISE INVOICE UPLOAD                                           
*                                                                               
* KWAN 02/17/06 MOVE INSERTION UPLOAD                                           
*                                                                               
* KWAN 10/06/05 SHOWINGS, REG PANELS AND ILLUMINATED PANEL UPLD FIX             
*                                                                               
* KWAN 09/14/05 CK FOR UPLD SEC (NOT ACTIVE, BUY IS NOT ACCESS AWARE)           
*                                                                               
* KWAN 08/22/05 DRAFT INSERTION REPLY FOR COS2 FACTOR FOR 3.3.0.8+              
*                                                                               
* KWAN 07/07/05 CHECK LOCK FOR CHANGE AND DELETE UPLOADS                        
*                                                                               
* KWAN 04/27/05 DROP CENTS TO FIT LEADING DIGIT FOR COLOR PREMIUMS              
*                                                                               
* KWAN 04/20/05 DRAFT ADD - REPLY RATE CODE FOR 3.2.0.3 +                       
*                                                                               
* KWAN 03/24/05 AD-ID UPLOAD                                                    
*                                                                               
* KWAN 03/14/05 NO ADBUYER UPLOADS FOR LOCKED EST, EXCEPT DRAFT ADD             
*                                                                               
* KWAN 12/26/05 ALLOW CUSTOM COLUMN UPLOADS FOR HEX SECURITY X'10'              
*                                                                               
* KWAN 03/21/04 REPLY CONTRACT LINEAGE EQUIVALENCY (CLE) FOR DRAFT ADD          
*                                                                               
* KWAN 02/24/04 HEX SECURITY FOR AB (T411FFD+12) AND RESTRUCTURING              
*                                                                               
* KWAN 02/12/04 ALLOW OTHER SPECIAL CHARS IN NEWSPAPER SPACE                    
*                                                                               
* KWAN 12/18/03 FIX LESS THAN A DOLLAR RATE CHANGES                             
*                                                                               
* KWAN 12/08/03 FIX BUY INITIAL BUG FOR AB 1.2.X.X                              
*                                                                               
* KWAN 10/27/03 GLOBBER CALL TO GET AND SAVE PC VERSION NUMBER                  
*                                                                               
* KWAN 10/10/03 DRAFT ADD REPLY - NET AND GROSS PAYABLE                         
*                                                                               
* KWAN 03/20/03 FIXES FOR ADBUYER 1.1                                           
*                                                                               
* KWAN 01/14/03 FORMAT RATE FOR NONE NEWSPAPER - BUG FIX                        
*                                                                               
* KWAN 10/02/02 DESCRIPTIVE MSG FOR MTRL CLS & SPC CLS DATES                    
*                                                                               
* KWAN 04/01/02 REPLY WARNING MSGS                                              
*                                                                               
* KWAN 10/12/01 CODES FOR PROCESSING WORKER FILE                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41117 - ADBUYER UPLOAD TRAFFICE CONTROLLER'                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  PPBUY00                                               *         
*                                                                     *         
*  COMMENTS     PROCESSES MAP DATA IN WORKER FILER                    *         
*                                                                     *         
*  INPUTS       NO SCREENS                                            *         
*                                                                     *         
*  OUTPUTS      ALL DATA WILL BE RETURNED TO WORKER FILE              *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- PROGRAM LITERALS                                *         
*               R9 -- GLOBAL STORAGE AREA                             *         
*               RA -- TWA                                             *         
*               RB -- BASE REGISTER                                   *         
*               RC -- GEND                                            *         
*               RD -- REGISTER CHAIN                                  *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41117 - ADBUYER UPLOAD TRAFFICE CONTROLLER'                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41117   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORK17X-WORK17D,T41117,RR=RE,CLEAR=YES                           
*                                                                               
         LR    R9,RC                                                            
         USING WORK17D,R9          R9 = A(GLOBAL STORAGE)                       
*                                                                               
         BASR  R8,0                                                             
         AHI   R8,GLOBALS-*                                                     
         USING GLOBALS,R8          R8 = A(GLOBAL LITERALS)                      
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         ST    RE,RELO17                                                        
*                                                                               
         L     RE,=V(WRKIO)                                                     
         A     RE,RELO17                                                        
         ST    RE,VWRKIO                                                        
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO1-WORK17D)                                            
         ST    RE,AWKAIO1                                                       
*                                                                               
         XC    WRKRCCNT,WRKRCCNT                                                
         MVI   SVSCRN,MAG          ORIGINAL (BASIC) SCREEN IS MAGAZINE          
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVI   CHGIND1,0           INITIALIZE CHANGE INDICATORS                 
         MVI   CHGIND2,0                                                        
         MVI   CHGIND3,0                                                        
         MVI   CHGIND4,0                                                        
         MVI   CHGIND5,0                                                        
*                                                                               
         B     *+8                                                              
         BRAS  RE,GETSECVL                                                      
*                                                                               
         B     PROCWR              START PROGRAM                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
XIT_R3R4 XIT1  REGS=(R3,R4)        WRK REC POINTER AND LENGTH                   
*                                                                               
XIT_R3   XIT1  REGS=(R3)           DON'T RESTORE WORKER FILE ELEM PTR           
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* GLOBBER CALL AND PREPARE WORKER FILE TO BE PROCESSED                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROCWR   DS    0H                                                               
         LA    R0,WRKIOPAR                                                      
         LHI   R1,50*4                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         LA    R4,WRKIOPAR                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         GOTOR VGLOBBER,DMCB,=C'GETD',WRKWKEY,L'WRKWKEY,GLVDLUWF                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T GET WORKER KEY                         
*                                                                               
         GOTOR (RF),(R1),=C'GETD',PCVERSN#,L'PCVERSN#,GLVDLUV#                  
*                                                                               
         GOTOR (RF),(R1),=C'CLEAR'                                              
*                                                                               
         MVI   WRKIFTYP,WRKIFTWF   SET TO OPEN A WORKER FILE                    
         MVI   WRKIACTN,WRKIAOPN   OPEN THE PASSED WORKER FILE                  
         MVC   WRKIACOM,ACOMFACS                                                
         L     RE,VTIA                                                          
         ST    RE,WRKIAREC                                                      
         AHI   RE,4096                                                          
         ST    RE,WRKIABUF                                                      
*                                                                               
         GOTOR VWRKIO,WRKIOB                                                    
         BE    *+6                                                              
         DC    H'0'                CAN'T OPEN WORKER FILE                       
*                                                                               
         SR    R0,R0               R0=RECORD NUMBER                             
PROCW10  AHI   R0,1                READ FIRST/NEXT WORKER FILE RECORD           
         ICM   R3,15,WRKIAREC      R3=A(RECORD)                                 
         STCM  R0,15,0(R3)                                                      
         ST    R0,WRKRNUMB                                                      
         MVC   4(4,R3),=C'REC#'                                                 
         XC    8(4,R3),8(R3)                                                    
*                                                                               
         GOTOR VDATAMGR,WRKIPARM,(0,=C'RANDOM')                                 
         BNE   PROCW90                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DETERMINE TRANSACTION CODE AND UPLOAD MODE                                    
* THEN CHECK FOR HEX SECURITY                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVI   DDLINKSW,0          INIT LINK SWITCH FOR UPLOAD MODE             
         MVI   LKDRFTSW,0          INIT LINK SWITCH FOR DRAFT MODE              
         MVI   ABUPLDSW,0          INIT ADBUYER UPLOAD BITS SWITCH              
         MVI   PLINKSW1,0          INIT PRINT LINK SWITCH 1                     
         XC    WRKMAPCD,WRKMAPCD   MAP CODE IN ERROR (FOR ERROR REPLY)          
         XC    SVULDATE,SVULDATE                                                
         XC    SVULTIME,SVULTIME                                                
         XC    SVIDSKV#,SVIDSKV#                                                
         XC    REQTOKEN,REQTOKEN   CLEAR REQUEST TOKEN                          
         MVI   PRSMAPSW,0          INIT PRISMA MAP CODE SWITCH                  
*                                                                               
         AHI   R3,4                POINT TO ELEM                                
         ST    R3,SVWRKELM         SAVE ELEM POINTER                            
         USING LQ_EL,R3                                                         
*                                                                               
PROCW20  CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    PROCW23                                                          
         CLI   LQ_EL,LQ_IMAPQ      INPUT MAP NUMB ELEM (UPLOAD MODE)?           
         BNE   *+12                                                             
         BRAS  RE,CKTRCODE         WILL DETERMINE TRANSACTION CODE              
         BE    PROCW23                                                          
         BRAS  RE,NXTWFELM                                                      
         B     PROCW20             SHOULD ONLY BE PROCESSED ONCE                
*                                                                               
PROCW23  B     PROCW23F                                                         
         CLI   SEC_UPLD,C'Y'       UPLOAD IS ALLOWED?                           
         BE    *+16                                                             
         LHI   R0,FACCERR                                                       
         BRAS  RE,GET_ETXT                                                      
         B     PROCW80                                                          
*                                                                               
PROCW23F L     R3,SVWRKELM         POINT TO BEGINNING OF ELEM AGAIN             
         BRAS  RE,CK_HXSEC         CHECK FOR HEX SECURITY CONTROL               
         BE    PROCW23H                                                         
         LHI   R0,FACCERR                                                       
         BRAS  RE,GET_ETXT                                                      
         B     PROCW80                                                          
*                                                                               
PROCW23H CLI   DDLINKSW,C'K'       IDESK INSERTION UPLOAD                       
         BE    PROCW23M                                                         
         CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    PROCW23M                                                         
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   *+12                                                             
         BRAS  RE,CKDRFMOD         WILL DETERMINE DRAFT MODE                    
         BE    PROCW23M                                                         
         BRAS  RE,NXTWFELM                                                      
         B     PROCW23H            SHOULD ONLY BE PROCESSED ONCE                
*                                                                               
PROCW23M BRAS  RE,CKUPLMOD         CK UPLOAD MODES                              
         BRAS  RE,CKIDKMOD         CK IDESK EXCEPTION MODES                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BUILD TOP BUY SCREEN (SAME FOR ALL MEDIAS) AND CALL T41101                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         L     R3,SVWRKELM         POINT TO BEGINNING OF ELEM AGAIN             
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BE    *+12                                                             
         CLI   DDLINKSW,C'D'       DELETE INSERTION UPLOAD?                     
         BNE   PROCW25                                                          
*                                                                               
         BRAS  RE,READBUYR         READ BUY RECORD VIA PASSIVE KEY              
         BNE   PROCW80                                                          
         B     PROCW30                                                          
*                                                                               
PROCW25  CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    PROCW30                                                          
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BE    *+12                                                             
PROCW27  BRAS  RE,NXTWFELM                                                      
         B     PROCW25                                                          
         BRAS  RE,CKTOPSCR         WILL BUILD TOP BUY SCREEN                    
         BNE   PROCW80                                                          
         B     PROCW27                                                          
*                                                                               
PROCW30  XC    BUYTR1,BUYTR1       NO TRANSACTION CODE YET                      
         MVI   BUYTR1H+5,0                                                      
         OI    BUYTR1H+6,X'80'                                                  
         MVI   DMCB,X'01'          CALL T41101 TO VALIDATE TOP SCR              
         BRAS  RE,CBUYOVLY         T41101 WILL ALSO GET MIDDLE SCR              
*                                                                               
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    PROCW30L                                                         
         CLI   DDLINKSW,C'N'       ADD? (NOTE: ONLY FOR ADBUYER)                
         BE    *+8                                                              
         CLI   DDLINKSW,C'C'       CHANGE?                                      
         BE    *+8                                                              
         CLI   DDLINKSW,C'D'       DELETE?                                      
         BNE   PROCW30L                                                         
         BRAS  RE,TSTLOCK          CHECK FOR LOCKS                              
         BE    *+16                                                             
         LHI   R0,DATALOCK                                                      
         BRAS  RE,GET_ETXT                                                      
         B     PROCW80                                                          
*                                                                               
PROCW30L CLI   ERRAREA,C'D'        FAKE ERROR (ACTION COMPLETED)?               
         BE    *+12                                                             
         CLI   ERRAREA,0           ERRORS DETECTED FROM TOP SCREEN?             
         BNE   PROCW80             YES, GO BUILD RETURN ERROR ELEM              
*                                                                               
         XC    BUYTR1,BUYTR1       CLEAR FIELD FIRST                            
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    *+12                                                             
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BNE   *+8                                                              
         MVI   BUYTR1,C'B'         TRANSACTION CODE IS BUY                      
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BE    *+12                                                             
         CLI   DDLINKSW,C'D'       DELETE INSERTION UPLOAD?                     
         BNE   *+8                                                              
         MVI   BUYTR1,C'R'         TRANSACTION CODE IS RECALL                   
*                                                                               
         MVI   BUYTR1H+5,1         INPUT LENGTH                                 
         OI    BUYTR1H+6,X'80'     TRANSMIT                                     
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    PROCW33                                                          
         CLI   SVESPROF,C'1'       LOCKED OUT                                   
         BE    *+12                                                             
         CLI   SVESPROF,C'2'       PERM LOCKOUT                                 
         BNE   PROCW33                                                          
         LHI   R0,LOCKERR                                                       
         BRAS  RE,GET_ETXT                                                      
         B     PROCW80                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BUILD AND VALIDATE MIDDLE BUY SCREEN FOR DIFFERENT MEDIAS                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROCW33  CLI   BUYTR1,C'R'         TRANSACTION CODE IS RECALL?                  
         BNE   PROCW50                                                          
         MVI   DMCB,X'05'                                                       
         BRAS  RE,CBUYOVLY         CALL T41105 TO RECALL INSERTION              
         CLI   ERRAREA,C'D'        FAKE ERROR (ACTION COMPLETED)?               
         BE    *+12                                                             
         CLI   ERRAREA,0           ERRORS DETECTED FROM TOP SCREEN?             
         BNE   PROCW80             YES, GO BUILD RETURN ERROR ELEM              
*                                                                               
         XC    BUYTR1,BUYTR1       PREPARE FOR NEXT TRANSACTION CODE            
         MVI   ERRAREA,0           RESET ERROR SWITCH                           
*                                                                               
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   PROCW43                                                          
*                                                                               
         TM    ABUPLDSW,ABRVINVQ   REVISE INVOICE UPLOAD?                       
         BZ    *+16                                                             
         BRAS  RE,PRC_RVIV         PROCESS REVISE INVOICE                       
         BNE   PROCW80                                                          
         B     PROCW85                                                          
*                                                                               
         MVI   BUYTR1,C'C'         TRANSACTION CODE IS CHANGE                   
         MVI   BUYTR1H+5,1                                                      
         OI    BUYTR1H+6,X'80'                                                  
         L     R3,SVWRKELM         POINT TO BEGINNING OF ELEM AGAIN             
*                                                                               
PROCW35  CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    PROCW40                                                          
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BE    *+12                                                             
PROCW37  BRAS  RE,NXTWFELM                                                      
         B     PROCW35                                                          
         BRAS  RE,CKTOPSCR         BUYER & INS DATE MAY HAVE CHANGED            
         BNE   PROCW80                                                          
         BRAS  RE,CKMIDSCR         BUILD MIDDLE SCREEN                          
         BNE   PROCW80                                                          
         B     PROCW37                                                          
*                                                                               
PROCW40  BRAS  RE,CKFLDADJ         FIELD ADJUSTMENT                             
         BRAS  RE,CKFLDLEN         CHK FOR FIELD LENGTH                         
*                                                                               
         MVI   DMCB,X'21'          CALL T41121 TO VALIDATE AD-ID                
         BRAS  RE,CBUYOVLY                                                      
         CLI   ERRAREA,0           ERRORS DETECTED FROM AD-ID MAP CODE?         
         BNE   PROCW80             YES, GO BUILD RETURN ERROR ELEM              
         MVC   SV_AD_ID,WORK+L'PJOBKJOB                                         
         TM    ABUPLDSW,ABADIDAQ   AD-ID ALONE UPLOAD?                          
         BZ    PROCW45                                                          
         MVI   DMCB,X'03'          VALIDATE AD CODE FOR AD-ID ALONE             
         MVI   WRKCBOPT,EDTADCDQ                                                
         BRAS  RE,CBUYOVLY                                                      
         CLI   ERRAREA,0           ERRORS DETECTED FROM AD-ID MAP CODE?         
         BNE   PROCW80             YES, GO BUILD RETURN ERROR ELEM              
         B     PROCW45                                                          
*                                                                               
PROCW43  CLI   DDLINKSW,C'D'       DELETE INSERTION UPLOAD?                     
         BE    *+6                                                              
         DC    H'0'                NO OTHER MODE CAN GET HERE!                  
         MVC   BUYTR1(2),=C'DL'    TRANSACTION CODE IS DELETE                   
         MVI   BUYTR1H+5,2                                                      
         OI    BUYTR1H+6,X'80'                                                  
*                                                                               
PROCW45  XC    TRADDR,TRADDR       NXTTR ROUTINE WILL SET THIS                  
*                                                                               
         LA    RE,MEDOVTAB                                                      
PROCW47  CLI   0(RE),X'FF'         END OF MEDIA OVERLAY TABLE?                  
         BNE   *+6                                                              
         DC    H'0'                MEDIA CODE NOT DEFINED!                      
         CLC   BUYMD(1),0(RE)                                                   
         BE    *+12                                                             
         LA    RE,2(RE)            NEXT ENTRY IN MEDIA OVERLAY TABLE            
         B     PROCW47                                                          
*                                                                               
         MVC   SVOVLAY#,1(RE)                                                   
         MVC   DMCB(1),1(RE)       OVERLAY NUMBER FOR MEDIA                     
         BRAS  RE,CBUYOVLY         CALL T41110,11,12,13 FOR MIDDLE SCR          
*                                                                               
         CLI   ERRAREA,C'D'        FAKE ERROR (ACTION COMPLETED)?               
         BE    *+12                                                             
         CLI   ERRAREA,0           ERRORS DETECTED FROM MIDDLE SCR?             
         BNE   PROCW80             YES, GO BUILD RETURN ERROR ELEM              
*                                                                               
         B     PROCW85             NO ERROR ENCOUNTERED                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* UPLOAD MODE IS DRAFT OR ADD INSERTION (TRANSACTION IS BUY)                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROCW50  BRAS  RE,CKINSMDL         CK FOR MOVE INSERTION - DELETION             
         BNE   PROCW80                                                          
*                                                                               
         L     R3,SVWRKELM         POINT TO BEGINNING OF WRK FILE ELEM          
PROCW55  CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    PROCW65                                                          
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BE    *+12                                                             
PROCW60  BRAS  RE,NXTWFELM                                                      
         B     PROCW55                                                          
         BRAS  RE,CKMIDSCR         CHK FOR MIDDLE SCREEN                        
         BNE   PROCW80                                                          
         B     PROCW60                                                          
*                                                                               
PROCW65  BRAS  RE,CKPRSMFD         CHECK FOR PRISMA FIELD ADJUSTMENTS           
*                                                                               
         XC    TRADDR,TRADDR       NXTTR ROUTINE WILL SET THIS                  
*                                                                               
         MVI   DMCB,X'21'          CALL T41121 TO VALIDATE AD-ID                
         BRAS  RE,CBUYOVLY                                                      
         CLI   ERRAREA,0           ERRORS DETECTED FROM AD-ID MAP CODE?         
         BNE   PROCW80             YES, GO BUILD RETURN ERROR ELEM              
         MVC   SV_ADCOD,WORK                                                    
         MVC   SV_AD_ID,WORK+L'PJOBKJOB                                         
*                                                                               
         LA    RE,MEDOVTAB                                                      
PROCW70  CLI   0(RE),X'FF'         END OF MEDIA OVERLAY TABLE?                  
         BNE   *+6                                                              
         DC    H'0'                MEDIA CODE NOT DEFINED!                      
         CLC   BUYMD(1),0(RE)                                                   
         BE    *+12                                                             
         LA    RE,2(RE)            NEXT ENTRY IN MEDIA OVERLAY TABLE            
         B     PROCW70                                                          
*                                                                               
         MVC   SVOVLAY#,1(RE)                                                   
         MVC   DMCB(1),1(RE)       OVERLAY NUMBER FOR MEDIA                     
         BRAS  RE,CBUYOVLY         CALL T41110,11,12,13 FOR BOTTOM SCR          
*                                                                               
         CLI   ERRAREA,C'D'        FAKE ERROR (ACTION COMPLETED)?               
         BE    *+12                                                             
         CLI   ERRAREA,0           ERRORS DETECTED FROM BOTTOM SCR?             
         BNE   PROCW80             YES, GO BUILD RETURN ERROR ELEM              
*                                                                               
         TM    ABUPLDSW,ABADIDAQ   AD-ID ALONE UPLOAD?                          
         BZ    PROCW76                                                          
         MVC   WORK(L'SV_ADCOD),SV_ADCOD                                        
         MVI   DMCB,X'03'          VALIDATE AD CODE FOR AD-ID ALONE             
         MVI   WRKCBOPT,EDTADCDQ                                                
         BRAS  RE,CBUYOVLY                                                      
         CLI   ERRAREA,C'D'        ERRORS DETECTED FROM AD-ID MAP CODE?         
         BNE   PROCW80             YES, GO BUILD RETURN ERROR ELEM              
*                                                                               
PROCW76  BRAS  RE,PRC_BYMV         PROCESS BUY MOVE                             
         JNE   PROCW80             ERROR OCCURRED                               
*                                                                               
         L     R3,SVWRKELM         POINT TO BEGINNING OF ELEM AGAIN             
         CLI   DDLINKSW,C'F'       DRAFT UPLOAD?                                
         BNE   *+12                                                             
         BRAS  RE,DRFTRPLY         DRAFT REPLY (RETURN LOOKED UP ITEMS)         
         B     PROCW87             WRITE BACK AND GET NEXT RECORD               
*                                                                               
         B     PROCW85             NO ERROR ENCOUNTERED                         
*                                                                               
PROCW80  L     R2,ADBERRFD         ADDRESS OF ERROR FLD                         
         L     R3,SVWRKELM         POINT TO BEGINNING OF ELEM AGAIN             
         MVI   BYTE2,C'Y'          YES, ERROR OCCURED                           
         BRAS  RE,BLDRPLY          BUILD REPLY ELEM                             
         B     PROCW87                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FINISH PROCESSING ONE RECORD, NOW UPDATE IT BEFORE GETTING NEXT ONE           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROCW85  MVI   BYTE2,C'N'          NO ERROR OCCURED, REPLY INS KEY ELEM         
         L     R3,SVWRKELM         POINT TO BEGINNING OF ELEM AGAIN             
         BRAS  RE,BLDRPLY          BUILD REPLY ELEM                             
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BNE   *+8                                                              
         BRAS  RE,DRFTRPLY         DRAFT REPLY (RETURN LOOKED UP ITEMS)         
*                                                                               
PROCW87  L     RE,WRKRCCNT                                                      
         AHI   RE,1                                                             
         ST    RE,WRKRCCNT         NUMBER OF WORKER RECS PROCESSED              
*                                                                               
         MVI   ERRAREA,0           RESET ERROR SWITCH FOR NEXT ROUND            
         XC    BUYMSG,BUYMSG                                                    
         BRAS  RE,CLRBSCR          CLEAR BUY SCREEN                             
*                                                                               
         CLI   BUYMDH,0            STILL HAVE SCREEN?                           
         JNE   PROCW88                                                          
         XC    DMCB(24),DMCB       LOAD SCREEN                                  
         MVC   DMCB+4(4),=X'D90411FF'                                           
         GOTO1 VCALLOV,DMCB,64(RA)                                              
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DMCB+4(3),=X'D90411'                                             
         MVC   DMCB+7(1),SVSCRN                                                 
         GOTO1 VCALLOV,DMCB,BUYHDH                                              
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* IF TRAILING 09 ELEM NEED TO BE REMOVED, DO IT HERE                            
*                                                                               
PROCW88  MVI   WRKIACTN,WRKIAPUT                                                
         GOTOR VWRKIO,WRKIOB       WRITE BACK THE UPDATED RECORD                
         BNE   *+12                                                             
         L     R0,WRKRNUMB                                                      
         B     PROCW10             GO GET NEXT RECORD                           
         DC    H'0'                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* END OF WORKER FILE, CLOSE WORKER FILE AND BACK TO T41100                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROCW90  MVI   WRKIACTN,WRKIACLO   CLOSE WORKER FILE                            
         GOTOR VWRKIO,WRKIOB                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRT_READ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMREAD'                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_WRIT LR    R0,RE                                                            
         MVC   COMMAND,=C'DMWRT '                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_RDHI LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PRT_DDIR GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR  ',KEY,KEY,   +        
               (TERMNAL,0)                                                      
         J     EXIT_VRE                                                         
*                                                                               
PRT_GETR LR    R0,RE                                                            
         MVC   COMMAND,=C'GETREC'                                               
         J     PRT_DFIL                                                         
*                                                                               
PRT_PUTR LR    R0,RE                                                            
         MVC   COMMAND,=C'PUTREC'                                               
*                                                                               
PRT_DFIL GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE ',           +        
               KEY+27,AREC,(TERMNAL,DMWORK)                                     
*                                                                               
EXIT_VRE LR    RE,R0               EXIT VIA SAVED RE                            
         BR    RE                                                               
*                                                                               
NXTWFELM SR    R0,R0               POINT TO NEXT WORKER FILE ELEM               
         ICM   R0,3,1(R3)                                                       
         AR    R3,R0                                                            
         BR    RE                                                               
*                                                                               
NXTELEM  SR    R0,R0               R5 POINTS TO FIRST BUY RECORD ELEM           
         IC    R0,1(R5)                                                         
         AR    R5,R0               FIRST ELEM IS ALWAYS X'20'                   
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
GET_ETXT ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    BUYMSG,BUYMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTOR (RF),DMCB+12,(R0),0,(C'E',DMCB),0,0,0                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
CBUYOVLY LR    R0,RE                                                            
         XC    DMCB+1(23),DMCB+1                                                
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         MVC   8+3(L'WRKCBOPT,R1),WRKCBOPT                                      
         GOTO1 (RF),(R1),(RC),(RA)                                              
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
BUMPFLDS SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCTR  RF,0                                                             
         CHI   RF,0                                                             
         JH    BUMPFLDS                                                         
         BR    RE                                                               
*                                                                               
BUMPFLD  SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
MEDOVTAB DS    0H                  MEDIA OVERLAY TABLE                          
         DC    C'I',X'10'          INTERACTIVE                                  
         DC    C'L',X'10'          SOCIAL                                       
         DC    C'B',X'10'          MOBILE                                       
         DC    C'D',X'10'          DIGITAL AUDIO                                
         DC    C'V',X'10'          NVIDEO (NATIONAL VIDEO)                      
         DC    C'W',X'10'          LVIDEO (LOCAL VIDEO)                         
         DC    C'M',X'11'          MAGAZINE                                     
         DC    C'S',X'11'          SUPPLEMENT (OR SEARCH)                       
         DC    C'T',X'11'          TRADE                                        
         DC    C'O',X'13'          OUTDOOR                                      
         DC    C'N',X'12'          NEWSPAPER                                    
         DC    X'FF'                                                            
*                                                                               
         DROP  RB,R3,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETSECVL NTR1  BASE=*,LABEL=*      GET SECURITY VALUES                          
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   SECRET,CSECRET-COMFACSD(RF)                                      
*                                                                               
         USING SECD,SECBLOCK                                                    
*                                                                               
         MVI   SEC_UPLD,0          CLR UPLOAD SECURITY INDICATOR                
*                                                                               
         XC    SECD(256),SECD                                                   
*                                                                               
         XC    WRKFULL1,WRKFULL1   GET LINK'S SECURITY VALUES                   
         MVI   WRKFULL1+0,X'04'                                                 
         MVI   WRKFULL1+1,X'14'                                                 
         GOTOR SECRET,DMCB,('SECPINIT+SECPOSP+SECPOLP',SECD),SECBLKLQ, +        
               (0,WRKFULL1)                                                     
*                                                                               
         LA    R2,SECACTS                                                       
         GOTOR SECRET,DMCB,('SECPRACT+SECPOLP',SECD),(0(R2),1(R2)),    +        
               (0,WRKFULL1)                                                     
         BNE   G_SECV_X                                                         
         MVI   SEC_UPLD,C'Y'       UPLOAD ACCESS IS OKAY                        
*                                                                               
G_SECV_X J     EXIT                                                             
*                                                                               
SECACTS  DS    0XL2                                                             
         DC    AL1(3,6)             UPLOAD                                      
SECACTSN EQU   (*-SECACTS)/L'SECACTS                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CK_HXSEC NTR1  BASE=*,LABEL=*      R3 POINTS FIELD DATA ELEM                    
*                                                                               
         USING LQ_EL,R3                                                         
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA INSERTION UPLOAD?                     
         JNZ   CKHXSX              SUPPRESS HEX SECURITY FOR PRISMA             
*                                                                               
         TM    T411FFD+12,X'01'    DISPLAY ONLY?                                
         BO    CKHXSERR            CANNOT DO UPLOADS IF DISPLAY ONLY!           
*                                                                               
* IF X'10' IS ON, IT MEANS ONLY INVOICE STATUSES, T/S AND CUSTOM COLUMN         
* DATA CAN BE CHANGED, ONLY MAP CODES IN TABLE ARE ALLOWED                      
*                                                                               
         TM    T411FFD+12,X'10'    ONLY ALLOWING INV STAT CHANGES?              
         BZ    CKHXS20                                                          
         MVI   WRKBYTE1,Y_HXTBQ    YES - MAP MUST BE IN TABLE                   
         LA    RF,X10_HXTB                                                      
         CLI   DDLINKSW,C'K'       IDESK INSERTION UPLOAD?                      
         BNE   *+8                                                              
         LA    RF,X10_IDKT                                                      
         BRAS  RE,CK_HXTAB         PASS HEX SECURITY CONTROL?                   
         BNE   CKHXSERR            BNE=NO                                       
*                                                                               
* IF X'08' IS ON, IT MEANS TEARSHEET DATA CANNOT BE CHANGED                     
* THUS, TEARSHEET DATA MAP SHOULD NOT BE DETECTED IN MAP ELEM                   
*                                                                               
CKHXS20  TM    T411FFD+12,X'08'    NO TEARSHEET DATA CHANGES?                   
         BZ    CKHXS30                                                          
         MVI   WRKBYTE1,N_HXTBQ    NO  - MAP MUST NOT BE IN TABLE               
         LA    RF,X08_HXTB                                                      
         BRAS  RE,CK_HXTAB         PASS HEX SECURITY CONTROL?                   
         BNE   CKHXSERR            BNE=NO                                       
*                                                                               
CKHXS30  DS    0H                  FOR FUTURE HEX SECURITY                      
*                                                                               
CKHXSX   J     SETCCEQ             SET CC TO EQUAL (SWITCH IS SET)              
*                                                                               
CKHXSERR J     SETCCNEQ            SET CC TO NOT EQUAL                          
*                                                                               
* ROUTINE TO VALIDATE MAP DATA SECURITY, RF POINTS VERIFYING MAP TABLE          
*                                                                               
* RF       = POINTS TO TABLE TO BE VARIFIED                                     
* WRKBYTE1 = Y_HXTBQ   MEANS ERROR IF MAP CODE IS IN TABLE                      
* WRKBYTE1 = N_HXTBQ   MEANS ERROR IF MAP CODE IS NOT IN TABLE                  
*                                                                               
* ON RETURN: CONDITION CODE NOT EQUAL MEANS ACCESS ERROR IS DETECTED            
*                                                                               
CK_HXTAB ST    RE,WRKSAVRE                                                      
         XC    WRKMAPCD,WRKMAPCD   FOR LOGGING MAP CODE IN ERROR                
         ST    RF,WRKFULL1         SAVE START OF VERIFYING TABLE                
         L     R3,SVWRKELM         POINT TO BEGINNING OF ELEM                   
CK_HTB10 CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    CK_HTBX                                                          
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   CK_HTB70                                                         
         L     RF,WRKFULL1         POINT TO START OF VERIFYING TABLE            
*                                                                               
CK_HTB20 CLI   WRKBYTE1,Y_HXTBQ    CK FOR MAP IN TABLE?                         
         BE    CK_HTB26                                                         
         CLI   WRKBYTE1,N_HXTBQ    CK FOR MAP NOT IN TABLE?                     
         BE    CK_HTB28                                                         
*                                                                               
* DEFAULT TO MAP MUST BE IN TABLE VALIDATION                                    
*                                                                               
CK_HTB26 OC    0(2,RF),0(RF)       END OF TABLE?                                
         BZ    CK_HTB80            BZ=UPLOAD IS NOT AUTHORIZED                  
         B     CK_HTB32                                                         
*                                                                               
CK_HTB28 OC    0(2,RF),0(RF)       END OF TABLE?                                
         BZ    CK_HTB70            BZ=NOT IN TABLE, CK NEXT MAP                 
         B     CK_HTB34                                                         
*                                                                               
CK_HTB32 CLC   LQ_DCODE,0(RF)      IN TABLE?                                    
         BE    CK_HTB70            YES, CK FOR NEXT DATA MAP                    
         B     CK_HTB40                                                         
*                                                                               
CK_HTB34 CLC   LQ_DCODE,0(RF)      NOT IN TABLE?                                
         BE    CK_HTB80            YES, ACCESS NOT ALLOWED                      
         B     CK_HTB40                                                         
*                                                                               
CK_HTB40 SR    RE,RE                                                            
         ICM   RE,3,0(RF)                                                       
         AHI   RE,1                TO CK FOR ODD MAP CODES                      
         STH   RE,WRKHALF1                                                      
*                                                                               
         CLI   WRKBYTE1,Y_HXTBQ    CK FOR MAP IN TABLE?                         
         BE    CK_HTB46                                                         
         CLI   WRKBYTE1,N_HXTBQ    CK FOR MAP NOT IN TABLE?                     
         BE    CK_HTB52                                                         
*                                                                               
CK_HTB46 CLC   LQ_DCODE,WRKHALF1   ODD NUMBERED MAP CODE?                       
         BE    CK_HTB70            YES, CK FOR NEXT DATA MAP                    
         B     CK_HTB60                                                         
*                                                                               
CK_HTB52 CLC   LQ_DCODE,WRKHALF1   ODD NUMBERED MAP CODE?                       
         BE    CK_HTB80            YES, ACCESS NOT ALLOWED                      
*                                                                               
CK_HTB60 LA    RF,2(RF)            NEXT ENTRY IN TABLE                          
         B     CK_HTB20                                                         
CK_HTB70 BRAS  RE,NXTWFELM                                                      
         B     CK_HTB10                                                         
*                                                                               
CK_HTB80 MVC   WRKMAPCD,LQ_DCODE   SAVE MAP CODE IN ERROR                       
*                                                                               
CK_HTBX  LH    RF,WRKMAPCD                                                      
         CHI   RF,0                WILL SET CONDITION CODE                      
X_WKSVRE L     RE,WRKSAVRE         EXIT VIA WRKSAVRE                            
         BR    RE                                                               
*                                                                               
X10_HXTB DS    0H                  INVOICE STATUS HEX SECURITY TABLE            
         DC    AL2(D#INSKEY)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#BUYERC)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#DACTN)        MAP CODE TO BE IGNORED                       
         DC    AL2(D#MATSTA)                                                    
         DC    AL2(D#DISSTA)                                                    
         DC    AL2(D#TEAREC)                                                    
         DC    AL2(D#TSHAPR)       TEARSHEET APPROVED                           
         DC    AL2(D#TSHSTA)       TEARSHEET STATUS                             
         DC    AL2(D#REPROQ)       REPRODUCTION QUALITY                         
         DC    AL2(D#TSHNOT)       TEARSHEET PAGE NOTATION                      
         DC    AL2(D#TSHCO1)       TEARSHEET COMMENT 1                          
         DC    AL2(D#TSHCO2)       TEARSHEET COMMENT 2                          
         DC    AL2(D#TSHCO3)       TEARSHEET COMMENT 3                          
         DC    AL2(D#TSHCO4)       TEARSHEET COMMENT 4                          
         DC    AL2(D#CCSEQN)       CUSTOM COLUMN SEQUENCE #                     
         DC    AL2(D#CCFDAT)       CUSTOM COLUMN FIELD DATA                     
         DC    AL2(D#REVINV)       REQUEST/RECEIVE INVOICE UPLOAD               
         DC    AL2(D#SRCOM1)       SPECIAL REMITTANCE COMMENT 1                 
         DC    AL2(D#SRCOM2)       SPECIAL REMITTANCE COMMENT 2                 
         DC    AL2(D#SRCOM3)       SPECIAL REMITTANCE COMMENT 3                 
         DC    AL2(D#SRCOM4)       SPECIAL REMITTANCE COMMENT 4                 
         DC    AL2(D#SRCOM5)       SPECIAL REMITTANCE COMMENT 5                 
         DC    AL2(0)              END OF X10 TABLE                             
*                                                                               
X10_IDKT DS    0H                  SAME AS X10_HXTB, FOR IDESK                  
         DC    AL2(D#CURDAT)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#CURTIM)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#DACTN)        MAP CODE TO BE IGNORED                       
         DC    AL2(D#INSKEY)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#SUBVER)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#MEDCOD)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#CLTCOD)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#PRDCOD)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#ESTNUM)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#PUBCOD)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#BUYERC)       MAP CODE TO BE IGNORED                       
         DC    AL2(D#MATSTA)                                                    
         DC    AL2(D#DISSTA)                                                    
         DC    AL2(D#TEAREC)                                                    
         DC    AL2(D#TSHAPR)       TEARSHEET APPROVED                           
         DC    AL2(D#TSHSTA)       TEARSHEET STATUS                             
         DC    AL2(D#REPROQ)       REPRODUCTION QUALITY                         
         DC    AL2(D#TSHNOT)       TEARSHEET PAGE NOTATION                      
         DC    AL2(D#TSHCO1)       TEARSHEET COMMENT 1                          
         DC    AL2(D#TSHCO2)       TEARSHEET COMMENT 2                          
         DC    AL2(D#TSHCO3)       TEARSHEET COMMENT 3                          
         DC    AL2(D#TSHCO4)       TEARSHEET COMMENT 4                          
         DC    AL2(D#CCSEQN)       CUSTOM COLUMN SEQUENCE #                     
         DC    AL2(D#CCFDAT)       CUSTOM COLUMN FIELD DATA                     
         DC    AL2(D#REVINV)       REQUEST/RECEIVE INVOICE UPLOAD               
         DC    AL2(D#SRCOM1)       SPECIAL REMITTANCE COMMENT 1                 
         DC    AL2(D#SRCOM2)       SPECIAL REMITTANCE COMMENT 2                 
         DC    AL2(D#SRCOM3)       SPECIAL REMITTANCE COMMENT 3                 
         DC    AL2(D#SRCOM4)       SPECIAL REMITTANCE COMMENT 4                 
         DC    AL2(D#SRCOM5)       SPECIAL REMITTANCE COMMENT 5                 
         DC    AL2(0)              END OF X10 TABLE                             
*                                                                               
X08_HXTB DS    0H                  TEARSHEET HEX SECURITY TABLE                 
         DC    AL2(D#TSHAPR)                                                    
         DC    AL2(D#TSHSTA)                                                    
         DC    AL2(D#REPROQ)                                                    
         DC    AL2(D#TSHNOT)                                                    
         DC    AL2(D#TSHCO1)                                                    
         DC    AL2(D#TSHCO2)                                                    
         DC    AL2(D#TSHCO3)                                                    
         DC    AL2(D#TSHCO4)                                                    
         DC    AL2(0)              END OF X08 TABLE                             
*                                                                               
Y_HXTBQ  EQU   2                   ERR IF MAP IS NOT IN HEX TABLE               
N_HXTBQ  EQU   4                   ERR IF MAP IS IN HEX TABLE                   
*                                                                               
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRCODE NTR1  BASE=*,LABEL=*      R3 POINTS FIELD DATA ELEM                    
*                                                                               
         LA    R5,TRCODETB                                                      
CKTRC20  CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    CKTRCNEQ                                                         
         CLC   3(2,R3),0(R5)                                                    
         BE    *+12                                                             
         LA    R5,3(R5)            NEXT ENTRY IN TABLE                          
         B     CKTRC20                                                          
*                                                                               
         MVC   DDLINKSW,2(R5)      SET SWITCH, (FOR PPBUY10,11,12,13)           
*                                                                               
CKTRCX   J     SETCCEQ             SET CC TO EQUAL (SWITCH IS SET)              
*                                                                               
CKTRCNEQ J     SETCCNEQ            SET CC TO NOT EQUAL                          
*                                                                               
* BYTE 1-2 : MAP REQUEST EQUATE                                                 
* BYTE 3   : UPLOAD MODE (F=DRAFT INS, N=NEW INS, C=CHG INS, D=DEL INS)         
*                                                                               
TRCODETB DS    0H                                                               
         DC    AL2(M#ULDFT),C'F'   UPLOAD - DRAFT INSERTION                     
         DC    AL2(M#ULNEW),C'N'   UPLOAD - ADD INSERTION                       
         DC    AL2(M#ULCHA),C'C'   UPLOAD - CHANGE INSERTION                    
         DC    AL2(M#ULDEL),C'D'   UPLOAD - DELETE INSERTION                    
         DC    AL2(M#ULIDK),C'K'   UPLOAD - IDESK INSERTION                     
         DC    X'FF'                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKDRFMOD NTR1  BASE=*,LABEL=*      R3 POINTS FIELD DATA ELEM                    
*                                                                               
         CLC   =AL2(D#DACTN),3(R3)                                              
         BNE   CKDRFNEQ                                                         
*                                                                               
         CLI   6(R3),C'D'          DRAFT CHANGE MODE?                           
         BE    *+6                                                              
         DC    H'0'                THERE ARE NO OTHER ALTERNATIVES              
*                                                                               
         MVI   LKDRFTSW,C'F'       SET SWITCH, (FOR PPBUY10,11,12,13)           
*                                                                               
CKDRFX   J     SETCCEQ             SET CC TO EQUAL (SWITCH IS SET)              
*                                                                               
CKDRFNEQ J     SETCCNEQ            SET CC TO NOT EQUAL                          
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKUPLMOD NTR1  BASE=*,LABEL=*      CK UPLOAD MODES                              
*                                                                               
         L     R3,SVWRKELM         POINT TO BEGINNING OF ELEM                   
         USING LQ_EL,R3                                                         
*                                                                               
CKUPLM10 CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    CKUPLM_X                                                         
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   CKUPLM70                                                         
*                                                                               
         CLC   =AL2(D#FROINS),3(R3)                                             
         BNE   *+12                                                             
         OI    ABUPLDSW,ABMOVINQ   THIS IS A MOVE INSERTION UPLOAD              
         STCM  R3,15,SVBYMVKY      SAVE ADDRESS OF "FROM" KEY                   
*                                                                               
         CLC   =AL2(D#REVINV),3(R3)                                             
         BNE   *+14                                                             
         OI    ABUPLDSW,ABRVINVQ   THIS IS A REVISE INVOICE UPLOAD              
         MVC   SVREVINV,6(R3)      SAVE REVISE INVOICE INPUT                    
*                                                                               
         CLC   =AL2(D#PLCOST),3(R3)                                             
         BNE   *+8                                                              
         OI    ABUPLDSW,ABPLCOSQ   PLANNED COST MAP CODE PRESENT                
*                                                                               
         CLC   =AL2(D#UNTRAT),3(R3)                                             
         JNE   *+8                                                              
         OI    ABUPLDSW,RATEMAPQ   RATE MAP CODE PRESENT                        
*                                                                               
         CLC   =AL2(D#COS2$$),3(R3)                                             
         JNE   *+8                                                              
         OI    PRSMAPSW,COS2MAPQ   COST 2 MAP CODE PRESENT                      
*                                                                               
         CLC   =AL2(D#INSORG),3(R3)                                             
         BNE   CKUPLM12                                                         
         CLI   6(R3),PPIDIMPQ      ADBUYER IMPORT?                              
         BNE   CKUPLM12                                                         
         OI    ABUPLDSW,ADBYIMPQ   ADBUYER IMPORT                               
*                                                                               
CKUPLM12 CLC   =AL2(D#CURDAT),3(R3)                                             
         BNE   CKUPLM16                                                         
* * * *  GOTO1 VPERVAL,DMCB,(10,6(R3)),(X'40',WRKELEM)                          
* * * *  LA    RE,WRKELEM                                                       
* * * *  USING PERVALD,RE                                                       
* * * *  MVC   SVULDATE,PVALBSTA                                                
* * * *  DROP  RE                                                               
         GOTO1 VDATCON,DMCB,(10,6(R3)),(3,SVULDATE)                             
*                                                                               
CKUPLM16 CLC   =AL2(D#CURTIM),3(R3)                                             
         BNE   CKUPLM20                                                         
         PACK  DUB,6+0(2,R3)                                                    
         CVB   RE,DUB                                                           
         STC   RE,SVULTIME+0       BINARY HOURS                                 
         PACK  DUB,6+2(2,R3)                                                    
         CVB   RE,DUB                                                           
         STC   RE,SVULTIME+1       BINARY MINUTES                               
         PACK  DUB,6+4(2,R3)                                                    
         CVB   RE,DUB                                                           
         STC   RE,SVULTIME+2       BINARY SECONDS                               
*                                                                               
CKUPLM20 CLI   DDLINKSW,C'K'       IDESK INSERTION UPLOAD?                      
         BNE   CKUPLM30                                                         
         CLC   =AL2(D#DACTN),3(R3)                                              
         BNE   CKUPLM30                                                         
         CLI   6(R3),C'A'          IDESK ADD INSERTION UPLOAD?                  
         BNE   *+8                                                              
         MVI   DDLINKSW,C'N'       RESET TO NEW INSERTION                       
         CLI   6(R3),C'C'          IDESK CHG INSERTION UPLOAD?                  
         BNE   *+8                                                              
         MVI   DDLINKSW,C'C'       RESET TO CHG INSERTION                       
         CLI   6(R3),C'D'          IDESK DEL INSERTION UPLOAD?                  
         BNE   *+8                                                              
         MVI   DDLINKSW,C'D'       RESET TO CHG INSERTION                       
         OI    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD                       
*                                                                               
CKUPLM30 CLC   =AL2(D#SUBVER),3(R3)                                             
         JNE   *+10                                                             
         MVC   SVIDSKV#,6(R3)                                                   
*                                                                               
         CLC   =AL2(00000012),3(R3)                                             
         JNE   CKUPLM32                                                         
         MVC   WRKTEMPS(6),6(R3)                                                
         OC    WRKTEMPS(6),SPACES  CONVERT TO UPPER CASES                       
         CLC   =C'PRISMA',WRKTEMPS                                              
         JNE   *+8                                                              
         OI    PLINKSW1,PRSMORGQ   TO INDICATE PRISMA ORIGIN                    
         CLC   =C'RADIA',WRKTEMPS                                               
         JNE   *+8                                                              
         OI    PLINKSW1,RADAORGQ   TO INDICATE RADIA ORIGIN                     
*                                                                               
CKUPLM32 CLI   DDLINKSW,C'F'       DRAFT INSERTION?                             
         JNE   CKUPLM34                                                         
         CLC   =AL2(D#QTOKEN),3(R3)                                             
         JNE   CKUPLM34                                                         
         MVC   REQTOKEN,6(R3)      SAVE REQUEST TOKEN                           
*                                                                               
CKUPLM34 DS    0H                                                               
*                                                                               
*                                                                               
CKUPLM70 BRAS  RE,NXTWFELM                                                      
         B     CKUPLM10            SHOULD ONLY BE PROCESSED ONCE                
*                                                                               
CKUPLM_X J     EXIT                                                             
*                                                                               
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKIDKMOD NTR1  BASE=*,LABEL=*      CK IDESK EXCEPTION MODES                     
*                                                                               
         XC    SVSPCDSP,SVSPCDSP   INIT SAVED SPACE DESCRIPTION                 
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    CKIDKM_X                                                         
         CLI   DDLINKSW,C'C'       CHG INSERTION?                               
         BNE   CKIDKM_X                                                         
*                                                                               
         L     R3,SVWRKELM         POINT TO BEGINNING OF ELEM                   
         USING LQ_EL,R3                                                         
*                                                                               
CKIDKM10 CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    CKIDKM50                                                         
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   CKIDKM40                                                         
*                                                                               
         CLC   =AL2(D#INSKEY),3(R3)                                             
         BNE   *+8                                                              
         STCM  R3,15,SVIDSKW1      SAVE ADDRESS OF KEY ELEM                     
*                                                                               
         CLC   =AL2(D#STRDAT),3(R3)                                             
         BNE   *+8                                                              
         STCM  R3,15,SVIDSKW2      SAVE ADDRESS OF DATE ELEM                    
*                                                                               
CKIDKM40 BRAS  RE,NXTWFELM                                                      
         B     CKIDKM10                                                         
*                                                                               
CKIDKM50 ICM   R3,15,SVIDSKW1                                                   
         MVC   WKSVAREC,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         BRAS  RE,GETBUYRC                                                      
         MVC   AREC,WKSVAREC                                                    
*                                                                               
         L     R5,AWKAIO1                                                       
         LA    R5,(PBDELEM-PBUYREC)(R5)                                         
         USING PBDELEM,R5                                                       
         CLI   PBDELEM,X'20'       MAIN BUY ELEMENT PRESENT?                    
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVSPCDSP,PBDSPACE   SAVE SPACE DECRIPTION                        
         DROP  R5                                                               
*                                                                               
         L     R5,AWKAIO1                                                       
         LA    R5,(PBDELEM-PBUYREC)(R5)                                         
         USING PPAYELEM,R5                                                      
         MVI   ELCODE,X'25'                                                     
         BRAS  RE,NXTELEM          PAY ELEM FOUND?                              
         BNE   *+18                                                             
         OC    PPDDATE,PPDDATE     HAVE PAY DATE?                               
         BZ    *-14                                                             
         B     CKIDKM56                                                         
         DROP  R5                                                               
*                                                                               
         L     R5,AWKAIO1                                                       
         LA    R5,(PBDELEM-PBUYREC)(R5)                                         
         USING PBILELEM,R5                                                      
         MVI   ELCODE,X'26'                                                     
         BRAS  RE,NXTELEM          BILL ELEM FOUND?                             
         BNE   *+18                                                             
         OC    PBLDATE,PBLDATE     HAVE BILL DATE?                              
         BZ    *-14                                                             
         B     CKIDKM56                                                         
         DROP  R5                                                               
*                                                                               
         L     R5,AWKAIO1                                                       
         LA    R5,(PBDELEM-PBUYREC)(R5)                                         
         USING PBILELEM,R5                                                      
         MVI   ELCODE,X'28'                                                     
         BRAS  RE,NXTELEM          OPEN RATE BILL ELEM FOUND?                   
         BNE   *+18                                                             
         OC    PBLDATE,PBLDATE     HAVE OPEN RATE BILL DATE?                    
         BZ    *-14                                                             
         B     CKIDKM56                                                         
         DROP  R5                                                               
*                                                                               
         B     CKIDKM70                                                         
*                                                                               
CKIDKM56 L     R5,AWKAIO1          TO CHECK INSERTION DATE IS CHANGED           
         USING PBUYREC,R5                                                       
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(5,WRKDUB1)                            
         ICM   R3,15,SVIDSKW2                                                   
         BRAS  RE,FMTIDKDT         FORMAT IDESK INSERTION DATE IN DUB           
         CLC   WRKDUB1,DUB                                                      
         BE    CKIDKM70                                                         
         MVI   DDLINKSW,C'N'       RESET TO NEW INSERTION                       
         OI    ABUPLDSW,ABMOVINQ   THIS IS A MOVE INSERTION UPLOAD              
         MVC   SVBYMVKY,SVIDSKW1   SAVE ADDRESS OF "FROM" KEY                   
         DROP  R5                                                               
*                                                                               
CKIDKM70 DS    0H                                                               
*                                                                               
CKIDKM_X J     EXIT                                                             
*                                                                               
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKINSMDL NTR1  BASE=*,LABEL=*      CK FOR MOVE INSERTION - DELETION             
*                                                                               
         TM    ABUPLDSW,ABMOVINQ   BUY MOVE?                                    
         BZ    CKMDL_X                                                          
*                                                                               
         ICM   R3,15,SVBYMVKY      ADDRESS OF "FROM" KEY                        
         BRAS  RE,GETBUYRC                                                      
         BNE   CKMDL_ER                                                         
*                                                                               
         GOTOR VCALLOV,DMCB,(3,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTOR (RF),(R1),(RC),(RA),DELBCHKQ                                     
         CLI   ERRAREA,0                                                        
         BE    CKMDL_X                                                          
         B     CKMDL_ER                                                         
*                                                                               
CKMDL_X  J     SETCCEQ                                                          
*                                                                               
CKMDL_ER J     SETCCNEQ                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRC_BYMV NTR1  BASE=*,LABEL=*      PROCESS BUY MOVE                             
*                                                                               
         TM    ABUPLDSW,ABMOVINQ   BUY MOVE?                                    
         BZ    P_BYM_X                                                          
*                                                                               
         USING PSERELEM,R5                                                      
         BRAS  R2,P_BYM_S#         GET SERIAL# ELEM                             
         MVC   SVBYS#_N,PSERNUM    SAVE "NEW" SERIAL#                           
         MVC   WRKSAVKY(25),REC    SAVE "NEW" INSERTION'S KEY                   
*                                                                               
         MVI   ERRAREA,0           RESET ERROR SWITCH FOR NEXT ROUND            
         XC    BUYMSG,BUYMSG                                                    
         BRAS  RE,CLRBSCR          CLEAR BUY SCREEN                             
         ICM   R3,15,SVBYMVKY      ADDRESS OF "FROM" KEY                        
         BRAS  RE,READBUYR         READ BUY RECORD VIA PASSIVE KEY              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DMCB,X'01'          CALL T41101 TO VALIDATE TOP SCR              
         BRAS  RE,CBUYOVLY         T41101 WILL ALSO GET MIDDLE SCR              
         BRAS  RE,P_BYM_CE         CHECK FOR ERRORS                             
         JNE   P_BYM_ER                                                         
*                                                                               
         CLI   SVESPROF,C'1'       LOCKED OUT?                                  
         BE    *+12                                                             
         CLI   SVESPROF,C'2'       PERM LOCKOUT?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    BUYTR1,BUYTR1       CLEAR FIELD FIRST                            
         MVI   BUYTR1,C'R'         TRANSACTION CODE IS RECALL                   
         MVI   BUYTR1H+5,1         INPUT LENGTH                                 
         OI    BUYTR1H+6,X'80'     TRANSMIT                                     
         MVI   DMCB,X'05'                                                       
         BRAS  RE,CBUYOVLY         CALL T41105 TO RECALL INSERTION              
         BRAS  RE,P_BYM_CE         CHECK FOR ERRORS                             
         JNE   P_BYM_ER                                                         
         XC    TRADDR,TRADDR       NXTTR ROUTINE WILL SET THIS                  
         MVI   ERRAREA,0           RESET ERROR SWITCH                           
         XC    BUYTR1,BUYTR1       PREPARE FOR NEXT TRANSACTION CODE            
         MVC   BUYTR1(2),=C'DL'    TRANSACTION CODE IS DELETE                   
         MVI   BUYTR1H+5,2                                                      
         OI    BUYTR1H+6,X'80'                                                  
         MVC   DMCB(1),SVOVLAY#    OVERLAY NUMBER FOR MEDIA                     
         BRAS  RE,CBUYOVLY         CALL T41110,11,12,13 FOR MIDDLE SCR          
         BRAS  RE,P_BYM_CE         CHECK FOR ERRORS                             
         JNE   P_BYM_ER                                                         
*                                                                               
         ICM   R3,15,SVBYMVKY      ADDRESS OF "FROM" KEY                        
         BRAS  RE,GETBUYRC                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  R2,P_BYM_S#         GET SERIAL# ELEM                             
         MVC   SVBYS#_O,PSERNUM    SAVE "OLD" SERIAL#                           
         MVC   PSERNUM,SVBYS#_N    DELETED BUY USE "NEW" SERIAL#                
         MVI   WRKDUB1+0,PBYMTO_Q  MOVED "TO"                                   
         MVC   WRKDUB1+1(L'SVBYS#_O),SVBYS#_O                                   
         BRAS  RE,P_BYM_EL                                                      
         MVC   SVBYMDAO,KEY+27     SAVE DISK ADDRESS OF "OLD" INSERTION         
         BRAS  RE,SVIDKELM         SAVE IDESK TARGET ELEMS                      
         BRAS  RE,PRT_PUTR                                                      
*                                                                               
         BRAS  RE,P_BYM_NR         GET "NEW" INSERTION                          
         MVC   SVBYMDAN,KEY+27     SAVE DISK ADDRESS OF "NEW" INSERTION         
         BRAS  R2,P_BYM_S#         GET SERIAL# ELEM                             
         MVC   PSERNUM,SVBYS#_O    SWAP SERIAL# WITH "FROM" INSERTION           
         MVI   WRKDUB1+0,PBYMFROQ  MOVED "FROM"                                 
         MVC   WRKDUB1+1(L'SVBYS#_N),SVBYS#_N                                   
         BRAS  RE,P_BYM_EL                                                      
         BRAS  RE,MVIDKELM         MOVE IDESK TARGET ELEMS                      
         BRAS  RE,PRT_PUTR                                                      
*                                                                               
         MVC   WRKDUB1+3(5),SVBYS#_O                                            
         MVC   WRKDUB2+0(4),SVBYMDAN                                            
         BRAS  RE,P_BYM_DA                                                      
*                                                                               
         MVC   WRKDUB1+3(5),SVBYS#_N                                            
         MVC   WRKDUB2+0(4),SVBYMDAO                                            
         BRAS  RE,P_BYM_DA                                                      
*                                                                               
         BRAS  RE,P_BYM_NR         GET "NEW" INSERTION                          
*                                                                               
P_BYM_X  J     SETCCEQ                                                          
*                                                                               
P_BYM_ER J     SETCCNEQ                                                         
*                                                                               
P_BYM_S# LA    R5,REC+33                                                        
         MVI   ELCODE,X'99'                                                     
         BRAS  RE,NXTELEM          SERIAL NUMBER ELEM FOUND?                    
         JE    *+6                                                              
         DC    H'0'                IT HAS TO BE THERE!                          
         BR    R2                                                               
*                                                                               
P_BYM_CE CLI   ERRAREA,C'D'        FAKE ERROR (ACTION COMPLETED)?               
         BCR   8,RE                                                             
         CLI   ERRAREA,0           ERRORS DETECTED FROM TOP SCREEN?             
         BCR   8,RE                                                             
         BR    RE                  CONDITION CODE IS NOT EQUAL                  
*                                                                               
P_BYM_NR ST    RE,WRKTOPRE         GET "NEW" INSERTION                          
         XC    KEY,KEY                                                          
         MVC   KEY(25),WRKSAVKY                                                 
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(25),KEYSAVE     "NEW" INSERTION FOUND?                       
         JE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,PRT_GETR                                                      
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                CANNOT CONTINUE W/O "NEW" INSERTION          
         J     EXIT_TOP                                                         
*                                                                               
P_BYM_EL ST    RE,WRKSAVRE                                                      
         XC    WRKTEMPS,WRKTEMPS   PROCESS BUY MOVE ELEM                        
         LA    RE,WRKTEMPS                                                      
         USING PBYMELEM,RE                                                      
         MVI   PBYMELCO,PBYMELCQ   BUY MOVE ELEM CODE                           
         MVI   PBYMELLN,PBYMELLQ   BUY MOVE ELEM LENGTH                         
         MVC   PBYMSTAT,WRKDUB1+0  BUY MOVED "TO" OR "FROM" STATUS BIT          
         MVC   PBYMSER#,WRKDUB1+1  BUY MOVED "TO" OR "FROM" SERIAL#             
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBYMELCQ                                                  
         BRAS  RE,NXTELEM                                                       
         JE    *-4                                                              
         GOTO1 VRECUP,DMCB,(1,REC),WRKTEMPS,(R5)                                
         J     X_WKSVRE                                                         
*                                                                               
P_BYM_DA ST    RE,WRKTOPRE         ADJUST DA FOR SERIAL PASSIVE                 
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PSERKEY,RE                                                       
         MVC   PSERKAGY,AGYALPHA                                                
         MVC   PSERKMED,WRKSAVKY+2 MEDIA CODE                                   
         MVI   PSERKRCD,PSERKIDQ                                                
         MVC   PSERKCLT,WRKSAVKY+4 CLIENT CODE                                  
         ZAP   DUB,WRKDUB1+3(5)                                                 
         ZAP   WORK(L'DUB),=P'1000000000'                                       
         SP    WORK(L'DUB),DUB                                                  
         MVC   PSERKNUM,WORK+3     SERIAL NUMBER IN PACK                        
         BRAS  RE,PRT_READ                                                      
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+27(4),WRKDUB2   USE TARGET DISK ADDRESS                      
         BRAS  RE,PRT_WRIT                                                      
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
EXIT_TOP L     RE,WRKTOPRE                                                      
         BR    RE                                                               
*                                                                               
SVIDKELM ST    RE,WRKSAVRE         SAVE IDESK ELEMS                             
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    SVIDKE_X                                                         
         L     R0,AWKAIO1                                                       
         LHI   R1,4096                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R1,SVIDKETB         POINT TO ELEM CODE TABLE                     
         L     R2,AWKAIO1          POINT TO SAVING ELEM STORAGE AREA            
SVIDKE16 CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    SVIDKE_X                                                         
         LA    R5,REC+33                                                        
         MVC   ELCODE,0(R1)                                                     
SVIDKE24 BRAS  RE,NXTELEM                                                       
         BNE   SVIDKE32                                                         
         SR    RE,RE                                                            
         IC    RE,1(R5)                                                         
         LR    RF,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R5)       SAVE TARGET ELEM IN STORAGE AREA             
         AR    R2,RF               POINT TO NEXT ENTRY IN STORAGE               
         B     SVIDKE24                                                         
SVIDKE32 LA    R1,1(R1)            PROCESS NEXT ELEM CODE IN TABLE              
         B     SVIDKE16                                                         
SVIDKE_X J     X_WKSVRE                                                         
SVIDKETB DC    X'44'               ADDITIONAL CHARGES                           
         DC    X'66'               REGULAR COMMENTS                             
         DC    X'67'               INSERTION ORDER COMMENTS                     
         DC    X'68'               POSITION INSTRUCTION COMMENTS                
         DC    X'87'               PAGE VIEWS                                   
         DC    X'88'               CLICK-THRUS                                  
         DC    X'92'               IMPRESSIONS                                  
         DC    X'93'               ACTUAL IMPRESSIONS                           
         DC    X'98'               INTERNET SITE                                
         DC    X'A0'               ESTIMATED CPM                                
         DC    X'A1'               ACTUAL CPM                                   
         DC    X'CC'               CUSTOM COLUMNS                               
         DC    X'FF'               END OF ELEM CODE TABLE                       
*                                                                               
MVIDKELM ST    RE,WRKSAVRE         MOVE IDESK ELEMS                             
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    MVIDKE_X                                                         
         L     R2,AWKAIO1          POINT TO SAVED ELEM STORAGE AREA             
MVIDKE16 CLI   0(R2),0             END OF SAVED ELEM STORAGE AREA?              
         BE    MVIDKE_X                                                         
         CLI   0(R2),X'CC'         CUSTOM COLUMN ELEM CODE?                     
         BNE   MVIDKE32                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'CC'                                                     
MVIDKE24 BRAS  RE,NXTELEM          CUSTOM COLUMN ELEM FOUND?                    
         BNE   MVIDKE32                                                         
         CLC   2(2,R5),2(R2)       ALREADY HAVE SAME CUSTOM COLUMN?             
         BE    MVIDKE38                                                         
         B     MVIDKE24                                                         
MVIDKE32 LA    R5,REC+33                                                        
         MVI   ELCODE,X'FF'                                                     
         BRAS  RE,NXTELEM          POINT TO END OF RECORD                       
         GOTO1 VRECUP,DMCB,(1,REC),(R2),(R5)                                    
MVIDKE38 SR    RE,RE                                                            
         IC    RE,1(R2)                                                         
         AR    R2,RE               POINT TO NEXT TARGET ELEM                    
         B     MVIDKE16                                                         
MVIDKE_X J     X_WKSVRE                                                         
*                                                                               
         DROP  RB,R5,RE                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRC_RVIV NTR1  BASE=*,LABEL=*      PROCESS REVISE INVOICE                       
*                                                                               
         TM    REC+27,X'80'        BUY IS DELETED?                              
         BZ    *+12                                                             
         LHI   R0,INVACTDL                                                      
         B     P_RIV_ER                                                         
*                                                                               
         CLI   SVREVINV,C'Q'       REQUEST INVOICE?                             
         BNE   *+12                                                             
         OI    CHGIND5,PCHGQIVQ    ACTIVITY BIT - REQUEST INVOICE               
         B     P_RIV20                                                          
         CLI   SVREVINV,C'R'       RECEIVE INVOICE?                             
         BNE   *+12                                                             
         OI    CHGIND5,PCHGRIVQ    ACTIVITY BIT - RECEIVE INVOICE               
         B     P_RIV20                                                          
         LHI   R0,INVERR           INVALID INPUT FLD                            
         B     P_RIV_ER                                                         
*                                                                               
P_RIV20  MVI   DMCB,X'03'                                                       
         MVI   WRKCBOPT,CHGELEMQ                                                
         BRAS  RE,CBUYOVLY                                                      
*                                                                               
         MVC   WKSVAREC,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(25),KEYSAVE     RECORD FOUND?                                
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,PRT_GETR                                                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AREC,WKSVAREC                                                    
         BRAS  RE,PRT_PUTR                                                      
*                                                                               
         MVI   WARN,0              NO NEED TO REPLY WARNING MSG                 
*                                                                               
P_RIV_X  J     SETCCEQ                                                          
*                                                                               
P_RIV_ER BRAS  RE,GET_ETXT                                                      
         MVC   WRKMAPCD,=AL2(D#REVINV)                                          
         J     SETCCNEQ                                                         
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETBUYRC NTR1  BASE=*,LABEL=*      GET BUY RECORD                               
*                                                                               
         XC    KEY,KEY             R3 POINTS TO KEY MAP CODE ELEM               
         LA    RE,KEY                                                           
         USING PSERKEY,RE                                                       
         MVC   PSERKAGY,AGYALPHA                                                
         MVC   PSERKMED,06(R3)     MEDIA CODE                                   
         MVI   PSERKRCD,PSERKIDQ                                                
         MVC   PSERKCLT,07(R3)     CLIENT CODE                                  
         PACK  DUB,10(9,R3)                                                     
         ZAP   WORK(L'DUB),=P'1000000000'                                       
         SP    WORK(L'DUB),DUB                                                  
         MVC   PSERKNUM,WORK+3     SERIAL NUMBER IN PACK                        
         DROP  RE                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
*                                                                               
         BRAS  RE,PRT_RDHI                                                      
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         CLI   DMCB+8,X'02'        DELETE RECORD GOT PASSED BACK?               
         BNE   G_BUY_ER                                                         
*                                                                               
         CLC   KEYSAVE(25),KEY     SAME RECORD?                                 
         BNE   G_BUY_ER                                                         
*                                                                               
         BRAS  RE,PRT_GETR                                                      
         CLI   DMCB+8,0                                                         
         BE    G_BUY_X                                                          
         CLI   DMCB+8,X'02'        DELETE RECORD GOT PASSED BACK?               
         BE    G_BUY_X                                                          
*                                                                               
         B     G_BUY_ER            CANNOT GET BUY RECORD!                       
*                                                                               
G_BUY_X  NI    DMINBTS,X'FF'-X'08' RESET PASS DELETED                           
         J     SETCCEQ                                                          
*                                                                               
G_BUY_ER NI    DMINBTS,X'FF'-X'08' RESET PASS DELETED                           
         J     SETCCNEQ                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
READBUYR NTR1  BASE=*,LABEL=*      R3 POINTS TO BEGINNING OF WORKER REC         
*                                                                               
         TM    ABUPLDSW,ABMOVINQ   BUY MOVE?                                    
         BNZ   RDBUYR30            R3 ALREADY POINTS TO KEY ELEM                
*                                                                               
RDBUYR20 CLI   0(R3),0             ELEM CODES EXIST?                            
         BNE   *+8                                                              
         B     RDBUYRER                                                         
         CLI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    RDBUYRER                                                         
         CLI   0(R3),LQ_RQSTQ      REQUEST DATA ELEM?                           
         BE    *+12                                                             
         BRAS  RE,NXTWFELM                                                      
         B     RDBUYR20                                                         
         CLC   3(2,R3),=AL2(D#INSKEY)                                           
         BNE   *-14                                                             
*                                                                               
RDBUYR30 BRAS  RE,GETBUYRC                                                      
         BNE   RDBUYRER                                                         
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    RDBUYR36                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBYDKELQ     IDESK INSERTION UPLOAD ELEM                  
         BRAS  RE,NXTELEM                                                       
         BNE   RDBUYR36            DATE/TIME STAMP NOT FOUND                    
         USING PBYDKELM,R5                                                      
         CLC   PBYDKDAT,SVULDATE   DATE IN RECORD IS MORE RECENT?               
         BH    RDBUYRE1                                                         
         CLC   PBYDKDAT,SVULDATE   SAME DATE?                                   
         BNE   RDBUYR36                                                         
         CLC   PBYDKTIM,SVULTIME   TIME IN RECORD IS MORE RECENT?               
         BH    RDBUYRE1                                                         
         DROP  R5                                                               
*                                                                               
RDBUYR36 LA    R5,REC                                                           
         USING PBUYKEY,R5                                                       
         MVC   DUB,13+6(R3)                                                     
         CLC   DUB,SPACES          INSERTION DATE PRESENT?                      
         BE    RDBUYR40                                                         
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(8,DUB)                                
         MVC   WRKTEMPS(8),13+6(R3)                                             
         OC    WRKTEMPS(3),SPACES  MAKE MMM INTO UPPER CASE                     
         CLC   DUB,WRKTEMPS        SAME INSERTION DATE?                         
         BNE   RDBUYRER                                                         
*                                                                               
RDBUYR40 XC    BUYMD,BUYMD                                                      
         MVC   BUYMD(1),PBUYKMED                                                
         MVI   BUYMDH+5,1                                                       
         OI    BUYMDH+6,X'80'      MEDIA CODE IS DISPLAYED                      
*                                                                               
         XC    BUYCL,BUYCL                                                      
         MVC   BUYCL(3),PBUYKCLT                                                
         MVI   BUYCLH+5,3                                                       
         CLI   PBUYKCLT+2,C' '                                                  
         BNE   *+8                                                              
         MVI   BUYCLH+5,2                                                       
         OI    BUYCLH+6,X'80'      CLIENT CODE IS DISPLAYED                     
*                                                                               
         XC    BUYPR,BUYPR                                                      
         MVC   BUYPR(3),PBUYKPRD                                                
         MVI   BUYPRH+5,3                                                       
         CLI   PBUYKPRD+2,C' '                                                  
         BNE   *+8                                                              
         MVI   BUYPRH+5,2                                                       
         OI    BUYPRH+6,X'80'      PRODUCT CODE IS DISPLAYED                    
*                                                                               
         XC    BUYES,BUYES                                                      
         EDIT  (B2,PBUYKEST),(3,BUYES),0,ALIGN=RIGHT,                  +        
               ZERO=NOBLANK,FILL=0                                              
         OI    BUYESH+4,X'08'      VALID NUMERIC FOR ESTIMATE                   
         MVI   BUYESH+5,3                                                       
         OI    BUYESH+6,X'80'      ESTIMATE IS DISPLAYED                        
*                                                                               
         XC    BUYPB,BUYPB                                                      
         GOTO1 APUBEDIT,DMCB,(X'08',PBUYKPUB),(C'S',BUYPB)                      
         SR    RE,RE                                                            
         LA    RF,BUYPB                                                         
*                                                                               
RDBUYR48 CLI   0(RF),0             END OF PUB FLD?                              
         BE    RDBUYR50                                                         
         AHI   RE,1                                                             
         CHI   RE,15               MAX FLD LENGTH ALREADY?                      
         BE    RDBUYR50                                                         
         LA    RF,1(RF)            POINT TO NEXT CHARACTER IN PUB FLD           
         B     RDBUYR48                                                         
*                                                                               
RDBUYR50 STC   RE,BUYPBH+5                                                      
         OI    BUYPBH+6,X'80'      PUBLICATION IS DISPLAYED                     
*                                                                               
         XC    BUYNM,BUYNM                                                      
         LA    R4,REC+33                                                        
         USING PBDELEM,R4                                                       
         CLI   0(R4),X'20'         BUY DESCRIPTION ELEM PRESENT?                
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    PBDBUYER,PBDBUYER   BUYER INITIAL PRESENT?                       
         BNZ   *+14                                                             
         MVC   BUYNM(3),=C'ADB'    USE A TEMP BUYER INITIAL                     
         B     *+10                                                             
         MVC   BUYNM(3),PBDBUYER                                                
         OC    BUYNM(3),SPACES     MAKE SURE ALL SPACES                         
         MVI   BUYNMH+5,3                                                       
         CLI   BUYNM+2,C' '        BUYER INITIAL IS TWO CHARS?                  
         BNE   *+8                                                              
         MVI   BUYNMH+5,2                                                       
         CLC   BUYNM+1(2),SPACES   BUYER INITIAL IS ONE CHAR?                   
         BNE   *+8                                                              
         MVI   BUYNMH+5,1                                                       
         OI    BUYNMH+6,X'80'      BUYER CODE DISPLAYED                         
         DROP  R4                                                               
*                                                                               
         XC    BUYDT1,BUYDT1                                                    
         XC    WRKTEMPS,WRKTEMPS                                                
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(5,WRKTEMPS)                           
         MVC   BUYDT1(5),WRKTEMPS                                               
         MVI   BUYDT1+5,C'-'                                                    
*                                                                               
         CLI   PBUYKLIN,99                                                      
         BNH   RDBUYR56                                                         
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         DP    DUB,=P'10'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+10(1),DUB+7(1)                                              
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RE,DUB                                                           
         LA    RE,RDBALPTB(RE)                                                  
         MVC   WORK+9(1),0(RE)                                                  
         MVC   BUYDT1+6(2),WORK+9                                               
         B     RDBUYR57                                                         
*                                                                               
RDBUYR56 EDIT  (B1,PBUYKLIN),(2,BUYDT1+6),0,ALIGN=RIGHT,               +        
               ZERO=NOBLANK,FILL=0                                              
*                                                                               
RDBUYR57 MVI   BUYDT1H+5,8                                                      
RDBUYR60 OI    BUYDT1H+6,X'80'     INSERTION DATE IS DISPLAYED                  
*                                                                               
         DROP  R5                                                               
*                                                                               
RDBUYRX  J     SETCCEQ             SET CONDITION CODE TO EQUAL                  
*                                                                               
RDBUYRE1 LHI   R0,DTTM_ERR         DATE/TIME CONFLICT ERROR                     
         J     RDBUYREX                                                         
*                                                                               
RDBUYRER LHI   R0,INSNTERR         INSERTION NOT FOUND ERROR                    
RDBUYREX BRAS  RE,GET_ETXT                                                      
         XC    ADBERRFD,ADBERRFD   ERROR FLD IS NOT DEFINED                     
         J     SETCCNEQ            SET CONDITION CODE TO NOT EQUAL              
*                                                                               
RDBALPTB DS    0H                                                               
         DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTOPSCR NTR1  BASE=*,LABEL=*      R3 POINTS FIELD DATA ELEM                    
*                                                                               
         MVC   HALF,3(R3)          MAP CODE                                     
         CLC   HALF,=AL2(D#INSKEY)                                              
         BE    CKTOPX              DONE, MAP CODE IS INS KEY                    
*                                                                               
         LA    R5,TOPFLDTB                                                      
CKTOP20  CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    CKTOPX                                                           
         CLC   HALF,0(R5)                                                       
         BE    *+12                                                             
         LA    R5,3(R5)            NEXT ENTRY IN TABLE                          
         B     CKTOP20                                                          
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BNZ   CKTOP40                                                          
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   CKTOP40                                                          
         ZICM  R2,3(R3),(3)        SHOULD BE ODD NUMBERED MAP CODE              
         LA    R6,REC                                                           
         CLI   3(R6),X'20'         BUY RECORD?                                  
         BE    *+6                                                              
         DC    H'0'                WRONG RECORD!                                
         ZICM  RE,1(R3),(3)                                                     
         CHI   RE,6                HEADER ONLY ELEM?                            
         BNH   CKTOP30                                                          
*                                                                               
* CHANGE MODE, NEED TO CHECK INSERTION DATE AND BUYER ID ARE STILL SAME         
*                                                                               
         CLC   =AL2(D#INSDAT),3(R3)                                             
         BNE   CKTOP25                                                          
         USING PBUYKEY,R6                                                       
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(8,DUB)                                
         LA    R6,33(R6)                                                        
         USING PBDELEM,R6                                                       
         MVI   BYTE,0              INIT INS DATE FORMAT SWITCH                  
         CLI   PBDFREQ,C'M'        MONTHLY INSERTION?                           
         BNE   *+10                                                             
         MVC   BYTE,PBDFREQ                                                     
         CLI   PBDBFD,C'W'         WEEK OF INSERTION?                           
         BE    *+12                                                             
         CLI   PBDBFD,C'B'         BEST FOOD DAY INSERTION?                     
         BNE   CKTOP22D                                                         
         CLI   BYTE,C'M'           ALREADY HAS MONTHLY INDICATOR?               
         BNE   *+12                                                             
         MVI   BYTE,X'FF'          MUTIPLE FORMATS DETECTED                     
         B     *+10                                                             
         MVC   BYTE,PBDBFD                                                      
         DROP  R6                                                               
CKTOP22D LA    R6,BUYDT1H                                                       
         ST    R6,ADBERRFD         ADDRESS OF ERROR FIELD (INS DATE)            
         CLI   BYTE,X'FF'          MUTIPLE INS DATE FORMAT FOUND?               
         BE    CKTOP25             YES, DO NOT COMPARE CHG PAIRS                
*                                                                               
         CLI   BYTE,C'M'           INS DATE IS IN MONTHLY FORMAT?               
         BNE   CKTOP22H                                                         
         CLC   DUB(03),6(R3)                                                    
         BNE   CKTOP25U            MONTH IS DIFFERENT, REC IS CHANGED           
         CLI   DUB+03,C'0'                                                      
         BNE   CKTOP25U            FOR MONTHLY, DAY PORTION MUST BE 01          
         CLI   DUB+04,C'1'                                                      
         BNE   CKTOP25U            FOR MONTHLY, DAY PORTION MUST BE 01          
         CLC   DUB+06(02),6+3+1(R3)                                             
         BNE   CKTOP25U            YEAR PORTION IS DIFFERENT                    
         B     CKTOP25                                                          
*                                                                               
CKTOP22H CLI   BYTE,C'B'           INS DATE IS IN BEST FOOD DAY FORMAT?         
         BE    *+12                                                             
         CLI   BYTE,C'W'           INS DATE IS IN WEEK OF FORMAT?               
         BNE   CKTOP22M                                                         
         CLC   BYTE,6(R3)          B OR W PREFIX IS SAME?                       
         BNE   CKTOP25U            NO, DATE IS DIFFERENT                        
         CLC   DUB,6+1(R3)                                                      
         BNE   CKTOP25U            B, W IS SAME, BUT DATE IS DIFFERENT          
         B     CKTOP25                                                          
*                                                                               
CKTOP22M CLC   DUB,6(R3)           NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKTOP25U                                                         
*                                                                               
CKTOP25  CLC   =AL2(D#BUYERC+0),3(R3)                                           
         BE    *+14                                                             
         CLC   =AL2(D#BUYERC+1),3(R3)                                           
         BNE   CKTOP30                                                          
*                                                                               
         B     CKTOP40             NO ODD-EVEN PAIRING TO BE COMPARED           
*                                                                               
CKTOP25U LHI   R0,DCHGDUER         DATA CHANGED BY DIFFERENT USERS              
         BRAS  RE,GET_ETXT                                                      
         B     CKTOPERR                                                         
*                                                                               
CKTOP30  AHI   R2,1                                                             
         BRAS  RE,NXTWFELM         BUMP TO NEW DATA (EVEN NUMBERED)             
         ZICM  RE,3(R3),(3)                                                     
         CR    R2,RE               ODD-EVEN PAIR IS IN RIGHT ORDER?             
         BE    *+6                                                              
         DC    H'0'                OUT OF SYNC!                                 
*                                                                               
CKTOP40  LA    R2,BUYMDH           POINT TO FIRST FLD ON TOP SCREEN             
         ZIC   RF,2(R5)            NUMBER OF FLDS TO BE BUMPED OVER             
         CHI   RF,0                                                             
         BE    *+8                                                              
         BRAS  RE,TBMPFLDS                                                      
*                                                                               
         ZICM  RF,1(R3),(3)        TWO BYTES LENGTH                             
         AHI   RF,-6               MINUS OVER HEAD                              
         BP    CKTOP45                                                          
*                                                                               
         ZICM  RF,0(R2),(1)        TOTAL FLD LENGTH (ICM GENERATES CC)          
         BZ    CKTOP90             DONE IF SCREEN END REACHED                   
         AHI   RF,-8               HEADER LENGTH                                
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BNO   *+8                                                              
         AHI   RF,-8               EXTENSION LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
         NI    6(R2),X'BF'         UNSET CURSOR                                 
         B     CKTOP90                                                          
*                                                                               
CKTOP45  ZIC   RE,0(R2)            COMPUTE MAX FLD LENGTH                       
         AHI   RE,-8               MAX FLD MINUS HEADER                         
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BNO   *+8                                                              
         AHI   RE,-8               MINUS EXTENSION LENGTH                       
*                                                                               
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD MAX FIELD LENGTH                         
*                                                                               
         CR    RF,RE               DATA LENGTH > THAN MAX FLD LENGTH?           
         BNH   *+6                                                              
         LR    RF,RE               CAN ONLY MOVE MAX FLD LENGTH                 
*                                                                               
         BCTR  RE,0                FOR EX INSTRUCTION                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD FIRST                            
*                                                                               
         NI    4(R2),X'FF'-X'20'   RESET PREVIOUSLY VALIDATED                   
*                                                                               
         CLC   HALF,=AL2(D#INSDAT)                                              
         BNE   CKTOP50                                                          
         CLI   6(R3),C'B'          BEST FOOD DAY?                               
         BNE   CKTOP47D                                                         
         CLI   6+1+3(R3),C'/'      BMMM/YYYY FORMAT?                            
         BNE   *+18                                                             
CKTOP47  MVI   5(R2),4             INPUT FLD LENGTH (BMMM OR WMMM)              
         MVC   8(4,R2),6(R3)       BMMM INPUT                                   
         B     CKTOP90                                                          
*                                                                               
CKTOP47B MVI   5(R2),6             INPUT FLD LENGTH (BMMMDD OR WMMMDD)          
         MVC   8(6,R2),6(R3)       BMMM INPUT                                   
         B     CKTOP90                                                          
*                                                                               
CKTOP47D CLI   6(R3),C'W'          WEEK OF?                                     
         BNE   CKTOP47K                                                         
         CLI   6+1+3(R3),C'/'      WMMM/YYYY FORMAT?                            
         BE    CKTOP47                                                          
         B     CKTOP47B                                                         
*                                                                               
CKTOP47K CLI   6+3(R3),C'/'        MONTHLY INSERTION (MMM/YYYY)?                
         BNE   *+18                                                             
         MVI   5(R2),3             INPUT FLD LENGTH (MMM)                       
         MVC   8(3,R2),6(R3)       MMM INPUT                                    
         B     CKTOP90                                                          
*                                                                               
         MVI   5(R2),5             INPUT FLD LENGTH (MMMDD)                     
         MVC   8(5,R2),6(R3)       MMMDD INPUT                                  
         B     CKTOP90                                                          
*                                                                               
CKTOP50  TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    CKTOP52                                                          
         CLC   HALF,=AL2(D#STRDAT)                                              
         BNE   CKTOP52                                                          
         BRAS  RE,FMTIDKDT         FORMAT IDESK INSERTION DATE IN DUB           
         MVC   8(5,R2),DUB                                                      
         MVI   5(R2),5             INPUT FLD LENGTH (MMMDD) - DAILY             
         XC    BUYNM,BUYNM                                                      
         MVC   BUYNM(3),=C'...'    DEFAULT IDESK BUYER INITIALS                 
         MVI   BUYNMH+5,3                                                       
         OI    BUYNMH+6,X'80'      BUYER CODE DISPLAYED                         
         B     CKTOP90                                                          
*                                                                               
CKTOP52  STC   RF,5(R2)            INPUT LENGTH                                 
         BCTR  RF,0                FOR EX INSTRUCTION                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),6(R3)       FIELD DATA                                   
*                                                                               
         CLC   HALF,=AL2(D#ESTNUM)                                              
         BNE   CKTOP55                                                          
         ZIC   R0,5(R2)                                                         
         GOTOR VCASHVAL,DMCB,(0,8(R2)),(X'40',(R0))                             
         CLI   0(R1),X'FF'                                                      
         BE    *+8                                                              
         OI    4(R2),X'08'         VALID NUMERIC FOR ESTIMATE                   
*                                                                               
CKTOP55  DS    0H                  FOR FUTURE USES                              
*                                                                               
CKTOP90  OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
CKTOPX   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTOPERR LTR   RB,RB               NOT EQUAL (ERROR)                            
*                                                                               
         J     XIT_R3              DON'T RESTORE WORKER FILE ELEM PTR           
*                                                                               
* FIRST 2 BYTES ARE FIELD CODE EQUATE                                           
* LAST BYTE IS NUMBER OF FIELDS NEED TO BE BUMPED                               
*                                                                               
TOPFLDTB DS    0H                                                               
         DC    AL2(D#MEDCOD+0),X'00' MEDIA                                      
         DC    AL2(D#BUYERC+0),X'03' BUYER                                      
         DC    AL2(D#BUYERC+1),X'03' BUYER                                      
         DC    AL2(D#CLTCOD+0),X'05' CLIENT                                     
         DC    AL2(D#PRDCOD+0),X'08' PRODUCT                                    
         DC    AL2(D#ESTNUM+0),X'0B' ESTIMATE                                   
         DC    AL2(D#PUBCOD+0),X'0F' PUBLICATION CODE                           
         DC    AL2(D#INSDAT+0),X'16' INSERTION DATE                             
         DC    AL2(D#STRDAT+0),X'16' INSERTION DATE (IDESK START DATE)          
         DC    X'FF'                                                            
*                                                                               
TBMPFLDS ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,TBMPFLDS                                                      
         BR    RE                                                               
*                                                                               
FMTIDKDT LR    R0,RE               FORMAT IDESK INSERTION DATE IN DUB           
         GOTO1 VPERVAL,DMCB,(10,6(R3)),(X'40',WRKELEM)                          
         XC    WRKTEMPS,WRKTEMPS                                                
         LA    RE,WRKELEM                                                       
         USING PERVALD,RE                                                       
         MVC   WRKTEMPS(L'PVALBSTA),PVALBSTA                                    
         DROP  RE                                                               
         GOTO1 VDATCON,DMCB,(3,WRKTEMPS),(5,DUB)                                
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMIDSCR NTR1  BASE=*,LABEL=*      R3 POINTS FIELD DATA ELEM                    
*                                                                               
         MVC   HALF,3(R3)          MAP CODE                                     
         CLC   HALF,=AL2(D#INSKEY)                                              
         BE    CKMIDX              DONE, MAP CODE IS INS KEY                    
*                                                                               
         LA    R5,MIDFLDTM         DEFAULT IS MAGAZINE                          
         LA    R6,15               NUMB OF FLDS TO ALLOCATION                   
*                                                                               
         CLI   BUYMD,C'I'          INTERACTIVE?                                 
         JE    CKMID10H                                                         
         CLI   BUYMD,C'L'          SOCIAL?                                      
         JE    CKMID10H                                                         
         CLI   BUYMD,C'B'          MOBILE?                                      
         JE    CKMID10H                                                         
         CLI   BUYMD,C'D'          DIGITAL AUDIO?                               
         JE    CKMID10H                                                         
         CLI   BUYMD,C'V'          NVIDEO? (NATIONAL VIDEO)                     
         JE    CKMID10H                                                         
         CLI   BUYMD,C'W'          LVIDEO? (LOCAL VIDEO)                        
         JE    CKMID10H                                                         
         J     CKMID10M                                                         
*                                                                               
CKMID10H LA    R5,MIDFLDTI                                                      
         LA    R6,14               NUMB OF FLDS TO ALLOCATION                   
*                                                                               
CKMID10M CLI   BUYMD,C'N'          NEWSPAPER?                                   
         BNE   *+12                                                             
         LA    R5,MIDFLDTN                                                      
         LA    R6,14               NUMB OF FLDS TO ALLOCATION                   
         CLI   BUYMD,C'O'          OUTDOOR?                                     
         BNE   *+12                                                             
         LA    R5,MIDFLDTO                                                      
         LA    R6,16               NUMB OF FLDS TO ALLOCATION                   
*                                                                               
         CLC   HALF,=AL2(D#ALLOCS)                                              
         BNE   CKMID20                                                          
         CLC   =C'ZZZ',BUYPR       POL BUY?                                     
         BE    *+6                                                              
         DC    H'0'                ALLOCATION IS INVALID!                       
         B     CKMID25                                                          
*                                                                               
CKMID20  CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    CKMIDX                                                           
         CLC   HALF,0(R5)                                                       
         BE    *+12                                                             
         LA    R5,3(R5)            NEXT ENTRY IN TABLE                          
         B     CKMID20                                                          
*                                                                               
CKMID25  TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BNZ   CKMID40                                                          
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   CKMID40                                                          
         CLI   REC+3,X'20'         BUY RECORD?                                  
         BE    *+6                                                              
         DC    H'0'                WRONG RECORD!                                
         XC    WRKMIDWK,WRKMIDWK                                                
         ZICM  RE,1(R3),(3)                                                     
         AHI   RE,-6               FLD DATA LENGTH                              
         CHI   RE,80                                                            
         BNH   *+6                                                              
         DC    H'0'                MID-SCR FLDS ARE < 80 CHARS FOR NOW!         
         CHI   RE,0                                                             
         BNH   CKMID30             NOTHING TO COMPARE                           
         STC   RE,WRKMIDWK                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WRKMIDWK+1(0),6(R3) SAVE ORIGINAL DATA TO BE COMPARED            
         LA    R4,REC+33                                                        
         USING PBDELEM,R4                                                       
*                                                                               
* CHANGE MODE, NEED TO CHECK FIELD DATA ARE STILL SAME                          
*                                                                               
         CLC   =AL2(D#ADCODE),3(R3)                                             
         BNE   CKMID25B                                                         
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,2                                                             
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERROR FIELD (ADCODE)              
         OC    WRKMIDWK+1(6),SPACES                                             
         CLC   PBDJOB,WRKMIDWK+1   NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
CKMID25B CLC   =AL2(D#SPCDSC),3(R3)                                             
         BNE   CKMID25D                                                         
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,3                                                             
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERROR FIELD (SPACE DESP)          
         OC    PBDSPACE,SPACES     CONVERT TO UPPER CASES                       
         CLI   REC+2,C'N'                                                       
         BE    CKMID25C                                                         
         ZIC   RE,WRKMIDWK                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PBDSPACE(0),WRKMIDWK+1                                           
         BNE   CKMID25U                                                         
         B     CKMID25D                                                         
*                                                                               
CKMID25C GOTOR FMTNSD,REC+33       FORMAT NEWSPAPER SPACE DESCRIPTION           
         ZIC   RE,WRKMIDWK                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WRKTEMPS(0),6(R3)   NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
CKMID25D CLC   =AL2(D#UNTRAT),3(R3)                                             
         BNE   CKMID25E                                                         
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,4                                                             
         CLI   REC+2,C'O'          OUTDOOR?                                     
         BNE   *+8                                                              
         LA    RF,6                BUMP OVER 6 FOR RATE                         
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERROR FIELD (RATE)                
         GOTOR FMT_COST,DMCB,(1,REC)                                            
         MVC   WRKTEMPS(L'FC_OCOST),WRKELEM+(FC_OCOST-F_COST_D)                 
*                                                                               
         SR    RE,RE               FIELD DATA LENGTH                            
         ICM   RE,3,1(R3)                                                       
         AHI   RE,-7               6 FROM OVERHEAD AND 1 FOR EX                 
         TM    FMTRATSW,PENNDRPQ   PENNY DROPPED?                               
         JZ    *+8                                                              
         AHI   RE,-(1)             ADJUST COMPARING INPUT LENGTH                
         TM    FMTRATSW,DIMEDRPQ   DIME DROPPED?                                
         JZ    *+8                                                              
         AHI   RE,-(1+1)           ADJUST COMPARING INPUT LENGTH                
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WRKTEMPS(0),6(R3)   NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
CKMID25E CLC   =AL2(D#SPCDAT),3(R3)                                             
         BNE   CKMID25F                                                         
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,5                                                             
         CLI   REC+2,C'O'          OUTDOOR?                                     
         BNE   *+8                                                              
         LA    RF,7                BUMP OVER 7 FOR SPACE CLOSING DATE           
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERR FLD (SPC CLOSING DT)          
         GOTO1 VDATCON,DMCB,(3,PBDCDATE),(8,DUB)                                
         CLC   DUB,6(R3)           NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
CKMID25F CLC   =AL2(D#MCLDAT),3(R3)                                             
         BNE   CKMID25G                                                         
*                                                                               
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,7                                                             
*                                                                               
         CLI   REC+2,C'I'          INTERACTIVE?                                 
         JE    CKMID2_F                                                         
         CLI   REC+2,C'L'          SOCIAL?                                      
         JE    CKMID2_F                                                         
         CLI   REC+2,C'B'          MOBILE?                                      
         JE    CKMID2_F                                                         
         CLI   REC+2,C'D'          DIGITAL AUDIO?                               
         JE    CKMID2_F                                                         
         CLI   REC+2,C'V'          NVIDEO? (NATIONAL VIDEO)                     
         JE    CKMID2_F                                                         
         CLI   REC+2,C'W'          LVIDEO? (LOCAL VIDEO)                        
         JE    CKMID2_F                                                         
         J     CKMID2_G                                                         
CKMID2_F LA    RF,6                BUMP OVER 6 FOR MATRL CLOSING DATE           
*                                                                               
CKMID2_G CLI   REC+2,C'N'          NEWSPAPER?                                   
         BNE   *+8                                                              
         LA    RF,6                BUMP OVER 6 FOR MATRL CLOSING DATE           
         CLI   REC+2,C'O'          OUTDOOR?                                     
         BNE   *+8                                                              
         LA    RF,8                BUMP OVER 8 FOR MATRL CLOSING DATE           
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERR FLD (MTRL CLOSING DT)         
         GOTO1 VDATCON,DMCB,(3,PBDMDATE),(8,DUB)                                
         CLC   DUB,6(R3)           NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
CKMID25G CLC   =AL2(D#ONSDAT),3(R3)                                             
         BNE   CKMID25H                                                         
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,6                                                             
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERR FLD (ONSALE DATE)             
         GOTO1 VDATCON,DMCB,(3,PBDSDATE),(8,DUB)                                
         CLC   DUB,6(R3)           NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
CKMID25H CLC   =AL2(D#PREMUM),3(R3)                                             
         BNE   CKMID25I                                                         
         CLI   REC+2,C'N'                                                       
         BE    *+6                 ONLY NEWSPAPER SCR HAS PREMIUM FLD           
         DC    H'0'                                                             
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,5                                                             
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERR FLD (PREMIUM)                 
         GOTOR FMTNPR,REC+33       FOR FORMATTING NEWSPAPER PREMIUM             
*                                                                               
         ZICM  RE,1(R3),(3)        FIELD DATA LENGTH                            
         AHI   RE,-7               6 FROM OVERHEAD AND 1 FOR EX                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WRKTEMPS(0),6(R3)   NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
CKMID25I CLC   =AL2(D#SHOWGS),3(R3)                                             
         BNE   CKMID25J                                                         
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,3                                                             
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERR FLD (SHOWINGS)                
         CP    PBDSHOW,=P'99999'                                                
         BNE   *+18                                                             
         CLC   =C'SPC',WRKMIDWK+1                                               
         BNE   CKMID25U                                                         
         B     CKMID30                                                          
         XC    WRKMIDWK,WRKMIDWK                                                
         EDIT  (P3,PBDSHOW),(5,WRKMIDWK),0,ALIGN=LEFT,ZERO=NOBLANK              
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)          FIELD DATA LENGTH                            
         AHI   RE,-6-1             6 FROM OVERHEAD AND 1 FOR EX                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WRKMIDWK(0),6(R3)   NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
CKMID25J CLC   =AL2(D#REGDSP),3(R3)                                             
         BNE   CKMID25K                                                         
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,4                                                             
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERR FLD (REG DISPLAYS)            
         XC    WRKMIDWK,WRKMIDWK                                                
         EDIT  (P3,PBDREG),(5,WRKMIDWK),0,ALIGN=LEFT,ZERO=NOBLANK               
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)          FIELD DATA LENGTH                            
         AHI   RE,-6-1             6 FROM OVERHEAD AND 1 FOR EX                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WRKMIDWK(0),6(R3)   NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
CKMID25K CLC   =AL2(D#ILLPAN),3(R3)                                             
         BNE   CKMID30                                                          
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,5                                                             
         BRAS  RE,MBMPFLDS                                                      
         ST    R2,ADBERRFD         ADDRESS OF ERR FLD (ILLUM PANELS)            
         XC    WRKMIDWK,WRKMIDWK                                                
         EDITR (P3,PBDILLUM),(5,WRKMIDWK),0,ALIGN=LEFT,ZERO=NOBLANK             
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)          FIELD DATA LENGTH                            
         AHI   RE,-6-1             6 FROM OVERHEAD AND 1 FOR EX                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WRKMIDWK(0),6(R3)   NOT CHANGED SINCE LAST DOWNLOAD?             
         BNE   CKMID25U                                                         
*                                                                               
         B     CKMID30             MID-SCR ORIGINAL DATA ARE CHECKED            
         DROP  R4                                                               
*                                                                               
CKMID25U LHI   R0,DCHGDUER         DATA CHANGED BY DIFFERENT USERS              
         BRAS  RE,GET_ETXT                                                      
         B     CKMIDERR                                                         
*                                                                               
CKMID30  ZICM  R2,3(R3),(3)        SHOULD BE ODD NUMBERED MAP CODE              
         AHI   R2,1                                                             
         BRAS  RE,NXTWFELM         BUMP TO NEW DATA (EVEN NUMBERED)             
         ZICM  RE,3(R3),(3)                                                     
         CR    R2,RE               ODD-EVEN PAIR IS IN RIGHT ORDER?             
         BE    *+6                                                              
         DC    H'0'                OUT OF SYNC!                                 
*                                                                               
CKMID40  LA    R2,BUYTR1H          POINT TO FIRST FLD ON TOP SCREEN             
         CLC   HALF,=AL2(D#ALLOCS)                                              
         BE    CKMID50             R6 HAS NUMBER OF FLDS TO ALLOCATION          
         ZIC   R6,2(R5)            NUMBER OF FLDS TO BE BUMPED OVER             
CKMID50  LR    RF,R6                                                            
         CHI   RF,0                                                             
         BE    *+8                                                              
         BRAS  RE,MBMPFLDS                                                      
*                                                                               
* FIELD ADJUSTMENT IF REQUIRED GOES HERE (IDESK SENDING COMMAS)                 
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JZ    CKMID54                                                          
         CLC   =AL2(D#UNTRAT),3(R3)                                             
         JNE   CKMID54                                                          
         SR    RF,RF                                                            
         ICM   RF,3,1(R3)                                                       
         AHI   RF,-6               GET INPUT LENGTH                             
         CHI   RF,11                                                            
         JNH   CKMID54                                                          
         LR    R1,RF               SAVE ORIGINAL INPUT LENGTH                   
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   WRKTEMPS(0),6(R3)   COPY INPUT FIELD                             
         EX    RF,0(RE)                                                         
         BASR  RE,0                                                             
         XC    6(0,R3),6(R3)       CLEAR INPUT FIELD                            
         EX    RF,0(RE)                                                         
         LA    RE,WRKTEMPS                                                      
         LA    RF,6(R3)                                                         
         LR    R0,R1               USE ORIGINAL LENGTH FOR LOOP                 
         SR    R1,R1               RESET LENGTH TO GET ADJUSTED LENGTH          
CKMID52H CLI   0(RE),C','          COMMAS?                                      
         JE    CKMID52M                                                         
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         AHI   R1,1                COUNTING ADJUSTED LENGTH                     
CKMID52M LA    RE,1(RE)                                                         
         JCT   R0,CKMID52H                                                      
         LR    RF,R1               ADJUSTED LENGTH W/O COMMAS                   
         J     CKMID55                                                          
*                                                                               
CKMID54  ZICM  RF,1(R3),(3)        TWO BYTES LENGTH                             
         AHI   RF,-6               MINUS OVER HEAD                              
         BP    CKMID55                                                          
*                                                                               
         ZICM  RF,0(R2),(1)        TOTAL FIELD LENGTH                           
         BZ    CKMID90             DONE IF SCREEN END REACHED                   
         AHI   RF,-8               HEADER LENGTH                                
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BNO   *+8                                                              
         AHI   RF,-8               EXTENSION LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
         NI    6(R2),X'BF'         UNSET CURSOR                                 
         B     CKMID90                                                          
*                                                                               
CKMID55  ZIC   RE,0(R2)            COMPUTE MAX FLD LENGTH                       
         AHI   RE,-8               MAX FLD MINUS HEADER                         
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BNO   *+8                                                              
         AHI   RE,-8               MINUS EXTENSION LENGTH                       
*                                                                               
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD MAX FIELD LENGTH                         
*                                                                               
         CR    RF,RE               DATA LENGTH > THAN MAX FLD LENGTH?           
         BNH   *+6                                                              
         LR    RF,RE               CAN ONLY MOVE MAX FLD LENGTH                 
*                                                                               
         BCTR  RE,0                FOR EX INSTRUCTION                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD FIRST                            
         NI    4(R2),X'FF'-X'20'   RESET PREVIOUSLY VALIDATED                   
         STC   RF,5(R2)            INPUT LENGTH                                 
         BCTR  RF,0                FOR EX INSTRUCTION                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),6(R3)       FIELD DATA                                   
*                                                                               
         CLC   HALF,=AL2(D#SPCDAT) SPACE CLOSING                                
         BE    CKMID56                                                          
         CLC   HALF,=AL2(D#MCLDAT) MATERIALS CLOSING                            
         BE    CKMID56                                                          
         CLC   HALF,=AL2(D#ONSDAT) ON SALE DATE                                 
         BE    CKMID56                                                          
         B     CKMID58                                                          
*                                                                               
CKMID56  DS    0H     FOR DATES-CHECK IF ONE DIGIT DAY SENT                     
         CLI   10(R3),C'/' WILL BE / IF 1 DIGIT DAY                             
         BNE   CKMID58                                                          
         MVI   5(R2),4     RESET LENGTH TO 4                                    
         MVI   12(R2),0    REMOVE /                                             
*                                                                               
CKMID58  CLC   HALF,=AL2(D#SHOWGS)                                              
         BNE   CKMID60                                                          
         CLC   8(3,R2),=C'SPC'     FIELD DATA IS SPC?                           
         BE    CKMID60             NO NEED TO MAKE FLD A VALID NUMERIC          
         ZIC   R0,5(R2)                                                         
         GOTOR VCASHVAL,DMCB,(0,8(R2)),(X'40',(R0))                             
         CLI   0(R1),X'FF'                                                      
         BE    *+8                                                              
         OI    4(R2),X'08'         VALID NUMERIC FOR SHOWING                    
*                                                                               
CKMID60  CLC   HALF,=AL2(D#REGDSP)                                              
         BNE   CKMID62                                                          
         ZIC   R0,5(R2)                                                         
         GOTOR VCASHVAL,DMCB,(0,8(R2)),(X'40',(R0))                             
         CLI   0(R1),X'FF'                                                      
         BE    *+8                                                              
         OI    4(R2),X'08'         VALID NUMERIC FOR REGULAR DISPLAY            
*                                                                               
CKMID62  CLC   HALF,=AL2(D#ILLPAN)                                              
         BNE   CKMID64                                                          
         ZIC   R0,5(R2)                                                         
         GOTOR VCASHVAL,DMCB,(0,8(R2)),(X'40',(R0))                             
         CLI   0(R1),X'FF'                                                      
         BE    *+8                                                              
         OI    4(R2),X'08'         VALID NUMERIC FOR ILLUM DISPLAY              
*                                                                               
CKMID64  DS    0H                  FOR FUTURE USES                              
*                                                                               
CKMID90  OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
CKMIDX   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKMIDERR LTR   RB,RB               NOT EQUAL (ERROR)                            
         J     XIT_R3                                                           
*                                                                               
* FIRST 2 BYTES ARE FIELD CODE EQUATE                                           
* LAST BYTE IS NUMBER OF FIELDS NEED TO BE BUMPED FROM TR CODE FLD              
*                                                                               
MIDFLDTI DS    0H                                                               
         DC    AL2(D#ADCODE),X'02' AD CODE               (06 CHARS)             
         DC    AL2(D#SPCDSC),X'03' SPACE DESCRIPTION     (17 CHARS)             
         DC    AL2(D#UNTRAT),X'04' RATE                  (11 CHARS)             
         DC    AL2(D#SPCDAT),X'05' SPACE CLOSING DATE    (05 CHARS)             
         DC    AL2(D#MCLDAT),X'06' MATERIAL CLOSING DATE (05 CHARS)             
         DC    X'FF'                                                            
*                                                                               
MIDFLDTM DS    0H                                                               
         DC    AL2(D#ADCODE),X'02' AD CODE               (06 CHARS)             
         DC    AL2(D#SPCDSC),X'03' SPACE DESCRIPTION     (17 CHARS)             
         DC    AL2(D#UNTRAT),X'04' RATE                  (11 CHARS)             
         DC    AL2(D#SPCDAT),X'05' SPACE CLOSING DATE    (05 CHARS)             
         DC    AL2(D#ONSDAT),X'06' ON-SALE DATE          (05 CHARS)             
         DC    AL2(D#MCLDAT),X'07' MATERIAL CLOSING DATE (05 CHARS)             
         DC    X'FF'                                                            
*                                                                               
MIDFLDTN DS    0H                                                               
         DC    AL2(D#ADCODE),X'02' AD CODE               (06 CHARS)             
         DC    AL2(D#SPCDSC),X'03' SPACE DESCRIPTION     (08 CHARS)             
         DC    AL2(D#UNTRAT),X'04' RATE                  (10 CHARS)             
         DC    AL2(D#PREMUM),X'05' PREMIUM               (11 CHARS)             
         DC    AL2(D#MCLDAT),X'06' MATERIAL CLOSING DATE (05 CHARS)             
         DC    X'FF'                                                            
*                                                                               
MIDFLDTO DS    0H                                                               
         DC    AL2(D#ADCODE),X'02' AD CODE               (06 CHARS)             
         DC    AL2(D#SHOWGS),X'03' SHOWING/GRP           (03 CHARS)             
         DC    AL2(D#REGDSP),X'04' REGULAR DISPLAYS      (04 CHARS)             
         DC    AL2(D#ILLPAN),X'05' ILLUMINATED PANELS    (04 CHARS)             
         DC    AL2(D#UNTRAT),X'06' RATE                  (11 CHARS)             
         DC    AL2(D#SPCDAT),X'07' SPACE CLOSING DATE    (05 CHARS)             
         DC    AL2(D#MCLDAT),X'08' MATERIAL CLOSING DATE (05 CHARS)             
         DC    X'FF'                                                            
*                                                                               
MBMPFLDS ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,MBMPFLDS                                                      
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKFLDLEN NTR1  BASE=*,LABEL=*      CALC FLD LEN IF INPUT IS PRESENT             
*                                                                               
         LA    R2,BUYNMH           POINT TO BUYER INITIAL FLD                   
         BRAS  RE,CHECKFLN                                                      
         LA    R2,BUYDT1H          POINT TO INSERTION DATE FLD                  
         BRAS  RE,CHECKFLN                                                      
*                                                                               
         LA    R2,BUYAD1H          POINT TO ADCODE FLD                          
         LA    R5,6                6 FLDS TO CHK ON MAG SCR (DEFAULT)           
         CLI   BUYMD,C'I'          INTERACTIVE?                                 
         JE    CKFLL05H                                                         
         CLI   BUYMD,C'L'          SOCIAL?                                      
         JE    CKFLL05H                                                         
         CLI   BUYMD,C'B'          MOBILE?                                      
         JE    CKFLL05H                                                         
         CLI   BUYMD,C'D'          DIGITIAL AUDIO?                              
         JE    CKFLL05H                                                         
         CLI   BUYMD,C'V'          NVIDEO? (NATIONAL VIDEO)                     
         JE    CKFLL05H                                                         
         CLI   BUYMD,C'W'          LVIDEO? (LOCAL VIDEO)                        
         JE    CKFLL05H                                                         
         J     CKFLL05M                                                         
*                                                                               
CKFLL05H LA    R5,5                5 FLDS TO CHK ON INTERACTIVE SCREEN          
*                                                                               
CKFLL05M CLI   BUYMD,C'N'          NEWSPAPER?                                   
         BNE   *+8                                                              
         LA    R5,5                5 FLDS TO CHK ON NWS SCR                     
         CLI   BUYMD,C'O'          OUTDOOR?                                     
         BNE   *+8                                                              
         LA    R5,7                7 FLDS TO CHK ON ODR SCR                     
*                                                                               
         CLC   =C'ZZZ',BUYPR       POL BUY?                                     
         BNE   *+8                                                              
         AHI   R5,9                NEED TO CHK ALLOCATION FLD TOO               
*                                                                               
CKFLL10  TM    1(R2),X'20'         PROTECTED FLD?                               
         BO    *+8                 YES, SKIP THIS FLD                           
         BRAS  RE,CHECKFLN                                                      
         ZIC   R1,0(R2)            BUMP TO NEXT FDL                             
         AR    R2,R1                                                            
         BCT   R5,CKFLL10                                                       
*                                                                               
CKFLLX   J     EXIT                                                             
*                                                                               
CKFIXLEN LA    RF,8(R2)            POINT TO BEGINNING OF DATA                   
         ZIC   R1,0(R2)            LENGTH OF HEADER + FIELD                     
         AHI   R1,-9               R1 = LENGTH OF FIELD + ONE BYTE              
         AR    RF,R1               POINT TO LAST CHAR OF FLD DATA               
         AHI   R1,1                R1 SHOULD NOW BE LENGTH OF SCR FLD           
CKFIXL10 CLI   0(RF),C' '          SCAN BACKWARDS FOR NON SPACE                 
         BH    CKFIXL20                                                         
         BCTR  RF,0                POINT TO PREVIOUS CHAR OF DATA               
         BCT   R1,CKFIXL10                                                      
         BR    RE                  NO INPUT (SHOULDN'T HAPPEN)                  
*                                                                               
CKFIXL20 STC   R1,5(R2)            SET INPUT LENGTH INTO HEADER                 
         OI    1(R2),X'01'         SET ON MODIFIED BIT                          
         BR    RE                                                               
*                                                                               
CHECKFLN ST    RE,WRKSAVRE                                                      
         ZIC   R1,0(R2)            LENGTH OF HEADER + FIELD                     
         AHI   R1,-8               LENGTH OF FIELD                              
         BCTR  R1,0                FOR EX INSTRUCTION                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)       FLD DATA PRESENT?                            
         BZ    *+12                                                             
         BRAS  RE,CKFIXLEN         CALCULATE FLD DATA LENGTH                    
         BRAS  RE,CKFIXNUM         FIX NUMERIC FLD ATTRIBUTE                    
         J     X_WKSVRE                                                         
*                                                                               
CKFIXNUM ST    RE,WRKFULL1                                                      
         ZIC   R0,5(R2)                                                         
         GOTOR VCASHVAL,DMCB,(0,8(R2)),(X'40',(R0))                             
         CLI   0(R1),X'FF'                                                      
         BE    *+8                                                              
         OI    4(R2),X'08'         SET NUMERIC FLD ATTRIBUTE                    
         L     RE,WRKFULL1                                                      
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DRAFT INSERTION IS PROCESSED, NEED TO RETURN LOOKED UP DATA                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DRFTRPLY NTR1  BASE=*,LABEL=*      R3 POINTS TO WORKER ELEM                     
*                                                                               
DRFTR20  CLI   0(R3),0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    *+12                                                             
         BRAS  RE,NXTWFELM                                                      
         B     DRFTR20                                                          
*                                                                               
         SR    R4,R4               SAVE RETURNED DATA HDR ELEM LENGTH           
         ICM   R4,3,1(R3)                                                       
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BNE   DRFTR30                                                          
         MVI   0(R3),LQ_DLDDQ      DOWNLOAD DATA ELEM CODE                      
         MVI   1(R3),0                                                          
         MVI   2(R3),5             2ND BYTE OF ELEM LENGTH                      
         MVC   3(2,R3),=AL2(E#INSDFT)                                           
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-5               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR30  BRAS  RE,DRFT_REC         START BUILDING RETURN DATA FIELDS            
         USING PBUYREC,R5                                                       
*                                                                               
         CLI   PCVERSN#,X'04'      HIGHER THAN 4.X.X.X?                         
         JL    DRFTR32                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY TOKEN                                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+L'REQTOKEN  LENGTH                                       
         MVC   3(2,R3),=AL2(D#QTOKEN)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(L'REQTOKEN,R3),REQTOKEN                                        
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-(6+L'REQTOKEN)  RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR32  MVI   0(R3),LQ_RAWDQ      REPLY MEDIA                                  
         MVI   1(R3),0                                                          
         MVI   2(R3),7             LENGTH                                       
         MVC   3(2,R3),=AL2(D#MEDCOD)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(1,R3),PBUYKMED                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-7               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY CLIENT                                 
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#CLTCOD)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(3,R3),PBUYKCLT                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY PRODUCT                                
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#PRDCOD)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(3,R3),PBUYKPRD                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY ESTIMATE                               
         MVI   1(R3),0                                                          
         MVI   2(R3),8             LENGTH                                       
         MVC   3(2,R3),=AL2(D#ESTNUM)                                           
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         MVC   6(2,R3),PBUYKEST                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-8               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY PUBLICATION CODE                       
         MVC   3(2,R3),=AL2(D#PUBCOD)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         ZIC   RE,BUYPBH+5                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                THERE HAS TO BE A PUB NUMBER!                
         AHI   RE,6                                                             
         STCM  RE,3,1(R3)          LENGTH                                       
         AHI   RE,-7               MINUS 6 FROM OVERHEAD & ONE FOR EX           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),BUYPB                                                    
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-14              RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         DROP  R5                                                               
*                                                                               
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         USING PBDELEM,R5                                                       
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                FIRST BUY ELEM MUST EXIST!                   
*                                                                               
         CLI   BUYMD,C'N'          NEWSPAPER?                                   
         BNE   DRFTR40B                                                         
         GOTOR FMTNSD,0(R5)        FORMAT NEWSPAPER SPACE DESCRIPTION           
         CLC   WRKTEMPS(17),SPACES ANYTHING IN NEWSPAPER SPACE DESP?            
         BE    DRFTR40V                                                         
         B     *+14                                                             
DRFTR40B OC    PBDSPACE,PBDSPACE   ANYTHING IN SPACE FIELD?                     
         BZ    DRFTR40V                                                         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY SPACE DESCRIPTION                      
         MVI   1(R3),0                                                          
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   3(2,R3),=AL2(D#SPCDSC)                                           
*                                                                               
         CLI   BUYMD,C'O'          OUTDOOR?                                     
         BE    DRFTR40D                                                         
         CLI   BUYMD,C'N'          NEWSPAPER?                                   
         BE    DRFTR40M                                                         
         MVI   2(R3),6+17          LENGTH                                       
         MVC   6(L'PBDSPACE,R3),PBDSPACE                                        
         AHI   R4,-6-17            RECALCULATE RETURNED DATA HDR EL LEN         
         B     DRFTR40U                                                         
*                                                                               
DRFTR40D BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         MVI   ELCODE,X'66'        LOOKING FOR VERY FIRST COMMENT ELEM          
         BRAS  RE,NXTELEM          COMMENT ELEM FOUND?                          
         BNE   DRFTR40V                                                         
         ZIC   R1,1(R5)                                                         
         AHI   R1,-2               MINUS ELEMENT OVERHEAD                       
         CHI   R1,17               COMMENT IS A SPACE DESCRIPTION?              
         BH    DRFTR40V                                                         
         LR    RE,R1                                                            
         AHI   RE,6                ADD OVERHEAD                                 
         STC   RE,2(R3)            LENGTH FOR REPLY ELEM                        
         SR    R4,RE               RECALCULATE RETURNED DATA HDR EL LEN         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),2(R5)       GET SPACE DESCRIPTION                        
         B     DRFTR40U                                                         
*                                                                               
DRFTR40M MVI   2(R3),6+8           NEWSPAPER HAS 8 CHARS SPACE DESP             
         MVC   6(8,R3),WRKTEMPS                                                 
         AHI   R4,-6-8             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR40U BRAS  RE,NXTWFELM                                                      
         B     DRFTR40X                                                         
*                                                                               
DRFTR40V XC    0(30,R3),0(R3)      NO SPACE DESCRIPTION IS REPLIED              
*                                                                               
DRFTR40X DS    0H                                                               
         DROP  R5                                                               
*                                                                               
         CLI   BUYMD,C'O'          OUTDOOR?                                     
         BNE   DRFTR60                                                          
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         USING PBDELEM,R5                                                       
         CLI   PBDSPACE,X'FF'      PACK NUMBERS PRESENT?                        
         BNE   DRFTR60                                                          
*                                                                               
         CP    PBDSHOW,=P'0'       SHOWING IS ZERO?                             
         BE    DRFTR50H                                                         
         MVI   0(R3),LQ_RAWDQ      REPLY SHOWING                                
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#SHOWGS)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         CP    PBDSHOW,=P'99999'                                                
         BNE   DRFTR50C                                                         
         MVC   6(3,R3),=C'SPC'                                                  
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
         B     DRFTR50F                                                         
*                                                                               
DRFTR50C XC    WRKTEMPS,WRKTEMPS                                                
         EDIT  (P3,PBDSHOW),(5,WRKTEMPS),0,ALIGN=LEFT                           
         LR    RE,R0               NUMBER OF SIGNIFICANT CHARS                  
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD NUMBER                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKTEMPS                                                 
         AHI   RE,7                6 OVERHEAD AND 1 FROM EX                     
         STC   RE,2(R3)            LENGTH                                       
         SR    R4,RE               RECALCULATE RETURNED DATA HDR EL LEN         
DRFTR50F BRAS  RE,NXTWFELM                                                      
*                                                                               
DRFTR50H CP    PBDREG,=P'0'        NUMBER OF REGULAR DISPLAY IS ZERO?           
         BE    DRFTR50M                                                         
         MVI   0(R3),LQ_RAWDQ      REPLY NUMBER OF REGULAR DISPLAY              
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#REGDSP)                                           
         MVI   5(R3),LD_SPAKQ      DATA TYPE - SIGNED PACKED                    
         MVC   6(3,R3),PBDREG                                                   
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
         BRAS  RE,NXTWFELM                                                      
*                                                                               
DRFTR50M CP    PBDILLUM,=P'0'      NUMBER OF ILLUM DISPLAY IS ZERO?             
         BE    DRFTR60                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY NUMBER OF ILLUMINATED DISPLAY          
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#ILLPAN)                                           
         MVI   5(R3),LD_SPAKQ      DATA TYPE - SIGNED PACKED                    
         MVC   6(3,R3),PBDILLUM                                                 
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
         BRAS  RE,NXTWFELM                                                      
*                                                                               
DRFTR60  BRAS  RE,DRFT_REC                                                      
         GOTOR FMT_COST,DMCB,(1,0(R5))                                          
         MVC   WRKTEMPS(L'FC_OCOST),WRKELEM+(FC_OCOST-F_COST_D)                 
         CLC   WRKTEMPS(11),SPACES NOTHING IN RATE?                             
         BNH   DRFTR65                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY RATE (PADDED WITH SPACES)              
         MVI   1(R3),0                                                          
         MVI   2(R3),6+11                                                       
         MVC   3(2,R3),=AL2(D#UNTRAT)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(11,R3),WRKTEMPS                                                
         AHI   R4,-6-11            RECALCULATE RETURNED DATA HDR EL LEN         
         BRAS  RE,NXTWFELM                                                      
*                                                                               
DRFTR65  CLI   BUYMD,C'N'          NEWSPAPAER?                                  
         BNE   DRFTR70                                                          
         BRAS  RE,DRFT_REC                                                      
         GOTOR FMTNPR,33(R5)       FOR FORMATTING NEWSPAPER PREMIUM             
         CLC   WRKTEMPS(11),SPACES NOTHING IN PREMIUM?                          
         BE    DRFTR65X                                                         
         MVI   0(R3),LQ_RAWDQ      REPLY PREMIUM (PADDED WITH SPACES)           
         MVI   1(R3),0                                                          
         MVI   2(R3),6+11                                                       
         MVC   3(2,R3),=AL2(D#PREMUM)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(11,R3),WRKTEMPS                                                
         AHI   R4,-6-11            RECALCULATE RETURNED DATA HDR EL LEN         
         BRAS  RE,NXTWFELM                                                      
         DROP  R5                                                               
DRFTR65X DS    0H                                                               
*                                                                               
* CONTRACT LINEAGE EQUIVALENCY IS NOT SUPPORTED IN EARLIER AD VERSION           
*                                                                               
         CLI   PCVERSN#,X'02'      HIGHER THAN 2.X.X.X?                         
         BL    DRFTR66X                                                         
         BRAS  RE,FMTNCL                                                        
         BNE   DRFTR66X            CLE NOT PRESENT                              
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY CLE                                    
         MVI   1(R3),0                                                          
         MVI   2(R3),6+12          LENGTH                                       
         MVC   3(2,R3),=AL2(D#CONLIE)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(12,R3),WRKTEMPS                                                
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-12            RECALCULATE RETURNED DATA HDR EL LEN         
DRFTR66X DS    0H                                                               
*                                                                               
DRFTR70  XC    WRKTEMPS,WRKTEMPS                                                
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         USING PBDELEM,R5                                                       
         MVC   WRKBYTE1,PBDWTSUM   USED IN FORMATTING ALLOCATIONS               
         DROP  R5                                                               
*                                                                               
         BRAS  RE,FMTPRDS                                                       
         OC    WRKTEMPS,WRKTEMPS                                                
         BZ    DRFTR75                                                          
         LA    RE,WRKTEMPS                                                      
         SR    RF,RF                                                            
DRFTR70D CLI   0(RE),0             END OF ALLOCATION STRING?                    
         BE    DRFTR70H                                                         
         CHI   RF,50               MAX STRING LENGTH ENCOUNTERED?               
         BNL   DRFTR70H                                                         
         AHI   RF,1                                                             
         LA    RE,1(RE)                                                         
         B     DRFTR70D                                                         
DRFTR70H MVI   0(R3),LQ_RAWDQ      REPLY ALLOCATION                             
         MVI   1(R3),0                                                          
         MVC   3(2,R3),=AL2(D#ALLOCS)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         CHI   RF,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKTEMPS                                                 
         AHI   RF,7                6 OVERHEAD AND 1 FROM EX                     
         STC   RF,2(R3)            LENGTH                                       
         SR    R4,RF               RECALCULATE RETURNED DATA HDR EL LEN         
         BRAS  RE,NXTWFELM                                                      
*                                                                               
DRFTR75  BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         USING PBDELEM,R5                                                       
         OC    PBDJOB,PBDJOB                                                    
         BZ    DRFTR75H                                                         
         CLI   PBDJOB,X'FF'        AD-ID ONLY?                                  
         BE    DRFTR75H                                                         
         MVI   0(R3),LQ_RAWDQ      REPLY ADCODE                                 
         MVI   1(R3),0                                                          
         MVI   2(R3),12            LENGTH                                       
         MVC   3(2,R3),=AL2(D#ADCODE)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(6,R3),PBDJOB                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-12              RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR75H CLC   PCVERSN#,=AL1(03,04,00,05)                                       
         BL    DRFTR76                                                          
         OC    SV_AD_ID,SV_AD_ID                                                
         BZ    DRFTR76                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY AD-ID                                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+L'SV_AD_ID  LENGTH                                       
         MVC   3(2,R3),=AL2(D#AD_ID)                                            
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(L'SV_AD_ID,R3),SV_AD_ID                                        
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-12              RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR76  MVI   WRKBYTE1,X'01'      DEFAULT IS LIVE                              
         CLI   PBDBFD,C'T'         TEST BUY?                                    
         BNE   *+8                                                              
         MVI   WRKBYTE1,X'02'      TEST BUY                                     
         TM    PBDSTAT2,X'40'      STEWARDSHIP INSERTION?                       
         BZ    *+8                                                              
         MVI   WRKBYTE1,X'10'      STEWARDSHIP INSERTION                        
         DROP  R5                                                               
*                                                                               
         BRAS  RE,DRFT_REC                                                      
         USING PBUYKEY,R5                                                       
         TM    PBUYCNTL,X'80'      BUY DELETED ?                                
         BZ    *+8                                                              
         OI    WRKBYTE1,X'04'      DELETED BUY                                  
         MVI   0(R3),LQ_RAWDQ      REPLY BUY STATUS                             
         MVI   1(R3),0                                                          
         MVI   2(R3),7             LENGTH                                       
         MVC   3(2,R3),=AL2(D#INSSTA)                                           
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         MVC   6(1,R3),WRKBYTE1                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-7               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         OC    PBUYKDAT,PBUYKDAT                                                
         BZ    DRFTR77                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY INSERTION DATE                         
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#INSDAT)                                           
         MVI   5(R3),LD_BDATQ      DATA TYPE - BINARY DATE                      
         MVC   6(3,R3),PBUYKDAT                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
         DROP  R5                                                               
*                                                                               
DRFTR77  BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         USING PBDELEM,R5                                                       
         OC    PBDCDATE,PBDCDATE                                                
         BZ    DRFTR80                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY SPACE CLOSING DATE                     
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#SPCDAT)                                           
         MVI   5(R3),LD_BDATQ      DATA TYPE - BINARY DATE                      
         MVC   6(3,R3),PBDCDATE                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR80  OC    PBDSDATE,PBDSDATE                                                
         BZ    DRFTR82                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY ON-SALE DATE                           
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#ONSDAT)                                           
         MVI   5(R3),LD_BDATQ      DATA TYPE - BINARY DATE                      
         MVC   6(3,R3),PBDSDATE                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR82  OC    PBDBDATE,PBDBDATE                                                
         BZ    DRFTR84                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY BILLABLE DATE                          
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#BBLDAT)                                           
         MVI   5(R3),LD_BDATQ      DATA TYPE - BINARY DATE                      
         MVC   6(3,R3),PBDBDATE                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR84  OC    PBDPDATE,PBDPDATE                                                
         BZ    DRFTR86                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY PAYABLE DATE                           
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#PBLDAT)                                           
         MVI   5(R3),LD_BDATQ      DATA TYPE - BINARY DATE                      
         MVC   6(3,R3),PBDPDATE                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR86  OC    PBDMDATE,PBDMDATE                                                
         BZ    DRFTR88                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY MATERIAL CLOSING DATE                  
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#MCLDAT)                                           
         MVI   5(R3),LD_BDATQ      DATA TYPE - BINARY DATE                      
         MVC   6(3,R3),PBDMDATE                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR88  CP    PBDACP,=P'0'                                                     
         BE    DRFTR90                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY AGENCY COMMISSION PERCENTAGE           
         MVI   1(R3),0                                                          
         MVI   2(R3),10            LENGTH                                       
         MVC   3(2,R3),=AL2(D#COMPCT)                                           
         MVI   5(R3),LD_SPAKQ      DATA TYPE - SIGNED PACKED                    
         ZAP   6(4,R3),PBDACP                                                   
         CP    6(4,R3),=P'-1'      100%?                                        
         JNE   *+10                                                             
         ZAP   6(4,R3),=P'100000'                                               
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-10              RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR90  CP    PBDCD,=P'0'                                                      
         BE    DRFTR92                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY CASH DISCOUNT PERCENTAGE               
         MVI   1(R3),0                                                          
         MVI   2(R3),8             LENGTH                                       
         MVC   3(2,R3),=AL2(D#DSCPCT)                                           
         MVI   5(R3),LD_SPAKQ      DATA TYPE - SIGNED PACKED                    
         MVC   6(2,R3),PBDCD                                                    
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-8               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR92  OC    PBDTAX,PBDTAX                                                    
         BZ    DRFTR94                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY TAX PERCENTAGE                         
         MVI   1(R3),0                                                          
         MVI   2(R3),9             LENGTH                                       
         MVC   3(2,R3),=AL2(D#TAXPCT)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(3,R3),PBDTAX                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-9               RECALCULATE RETURNED DATA HDR EL LEN         
         DROP  R5                                                               
*                                                                               
DRFTR94  MVI   ELCODE,X'80'        SPECIAL REP ELEM CODE                        
         BRAS  RE,NXTELEM                                                       
         BNE   DRFTR95                                                          
         USING PBSREPEL,R5                                                      
         OC    PBSREP,PBSREP                                                    
         BZ    DRFTR95                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY SPECIAL REP                            
         MVI   1(R3),0                                                          
         MVI   2(R3),10            LENGTH                                       
         MVC   3(2,R3),=AL2(D#SPREP)                                            
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(4,R3),PBSREP                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-10              RECALCULATE RETURNED DATA HDR EL LEN         
         DROP  R5                                                               
*                                                                               
DRFTR95  BRAS  RE,DRFT_REC                                                      
         GOTOR VGETINS,DMCB,0(R5),PVALUES,7(R5),(C'F',0),0,0                    
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY GROSS ORDERED                          
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#GRSORD)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),GROSS                                                    
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         L     RE,GROSS                                                         
         S     RE,AGYCOM                                                        
         ST    RE,WRKFULL1                                                      
         MVI   0(R3),LQ_RAWDQ      REPLY NET ORDERED                            
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#NETORD)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),WRKFULL1                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY GROSS LESS CD (BILLABLE)               
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#GLCDO)                                            
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),BLABLE                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY NET LESS CD (PAYABLE)                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#NLCDO)                                            
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),PYABLE                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY AGENCY COMMISSION AMT                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#COMAMT)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),AGYCOM                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY CASH DISCOUNT AMT                      
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#DSCAMT)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),CSHDSC                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY CASH DISCOUNT AMT                      
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#TAXAMT)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),TAX                                                      
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
* NET AND GROSS PAYABLE ARE NOT SUPPORTED IN EARLIER ADBUYER VERSIONS           
*                                                                               
         CLI   PCVERSN#,X'02'      HIGHER THAN 2.X.X.X?                         
         BL    DRFTR97X                                                         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY NET PAYABLE                            
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#NPYBLE)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,PYABLE                                                     
         ICM   RF,15,CSHDSC                                                     
         AR    RE,RF                                                            
         ST    RE,FULL                                                          
         ICM   RE,15,PGROSS                                                     
         ICM   RF,15,PAGYCOM                                                    
         SR    RE,RF                                                            
         L     RF,FULL                                                          
         SR    RF,RE                                                            
         STCM  RF,15,6(R3)         PYABLE+CSHDSC-SUM(PPGROSS-PPAGYCOM)          
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY GROSS PAYABLE                          
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#GPYBLE)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,GROSS                                                      
         ICM   RF,15,PGROSS                                                     
         SR    RE,RF                                                            
         STCM  RE,15,6(R3)         GROSS-SUM(PPGROSS)                           
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
DRFTR97X DS    0H                                                               
*                                                                               
         CLC   PCVERSN#,=AL1(03,02,00,03)                                       
         BL    DRFTR98                                                          
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         USING PBDELEM,R5                                                       
         OC    PBDRCODE,PBDRCODE   ANYTHING IN RATE CODE?                       
         BZ    DRFTR98                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY RATE CODE                              
         MVI   1(R3),0                                                          
         MVI   2(R3),6+L'PBDRCODE  LENGTH                                       
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   3(2,R3),=AL2(D#RATECD)                                           
         MVC   6(L'PBDRCODE,R3),PBDRCODE                                        
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+L'PBDRCODE)   RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
DRFTR98  BRAS  RE,RPYC2FAC         REPLY COS2 FACTOR DATA                       
         BRAS  RE,RPYC2$$$         REPLY COS2 $ DATA                            
         BRAS  RE,RPYPLCOS         REPLY PLANNED COST                           
         BRAS  RE,RPYCONUN         REPLY CONTRACT UNIT VALUE                    
         BRAS  RE,RPYFXVAL         REPLY FX VALUES                              
*                                                                               
         CHI   R4,1                                                             
         BNL   *+6                                                              
         DC    H'0'                RETURN RECORD IS TOO SMALL                   
         MVI   0(R3),LQ_RDATQ                                                   
         STCM  R4,3,1(R3)          UPDATED ELEM LENGHT                          
*                                                                               
DRFTRX   J     EXIT                                                             
*                                                                               
FMTPRDS  DS    0H                  FORMAT POL PRD ALLOCATIONS                   
         ST    RE,WRKSAVRE                                                      
         XC    WRKTEMPS,WRKTEMPS                                                
         SR    R0,R0                                                            
         LA    R6,WRKTEMPS                                                      
*                                                                               
         TM    WRKBYTE1,X'80'      TEST UNEQUAL SPLIT                           
         BZ    FMTP2               NO-EQUAL                                     
         IC    R0,WRKBYTE1         GET SUM OF WEIGHTS                           
         N     R0,=F'127'                                                       
         BRAS  RE,FMTPEDT                                                       
         MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
*                                                                               
FMTP2    MVI   ELCODE,X'21'                                                     
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   FMTPX                                                            
FMTP4    MVC   0(3,R6),2(R5)       PRD CODE                                     
         CLI   2(R6),C' '                                                       
         BNE   *+10                                                             
         MVI   2(R6),0                                                          
         BCTR  R6,0                                                             
         LA    R6,3(R6)                                                         
         TM    WRKBYTE1,X'80'      TEST UNEQUAL SPLIT                           
         BZ    FMTP6                                                            
         MVI   0(R6),C'-'                                                       
         LA    R6,1(R6)                                                         
         ZIC   R0,3+2(R5)          COST SHARE                                   
         BRAS  RE,FMTPEDT                                                       
         CLI   5(R5),0                                                          
         BNZ   *+12                                                             
         MVI   0(R6),C'0'                                                       
         LA    R6,1(R6)                                                         
         CLC   5(1,R5),6(R5)       TEST COST SHARE=SPACE SHARE                  
         BE    FMTP6                                                            
         MVI   0(R6),C'-'                                                       
         LA    R6,1(R6)                                                         
         IC    R0,4+2(R5)          SPACE SHARE                                  
         BRAS  RE,FMTPEDT                                                       
FMTP6    BRAS  RE,NXTELEM                                                       
         BNE   FMTPX                                                            
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
         B     FMTP4                                                            
FMTPX    J     X_WKSVRE                                                         
*                                                                               
FMTPEDT  EDIT  (R0),(3,0(R6)),ALIGN=LEFT                                        
         AR    R6,R0               POINT TO NEXT OUTPUT POSITION                
         BR    RE                                                               
*                                                                               
DRFT_REC LA    R5,NEWREC                                                        
         CLI   SVTRCODE,C'B'                                                    
         JE    *+8                                                              
         LA    R5,REC                                                           
         BR    RE                                                               
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYC2FAC NTR1  BASE=*,LABEL=*      REPLY COS2 FACTOR DATA                       
*                                                                               
         CLC   PCVERSN#,=AL1(03,03,00,08)                                       
         BL    R_C2F_X                                                          
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         MVI   ELCODE,X'91'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   R_C2F_X                                                          
         USING PCOS2FEL,R5                                                      
         OC    PCOS2FAC,PCOS2FAC   ANYTHING IN COS2 FACTOR?                     
         BZ    R_C2F_X                                                          
         CP    PCOS2FAC,=P'0'      COS2 FACTOR IS ZERO?                         
         BE    R_C2F_X                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY RATE CODE                              
         MVI   1(R3),0                                                          
         MVI   2(R3),6+L'BUYCOS2F  LENGTH                                       
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   3(2,R3),=AL2(D#C2FACT)                                           
         EDITR PCOS2FAC,BUYCOS2F,6,FLOAT=-,ALIGN=LEFT                           
         MVC   6(L'BUYCOS2F,R3),BUYCOS2F                                        
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+L'BUYCOS2F)   RECALCULATE RETURNED DATA HDR EL LEN         
         DROP  R5                                                               
*                                                                               
         BRAS  RE,DRFT_REC                                                      
         GOTOR VGETINS,DMCB,0(R5),(C'O',PVALUES),7(R5),0,0,0                    
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY COS2 FACTOR GROSS AMOUNT               
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#C2FGRS)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,GROSS                                                      
         STCM  RE,15,6(R3)         COS2 FACTOR GROSS AMOUNT                     
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+4)            RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY COS2 FACTOR NET AMOUNT                 
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#C2FNET)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,GROSS                                                      
         S     RE,AGYCOM                                                        
         STCM  RE,15,6(R3)         COS2 FACTOR NET AMOUNT                       
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+4)            RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
R_C2F_X  J     XIT_R3R4                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYC2$$$ NTR1  BASE=*,LABEL=*      REPLY COS2 $ DATA                            
*                                                                               
         CLC   PCVERSN#,=AL1(04,00,00,04)                                       
         BL    R_C2$_X                                                          
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         MVI   ELCODE,PORELMEQ                                                  
         BRAS  RE,NXTELEM                                                       
         BNE   R_C2$_X                                                          
         USING PORELEM,R5                                                       
         TM    PORCOSS1,PORCOS$Q   COS2 $ NON-FINANCIAL?                        
         BZ    R_C2$_X                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY RATE CODE                              
         MVI   1(R3),0                                                          
         MVI   2(R3),6+L'BUYCOS2$  LENGTH                                       
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   3(2,R3),=AL2(D#COS2$$)                                           
         CLI   PORC$TYP,PORC$NEQ   COS2 $ ENTERED AS NET?                       
         BE    R_C2$30                                                          
         EDITR PORCOS,BUYCOS2$,2,ALIGN=LEFT,FLOAT=-,IZERO=Y                     
         J     R_C2$40                                                          
*                                                                               
R_C2$30  MVC   BUYCOS2$(L'PORC$TYP),PORC$TYP                                    
         ZAP   DUB,PORCOS                                                       
         CVB   R1,DUB                                                           
         DROP  R5                                                               
*                                                                               
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         ZAP   DUB,(PBDACP-PBDELEM)(L'PBDACP,R5)                                
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDITR (R1),(10,BUYCOS2$+1),2,ALIGN=LEFT,FLOAT=-,IZERO=Y                
*                                                                               
R_C2$40  MVC   6(L'BUYCOS2$,R3),BUYCOS2$                                        
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+L'BUYCOS2$)   RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         BRAS  RE,DRFT_REC                                                      
         GOTOR VGETINS,DMCB,0(R5),(C'O',PVALUES),7(R5),0,0,0                    
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY COS2 FACTOR GROSS AMOUNT               
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#C2FGRS)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,GROSS                                                      
         STCM  RE,15,6(R3)         COS2 FACTOR GROSS AMOUNT                     
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+4)            RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY COS2 FACTOR NET AMOUNT                 
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#C2FNET)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,GROSS                                                      
         S     RE,AGYCOM                                                        
         STCM  RE,15,6(R3)         COS2 FACTOR NET AMOUNT                       
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+4)            RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
R_C2$_X  J     XIT_R3R4                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYPLCOS NTR1  BASE=*,LABEL=*      REPLY PLANNED COST                           
*                                                                               
         CLC   PCVERSN#,=AL1(03,04,00,31)                                       
         BL    R_PLC_X                                                          
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         MVI   ELCODE,BYPCIDQ                                                   
         BRAS  RE,NXTELEM                                                       
         BNE   R_PLC_X                                                          
         USING BYPCELD,R5                                                       
         CLI   BYPCIND,C'X'        SPECIAL ELEM DELETION CODE?                  
         BE    R_PLC_X                                                          
         DROP  R5                                                               
*                                                                               
         BRAS  RE,DRFT_REC                                                      
         GOTOR FMT_COST,DMCB,(2,0(R5))                                          
         CLC   WRKELEM+(FC_OCOST-F_COST_D)(L'FC_OCOST),SPACES                   
         BNH   R_PLC30                                                          
         LA    RF,WRKELEM+(FC_OCOST-F_COST_D)+(L'FC_OCOST-1)                    
         BRAS  RE,LAST_CHR                                                      
         LA    RE,WRKELEM+(FC_OCOST-F_COST_D)                                   
         SR    RF,RE                                                            
         AHI   RF,1+6                                                           
         MVI   0(R3),LQ_RAWDQ      REPLY RATE CODE                              
         MVI   1(R3),0                                                          
         STCM  RF,3,1(R3)          LENGTH                                       
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   3(2,R3),=AL2(D#PLCOST)                                           
         SHI   RF,6+1                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKELEM+(FC_OCOST-F_COST_D)                              
         BRAS  RE,NXTWFELM                                                      
         AHI   RF,6+1                                                           
         SR    R4,RF               RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
R_PLC30  GOTOR VGETINS,DMCB,0(R5),(C'P',PVALUES),7(R5),0,0,0                    
         CLI   DMCB+8,C'X'                                                      
         BE    R_PLC_X                                                          
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY COS2 FACTOR GROSS AMOUNT               
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#PC_GRO)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,GROSS                                                      
         STCM  RE,15,6(R3)         COS2 FACTOR GROSS AMOUNT                     
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+4)            RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY COS2 FACTOR NET AMOUNT                 
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#PC_NET)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,GROSS                                                      
         S     RE,AGYCOM                                                        
         STCM  RE,15,6(R3)         COS2 FACTOR NET AMOUNT                       
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+4)            RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
R_PLC_X  J     XIT_R3R4                                                         
*                                                                               
LAST_CHR CLI   0(RF),0                                                          
         JE    *+8                                                              
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-18                                                             
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYCONUN NTR1  BASE=*,LABEL=*      REPLY CONTRACT UNIT VALUE                    
*                                                                               
         MVC   WKSVAREC,AREC       SAVE ORIGINAL AIO AND KEY                    
         MVC   WKSV_KEY,KEY                                                     
*                                                                               
         XC    WRKFULL2,WRKFULL2   REPLY BINARY CONTRACT UNIT                   
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         USING PBDELEM,R5                                                       
         OC    PBDCU,PBDCU         ANYTHING IN CONTRACT UNIT?                   
         BZ    R_CUV04                                                          
         CLC   PBDCU,=X'000001'    CONTRACT UNIT IS ZERO?                       
         BE    R_CUV50                                                          
         MVC   WRKFULL2(L'PBDCU),PBDCU                                          
         B     R_CUV50                                                          
         DROP  R5                                                               
*                                                                               
R_CUV04  OC    SVCON(L'SVCON-1),SVCON                                           
         BZ    R_CUV_X             NO CONTRACT IS OPEN, NO CU LOOK UP           
         BRAS  RE,DRFT_REC                                                      
         USING PBUYREC,R5                                                       
         XC    KEY,KEY                                                          
         MVC   AREC,ACONIO         READ INTO CONIO                              
         L     RF,ACONIO                                                        
         XC    0(256,RF),0(RF)                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(2,DUB)                                
*                                                                               
         SR    R6,R6               COUNTER                                      
         LA    R7,SVCON                                                         
R_CUV08  CHI   R6,24               MAXIMUM NUMBER OF ENTRIES REACHED?           
         BNL   R_CUV40                                                          
         OC    4(4,R7),4(R7)       DISK ADDRESS PRESENT?                        
         BZ    R_CUV30                                                          
         TM    0(R7),X'F0'         CONTRACT IS LOCKED?                          
         BO    DSPCU10                                                          
*                                                                               
         CLC   0(2,R7),DUB         CONTRACT START DT > INSERTION DT?            
         BH    R_CUV30                                                          
         CLC   2(2,R7),DUB         CONTRACT END DT < INSERTION DT?              
         BL    R_CUV30                                                          
*                                                                               
DSPCU10  TM    SVAORC,X'08'        CONTRACT RATE LOOK-UP                        
         BZ    R_CUV15                                                          
         CLC   SVAOR,AGYALPHA      AOR?                                         
         BE    R_CUV15                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVAORSE                                                  
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0             ADVERTISER FILE NOT ACTIVE?                  
         BNE   R_CUV_X                                                          
*                                                                               
R_CUV15  MVC   KEY+27(4),4(R7)                                                  
         BRAS  RE,PRT_GETR                                                      
*                                                                               
         TM    SVAORC,X'08'        CONTRACT RATE LOOK-UP                        
         BZ    R_CUV20                                                          
         CLC   SVAOR,AGYALPHA      AOR?                                         
         BE    R_CUV20                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH BACK                  
*                                                                               
R_CUV20  L     RF,ACONIO                                                        
         USING PCONRECD,RF                                                      
         CLC   PCONSDT,PBUYKDAT    CONTRACT START DT > INSERTION DT?            
         BH    R_CUV30                                                          
         CLC   PCONEDT,PBUYKDAT    CONTRACT END DT < INSERTION DT?              
         BL    R_CUV30                                                          
         B     R_CUV40             RIGHT CONTRACT FOUND                         
         DROP  RF                                                               
*                                                                               
R_CUV30  LA    R7,8(R7)                                                         
         AHI   R6,1                ONE ENTRY IS SEARCHED, MAX IS 24             
         B     R_CUV08                                                          
*                                                                               
R_CUV40  LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
         L     RF,ACONIO                                                        
         OC    0(256,RF),0(RF)     CONTRACT RECORD PRESENT?                     
         BZ    R_CUV_X                                                          
*                                                                               
         XC    WRKFULL2,WRKFULL2                                                
         L     RF,ACONIO                                                        
         GOTO1 =V(PPGETCU),DMCB,PBUYREC,(RF),VDATAMGR,RR=RELO17                 
         CLI   DMCB,X'FF'                                                       
         BE    *+10                                                             
         MVC   WRKFULL2(3),DMCB+5                                               
         OC    WRKFULL2(3),WRKFULL2                                             
         BZ    R_CUV_X                                                          
         CLC   WRKFULL2(3),=X'000001'                                           
         BNE   *+10                                                             
         XC    WRKFULL2,WRKFULL2                                                
*                                                                               
R_CUV50  MVI   0(R3),LQ_RAWDQ      REPLY CONTRACT UNIT                          
         MVI   1(R3),0                                                          
         MVI   2(R3),6+3           LENGTH                                       
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         MVC   3(2,R3),=AL2(D#CONUVL)                                           
         MVC   6(3,R3),WRKFULL2                                                 
         BRAS  RE,NXTWFELM                                                      
         SHI   R4,(6+3)            RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
R_CUV_X  MVC   AREC,WKSVAREC       RESTORE ORIGINAL AIO AND KEY                 
         MVC   KEY,WKSV_KEY                                                     
         J     XIT_R3R4                                                         
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYFXVAL NTR1  BASE=*,LABEL=*      REPLY FX VALUES                              
*                                                                               
         CLC   PCVERSN#,=AL1(03,05,00,41)                                       
         BL    R_FXV_X                                                          
*                                                                               
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         MVI   ELCODE,BYCCIDQ                                                   
R_FXV20  BRAS  RE,NXTELEM                                                       
         BNE   R_FXV_X                                                          
         USING BYCCELM,R5                                                       
         CLC   =AL2(FXRATEQ),BYCCSQN                                            
         BNE   R_FXV20                                                          
         MVC   WRKTEMPS,SPACES                                                  
         EDITR (P8,BYCCDATA),(15,WRKTEMPS),5,FLOAT=-,ALIGN=LEFT,IZERO=Y         
         MVI   0(R3),LQ_RAWDQ      REPLY FX RATE                                
         MVI   1(R3),0                                                          
         MVI   2(R3),6+15          LENGTH                                       
         MVC   3(2,R3),=AL2(D#FXRATE)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(15,R3),WRKTEMPS                                                
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
         DROP  R5                                                               
*                                                                               
         BRAS  RE,DRFT_REC                                                      
         GOTOR VGETINS,DMCB,0(R5),PVALUES,7(R5),(C'A',0),0,=C'FX'               
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY FX AGENCY COMMISSION AMT               
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#FXACAM)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),AGYCOM                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY FX CASH DISCOUNT AMT                   
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#FXCDAM)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),CSHDSC                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         L     RE,GROSS                                                         
         S     RE,AGYCOM                                                        
         ST    RE,WRKFULL1                                                      
         MVI   0(R3),LQ_RAWDQ      REPLY FX NET ORDERED                         
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#FXNETO)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),WRKFULL1                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY FX GROSS ORDERED                       
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#FXGRSO)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),GROSS                                                    
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY FX NET PAYABLE                         
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#FXNPYB)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,PYABLE                                                     
         ICM   RF,15,CSHDSC                                                     
         AR    RE,RF                                                            
         ST    RE,FULL                                                          
         ICM   RE,15,PGROSS                                                     
         ICM   RF,15,PAGYCOM                                                    
         SR    RE,RF                                                            
         L     RF,FULL                                                          
         SR    RF,RE                                                            
         STCM  RF,15,6(R3)         PYABLE+CSHDSC-SUM(PPGROSS-PPAGYCOM)          
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY FX GROSS PAYABLE                       
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#FXGPYB)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,GROSS                                                      
         ICM   RF,15,PGROSS                                                     
         SR    RE,RF                                                            
         STCM  RE,15,6(R3)         GROSS-SUM(PPGROSS)                           
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY FX NET LESS CD ORDERED                 
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#FXNLCO)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),PYABLE                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY GROSS LESS CD ORDERED                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#FXGLCO)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),BLABLE                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         GOTOR VGETINS,DMCB,0(R5),PVALUES,7(R5),(0,0),0,0                       
*                                                                               
         L     RE,GROSS                                                         
         S     RE,AGYCOM                                                        
         ST    RE,WRKFULL1                                                      
         MVI   0(R3),LQ_RAWDQ      REPLY NET ORDERED + FX NET ORDERED           
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#S_NETO)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),WRKFULL1                                                 
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY GROSS ORD'D + FX GROSS ORD'D           
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#S_GRSO)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),GROSS                                                    
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY NET PAYABLE + FX NP                    
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#S_NPYB)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,PYABLE                                                     
         ICM   RF,15,CSHDSC                                                     
         AR    RE,RF                                                            
         ST    RE,FULL                                                          
         ICM   RE,15,PGROSS                                                     
         ICM   RF,15,PAGYCOM                                                    
         SR    RE,RF                                                            
         L     RF,FULL                                                          
         SR    RF,RE                                                            
         STCM  RF,15,6(R3)         PYABLE+CSHDSC-SUM(PPGROSS-PPAGYCOM)          
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY GROSS PAYABLE + FX GP                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#S_GPYB)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         ICM   RE,15,GROSS                                                      
         ICM   RF,15,PGROSS                                                     
         SR    RE,RF                                                            
         STCM  RE,15,6(R3)         GROSS-SUM(PPGROSS)                           
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY NLCD ORDERED + FX NLCD ORDERED         
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#S_NLCO)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),PYABLE                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY GLCD ORDERED + FX GLCD ORDERED         
         MVI   1(R3),0                                                          
         MVI   2(R3),6+4           LENGTH                                       
         MVC   3(2,R3),=AL2(D#S_GLCO)                                           
         MVI   5(R3),LD_CBINQ      DATA TYPE - BINARY CASH AMOUNT               
         MVC   6(4,R3),BLABLE                                                   
         BRAS  RE,NXTWFELM                                                      
         AHI   R4,-6-4             RECALCULATE RETURNED DATA HDR EL LEN         
*                                                                               
R_FXV_X  J     XIT_R3R4                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BUILD REPLY ERROR ELEMS OR JUST INSERTION KEY                                 
*                                                                               
* R2    - FIELD WHERE ERROR OCCURED (NULL IF FLD IS NOT DEFINED)                
* R3    - WORKER ELEM                                                           
* BYTE2 - Y = ERROR OCCURED, N = NO ERROR                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDRPLY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BYTE2,C'Y'          ERROR FOUND?                                 
         BNE   BDE15                                                            
         OC    BUYMSG,BUYMSG       ANY MSG FOUND IN HEADER?                     
         BZ    BLDREPX             NO, DONE (ERRORS ALREADY LOGGGED)            
*                                                                               
BDE15    XC    WRKTEMPS,WRKTEMPS                                                
         XC    WRKSAVKY,WRKSAVKY                                                
         MVI   WRKBYTE1,0                                                       
*                                                                               
BDE20    CLI   0(R3),0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    BDE25                                                            
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BE    BDE25                                                            
*                                                                               
         CLI   0(R3),LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   BDE25                                                            
         CLC   3(2,R3),=AL2(D#INSKEY)                                           
         BNE   BDE25                                                            
         ZICM  RE,1(R3),(3)                                                     
         CHI   RE,255                                                           
         BNH   *+6                                                              
         DC    H'0'                INVALID INSERTION KEY (>255)                 
         CHI   RE,6                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID INSERTION KEY (<0)                   
         STC   RE,WRKTEMPS                                                      
         AHI   RE,-6-1             6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WRKTEMPS+1(0),6(R3)                                              
*                                                                               
BDE25    CLI   LKDRFTSW,C'F'       DRAFT CHANGE MODE?                           
         BNE   BDE28                                                            
         CLI   0(R3),LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   BDE28                                                            
         CLC   3(2,R3),=AL2(D#ADBKEY)                                           
         BNE   BDE28                                                            
         ZICM  RE,1(R3),(3)                                                     
         CHI   RE,6+20             "ADBUYER ONLY" KEY IS <20?                   
         BNH   *+6                                                              
         DC    H'0'                INVALID "ADBUYER ONLY" KEY (>20)             
         CHI   RE,6                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID "ADBUYER ONLY" KEY (<0)              
         STC   RE,WRKSAVKY                                                      
         AHI   RE,-6-1             6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WRKSAVKY+1(0),6(R3)                                              
*                                                                               
BDE28    CLI   0(R3),LQ_DLDDQ      DOWLOAD DATA ELEM?                           
         BNE   *+8                                                              
         OI    WRKBYTE1,DLDATABD   DOWNLOAD DATA ALREADY CONSTRUCTED            
         CLI   0(R3),LQ_RAWDQ      REPLY DATA ELEM?                             
         BNE   BDE30                                                            
         CLC   3(2,R3),=AL2(D#INSKEY)                                           
         BNE   BDE30                                                            
         OI    WRKBYTE1,INSKEYBD   REPLY INS. KEY ALREADY CONSTRUCTED           
BDE30    CLI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    BDE35                                                            
         BRAS  RE,NXTWFELM                                                      
         B     BDE20                                                            
*                                                                               
BDE35    MVC   WRKHALF1,1(R3)      SAVE RETURNED DATA HDR ELEM LENGTH           
         LH    RE,WRKHALF1                                                      
         CHI   RE,100              ENOUGH ROOM?                                 
         BNL   *+6                                                              
         DC    H'0'                RETURN RECORD IS TOO SMALL                   
*                                                                               
         TM    WRKBYTE1,DLDATABD   DOWNLOAD DATA ELEM BUILD?                    
         BO    BDE40               YES, NO NEED TO BUILD ANOTHER                
*                                                                               
         MVI   0(R3),LQ_DLDDQ      DOWNLOAD DATA ELEM CODE                      
         MVI   1(R3),0                                                          
         MVI   2(R3),5             2ND BYTE OF ELEM LENGTH                      
*                                                                               
         CLI   DDLINKSW,C'K'       IDESK INSERTION UPLOAD?                      
         BE    *+12                                                             
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    *+14                                                             
         MVC   3(2,R3),=AL2(E#IDKRPY)                                           
         B     BDE38                                                            
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INS?                                   
         BNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSDER)                                           
         CLI   DDLINKSW,C'N'       NEW INS?                                     
         BNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSADD)                                           
         CLI   DDLINKSW,C'C'       CHANGE INS?                                  
         BNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSCHA)                                           
         CLI   DDLINKSW,C'D'       DELETE INS?                                  
         BNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSDEL)                                           
*                                                                               
BDE38    LH    RE,WRKHALF1                                                      
         AHI   RE,-5               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RE,WRKHALF1                                                      
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDE40    CLI   LKDRFTSW,C'F'       DRAFT CHANGE MODE?                           
         BNE   BDE42                                                            
         MVI   0(R3),LQ_RAWDQ      REPLY DRAFT CHANGE ACTION                    
         MVI   1(R3),0                                                          
         MVI   2(R3),7             LENGTH                                       
         MVC   3(2,R3),=AL2(D#DACTN)                                            
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVI   6(R3),C'D'                                                       
         BRAS  RE,NXTWFELM                                                      
         LH    RF,WRKHALF1                                                      
         AHI   RF,-7               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,WRKHALF1                                                      
*                                                                               
         OC    WRKSAVKY,WRKSAVKY   NEED TO REPLY "ADBUYER ONLY" KEY?            
         BZ    BDE42                                                            
         MVI   0(R3),LQ_RAWDQ      REPLY "ADBUYER ONLY" KEY                     
         ZIC   RE,WRKSAVKY                                                      
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF1                                                      
         SR    RF,RE               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,WRKHALF1                                                      
         MVC   3(2,R3),=AL2(D#ADBKEY)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         AHI   RE,-6-1             6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKSAVKY+1  INSERTION KEY                                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
* NEED TO REPLY INS. KEY FOR ALL UPLOAD MODES EXCEPT DRAFT INSERTION            
*                                                                               
BDE42    TM    WRKBYTE1,INSKEYBD   REPLY INS. KEY ELEM BUILD?                   
         BO    BDE50               YES, NO NEED TO BUILD ANOTHER                
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    BDE50                                                            
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BNE   BDE45                                                            
         CLI   BYTE2,C'Y'          ERROR FOUND?                                 
         BE    BDE52               YES, CANNOT BUILD KEY IF ERROR FOUND         
         CLI   LKDRFTSW,C'F'       DRAFT CHANGE MODE?                           
         BE    BDE90               "ADBYER ONLY" KEY IS REPLIED INSTEAD         
*                                                                               
         CLI   REC+3,X'20'         BUY RECORD?                                  
         BE    *+6                                                              
         DC    H'0'                NOT A BUY RECORD, VERY WRONG!                
         XC    WRKTEMPS,WRKTEMPS                                                
         MVI   WRKTEMPS,6+13       LENGTH OF REPLY INS KEY ELEM                 
         MVC   WRKTEMPS+1(1),REC+2 MEDIA CODE                                   
         MVC   WRKTEMPS+2(3),REC+4 CLIENT CODE                                  
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'99'                                                     
         BRAS  RE,NXTELEM          SERIAL NUMBER ELEM FOUND?                    
         BE    *+6                                                              
         DC    H'0'                IT HAS TO BE THERE!                          
         EDIT  (P5,2(R5)),(9,WRKTEMPS+5),0,ALIGN=RIGHT,                +        
               ZERO=NOBLANK,FILL=0                                              
*                                                                               
BDE45    MVI   0(R3),LQ_RAWDQ      REPLY INSERTION KEY                          
         ZIC   RE,WRKTEMPS                                                      
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF1                                                      
         SR    RF,RE               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,WRKHALF1                                                      
         MVC   3(2,R3),=AL2(D#INSKEY)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         AHI   RE,-7               6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKTEMPS+1  INSERTION KEY                                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    BDE48                                                            
         CLC   SVIDSKV#,=C'0001'                                                
         BL    BDE47                                                            
         OC    REC+16(L'PBUYKDAT),REC+16                                        
         BZ    BDE47                                                            
         MVI   0(R3),LQ_RAWDQ      REPLY INSERTION DATE                         
         MVI   1(R3),0                                                          
         MVI   2(R3),6+3           LENGTH                                       
         MVC   3(2,R3),=AL2(D#INSDAT)                                           
         MVI   5(R3),LD_BDATQ      DATA TYPE - BINARY DATE                      
         MVC   6(3,R3),REC+16                                                   
         BRAS  RE,NXTWFELM                                                      
*                                                                               
         CLI   REC+24,0            HAVE LINE NUMBER?                            
         BE    BDE46M                                                           
         MVI   0(R3),LQ_RAWDQ      REPLY LINE NUMBER                            
         MVI   1(R3),0                                                          
         MVI   2(R3),6+1           LENGTH                                       
         MVC   3(2,R3),=AL2(D#REFNUM)                                           
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         MVC   6(1,R3),REC+24                                                   
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDE46M   LA    R5,REC+33                                                        
         USING PBDELEM,R5                                                       
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+12                                                             
         MVI   WRKBYTE2,C'M'                                                    
         B     BDE46W                                                           
         MVC   WRKBYTE2,PBDBFD                                                  
         CLI   WRKBYTE2,C'W'       WEEK OF?                                     
         BE    BDE46W                                                           
         CLI   WRKBYTE2,C'B'       BEST FOOD DAY?                               
         BE    BDE46W                                                           
         MVI   WRKBYTE2,C'D'       DEFAULT TO DAILY                             
BDE46W   MVI   0(R3),LQ_RAWDQ      REPLY FREQUENCY                              
         MVI   1(R3),0                                                          
         MVI   2(R3),6+1           LENGTH   FREQUENCY                           
         MVC   3(2,R3),=AL2(D#INSFRQ)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(1,R3),WRKBYTE2                                                 
         BRAS  RE,NXTWFELM                                                      
         DROP  R5                                                               
*                                                                               
BDE47    CLI   REC+14,0            HAVE ZONE?                                   
         JE    BDE48                                                            
         MVI   0(R3),LQ_RAWDQ      REPLY LINE NUMBER                            
         MVI   1(R3),0                                                          
         MVI   2(R3),6+1           LENGTH                                       
         MVC   3(2,R3),=AL2(D#PUBZON)                                           
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         MVC   6(1,R3),REC+14                                                   
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDE48    TM    ABUPLDSW,ABMOVINQ   BUY MOVE?                                    
         BZ    BDE50                                                            
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BO    BDE50                                                            
         EDIT  SVBYS#_N,(9,WRKTEMPS+5),0,ALIGN=RIGHT,                  +        
               ZERO=NOBLANK,FILL=0                                              
         MVI   0(R3),LQ_RAWDQ      REPLY "FROM" INSERTION'S KEY                 
         ZIC   RE,WRKTEMPS                                                      
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF1                                                      
         SR    RF,RE               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,WRKHALF1                                                      
         MVC   3(2,R3),=AL2(D#INSKEY)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         AHI   RE,-7               6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKTEMPS+1  INSERTION KEY                                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDE50    CLI   BYTE2,C'Y'          ERROR NEED TO BE RECORDED?                   
         BE    BDE52                                                            
*                                                                               
         TM    ABUPLDSW,ABMOVINQ   BUY MOVE?                                    
         BZ    BDE50F                                                           
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    BDE50F                                                           
         LA    R6,7                INS DATE CHG INITIATED DEL/ADD UPDT          
         MVC   WRKHALF2,=AL2(D#INSDAT)                                          
         BRAS  RE,BDRPLWRN         REPLY WARNING MSG                            
         B     BDE50P                                                           
*                                                                               
BDE50F   TM    WARN,X'80'                                                       
         BZ    BDE50H                                                           
         LA    R6,1                SPACE DESC WARNING                           
         MVC   WRKHALF2,=AL2(D#SPCDSC)                                          
         BRAS  RE,BDRPLWRN         REPLY WARNING MSG                            
*                                                                               
BDE50H   TM    WARN,X'40'                                                       
         BZ    BDE50J                                                           
         LA    R6,2                PRD ALLOC WARNING                            
         MVC   WRKHALF2,=AL2(D#ALLOCS)                                          
         BRAS  RE,BDRPLWRN         REPLY WARNING MSG                            
*                                                                               
BDE50J   TM    WARN,X'20'                                                       
         BZ    BDE50L                                                           
         LA    R6,3                INSERTION DELETED WARNING                    
         MVC   WRKHALF2,=AL2(D#INSDAT)                                          
         BRAS  RE,BDRPLWRN         REPLY WARNING MSG                            
*                                                                               
BDE50L   TM    WARN,X'10'                                                       
         BZ    BDE50M                                                           
         LA    R6,6                EXCLUSION CLASS WARNING (ADDED)              
         CLI   BUYTR1,C'B'         BUYING INSERTION?                            
         BNE   *+8                                                              
         LA    R6,5                EXCLUSION CLASS WARNING (CHANGED)            
         MVC   WRKHALF2,=AL2(D#PRDCOD)                                          
         BRAS  RE,BDRPLWRN         REPLY WARNING MSG                            
*                                                                               
BDE50M   TM    PRSMAPSW,COS2MAPQ   COST 2 MAP CODE PRESENT?                     
         JZ    BDE50N                                                           
         TM    ABUPLDSW,IDSKUPLQ   PRISMA INSERTION UPLOAD?                     
         JZ    BDE50N                                                           
         TM    WARN,WX08C2RQ       WARNING - COS2 RATE ENABLED?                 
         JZ    BDE50N                                                           
         LA    R6,8                COST 2 NOT UPDATED FOR NON-COS2 CLT          
         MVC   WRKHALF2,=AL2(D#CLTCOD)                                          
         BRAS  RE,BDRPLWRN         REPLY WARNING MSG                            
*                                                                               
BDE50N   CLI   BUYMD,C'N'          NEWSPAPER?                                   
         BNE   BDE50P                                                           
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         USING PBDELEM,R5                                                       
         CLC   PBDSPACE(2),=X'7B00'                                             
         BE    BDE50P                                                           
         CLC   PBDSPACE(2),=X'7B40'                                             
         BE    BDE50P                                                           
         CLC   PBDSPACE(2),=X'5C40'                                             
         BNH   BDE50P                                                           
         CP    PBDUNITS,=P'0'                                                   
         BNE   BDE50P                                                           
         DROP  R5                                                               
         LA    R6,4                CONTRACT LINEAGE EQUIVALENCY WARNING         
         MVC   WRKHALF2,=AL2(D#CONLIE)                                          
         BRAS  RE,BDRPLWRN         REPLY WARNING MSG                            
*                                                                               
BDE50P   DS    0H                  FOR FUTURE WARNING MSGS                      
         B     BDE90                                                            
*                                                                               
BDE52    CLI   PCVERSN#,X'04'      HIGHER THAN 4.X.X.X?                         
         JL    BDE52D                                                           
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         JNE   BDE52D                                                           
         CLC   REQTOKEN,SPACES     HAVE REQUEST TOKEN?                          
         JNH   BDE52D                                                           
         MVI   0(R3),LQ_RAWDQ      REPLY TOKEN                                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+L'REQTOKEN  LENGTH                                       
         MVC   3(2,R3),=AL2(D#QTOKEN)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(L'REQTOKEN,R3),REQTOKEN                                        
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDE52D   OC    WRKMAPCD,WRKMAPCD   SAVED MAP CODE IN ERROR PRESENT?             
         BZ    *+14                                                             
         MVC   WRKHALF2,WRKMAPCD   USE SAVED MAP CODE FOR ERROR REPLY           
         B     BDE60                                                            
         XC    WRKHALF2,WRKHALF2                                                
         BRAS  RE,BDGETMC          GET MAP CODE                                 
         OC    WRKHALF2,WRKHALF2   CANNOT FIND FLD NUMBER IN ERROR?             
         BZ    *+8                                                              
BDE60    BRAS  RE,BDRPLFLD         REPLY ERROR FIELD NUMBER                     
         BRAS  RE,BDRPLMSG         REPLY ERROR MESSAGE (TEXT)                   
*                                                                               
BDE90    MVI   0(R3),LQ_RDATQ      REBUILD RETURNED DATA HEADER ELEM            
         MVC   1(2,R3),WRKHALF1                                                 
*                                                                               
BLDREPX  J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BDGETMC  ST    RE,WRKSAVRE                                                      
         LA    RF,BUYMDH                                                        
         CR    R2,RF               INVALID FLD IS MEDIA?                        
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#MEDCOD)                                          
         B     BDGETMCX                                                         
         LA    RF,BUYNMH                                                        
         CR    R2,RF               INVALID FLD IS BUYER?                        
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#BUYERC)                                          
         B     BDGETMCX                                                         
         LA    RF,BUYCLH                                                        
         CR    R2,RF               INVALID FLD IS CLIENT?                       
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#CLTCOD)                                          
         B     BDGETMCX                                                         
         LA    RF,BUYPRH                                                        
         CR    R2,RF               INVALID FLD IS PRODUCT?                      
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#PRDCOD)                                          
         B     BDGETMCX                                                         
         LA    RF,BUYESH                                                        
         CR    R2,RF               INVALID FLD IS ESTIMATE?                     
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#ESTNUM)                                          
         B     BDGETMCX                                                         
         LA    RF,BUYPBH                                                        
         CR    R2,RF               INVALID FLD IS PUBLICATION?                  
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#PUBCOD)                                          
         B     BDGETMCX                                                         
         LA    RF,BUYDT1H                                                       
         CR    R2,RF               INVALID FLD IS INSERTION DATE?               
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#INSDAT)                                          
         B     BDGETMCX                                                         
*                                                                               
         LA    R4,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,2                                                             
         BRAS  RE,BDBMPFDS         POINT TO ADCODE FIELD                        
         CR    R2,R4               INVALID FLD IS ADCODE?                       
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#ADCODE)                                          
         B     BDGETMCX                                                         
*                                                                               
         BRAS  RE,BDBMPFD          PASS ADCODE FLD                              
*                                                                               
         CLI   BUYMD,C'I'          INTERACTIVE?                                 
         JE    BDGETM20                                                         
         CLI   BUYMD,C'L'          SOCIAL?                                      
         JE    BDGETM20                                                         
         CLI   BUYMD,C'B'          MOBILE?                                      
         JE    BDGETM20                                                         
         CLI   BUYMD,C'D'          DIGITAL AUDIO?                               
         JE    BDGETM20                                                         
         CLI   BUYMD,C'V'          NVIDEO? (NATIONAL VIDEO)                     
         JE    BDGETM20                                                         
         CLI   BUYMD,C'W'          LVIDEO? (LOCAL VIDEO)                        
         JE    BDGETM20                                                         
         J     BDGETM30                                                         
*                                                                               
BDGETM20 CR    R2,R4               INVALID FLD IS SPACE?                        
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#SPCDSC)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS RATE?                         
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#UNTRAT)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS SPC CLOSING DATE?             
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#SPCDAT)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS MATRL CLOSING DATE?           
         BNE   *+10                                                             
         MVC   WRKHALF2,=AL2(D#MCLDAT)                                          
         B     BDGETMCX                                                         
*                                                                               
BDGETM30 CLI   BUYMD,C'N'          NEWSPAPER?                                   
         BNE   BDGETM50                                                         
         CR    R2,R4               INVALID FLD IS SPACE?                        
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#SPCDSC)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS RATE?                         
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#UNTRAT)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS PREMIUM?                      
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#PREMUM)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS MATRL CLOSING DATE?           
         BNE   *+10                                                             
         MVC   WRKHALF2,=AL2(D#MCLDAT)                                          
         B     BDGETMCX                                                         
*                                                                               
BDGETM50 CLI   BUYMD,C'O'          OUTDOOR?                                     
         BNE   BDGETM70                                                         
         CR    R2,R4               INVALID FLD IS SHOWING?                      
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#SHOWGS)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS REGULAR DISPLAY?              
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#REGDSP)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS ILLUMINATED PANELS?           
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#ILLPAN)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS RATE?                         
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#UNTRAT)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS SPC CLOSING DATE?             
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#SPCDAT)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS MATRL CLOSING DATE?           
         BNE   *+10                                                             
         MVC   WRKHALF2,=AL2(D#MCLDAT)                                          
         B     BDGETMCX                                                         
*                                                                               
BDGETM70 DS    0H                  MUST BE MAGAZINE/SUPPLEMENT/TRADE            
         CR    R2,R4               INVALID FLD IS SPACE?                        
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#SPCDSC)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS RATE?                         
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#UNTRAT)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS SPC CLOSING DATE?             
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#SPCDAT)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS ON-SALE DATE?                 
         BNE   *+14                                                             
         MVC   WRKHALF2,=AL2(D#ONSDAT)                                          
         B     BDGETMCX                                                         
         BRAS  RE,BDBMPFD                                                       
         CR    R2,R4               INVALID FLD IS MATRL CLOSING DATE?           
         BNE   *+10                                                             
         MVC   WRKHALF2,=AL2(D#MCLDAT)                                          
*                                                                               
BDGETMCX J     X_WKSVRE                                                         
*                                                                               
BDBMPFD  ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         BR    RE                                                               
*                                                                               
BDBMPFDS ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   RF,BDBMPFDS                                                      
         BR    RE                                                               
*                                                                               
BDRPLWRN ST    RE,WRKTOPRE                                                      
         TM    ABUPLDSW,ABRVINVQ   REVISE INVOICE UPLOAD?                       
         BNZR  RE                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY WARNING MSG HEADER                     
         MVI   1(R3),0                                                          
         MVI   2(R3),7             LENGTH                                       
         MVC   3(2,R3),=AL2(D#MSGTYP)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVI   6(R3),C'W'          W=WARNING                                    
         BRAS  RE,NXTWFELM                                                      
         LH    RF,WRKHALF1                                                      
         AHI   RF,-7               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,WRKHALF1                                                      
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R6),0,(C'W',DMCB),0,0,0                            
         BRAS  RE,BDRPLFLD                                                      
         BRAS  RE,BDRPLMSG                                                      
         L     RE,WRKTOPRE                                                      
         BR    RE                                                               
*                                                                               
BDRPLFLD ST    RE,WRKSAVRE                                                      
         MVI   0(R3),LQ_RAWDQ      REPLY ERROR FIELD NUMBER                     
         MVI   1(R3),0                                                          
         MVI   2(R3),8             LENGTH                                       
         MVC   3(2,R3),=AL2(D#ERRNUM)                                           
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         MVC   6(2,R3),WRKHALF2    MAP CODE (WHERE ERROR OCCURED)               
         LH    RE,WRKHALF1                                                      
         AHI   RE,-8               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RE,WRKHALF1                                                      
         BRAS  RE,NXTWFELM                                                      
         J     X_WKSVRE                                                         
*                                                                               
BDRPLMSG ST    RE,WRKSAVRE                                                      
         MVI   0(R3),LQ_RAWDQ      REPLY ERROR MESSAGE (TEXT)                   
         MVC   3(2,R3),=AL2(D#ERRDSC)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         BRAS  RE,BDR_FMSG         FORMAT AND COUNT MSG CHARS                   
*                                                                               
         CLC   =C'EP/0001',6(R3)   MISSING INPUT FLD ERROR MSG?                 
         BNE   BDRPLM50                                                         
         CLC   =AL2(D#MCLDAT),WRKHALF2                                          
         BNE   BDRPLM40                                                         
         LHI   R0,MCDTMERR         MATERIAL CLOSING DATE MISSING                
BDRPLM30 BRAS  RE,GET_ETXT                                                      
         BRAS  RE,BDR_FMSG         FORMAT AND COUNT MSG CHARS                   
         B     BDRPLM50                                                         
*                                                                               
BDRPLM40 CLC   =AL2(D#SPCDAT),WRKHALF2                                          
         BNE   BDRPLM50                                                         
         LHI   R0,SCDTMERR         SPACE CLOSING DATE MISSING                   
         B     BDRPLM30                                                         
*                                                                               
BDRPLM50 LH    RE,WRKHALF1                                                      
         AHI   R4,6+1              6 FOR OVERHEAD AND 1 FOR EX                  
         STCM  R4,3,1(R3)          LENGTH                                       
         SR    RE,R4               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RE,WRKHALF1                                                      
         BRAS  RE,NXTWFELM         POINT R3 TO NEXT ELEM                        
         J     X_WKSVRE                                                         
*                                                                               
BDR_FMSG OC    BUYMSG,SPACES       MAKE SURE NO TRAILING NULLS                  
         LA    R4,L'BUYMSG                                                      
         LA    RF,BUYMSG+L'BUYMSG-1                                             
         CLI   0(RF),C' '                                                       
         BNE   *+12                                                             
         BCTR  R4,0                TRAILING SPACES ARE STRIPPED                 
         BCTR  RF,0                                                             
         B     *-12                                                             
         BCTR  R4,0                                                             
         CHI   R4,0                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID MSG LENGTH                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),BUYMSG                                                   
         BR    RE                                                               
*                                                                               
DLDATABD EQU   X'80'               DOWNLOAD DATA ELEM IS BUILT                  
INSKEYBD EQU   X'40'               REPLY KEY ELEM IS BUILT                      
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRBSCR  NTR1  BASE=*,LABEL=*      CLEAR ALL UNPROTECTED FLDS ON SCR            
*                                                                               
         LA    R2,BUYMDH           POINT TO FIRST FIELD ON SCREEN               
         SR    RF,RF                                                            
         CLI   0(R2),0             HAVE SCREEN?                                 
         JE    CLRBSX                                                           
*                                                                               
CLRBS10  TM    1(R2),X'20'                                                      
         BO    CLRBS50             SKIP IF PROTECTED FIELD                      
*                                                                               
         ICM   RF,1,0(R2)          TOTAL FIELD LENGTH                           
         BZ    CLRBSX              DONE IF SCREEN END REACHED                   
*                                                                               
         AHI   RF,-8               HEADER LENGTH                                
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BNO   *+8                                                              
         AHI   RF,-8               EXTENSION LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
*                                                                               
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
         NI    6(R2),X'BF'         UNSET CURSOR                                 
         FOUT  (R2)                FORCE RE-TRANSMISSION                        
*                                                                               
CLRBS50  IC    RF,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         LA    R2,0(RF,R2)                                                      
         B     CLRBS10                                                          
*                                                                               
CLRBSX   J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMT_COST NTR1  BASE=*,LABEL=*      FORMAT COST                                  
*                                                                               
F        USING F_COST_D,WRKELEM                                                 
         ICM   R5,15,0(R1)         POINT TO BUY RECORD                          
*                                                                               
         XC    F.F_COST_D(FC_BLKLQ),F.F_COST_D                                  
         MVC   F.FC_TMPWK,SPACES                                                
         MVC   F.FC_OCOST,SPACES                                                
         MVI   FMTRATSW,0          INIT FORMAT RATE SWITCH                      
*                                                                               
         LA    R5,33(R5)           POINT TO 1ST BUY ELEM                        
         MVC   F.FC_RTIND,PBDRLIND-PBDELEM(R5)                                  
         MVC   F.FC_ACPCT,PBDACP-PBDELEM(R5)                                    
*                                                                               
         CLI   0(R1),1             FORMATTING RATE?                             
         BNE   F_COS12                                                          
         ZAP   F.FC_COST_,PBDCOS-PBDELEM(L'PBDCOS,R5)                           
         MVC   F.FC_NETSW,PBDCTYP-PBDELEM(R5)                                   
         MVC   F.FC_COTYP,PBDCOSTY-PBDELEM(R5)                                  
         MVC   F.FC_COIND,PBDCOSIN-PBDELEM(R5)                                  
         B     F_COS30                                                          
*                                                                               
F_COS12  CLI   0(R1),2             FORMATTING PLANNED COST?                     
         BNE   F_COS20                                                          
         MVI   ELCODE,BYPCIDQ                                                   
         BRAS  RE,NXTELEM          PLANNED COST ELEM FOUND?                     
         BNE   F_COS_X                                                          
         USING BYPCELD,R5                                                       
         ZAP   F.FC_COST_,BYPCCST  COST                                         
         MVC   F.FC_NETSW,BYPCNIND NET COST SWITCH (ENTERED AS NET)             
         MVC   F.FC_COTYP,BYPCTYP  COST TYPE (U=UNIT COST)                      
         MVC   F.FC_COIND,BYPCIND  COST INDICATOR                               
         B     F_COS30                                                          
*                                                                               
F_COS20  DC    H'0'                NO OTHER COST AT THIS TIME                   
*                                                                               
F_COS30  ICM   RE,15,0(R1)         POINT TO BUY RECORD                          
         CLI   PBUYKMED-PBUYKEY(RE),C'N'                                        
         BNE   F_COS40                                                          
         ZAP   DUB,F.FC_COST_      FORMAT COST FOR NEWSPAPER                    
         CVB   R1,DUB                                                           
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         JE    *+12                                                             
         CLI   F.FC_COIND,C'R'     ROADSIDE (DISPLAY AS NET)?                   
         JNE   F_COS32                                                          
         ZAP   DUB,F.FC_ACPCT                                                   
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
F_COS32  CLI   F.FC_COTYP,C'U'     UNIT RATE?                                   
         BE    F_COS37                                                          
         C     R1,=F'99999999'     TOTAL RATE OVER 999,999.99?                  
         BNH   F_COS34                                                          
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'          HAVE ENTERED PENNIES WHEN BUYING             
         LTR   R1,R1               (NO ROOM)                                    
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
F_COS33  EDITR (R1),(9,F.FC_TMPWK+5),0,FLOAT=-,ALIGN=LEFT,IZERO=Y               
         B     F_COS38                                                          
*                                                                               
F_COS34  CHI   R1,0                NEGATIVE RATE?                               
         BNL   F_COS36                                                          
         C     R1,=F'-9999999'     TOTAL RATE LESS THAN -99,999.99?             
         BH    F_COS35                                                          
         OI    FMTRATSW,DIMEDRPQ                                                
         MHI   R1,-1               DROP PENNIES AND DIMES                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         MHI   R1,-1                                                            
         B     F_COS33                                                          
*                                                                               
F_COS35  C     R1,=F'-999999'      TOTAL RATE LESS THAN -9,999.99?              
         BNL   F_COS36                                                          
         OI    FMTRATSW,PENNDRPQ                                                
         MHI   R1,-1               DROP PENNIES, KEEP DIMES                     
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MHI   R1,-1                                                            
         EDITR (R1),(9,F.FC_TMPWK+5),1,FLOAT=-,ALIGN=LEFT,IZERO=Y               
         B     F_COS38                                                          
*                                                                               
F_COS36  EDITR (R1),(9,F.FC_TMPWK+5),2,FLOAT=-,ALIGN=LEFT,IZERO=Y               
         B     F_COS38                                                          
*                                                                               
F_COS37  EDITR (R1),(11,F.FC_TMPWK+5),5,FLOAT=-,ALIGN=LEFT,IZERO=Y              
*                                                                               
         LA    R1,F.FC_TMPWK+5     START OF OUTPUT                              
         AR    R1,R0               + LENGTH                                     
         SHI   R1,3                BACK UP TO LAST 3 BYTES                      
         CLC   =C'000',0(R1)                                                    
         BNE   *+10                                                             
         MVC   0(3,R1),SPACES      MOVE SOME BLANKS                             
*                                                                               
F_COS38  LA    R1,F.FC_TMPWK+5                                                  
         CLI   F.FC_COTYP,C'U'     UNIT RATE?                                   
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COTYP                                               
         CLI   F.FC_COIND,C' '     DEFAULT COST TYPE?                           
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COIND                                               
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_NETSW                                               
         TM    F.FC_RTIND,X'08'    FROZEN RATE?                                 
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
         MVC   F.FC_OCOST(11),0(R1)                                             
         CP    F.FC_COST_,=P'0'                                                 
         BNE   F_COS_X                                                          
         MVC   F.FC_OCOST,SPACES                                                
         MVC   F.FC_OCOST(4),=C'FREE'                                           
         CLI   F.FC_COIND,C'S'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'SFREE'                                          
         CLI   F.FC_COIND,C'R'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'RFREE'                                          
         B     F_COS_X                                                          
*                                                                               
F_COS40  ZAP   DUB,F.FC_COST_      FORMAT COST FOR NON-NEWSPAPER                
         CVB   R1,DUB                                                           
         CLI   F.FC_NETSW,C'N'     NET INPUT SO DISPLAY AS NET                  
         JE    *+12                                                             
         CLI   F.FC_COIND,C'R'     ROADSIDE (DISPLAY AS NET)?                   
         JNE   F_COS42                                                          
         ZAP   DUB,F.FC_ACPCT                                                   
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               =NET PCT                                     
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
F_COS42  EDITR (R1),(10,F.FC_TMPWK+2),2,ALIGN=LEFT,FLOAT=-,IZERO=Y              
         LA    R1,F.FC_TMPWK+2                                                  
         CLI   F.FC_COIND,C' '     DEFAULT COST TYPE?                           
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COIND                                               
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_NETSW                                               
         TM    F.FC_RTIND,X'08'    FROZEN RATE?                                 
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
         MVC   F.FC_OCOST(11),0(R1)                                             
         CP    F.FC_COST_,=P'0'                                                 
         BNE   F_COS_X                                                          
         MVC   F.FC_OCOST,SPACES                                                
         MVC   F.FC_OCOST(4),=C'FREE'                                           
         CLI   F.FC_COIND,C'S'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'SFREE'                                          
         CLI   F.FC_COIND,C'R'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'RFREE'                                          
*                                                                               
F_COS_X  J     EXIT                                                             
*                                                                               
         DROP  F                                                                
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FORMAT NEWSPAPER PREMIUMS                                                     
*                                                                               
* WRKTEMPS - RETURNS FORMATTED DATA                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTNPR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R5,R1               R1 POINTS TO FIRST BUY ELEM                  
         USING PBDELEM,R5                                                       
         MVC   WRKTEMPS(25),SPACES                                              
         LA    R2,WRKTEMPS                                                      
         CLI   PBDCL,0                                                          
         BE    FMTNPR02                                                         
         MVC   0(1,R2),PBDCL                                                    
         MVI   1(R2),C'C'                                                       
         MVI   2(R2),C'/'                                                       
         LA    R2,3(R2)                                                         
         B     FMTNPR04                                                         
*                                                                               
FMTNPR02 CP    PBDPRCOS,=P'0'                                                   
         BE    FMTNPRX                                                          
*                                                                               
FMTNPR04 ZAP   DUB,PBDPRCOS                                                     
         CVB   R1,DUB              R1=PREMIUM COST                              
         CLI   PBDPCTYP,C'N'       NET INPUT SO DISPLAY AS NET                  
         BNE   FMTNPR06                                                         
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
FMTNPR06 CLI   PBDPRIN,C' '        TEST DEFAULT IND                             
         BNH   *+14                                                             
         MVC   0(1,R2),PBDPRIN                                                  
         AHI   R2,1                                                             
         CLI   PBDPCTYP,C' '       TEST PREMIUM COST TYPE                       
         BNH   *+14                                                             
         MVC   0(1,R2),PBDPCTYP                                                 
         AHI   R2,1                                                             
         CLI   PBDCL,0             CHECK FOR COLOR                              
         BE    FMTNPR20                                                         
*                                                                               
         CLI   PBDPRIN,C' '        CHECK FOR PR RATE IND                        
         BH    FMTNPR08            IF SPACE I CAN DISPLAY 8 DIGITS              
         CLI   PBDPCTYP,C' '       CHECK FOR PR COST TYPE                       
         BNH   FMTNPR14            IF SPACE I CAN DISPLAY 8 DIGITS              
*                                                                               
FMTNPR08 C     R1,=F'-99999'       CHECK FOR NEGATIVE > -999.99                 
         BL    FMTNPR10                                                         
         C     R1,=F'1000000'      SEE IF OVER 10,000.00                        
         BL    FMTNPR12                                                         
*                                                                               
FMTNPR10 CVD   R1,DUB              SEE IF PENNY CAN BE DROPPED                  
         C     R1,=F'-10000000'    CK FOR -100,000.00                           
         BNH   F_NPR10H                                                         
         C     R1,=F'10000000'     CK FOR 100,000.00                            
         BL    F_NPR10G                                                         
*                                                                               
F_NPR10H DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(8-2)        NO DECIMAL                                   
         CVB   R1,DUB                                                           
         EDITR (R1),(7,0(R2)),0,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTNPRX                                                          
*                                                                               
F_NPR10G DP    DUB,=P'10'                                                       
         ZAP   DUB,DUB(8-2)        ONE DECIMAL                                  
         CVB   R1,DUB                                                           
         EDITR (R1),(7,0(R2)),1,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTNPRX                                                          
*                                                                               
FMTNPR12 CLI   PBDCL,0                                                          
         BE    FMTNPR13                                                         
         CLI   WRKTEMPS+3,C'0'                                                  
         BH    FMTNPR13                                                         
         C     R1,=F'999999'       GREATER THAN 9,999.99?                       
         BNH   FMTNPR13                                                         
         CVD   R1,DUB                                                           
         DP    DUB,=P'10'                                                       
         ZAP   DUB,DUB(6)                                                       
         CVB   R1,DUB                                                           
         EDITR (R1),(7,0(R2)),1,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTNPRX                                                          
*                                                                               
FMTNPR13 EDITR (R1),(7,0(R2)),2,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTNPRX                                                          
*                                                                               
FMTNPR14 C     R1,=F'-9999999'     SEE IF NEGATIVE > -99,999.99                 
         BL    FMTNPR16                                                         
         C     R1,=F'10000000'     SEE IF OVER 100,000.00                       
         BL    FMTNPR18                                                         
*                                                                               
FMTNPR16 CVD   R1,DUB              SEE IF PENNY CAN BE DROPPED                  
         C     R1,=F'-10000000'    CK FOR -100,000.00                           
         BNH   F_NPR16H                                                         
         C     R1,=F'100000000'    CK FOR 1,000,000.00                          
         BL    F_NPR16G                                                         
*                                                                               
F_NPR16H DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(8-2)        NO DECIMAL                                   
         CVB   R1,DUB                                                           
         EDITR (R1),(8,0(R2)),0,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTNPRX                                                          
*                                                                               
F_NPR16G DP    DUB,=P'10'                                                       
         ZAP   DUB,DUB(8-2)        ONE DECIMAL                                  
         CVB   R1,DUB                                                           
         EDITR (R1),(8,0(R2)),1,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTNPRX                                                          
*                                                                               
FMTNPR18 EDITR (R1),(8,0(R2)),2,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,IZERO=Y         
         B     FMTNPRX                                                          
*                                                                               
FMTNPR20 EDITR (R1),(10,0(R2)),2,FLOAT=-,ALIGN=LEFT,ZERO=NOBLANK,      +        
               IZERO=Y                                                          
*                                                                               
FMTNPRX  DS    0H                  RETURN FORMATTED VALUE                       
         J     EXIT                                                             
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FORMAT NEWSPAPER SPACE DESCRIPTION                                            
*                                                                               
* WRKTEMPS - RETURNS FORMATTED DATA                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTNSD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R5,R1               R1 POINTS TO FIRST BUY ELEM                  
         USING PBDELEM,R5                                                       
         MVC   WRKTEMPS(25),SPACES                                              
*                                                                               
         OC    PBDSPACE,PBDSPACE   ANYTHING IN SPACE?                           
         BZ    FMTNSD02                                                         
         CLC   PBDSPACE,SPACES     ANYTHING IN SPACE?                           
         BE    FMTNSD02                                                         
*                                                                               
         CLC   =X'7B00',PBDSPACE   # AND ZERO - TREAT AS NONE-SPACE BUY         
         BE    FMTNSD02                                                         
         CLC   =X'7B40',PBDSPACE   # AND SPACE- TREAT AS NONE-SPACE BUY         
         BE    FMTNSD02                                                         
*                                                                               
         CLI   PBDSPACE,C'*'       OTHER SPECIAL CHARS LOWER THAN C'*'?         
         BL    *+14                                                             
*                                                                               
         CLC   =X'5C40',PBDSPACE   TEST SPACE BUY                               
         BNL   FMTNSD02                                                         
*                                                                               
         MVC   WRKTEMPS(8),PBDSPACE                                             
         B     FMTNSDX                                                          
*                                                                               
FMTNSD02 MVC   WRKTEMPS(25),SPACES                                              
         LA    R2,WRKTEMPS                                                      
         CLI   PBDSPACE,C'*'                                                    
         BNE   *+12                                                             
         MVI   0(R2),C'*'                                                       
         AHI   R2,1                                                             
         CLI   PBDSPACE,C'#'       SPECIAL FOR NO ASC CHECKING                  
         BNE   *+12                                                             
         MVI   0(R2),C'#'                                                       
         AHI   R2,1                                                             
         ZAP   DUB,PBDUNITS                                                     
         BNZ   *+16                                                             
         MVI   0(R2),C'0'                                                       
         AHI   R2,1                                                             
         B     *+10                                                             
         GOTOR FMTNSDE1                                                         
         AR    R2,R0                                                            
*                                                                               
         MVC   0(1,R2),PBDUIND                                                  
         OI    0(R2),C' '          TO SET X'89' TO X'C9'                        
         AHI   R2,1                                                             
*                                                                               
         OC    PBDCLMS,PBDCLMS                                                  
         BZ    FMTNSD04                                                         
         ZAP   DUB,PBDCLMS                                                      
         BZ    FMTNSD04                                                         
         MVI   0(R2),C'/'                                                       
         AHI   R2,1                                                             
         GOTOR FMTNSDE2                                                         
*                                                                               
FMTNSD04 DS    0H                                                               
*                                                                               
FMTNSDX  J     EXIT                                                             
*                                                                               
FMTNSDE1 CLI   PBDUIND,C'I'-X'40'                                               
         BNE   FMTNSDE2                                                         
         EDIT  (P8,DUB),(6,0(R2)),2,ALIGN=LEFT                                  
         BR    RE                                                               
*                                                                               
FMTNSDE2 EDIT  (P8,DUB),(5,0(R2)),ALIGN=LEFT                                    
         BR    RE                                                               
*                                                                               
         DROP  RB,R5                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FORMAT NEWSPAPER CONTRACT LINEAGE EQUIVALENCY                                 
*                                                                               
* WRKTEMPS - RETURNS FORMATTED DATA                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTNCL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,DRFT_REC                                                      
         LA    R5,33(R5)                                                        
         USING PBDELEM,R5                                                       
*                                                                               
         MVC   WRKTEMPS(12),SPACES                                              
*                                                                               
         CLC   =X'7B00',PBDSPACE                                                
         BE    FMTNCLX                                                          
         CLC   =X'7B40',PBDSPACE                                                
         BE    FMTNCLX                                                          
         CLC   =X'5C40',PBDSPACE                                                
         BNL   FMTNCLX                                                          
*                                                                               
         LA    R2,WRKTEMPS         SET CONTRACT LINEAGE EQUIVALENCY             
         CLI   PBDUIND,C'I'-X'40'  SEE IF LOWER CASE I                          
         BNE   FMTNCL02                                                         
         EDIT  PBDUNITS,(6,0(R2)),2,ALIGN=LEFT                                  
         AR    R2,R0                                                            
         MVI   0(R2),C'I'                                                       
         B     FMTNCLX                                                          
*                                                                               
FMTNCL02 EDIT  PBDUNITS,(5,0(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         CLI   PBDUIND,C'I'                                                     
         BNE   *+12                                                             
         MVI   0(R2),C'I'                                                       
         AHI   R2,1                                                             
         CP    PBDCLMS,=P'0'                                                    
         BE    FMTNCLX                                                          
         MVI   0(R2),C'/'                                                       
         AHI   R2,1                                                             
         EDIT  PBDCLMS,(5,0(R2)),ALIGN=LEFT                                     
*                                                                               
FMTNCLX  DS    0H                                                               
         CLC   WRKTEMPS(12),SPACES                                              
         JE    SETCCNEQ            CLE IS NOT PRESENT                           
         JNE   SETCCEQ             RETURN FORMATTED CLE                         
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKFLDADJ NTR1  BASE=*,LABEL=*      CHECK FOR FIELD ADJUSTMENTS                  
*                                                                               
         CLI   BUYMD,C'N'          NEWSPAPER?                                   
         BNE   FLDADJ20                                                         
         TM    T411FFD+12,X'10'    ONLY ALLOWING INV STAT CHANGES?              
         BZ    FLDADJ20                                                         
         GOTOR FMT_COST,DMCB,(1,REC)                                            
         XC    WRKTEMPS,WRKTEMPS                                                
         MVC   WRKTEMPS(L'FC_OCOST),WRKELEM+(FC_OCOST-F_COST_D)                 
*                                                                               
         LA    RE,WRKTEMPS                                                      
         SR    RF,RF                                                            
         CLI   0(RE),C' '          END OF INPUT?                                
         JNH   *+16                                                             
         AHI   RF,1                                                             
         LA    RE,1(RE)                                                         
         J     *-16                                                             
*                                                                               
         CHI   RF,10               LARGE NEWSPAER RATE?                         
         JNH   FLDADJ20                                                         
         XC    WRKELEM,WRKELEM                                                  
         LA    RE,WRKELEM                                                       
         LA    R1,WRKTEMPS                                                      
FLDADJ12 CLI   0(R1),C'T'          TOTAL RATE?                                  
         JE    *+14                                                             
         MVC   0(1,RE),0(R1)       MOVE EVERYTHING EXCEPT TOTAL RATE            
         AHI   RE,1                                                             
         AHI   R1,1                                                             
         BCT   RF,FLDADJ12                                                      
*                                                                               
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,4                BUMP TO RATE FIELD                           
         BRAS  RE,BUMPFLDS                                                      
         MVC   8(10,R2),WRKELEM    STRIPPED 'T' TO INCLUDE PENNY                
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
FLDADJ20 DS    0H                                                               
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPRSMFD NTR1  BASE=*,LABEL=*      CHECK FOR PRISMA FIELDS ON ADD               
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JZ    CKPRSF_X                                                         
         CLI   DDLINKSW,C'N'       NEW INSERTION?                               
         JNE   CKPRSF_X                                                         
*                                                                               
         CLC   SVSPCDSP,SPACES     HAVE SAVED SPACE DESCRIPTION?                
         JNH   CKPRSF20                                                         
         LA    R2,BUYTR1H          FIRST FIELD OF MIDDLE SCR                    
         LA    RF,3                                                             
         BRAS  RE,BUMPFLDS         BUMPED TO SPACE DESCRIPTION                  
         CLI   5(R2),0             HAVE SPACE DECRIPTION?                       
         JH    CKPRSF20                                                         
*                                                                               
         LA    RE,L'SVSPCDSP                                                    
         LA    RF,SVSPCDSP+L'SVSPCDSP-1                                         
         CLI   0(RF),0                                                          
CKPRSF12 CLI   0(RF),C' '                                                       
         JH    CKPRSF16                                                         
         CHI   RE,0                                                             
         JL    CKPRSF20                                                         
         BCTR  RE,0                INPUT FIELD LENGTH                           
         BCTR  RF,0                POINT TO PREVIOUS CHARACTER                  
         J     CKPRSF12                                                         
*                                                                               
CKPRSF16 XC    8(17,R2),8(R2)      CLEAR SPACE DESCRIPTION FIELD                
         NI    4(R2),X'FF'-X'20'   RESET PREVIOUSLY VALIDATED                   
         STC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                FOR EX INSTRUCTION                           
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   8(0,R2),SVSPCDSP    FIELD DATA                                   
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
CKPRSF20 DS    0H                  FOR FUTURE FIELD ADJUSTMENTS                 
*                                                                               
CKPRSF_X J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
*                                                                               
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
         JE    TSTLKERR                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK2                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLK4                                                           
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK3   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         JE    TSTLKERR                                                         
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
         JE    TSTLKERR                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK5                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         JNE   SETCCEQ                                                          
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK6   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         JE    TSTLKERR                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK6                                                           
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
TSTLKERR TM    ABUPLDSW,IDSKUPLQ   PRISMATE UPDATE?                             
         JZ    SETCCNEQ                                                         
         CLI   DDLINKSW,C'N'       NEW BUY?                                     
         JNE   SETCCNEQ                                                         
         DC    H'0'                DUMP TO PREVENT DUPLICATES                   
*                                                                               
         DROP  RB,L                                                             
*                                                                               
GLOBALS  DS    0D                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP                                                                   
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   REC                 MAP BUY RECORD TO REC                        
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WORK17D  DSECT                                                                  
*                                                                               
SPACES   DS    CL(L'BUYMSG)        C' '                                         
*                                                                               
RELO17   DS    F                                                                
AWKAIO1  DS    A                                                                
VWRKIO   DS    A                                                                
WRKIOPAR DS    50F                 200 BYTES FOR WRKIO PARAMETERS               
*                                                                               
SVWRKELM DS    F                   SAVE POINTER TO WORKER ELEM                  
WRKRNUMB DS    F                   WORKER REC NUMBER (FOR RANDOM READS)         
WRKRCCNT DS    F                   NUMB OF WORKER REC PROCESSED COUNTER         
WRKMAPCD DS    H                   SAVE MAP CODE                                
*                                                                               
WRKBYTE1 DS    X                                                                
WRKBYTE2 DS    X                                                                
WRKHALF1 DS    H                                                                
WRKHALF2 DS    H                                                                
WRKFULL1 DS    F                                                                
WRKFULL2 DS    F                                                                
WRKDUB1  DS    D                                                                
WRKDUB2  DS    D                                                                
*                                                                               
WRKCBYRE DS    F                   SAVE RE FOR BUY OVERLAY CALLS                
WRKTOPRE DS    F                   HIGHER RE ADDRESS                            
WRKSAVRE DS    F                   SAVE RE                                      
WRKSAVKY DS    CL25                                                             
WRKCBOPT DS    X                   CONTROL BLOCK ADDITIONAL PARAMETER           
*                                                                               
WRKTEMPS DS    XL50                TEMP WORKING STORAGE                         
WRKMIDWK DS    XL80                MID SCR FLD HAS MAX OF 80 CHARS              
WRKELEM  DS    XL255                                                            
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
BUYCOS2F DS    XL(2*L'PCOS2FAC)    COS2 FACTOR                                  
BUYCOS2$ DS    CL11                COS2 $                                       
*                                                                               
WKSVAREC DS    XL(L'AREC)          ORIGINAL AREC POINTER                        
WKSV_KEY DS    XL(L'KEY)                                                        
*                                                                               
SVOVLAY# DS    X                   SAVE OVERLAY NUMBER                          
SVBYMVKY DS    XL4                 ADDRESS OF BUY MOVE "FROM" KEY               
SVBYMDAO DS    XL4                 DISK ADDRESS OF "OLD" INSERTION              
SVBYMDAN DS    XL4                 DISK ADDRESS OF "NEW" INSERTION              
SVBYS#_O DS    PL(L'PSERNUM)       BUY SERIAL# FOR BUY MOVE - "OLD"             
SVBYS#_N DS    PL(L'PSERNUM)       BUY SERIAL# FOR BUY MOVE - "NEW"             
SV_AD_ID DS    CL(L'PJOBADID)      AD-ID                                        
SV_ADCOD DS    CL(L'PJOBKJOB)      AD CODE                                      
*                                                                               
SVULDATE DS    XL(L'PBYDKDAT)      SAVE UPLOAD DATE                             
SVULTIME DS    XL(L'PBYDKTIM)      SAVE UPLOAD TIME                             
*                                                                               
FMTRATSW DS    X                   FORMAT RATE SWITCH                           
PENNDRPQ EQU   X'80'               PENNIES DROPPED                              
DIMEDRPQ EQU   X'80'               DIMES DROPPED                                
*                                                                               
SVREVINV DS    C                   REVISE INVOICE VALUE                         
*                                                                               
SVIDSKV# DS    CL4                 IDESK VERSION NUMBER                         
SVIDSKW1 DS    XL4                 IDESK TEMP ELEMENT ADDRESS 1                 
SVIDSKW2 DS    XL4                 IDESK TEMP ELEMENT ADDRESS 2                 
*                                                                               
PRSMAPSW DS    XL1                 PRISMA MAP CODE SWITCH                       
COS2MAPQ EQU   X'80'               COST 2 MAP CODE PRESENT                      
*                                                                               
SVSPCDSP DS    CL(L'PBDSPACE)      SAVE SPACE DESCRIPTION                       
*                                                                               
SEC_UPLD DS    C                   SECURITY - UPLOAD                            
*                                                                               
SECBLOCK DS    XL(SECBLKLQ)                                                     
SECBLKLQ EQU   1024                                                             
*                                                                               
WKAIO1   DS    XL4096                                                           
*                                                                               
WORK17X  EQU   *                   END OF LOCAL WORKING STORAGE AREA            
*                                                                               
FXRATEQ  EQU   X'200F'             FXRATE STANDARD CUSTOM COLUMN CODE           
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076PPBUY17   01/25/21'                                      
         END                                                                    
