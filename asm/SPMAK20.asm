*          DATA SET SPMAK20    AT LEVEL 087 AS OF 10/14/20                      
*PHASE T22820A                                                                  
*INCLUDE SPXBHIST                                                               
*                                                                               
***********************************************************************         
* SOME BASICS --                                                      *         
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY              *         
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                              *         
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES                    *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-48147  10/14/20 FIX 6AM INVOICE DETAIL > 24 HOURS         *         
* AKAT SPEC-43297  02/27/20 FIX CANADIAN RATE BUG                     *         
* AKAT SPEC-31662  07/30/19 CANADIAN NET$ SUPPORT                     *         
* AKAT SPEC-9602   04/26/17 CREDIT MULTIPLE SPOTS AT THE SAME TIME    *         
* AKAT SPEC-10300  04/26/17 FIX BUG THAT ALLOWED CHANGE IF EST LOCKED *         
* AKAT DSSUP-7406  05/06/16 SUPPORT NEW 3AM BROADCAST TIME            *         
***********************************************************************         
* WHEN    LEV WHAT                                              *               
* ----    --- ----                                              *               
* 16OCT14 083 FIX BROADCAST START TIME FOR US                   *               
* 22NOV13 082 SUPPORT CROSS NETWORK MAKEGOODS                   *               
* 07AUG13 079 USE ALL 3 AVAILABLE BUYLINES FOR ALLOCATIONS      *               
* 24JAN12 078 DON'T OVERRIDE SPOT COST IF USING TOLERANCE       *               
* 04APR11 077 2-BYTE BUYLINE SUPPORT                            *               
* 11JUN08 073 SAVE ORIGINAL INVOICE DETAIL COST IF CHANGING     *               
* 22FEB08 072 NEED TO GROSS UP NET INVOICE COSTS TO COMPARE TO  *               
*             COST SENT BY PC, THEN NET DOWN TO SAVE            *               
* 12JAN08 071 SUPPORT NEW OVERRIDE TO SKIP INTERVAL CHECKING    *               
* 22AUG07 070 FIX BAD INSTRUCTION IN CSKED & UPDINV             *               
*         --- TEST RECLEN AFTER HELLO CALL FOR COMMENTS         *               
* 30OCT06 068 DON'T SET STATUS FLAGS ON CERTAIN ACTIONS         *               
* 08JUN06 067 SET START OF DAY TIME CORRECTLY!                  *               
*         --- PASS DEMOS=NO OPTION TO BUY PGM                   *               
*         --- SET ERROR FLAGS IN MM EL & ON KEYS                *               
* 07AUG06 066 DON'T SET INVOICES MATCHED IF PROD DOESN'T MATCH  *               
* 20APR06 065 COST OVERRIDES FOR CANADA AT THE NETWORK LEVEL    *               
* 06DEC05 064 PROGRAM EXCHANGE FEATURES                         *               
*         --- SET LAST MATCHED DATA WHEN SETTING MATCHED (H3BQ) *               
* 09AUG05 063 DON'T CHANGE PAID SPOTS                           *               
* 25MAR05 062 USE MATCHED AFFID FOR CHG RNO COST IF PASS 2      *               
* 11NOV04 061 NEW ACTION TO CHANGE ONR COST                     *               
*         --- FIX GETRATE CALLS FOR TAX!                        *               
* 10NOV04 060 SEND NET COST IN SENDBMG                          *               
* 09JUL04 059 EARNED DISCOUNT/COST2                             *               
*         --- 2 DECIMAL DEMOS                                   *               
* 13SEP04 058 TEST READ-ONLY MODE                               *               
* 30MAR04 057 GET CORRECT NUMBER OF CHKSUM ENTRIES!             *               
* 23MAR04 056 NEW TESTS FOR CABLE                               *               
* 16JUL03 054 SKIP COST OVERRIDE FOR CANADIAN NETWORK           *               
* 15JUL03 053 FIX SPECIAL XOTO CODE FOR MULTIPLE SPOTS          *               
* 13MAY03 052 VER 3.1 - PROGRAM EXCHANGE                        *               
* 26FEB03 051 TEST IF REC DELETED SINCE MM OPENED               *               
* 13FEB03 050 FIX UNMATCH WHEN PRD'S NEQ                        *               
* 16JAN03 049 READ PASSIVES BY BUBBL CODE                       *               
* 26JUN02 048 VERSION 3.0                                       *               
*         --- APPROVE/UN-APPROVE                                *               
*         --- MAKEGOOD ANALYSIS                                 *               
*         --- MAKEGOOD ACROSS MONTH                             *               
*         --- FIX CHKSKED CODE                                  *               
*         --- TRY TO DEAL WITH INV PRD OF POL                   *               
* 21JUN02 047 FIX COST OVERRIDE                                 *               
* 10JUN02 046 USE I2Z PROFILE                                   *               
* 15APR02 045 DON'T DIE IN RECUP                                *               
* 28FEB02 044 DON'T DIE IN TESTI2 IF INV NOT FOUND              *               
*         --- NOP NON-EXECUTING CODE (VERSION CONTROL)          *               
* 14NOV01 043 FIX PIGGY BUYS FOR TRU POL                        *               
*         --- BDATA T TYPES NOW ALWAYS 8 CHARS                  *               
*         --- SKIP MATCHED SPOT IN SNVUPD                       *               
*         --- SUPPORT REASON CODES                              *               
*         --- ADD HISTORY SUPPORT                               *               
*         --- TEST ALLOCATION LOCKS                             *               
* 16DEC01 042 MAKE COMMENTS UPPERCASE                           *               
* 01OCT01 041 TEST INVOICE CHECKSUM BEFORE ALLOWING CHANGES     *               
*         --- UPDATE CHECKSUM IN UPDINVS                        *               
*         --- DISABLE PIGGY BUYS FOR TRUE POL  *** TEMP FIX *** *               
* 24SEP01 039 ALWAYS CHECK RETURN CODE FROM GLOBBER             *               
* 18SEP01 038 CALL GLOBBER FOR CLEAR IN CALLBUY                 *               
* 06JUN01 038 FIX SRTBDATA AND NXTBDATA FOR PIGGYS              *               
*         --- PUT M= PRD ALLOC ON FIRST LINE                    *               
* 29MAY01 037 FIX CODE FOR PIGGYS                               *               
*         --- FIX CODE FOR BRAND BUYING (NOP'ED)                *               
* 14MAY01 036 SUPPORT PURPOSE CODES                             *               
*             MAKE SURE DOING READ FOR UPDATE                   *               
*                                                               *               
*===============================================================*               
*                                                                               
T22820   TITLE 'SPMAK20 - MATCHMAKER - TRANSFER DATA TO SPOT BUY'               
T22820   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMK20**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
* THIS MASK FOR EDIT WHEN HAVE NO BASE REG                                      
         MVC   EDSAVE(17),=X'4040404040402020202020202020202020'                
*                                                                               
***         MVC   V20000,=X'02000000' VERSION 2.0.0                             
*                                                                               
         BRAS  RE,CLRBYDTA                                                      
*                                                                               
         CLI   SVRCVEL+1,0         THIS WILL BE 0 ON RE-ENTRY                   
         BNE   *+10                                                             
         MVC   SVRCVEL+1(1),SVOLDRCV                                            
*                                                                               
         MVC   SVOLDRCV,SVRCVEL+1  SAVE ORIGINAL RCVEL                          
*                                                                               
         CLI   SVRESUME,0          TEST RETURN FROM GLOBBER CALL                
         BNE   MK4                  YES                                         
         BRAS  RE,SAVCMT           SAVE OFF COMMENTS FIRST TIME IN              
         LR    RE,RA                AND REASON CODES                            
         AHI   RE,SVRCD-TWAD                                                    
         MVC   0(L'SVRCD,RE),RH6RSNCD                                           
         XC    EPRD,EPRD                                                        
         MVI   EEST,0                                                           
         BRAS  RE,TESTRO           TEST READ ONLY MODE                          
         BRAS  RE,TESTLOCK                                                      
         BRAS  RE,TESTI2           SEE IF I2 RE-RUN OR INVOICES CHANGED         
         B     MK10                                                             
*                                                                               
MK4      BRAS  RE,GETBYDTA         GET DATA RETURNED FROM SPO/BUY               
         BNE   EXIT                EXIT ON ERROR                                
*                                                                               
MK10     LA    R2,BDATA                                                         
         ST    R2,DPOINTER                                                      
*                                                                               
         CLI   SVRCVEL+1,H48Q      OVERRIDE MATCH CRITERIA                      
         BE    *+12                                                             
         CLI   SVRCVEL+1,H33Q      CHANGE BUY (TIME/COST)                       
         BNE   MK20                                                             
         BAS   RE,CHGBUY                                                        
         B     EXIT                                                             
*                                                                               
MK20     CLI   SVRCVEL+1,H34Q      COMMENT REQUEST                              
         BNE   MK30                                                             
         BRAS  RE,SENDCMTS                                                      
         B     EXIT                                                             
*                                                                               
MK30     CLI   SVRCVEL+1,H44Q      ADD NEW MAKEGOOD ACROSS MONTH?               
         BE    *+12                                                             
         CLI   SVRCVEL+1,H35Q      ADD NEW MAKEGOOD?                            
         BNE   MK40                                                             
         BAS   RE,NEWMG                                                         
         B     EXIT                                                             
*                                                                               
MK40     CLI   SVRCVEL+1,H36Q      MOVE SPOT?                                   
         BNE   MK50                                                             
         BAS   RE,MOVSPOT                                                       
         B     EXIT                                                             
*                                                                               
MK50     CLI   SVRCVEL+1,H37Q      NEW BUY?                                     
         BNE   MK60                                                             
         BAS   RE,NEWBUY                                                        
         B     EXIT                                                             
*                                                                               
MK60     CLI   SVRCVEL+1,H38Q      +OTO?                                        
         BNE   MK70                                                             
         BAS   RE,PLUS_OTO                                                      
         B     EXIT                                                             
*                                                                               
MK70     CLI   SVRCVEL+1,H39Q      -OTO?                                        
         BNE   MK80                                                             
         BAS   RE,MINS_OTO                                                      
         B     EXIT                                                             
*                                                                               
MK80     CLI   SVRCVEL+1,H3BQ      SET MATCHED?                                 
         BNE   MK90                                                             
         BAS   RE,MATCHED                                                       
         B     EXIT                                                             
*                                                                               
MK90     CLI   SVRCVEL+1,H3CQ      UNMATCH?                                     
         BNE   MK100                                                            
         BAS   RE,UNMATCH                                                       
         B     EXIT                                                             
*                                                                               
MK100    CLI   SVRCVEL+1,H3DQ      EMAIL RNO SPOTS                              
         BNE   MK110                                                            
         BAS   RE,EMAIL                                                         
         B     EXIT                                                             
*                                                                               
MK110    CLI   SVRCVEL+1,H3EQ      CHANGE BUYLINE COST                          
         BNE   MK120                                                            
         BAS   RE,CHGBUY                                                        
         B     EXIT                                                             
*                                                                               
MK120    CLI   SVRCVEL+1,H40Q      HISTORY REQUEST                              
         BNE   MK130                                                            
         BRAS  RE,SENDHIST                                                      
         B     EXIT                                                             
*                                                                               
MK130    CLI   SVRCVEL+1,H41Q      SET APPROVED?                                
         BNE   MK140                                                            
         BRAS  RE,APPROVE                                                       
         B     EXIT                                                             
*                                                                               
MK140    CLI   SVRCVEL+1,H42Q      UN-APPROVE                                   
         BNE   MK150                                                            
         BRAS  RE,UNAPPRV                                                       
         B     EXIT                                                             
*                                                                               
MK150    CLI   SVRCVEL+1,H43Q      MAKEGOOD ANALYSIS                            
         BNE   MK160                                                            
         NI    L_FLAGS,X'FF'-L_NOMGA                                            
         BRAS  RE,GETMGA                                                        
         B     EXIT                                                             
*                                                                               
MK160    CLI   SVRCVEL+1,H45Q      CHANGE COMMENTS                              
         BNE   *+12                                                             
         BRAS  RE,CHACOM                                                        
         B     EXIT                                                             
*                                                                               
         CLI   SVRCVEL+1,H47Q      CHANGE RNO DATA?                             
         BNE   *+12                                                             
         BAS   RE,CHGRNO                                                        
         B     EXIT                                                             
*                                                                               
EQEXIT   CR    RE,RE                                                            
         J     EXIT                                                             
*                                                                               
NEQEXIT  CR    RB,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*=================================================================*             
* SET RNO(S) WITH EMAIL SENT FLAG                                 *             
*=================================================================*             
         SPACE 1                                                                
EMAIL    NTR1                                                                   
         L     R2,DPOINTER                                                      
         MVI   SVREASON,SVRPASS2                                                
*                                                                               
         BRAS  RE,SNVUPD                                                        
         MVI   BYTE,C'A'           GET AFFID DATE                               
         BRAS  RE,NXTBDATA                                                      
         BE    *-12                                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* CHANGE RNO COST                                                 *             
*=================================================================*             
         SPACE 1                                                                
CHGRNO   NTR1                                                                   
         L     R2,DPOINTER                                                      
         MVI   SVREASON,SVRPASS2                                                
         BRAS  RE,SNVUPD                                                        
         MVI   BYTE,C'A'           GET NEXT INVOICE DATA                        
         BRAS  RE,NXTBDATA         HAVE MORE INVOICE DATA?                      
         BE    *-12                YES - PROCESS IT                             
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* MATCHED SET MATCHED FLAGS                                       *             
*=================================================================*             
         SPACE 1                                                                
MATCHED  NTR1                                                                   
         OI    L_FLAGS,L_NOSTAT    DON'T UPDATE STAT1                           
         MVI   BYTE,X'80'          SET MATCHED                                  
         BRAS  RE,UPDINVS          UPDATE INVOICE KEYS/RECS                     
         BRAS  RE,UPDPASS          UPDATE INVOICE PASSIVES                      
         NI    L_FLAGS,X'FF'-L_NOSTAT                                           
         B     EXIT                                                             
         SPACE 2                                                                
*=================================================================*             
* APPROVE - SET APPROVED FLAG                                     *             
*=================================================================*             
         SPACE 1                                                                
APPROVE  NTR1                                                                   
         OI    L_FLAGS,L_NOSTAT    DON'T UPDATE STAT1                           
         MVI   BYTE,X'02'          SET APPROVED                                 
         BRAS  RE,UPDINVS          UPDATE INVOICE KEYS/RECS                     
         BRAS  RE,UPDPASS          UPDATE INVOICE PASSIVES                      
         NI    L_FLAGS,X'FF'-L_NOSTAT                                           
         B     EXIT                                                             
         SPACE 2                                                                
*=================================================================*             
* UNAPPRV - RE-SET APPROVED FLAG                                  *             
*=================================================================*             
         SPACE 1                                                                
UNAPPRV  NTR1                                                                   
         OI    L_FLAGS,L_NOSTAT    DON'T UPDATE STAT1                           
         MVI   BYTE,X'FF'-X'02'    SET UN-APPROVED                              
         BRAS  RE,UPDINVS          UPDATE INVOICE KEYS/RECS                     
         BRAS  RE,UPDPASS          UPDATE INVOICE PASSIVES                      
         NI    L_FLAGS,X'FF'-L_NOSTAT                                           
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* UNMATCH                                                         *             
*                                                                 *             
* HERE BDATA CONTAINS SPOT & AFFID DATA:                          *             
*                                                                 *             
*  'B'EL 'D'DD 'N'N 'A'DD 'T'TTPCCC 'I'INVOICENUM                 *             
*     SI    TT    N    TT    IIROOO                               *             
*     TN                     MMDSSS                               *             
*                                                                 *             
* SET UP CALL TO BUY PROGRAM WITH I,DEL=MMMDD-NN                  *             
*                                                                 *             
*=================================================================*             
         SPACE 1                                                                
UNMATCH  NTR1                                                                   
         BRAS  RE,CLRBYDTA                                                      
         LA    R4,BUYDATA1                                                      
*                                                                               
         CLI   SVREASON,SVRIDEL    FIRST TIME?                                  
         BNE   UM8                  YES                                         
*                                                                               
         OC    SVDPNTRD,SVDPNTRD   STILL MORE I,DEL'S                           
         BNZ   UM8                  NO - EXIT                                   
*                                                                               
         LA    R2,BDATA                                                         
         ST    R2,DPOINTER                                                      
*                                                                               
UM6      MVI   BYTE,C'B'                                                        
         CLI   0(R2),C'B'                                                       
         BE    *+12                                                             
         BRAS  RE,NXTBDATA                                                      
         BNE   UMX                                                              
         MVC   BUYEST,1(R2)                                                     
         MVC   BBUYLIN,2(R2)       BUYLINE NUMBER                               
         MVC   BYCBLNET,4(R2)      MOVE BUY CABLE NETWORK                       
         MVC   BBCBLNET,7(R2)      MOVE BINARY BUY CABLE NETWORK                
*                                                                               
         MVI   BYTE,C'D'                                                        
         BRAS  RE,NXTBDATA                                                      
         BNE   UMX                                                              
         LA    R5,1(R2)                                                         
*                                                                               
         MVI   BYTE,C'N'                                                        
         BRAS  RE,NXTBDATA                                                      
         BNE   UMX                                                              
         MVC   SPOTNUM+1(1),1(R2)                                               
*                                                                               
         MVI   BYTE,C'A'                                                        
         BRAS  RE,NXTBDATA                                                      
         ST    R2,DPOINTER                                                      
         BNE   UMX                 DONE                                         
         BRAS  RE,SNVUPD                                                        
         MVI   BYTE,X'7F'          SET UNMATCHED STATUS                         
         BRAS  RE,UPDINVS          UPDATE INVOICE KEYS/RECS                     
         BRAS  RE,UPDPASS          UPDATE PASSIVE POINTERS                      
         B     UM6                                                              
*                                                                               
UM8      OC    SVDPNTRD,SVDPNTRD   IS THIS A RETURN CALL?                       
         BZ    UM10                                                             
*                                                                               
         LA    RF,BDATA                                                         
         AH    RF,SVDPNTRD         CONVERT SAVED DSPL TO ADDRESS                
         ST    RF,DPOINTER                                                      
         XC    SVDPNTRD,SVDPNTRD                                                
*                                                                               
UM10     BAS   RE,PUTLIN           GET THE BUYLINE & PUT IT OUT                 
         MVC   MOLINE,BBUYLIN                                                   
         MVC   LASTEST,BUYEST                                                   
         MVI   SVREASON,SVRIDEL    SET REASON CODE                              
*                                                                               
UM12     LA    R4,BUYDATA2                                                      
         ST    R4,BUYDSTR                                                       
         LA    R0,L'BUYDATA2(R4)                                                
         AHI   R0,-13              STOP 13 CHARS BEFORE END                     
         ST    R0,BUYDEND          SAVE DATA END ADDRESS                        
* REMEMBER WE ARE POINTING AT C'D' DAT DAT                                      
         L     R2,DPOINTER                                                      
*                                                                               
UM14     MVC   0(6,R4),=C'I,DEL='                                               
         AHI   R4,6                                                             
*                                                                               
UM20     BAS   RE,PUTDATE                                                       
         MVI   0(R4),C'-'                                                       
         AHI   R4,1                                                             
         BAS   RE,PUTSPOT                                                       
*                                                                               
         L     R2,DPOINTER                                                      
         MVI   BYTE,C'B'           GET TO NEXT BUYLINE                          
         BRAS  RE,NXTBDATA                                                      
         ST    R2,DPOINTER                                                      
         BNE   UM40                LAST ONE                                     
*                                                                               
         CLC   MOLINE,2(R2)        DIFFERENT BUY LINE?                          
         BNE   UM40                 YES - DELETE THESE AFFIDS NOW               
         CLC   LASTEST,1(R2)       DIFFERENT EST?                               
         BNE   UM40                 YES - DELETE THESE AFFIDS NOW               
*                                                                               
         C     R4,BUYDEND          PAST END OF CURRENT LINE                     
         BH    UM40                YES - DELETE THESE AFFIDS NOW                
*                                                                               
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         MVI   BYTE,C'D'           GET PAST BUYLINE DATA TO DATE-SPNUM          
         BRAS  RE,NXTBDATA                                                      
         ST    R2,DPOINTER                                                      
         BE    UM20                SET NEXT DATE-SPOTNUM                        
         DCHO                                                                   
*                                                                               
UM40     CLI   0(R2),0             TEST ANY MORE DATA                           
         BE    UM60                 NO                                          
         LA    RE,BDATA            SAVE DISPLACEMENT INTO BDATA                 
         SR    R2,RE                                                            
         STH   R2,SVDPNTRD                                                      
*                                                                               
UM60     BRAS  RE,CALLBUY                                                       
*                                                                               
UMX      B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* MOVE SPOT                                                       *             
*=================================================================*             
         SPACE 1                                                                
MOVSPOT  NTR1                                                                   
         OI    L_FLAGS,L_NOSTAT    DON'T UPDATE STAT1                           
         CLI   SVREASON,SVRMOTO    JUST DID MINUS PASS?                         
         BNE   *+12                 NO                                          
         BAS   RE,CHACOM           DONE - ADD COMMENTS (IF ANY)                 
         B     MSX                                                              
*                                                                               
         CLI   SVREASON,SVRPOTO    JUST DID PLUS PASS?                          
         BE    MS20                 YES - GO DO MINUS PASS                      
*                                                                               
         CLI   SVREASON,SVRTMOTO   DID WE DO TEST -OTO?                         
         BNE   MS20                 NO - GO DO IT NOW                           
         BAS   RE,PLUS_OTO                                                      
         B     MSX                                                              
*                                                                               
MS20     BAS   RE,MINS_OTO         SECOND PASS                                  
*                                                                               
MSX      NI    L_FLAGS,X'FF'-L_NOSTAT                                           
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* ADD A NEW BUY                                                   *             
*=================================================================*             
         SPACE 1                                                                
NEWBUY   NTR1                                                                   
*                                                                               
         B     NB1                 NOP DISABLE CODE                             
         CLC   APRD2,SPACES        IS THERE A PIGGY?                            
         BNH   NB1                  NO                                          
         BRAS  RE,GTPOLPRD                                                      
         CLI   BPOLPRD,X'FF'       TEST POL EST OPEN                            
         BNE   NB1                  NO - NOT POL                                
         CLI   SVCPROF+0,C'0'      TEST TRUE POL CLIENT                         
         BNE   NB1                  NO - OK                                     
         MVC   ERROR,=H'838'       TPOL BUY TEMP DISABLED FOR PIGGYS            
         GOTO1 SENDMSG                                                          
*                                                                               
NB1      LA    R4,BUYDATA1                                                      
         XC    BUYDATA1,BUYDATA1                                                
         CLI   SVREASON,0          FIRST TIME?                                  
         BE    NB10                 YES                                         
* NOTE THAT CHKBUY IS CALLED **AFTER** RETURN FROM SPOT/BUY                     
         BRAS  RE,CHKBUY           GO CHECK AFFIDS VS. REGELS                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SVREASON,SVRPASS2                                                
         BNE   NB2                                                              
         BRAS  RE,ADJSPOTS         ADJUST NEW BUY SPOTS                         
         BAS   RE,CHACOM           ADD COMMENTS                                 
         CLI   SVRCVEL+1,H44Q      THIS REALLY A NEW MG?                        
         BE    NB1A                 YES - DON'T SEND NOW                        
         CLI   SVRCVEL+1,H35Q      THIS REALLY A NEW MG?                        
         BE    NB1A                 YES - DON'T SEND NOW                        
         BRAS  RE,SENDBMG          RETURN NEW LINE NUMBER                       
*                                                                               
NB1A     MVI   SVREASON,SVRBUYX    LAST NEW BUY PASS                            
         B     NBX                                                              
*                                                                               
NB2      MVI   SVREASON,SVRPASS2    NO - SET NEW BUY CALL                       
         MVC   BUYDATA1,SVBUYDT1   RESTORE SAVED BUY DATA                       
         MVC   BUYDATA2,SVBUYDT2                                                
         MVC   BUYDATA3,SVBUYDT3                                                
         MVC   BUYDATA4,SVBUYDT4                                                
         B     NB40                                                             
*                                                                               
NB10     BRAS  RE,SRTBDATA                                                      
*                                                                               
         MVI   SVREASON,SVRPASS1   SET REASON TO BUY VALIDATION CALL            
         MVI   0(R4),C'$'          TELL BUY NOT TO ADD THIS PASS                
         AHI   R4,1                                                             
*                                                                               
         MVI   0(R4),C'B'                                                       
         AHI   R4,1                                                             
         BRAS  RE,SETDLM                                                        
*                                  BUY PERIOD                                   
         MVC   0(11,R4),RH6DATE                                                 
         AHI   R4,11                                                            
         BRAS  RE,SETDLM                                                        
*                                  DAYS                                         
         MVC   0(8,R4),RH6DAYS                                                  
         AHI   R4,8                                                             
         BRAS  RE,SETDLM                                                        
*                                  NUMBER PER WEEK                              
         MVC   0(2,R4),RH6NPW                                                   
         AHI   R4,2                                                             
         BRAS  RE,SETDLM                                                        
*                                  TIME EXPRESSION                              
         MVC   0(11,R4),RH6TIME                                                 
         AHI   R4,11                                                            
         BRAS  RE,SETDLM                                                        
*                                  DAYPART                                      
         MVC   0(1,R4),RH6DPT                                                   
         AHI   R4,1                                                             
         BRAS  RE,SETDLM                                                        
*                                  SPOT LENGTH                                  
         MVC   0(3,R4),RH6SLN                                                   
         AHI   R4,3                                                             
         BRAS  RE,SETDLM                                                        
*                                  PROGRAM NAME                                 
         MVC   0(17,R4),RH6PROG                                                 
         AHI   R4,17                                                            
         BRAS  RE,SETDLM                                                        
*                                  ADJACENCY CODE                               
         OC    RH6ADJ,RH6ADJ                                                    
         BZ    NB20                                                             
         MVC   0(2,R4),RH6ADJ                                                   
         AHI   R4,2                                                             
         BRAS  RE,SETDLM                                                        
*                                  RATE TYPE/COST                               
NB20     MVC   0(L'QCOST,R4),QCOST                                              
         AHI   R4,L'QCOST                                                       
         BRAS  RE,SETDLM                                                        
         OC    QCOST2,QCOST2       ANY SECOND COST?                             
         BZ    NB20A                NO                                          
         AHI   R4,-1                                                            
         MVI   0(R4),C'/'                                                       
         MVC   1(L'QCOST2,R4),QCOST2                                            
         AHI   R4,L'QCOST2+1                                                    
         BRAS  RE,SETDLM                                                        
*                                                                               
* FOR CANADIAN NETWORK, KEEP PRD ALLOC (M=) ON 1ST LINE                         
NB20A    CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   NB20B                NO                                          
         CLI   QMED,C'N'           TEST NETWORK                                 
         BE    NB21                 YES - NO DEMO OVERRIDES                     
*                                                                               
NB20B    LA    R4,BUYDATA2         PUT DEMO OVERRIDES ON 2ND LINE               
         CLC   RH6DEM(80),SPACES   SPACES IS ONLY CL80 - THAT'S ENOUGH          
         BNH   NB21                                                             
         MVC   0(L'RH6DEM,R4),RH6DEM                                            
*                                                                               
         CLC   APRD2,SPACES        IF WE NEED M= ON NEXT LINE, ADD ','          
         BNH   NB20C                                                            
         BRAS  RE,GTPOLPRD         BUT ONLY IF IT'S BRAND POL                   
         CLI   BPOLPRD,X'FF'       TEST POL EST OPEN                            
         BNE   NB20C                NO - NOT POL                                
         CLI   SVCPROF+0,C'0'      TEST TRUE POL CLIENT                         
         BE    NB20C                YES - NEED TO ALLOCATE (NOT M=)             
         CLI   0(R4),C' '                                                       
         BNH   *+12                                                             
         AHI   R4,1                                                             
         B     *-12                                                             
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         B     NB21                                                             
*                                                                               
NB20C    LA    R4,BUYDATA3                                                      
         B     NB22                                                             
*                                  PRODUCT ALLOCATION FOR PIGGYBACKS            
NB21     CLC   APRD2,SPACES        IS THERE A PIGGY?                            
         BNH   NB22                 NO                                          
         BRAS  RE,GTPOLPRD         IF IT'S BRAND POL, NEED TO USE M=            
         CLI   BPOLPRD,X'FF'       TEST POL EST OPEN                            
         BNE   NB22                 NO - NOT POL                                
         CLI   SVCPROF+0,C'0'      TEST TRUE POL CLIENT                         
         BE    NB22                 YES - NEED TO ALLOCATE (NOT M=)             
         MVC   0(2,R4),=C'M='                                                   
         MVC   2(3,R4),APRD                                                     
         AHI   R4,4                POINT TO LAST CHAR OF PRD                    
         CLI   0(R4),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         AHI   R4,1                CURRENT OUTPUT LOC                           
         MVI   0(R4),C'-'                                                       
         MVC   1(3,R4),APRD2                                                    
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         AHI   R4,1                                                             
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         B     NB30                                                             
* IF PRODUCT IN HEADLINES IS POL, NEED TO ALLOCATE                              
NB22     OC    APRD,APRD           TEST AFFID PRD                               
         BZ    NB30                                                             
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+16                 NO                                          
         CLI   QMED,C'N'           TEST NETWORK                                 
         BNE   *+8                                                              
         LA    R4,BUYDATA2         NEED SECOND INPUT LINE (M= ON PRIOR)         
*                                                                               
         BRAS  RE,GTPOLPRD                                                      
         CLI   BPOLPRD,X'FF'       TEST POL EST OPEN                            
         BNE   NB30                 NO - PRD DEFINITELY NOT POL                 
         CLI   SVCPROF+0,C'0'      TEST BRAND POL CLIENT                        
         BNE   NB30                 YES - THEN USE BRAND                        
         XC    LADATE,LADATE                                                    
         LA    R2,BDATA                                                         
*                                                                               
* MAKE SURE LAST SIGNIFICANT CHAR ISN'T A ','                                   
         LR    RE,R4                                                            
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNH   *-6                                                              
*                                                                               
         CLI   0(RE),C','                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
*                                                                               
         BRAS  RE,BLDALLOC         BUILD ALLOCATIONS                            
         B     NB31                DELIMETERS ALREADY CLEARED                   
*                                                                               
NB30     BCTR  R4,0                CLEAR LAST DELIMITER (IF ANY)                
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
*                                                                               
NB31     CLI   SVREASON,SVRPASS1   TEST THIS IS NEWBUY PASS 1                   
         BNE   NB40                                                             
         MVC   SVBUYDT1(77),BUYDATA1+1 SAVE BUY INPUT ($B,...)                  
         MVC   SVBUYDT2(78),BUYDATA2                                            
         MVC   SVBUYDT3(78),BUYDATA3                                            
         MVC   SVBUYDT4(78),BUYDATA4                                            
*                                                                               
NB40     BRAS  RE,CALLBUY                                                       
*                                                                               
NBX      B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* CHANGE COMMENTS                                                 *             
*=================================================================*             
         SPACE 1                                                                
CHACOM   NTR1                                                                   
         TM    FLAGS,FLCMTRCV      ANY COMMENT ELEM RECEIVED?                   
         BZ    CCX                  NO                                          
         LR    RE,RA               RESTORE SAVED COMMENT DATA                   
         AHI   RE,SVCOM-TWAD                                                    
         LHI   RF,RH6COML                                                       
         LA    R0,RH6COM1                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*         OC    RH6COM1,RH6COM1     ANY COMMENTS?                               
*         BZ    CCX                  NO                                         
*                                                                               
         CLI   SVRCVEL+1,H45Q      CHANGE COMMENTS                              
         BNE   CC6                                                              
         L     R2,DPOINTER                                                      
         CLI   0(R2),C'B'          LINE NUMBER?                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUYEST,1(R2)                                                     
         MVC   BBUYLIN,2(R2)       BUYLINE NUMBER                               
         MVC   BYCBLNET,4(R2)      MOVE BUY CABLE NETWORK                       
         MVC   BBCBLNET,7(R2)      MOVE BINARY BUY CABLE NETWORK                
*                                                                               
CC6      BRAS  RE,GETBUY           RTFB                                         
         L     R6,AIO                                                           
*                                                                               
* DELETE X'66' COMMENT ELEMENTS & ADD BACK NEW COMMENTS                         
         GOTO1 VHELLO,DMCB,(C'D',=C'SPTFIL'),(X'66',(R6))                       
         XC    BLOCK,BLOCK                                                      
         MVI   BLOCK,X'66'                                                      
         LA    R0,5                                                             
         LA    R4,RH6COM1                                                       
*                                                                               
CC10     DS    0H                                                               
         LA    R1,L'RH6COM1-1(R4)  POINT TO END OF COMMENT                      
         CLI   0(R1),C' '          BACK UP TO LAST SIG CHAR                     
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         SR    R1,R4               R1=L'COMMENT - 1                             
         BM    CC20                NO COMMENT                                   
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+3(0),0(R4)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    BLOCK+3(0),SPACES                                                
*                                                                               
         AHI   R1,1+3                                                           
         STC   R1,BLOCK+1          ELEMENT LENGTH                               
         LA    R1,6                                                             
         SR    R1,R0                                                            
         STC   R1,BLOCK+2          COMMENT NUMBER                               
         GOTO1 VHELLO,DMCB,(C'P',=C'SPTFIL'),(X'20',(R6)),BLOCK,       X        
               =C'ADD=CODE'                                                     
         CLI   12(R1),0            TAKE NO PRISONERS!                           
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
CC20     AHI   R4,L'RH6COM1                                                     
         BCT   R0,CC10                                                          
*                                                                               
         BRAS  RE,PUTBUY           PUT THE BUY RECORD USING GETBUY              
*                                                                               
CCX      B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* HANDLE MAKEGOOD REQUEST                                         *             
*=================================================================*             
         SPACE 1                                                                
NEWMG    NTR1                                                                   
         CLI   SVREASON,SVRMG      ALREADY DID C,MG= PASS                       
         BE    NM2                                                              
         CLC   VERSION,=X'04000034' VERSION 4.0.0.52 OR HIGHER?                 
         BNL   NM1                 YES - SUPPORT CROSS NTWK MAKEGOODS           
         CLC   BYCBLNET,SPACES     HAVE A CABLE NETWORK?                        
         BNH   NM1                 NO                                           
         OC    QMGCD,QMGCD         DID WE GET A MG GROUP CODE?                  
         BZ    NM1                 NO                                           
         CLI   SVREASON,0          FIRST TIME?                                  
         BNE   NM1                 NO                                           
         OI    L_FLAGS,L_NOMGA     SUPPRESS MGA FLAG                            
         OI    BYCBLNET+2,X'40'    IN CASE LAST BYTE IS X'00'                   
         BRAS  RE,GETMGA           ERR MSG IF MKGD IS NOT FOR THIS NTWK         
*                                                                               
NM1      BAS   RE,NEWBUY                                                        
         CLI   SVREASON,SVRBUYX    JUST COMPLETED LAST PASS?                    
         BNE   NMGX                 NO - NEED TO DO 2ND PASS (SNVUPD)           
         B     NM4                                                              
*                                                                               
NM2      OC    SVDPNTRD,SVDPNTRD   STILL MORE MAKEGOODS?                        
         BNZ   NM4                                                              
         BAS   RE,CHACOM                                                        
         BRAS  RE,SENDBMG                                                       
         B     NMGX                                                             
*                                                                               
NM4      BRAS  RE,CLRBYDTA                                                      
         OC    SVDPNTRD,SVDPNTRD   IS THIS A RETURN CALL?                       
         BZ    NM10                                                             
         LA    RF,BDATA            CONVERT SAVED DSPL TO ADDRESS                
         AH    RF,SVDPNTRD                                                      
         ST    RF,DPOINTER                                                      
         XC    SVDPNTRD,SVDPNTRD                                                
*                                                                               
NM10     LA    R4,BUYDATA1                                                      
         SR    R0,R0                                                            
         ICM   R0,3,BBUYLIN        GET BUYLINE NUMBER ADDED                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
*                                                                               
         LA    R4,BUYDATA2                                                      
         MVC   0(5,R4),=C'C,MG='                                                
         AHI   R4,5                                                             
*                                                                               
         OC    QMGCD,QMGCD         DID WE GET A MG GROUP CODE?                  
         BZ    NM20                                                             
         MVC   0(L'QMGCD,R4),QMGCD                                              
         B     NM50                                                             
*                                                                               
NM20     BAS   RE,PUTLIN           GET THE BUYLINE & PUT IT OUT                 
         BAS   RE,PUTDATE          GET THE DATE & PUT IT OUT                    
         MVI   0(R4),C'-'                                                       
         AHI   R4,1                                                             
         BAS   RE,PUTSPOT          GET THE SPOT NUMBER & PUT IT OUT             
         CLC   VERSION,=X'04000034' BEFORE VERSION 4.0.0.52?                    
         BL    NM25                NO - DON'T SUPPORT CROSS NETWORK             
         CLC   BYCBLNET,SPACES     HAVE A CABLE NETWORK?                        
         BNH   NM25                NO                                           
         MVI   0(R4),C'/'          YES - CAN BE CROSS NETWORK                   
         MVC   1(3,R4),BYCBLNET    CABLE NETWORK                                
         AHI   R4,4                +4 BYTES FOR "/NET"                          
*                                                                               
NM25     L     R2,DPOINTER                                                      
         CLI   0(R2),C'B'          ALREADY POINTING AT A LINE?                  
         BE    NM30                 YES                                         
         MVI   BYTE,C'B'            NO - SEE IF THERE IS ANOTHER LINE           
         BRAS  RE,NXTBDATA                                                      
         BNE   NM50                TEST ANY MORE DATA                           
*                                                                               
NM30     LA    RE,BDATA            SAVE DISPLACEMENT INTO BDATA                 
         SR    R2,RE                                                            
         STH   R2,SVDPNTRD                                                      
*                                                                               
NM50     MVI   SVREASON,SVRMG                                                   
         BRAS  RE,CALLBUY                                                       
*                                                                               
NMGX     B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* HANDLE TIME/COST CHANGE                                         *             
*=================================================================*             
         SPACE 1                                                                
CHGBUY   NTR1                                                                   
         CLI   SVREASON,SVRCHG     DID WE ALREADY DO CHANGE?                    
         BNE   CHB30                NO - GO DO IT                               
*                                                                               
         CLI   SVRCVEL+1,H3EQ      REALLY A CHG BUYLINE COST?                   
         BE    CHB22                YES - DON'T UPDATE INVOICE/BUY              
*                                                                               
* GET BUYLINE NUMBER & AFFID DATE TO UPDATE                                     
         CLI   0(R2),C'D'          ALREADY POINTING AT SPOT DATE?               
         BE    CHB10                                                            
         MVI   BYTE,C'D'                                                        
         BRAS  RE,NXTBDATA                                                      
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
CHB10    LA    R5,1(R2)            R5=A(SPOT DATE)                              
*                                                                               
         MVI   BYTE,C'N'           GET THE LINE NUMBER                          
         LR    RF,R2               SAVE CURRENT LOCATION IN BDATA               
         BRAS  RE,NXTBDATA                                                      
         BE    CHB20                                                            
         LR    R2,RF               RESTORE TO LOOK FOR AN OTO                   
         MVI   BYTE,C'O'           IT COULD BE AN OTO (OR COULD IT?)            
         BRAS  RE,NXTBDATA                                                      
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
CHB20    MVC   SPOTNUM+1(1),1(R2)                                               
         MVI   BYTE,C'A'           GET AFFID DATE                               
         BRAS  RE,NXTBDATA         R2 = A(AFFID DATE)                           
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         MVI   SVREASON,SVRPASS2   FAKE OUT SNVUPD                              
         BRAS  RE,SNVUPD           UPDATE SNV & BUY RECORDS                     
CHB22    BAS   RE,CHACOM           ADD COMMENTS                                 
         B     CHBX                                                             
*                                                                               
CHB30    LA    R4,BUYDATA1                                                      
*                                                                               
         BAS   RE,PUTLIN           GET THE BUYLINE & PUT IT OUT                 
*                                                                               
         LA    R4,BUYDATA2                                                      
         CLC   QCOST,SPACES        CHANGE COST?                                 
         BNH   CHB60                                                            
         CLI   SVRCVEL+1,H3EQ      REALLY A CHG BUYLINE COST?                   
         BE    CHB32                YES                                         
         BRAS  RE,GTPOLPRD                                                      
         CLI   BPOLPRD,X'FF'       ALLOCATE COST IF POL                         
         BE    CHB40                                                            
*                                                                               
CHB32    MVC   0(7,R4),=C'C,COST='                                              
         AHI   R4,7                                                             
         MVC   0(L'QCOST,R4),QCOST                                              
         AHI   R4,L'QCOST                                                       
         OC    QCOST2,QCOST2       ANY SECOND COST?                             
         BZ    CHB50                NO                                          
         CLI   0(R4),C' '          FIND LAST USED SPACE                         
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'/'                                                       
         MVC   2(L'QCOST2,R4),QCOST2                                            
         B     CHB50                                                            
*                                                                               
CHB40    MVC   0(2,R4),=C'A,'                                                   
         AHI   R4,2                                                             
         BAS   RE,PUTDATE                                                       
         MVI   0(R4),C'-'                                                       
         AHI   R4,1                                                             
         BAS   RE,PUTSPOT          GET THE SPOT NUMBER & PUT IT OUT             
         MVI   0(R4),C'$'                                                       
***      MVC   1(L'QCOST,R4),QCOST                                              
         LA    RE,QCOST            COST MAY HAVE RATE TYPE                      
         LA    RF,L'QCOST-1        L'QCOST -1 FOR EX                            
         CLI   0(RE),C'0'          NUMERIC?                                     
         BNL   *+10                YES - SEND AS-IS                             
         LA    RE,1(RE)            BUMP PAST RATE TYPE                          
         BCTR  RF,0                DECREMENT LENGTH BY 1                        
         EX    RF,*+8              EXECUTE                                      
         B     *+10                BRANCH OVER MVC                              
         MVC   1(0,R4),0(RE)       MOVE QCOST (WITHOUT RATE TYPE)               
*                                                                               
CHB50    LA    R4,BUYDATA3         POINT TO NEXT INPUT LINE                     
*                                                                               
CHB60    CLI   SVRCVEL+1,H3EQ      REALLY A CHG LINE COST?                      
         BE    CHB70                YES - NEVER CHANGE TIME                     
         CLC   RH6TIME,SPACES                                                   
         BNH   CHB70                                                            
         MVC   0(6,R4),=C'C,TIM='                                               
         MVC   6(11,R4),RH6TIME                                                 
*                                                                               
CHB70    MVI   SVREASON,SVRCHG                                                  
         BRAS  RE,CALLBUY                                                       
CHBX     B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* HANDLE PLUS OTO REQUEST                                         *             
*=================================================================*             
         SPACE 1                                                                
PLUS_OTO NTR1                                                                   
         CLI   SVRCVEL+1,H38Q      +OTO?                                        
         BNE   PO20                 NO - MUST BE MOVE SPOT                      
         CLI   SVREASON,SVRPOTO    DID WE ALREADY DO +OTO?                      
         BNE   PO20                 NO - GO DO IT                               
*                                                                               
         LA    R5,OTODATA          R5 = OTO DATA ADDED                          
         MVC   SPOTNUM+1(1),OTODATA+2                                           
*                                                                               
         CLI   SVSKNUM,0           DID WE REALLY DO A SKED?                     
         BE    PO10                 NO                                          
         MVC   0(2,R5),SVSKDATE     YES - USE SKED DATA, NOT OTO DATA           
         MVC   SPOTNUM+1(1),SVSKNUM                                             
*                                                                               
* R2 POINTS TO BDATA                                                            
PO10     MVI   BYTE,C'A'           GET AFFID DATE                               
         BRAS  RE,NXTBDATA         R2 = A(AFFID DATE)                           
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         MVI   SVREASON,SVRPASS2   FAKE OUT SNVUPD                              
         BRAS  RE,SNVUPD           UPDATE SNV & BUY RECORDS                     
         BAS   RE,CHACOM           ADD COMMENTS                                 
*                                                                               
* SEND HABS THE SPOT NUMBER JUST ADDED                                          
         LA    R1,H08Q             FIND SPOT DETAIL HEADER                      
         BRAS  RE,SENDH                                                         
         SR    R5,R5               SET OUTPUT LENGTH                            
         LA    R4,SPOTNUM+1                                                     
         LA    R1,3                SPOT DETAIL DATA CODE                        
         BRAS  RE,SENDD                                                         
         B     POX                                                              
*                                                                               
PO20     MVI   SVREASON,SVRPOTO                                                 
         BRAS  RE,CLRBYDTA                                                      
         LA    R4,BUYDATA1                                                      
         BAS   RE,PUTLIN           GET THE BUYLINE & PUT IT OUT                 
*                                                                               
         LA    R4,BUYDATA2                                                      
         ST    R4,BUYDSTR                                                       
         LA    R0,L'BUYDATA2(R4)                                                
         AHI   R0,-11                                                           
         ST    R0,BUYDEND                                                       
*                                                                               
         BRAS  RE,CHKSKED                                                       
         BE    PO50                CC EQ MEANS SKED                             
*                                                                               
         MVC   0(3,R4),=C'O,+'                                                  
         AHI   R4,3                                                             
*                                                                               
         CLI   SVRCVEL+1,H36Q      IS THIS A MOVE SPOT REQ?                     
         BNE   PO30                 NO - USE DATE IN BDATA                      
         GOTO1 VDATCON,DMCB,(3,BSDATE),(4,(R4))                                 
         AHI   R4,5                                                             
         B     PO32                                                             
*                                                                               
PO30     BAS   RE,PUTDATE                                                       
*                                                                               
PO32     CLC   QCOST,SPACES        ANY COST OVERRIDE?                           
         BNH   PO40                                                             
         MVI   0(R4),C'$'                                                       
         MVC   1(L'QCOST,R4),QCOST                                              
         AHI   R4,1                                                             
         CLI   0(R4),C' '                                                       
         BH    *-8                                                              
*                                                                               
PO40     BRAS  RE,TSTPROD                                                       
         BNE   PO50                                                             
*                                                                               
         MVC   0(3,R4),APRD                                                     
         CLC   APRD2,SPACES        IS THERE A PIGGY?                            
         BNH   PO50                 NO                                          
         AHI   R4,2                MAY BE 2 CHAR PRD                            
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         AHI   R4,1                                                             
         MVI   0(R4),C'-'                                                       
         MVC   1(3,R4),APRD2                                                    
*                                                                               
PO50     BRAS  RE,CALLBUY                                                       
POX      B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* HANDLE MINUS OTO REQUEST                                        *             
* BUFFER LOOKS LIKE  C'B'/EST-LIN                                 *             
*                    C'D'/DATE                                    *             
*                    C'N'/C'O' FOR SPOTNUM OR OTONUM              *             
*=================================================================*             
         SPACE 1                                                                
MINS_OTO NTR1                                                                   
         BRAS  RE,CLRBYDTA                                                      
         LA    R4,BUYDATA1                                                      
*                                                                               
         CLI   SVRCVEL+1,H36Q      MOVE SPOT?                                   
         BNE   MO1                                                              
         CLI   SVREASON,SVRPOTO    DID WE DO THE PLUS PASS?                     
         BE    MO10                 YES - DO MINUS PASS FOR REAL                
         MVI   0(R4),C'$'          TEST FLAG FOR BUY PGM                        
         AHI   R4,1                                                             
         BAS   RE,PUTLIN           GET THE BUYLINE & PUT IT OUT                 
         MVC   MOLINE,BBUYLIN                                                   
         MVI   SVREASON,SVRTMOTO   SET REASON CODE TO TEST -OTO                 
         B     MO12                                                             
*                                                                               
MO1      CLI   SVREASON,SVRMOTO    FIRST TIME?                                  
         BNE   MO2                  YES                                         
*                                                                               
         OC    SVDPNTRD,SVDPNTRD   STILL MORE -OTO'S                            
         BNZ   MO2                  YES - DO THEM NOW                           
         BAS   RE,CHACOM                                                        
         B     MOX                                                              
*                                                                               
MO2      OC    SVDPNTRD,SVDPNTRD   IS THIS A RETURN CALL?                       
         BZ    MO10                                                             
*                                                                               
         LA    RF,BDATA                                                         
         AH    RF,SVDPNTRD         CONVERT SAVED DSPL TO ADDRESS                
         ST    RF,DPOINTER                                                      
         XC    SVDPNTRD,SVDPNTRD                                                
*                                                                               
         CLI   0(RF),C'B'          THIS A NEW LINE NUMBER?                      
         BE    MO10                 YES - USE IT                                
         EDIT  MOLINE,(3,(R4)),ALIGN=LEFT                                       
         B     *+14                                                             
*                                                                               
MO10     BAS   RE,PUTLIN           GET THE BUYLINE & PUT IT OUT                 
         MVC   MOLINE,BBUYLIN                                                   
         MVI   SVREASON,SVRMOTO    SET REASON CODE                              
*                                                                               
MO12     LA    R4,BUYDATA2                                                      
         ST    R4,BUYDSTR                                                       
         LA    R0,L'BUYDATA2(R4)                                                
         AHI   R0,-11              STOP 11 CHARS BEFORE END                     
         ST    R0,BUYDEND          SAVE DATA END ADDRESS                        
* REMEMBER WE ARE POINTING AT C'D' DAT DAT                                      
         L     R2,DPOINTER                                                      
         CLI   3(R2),C'O'          TEST OTO                                     
         BE    MO14                THEN DON'T EVER DO SKED                      
         BRAS  RE,CHKSKED                                                       
         BNE   MO14                WE AREN'T DOING A SKED                       
         L     R2,DPOINTER         R2=POSSIBLE NEXT LINE                        
         B     MO40                                                             
*                                                                               
MO14     MVC   0(3,R4),=C'O,-'                                                  
         OC    XOTOCODE,XOTOCODE   ANY SPECIAL -OTO CHARACTER?                  
         BZ    *+10                 NO                                          
         MVC   2(L'XOTOCODE,R4),XOTOCODE                                        
         AHI   R4,3                                                             
*                                                                               
MO20     BAS   RE,PUTDATE                                                       
         MVI   0(R4),C'-'                                                       
         AHI   R4,1                                                             
         BAS   RE,PUTSPOT                                                       
*                                                                               
         L     R2,DPOINTER                                                      
         CLI   0(R2),0                                                          
         BE    MO40                                                             
*                                                                               
         CLI   0(R2),C'B'          DIFFERENT BUY LINE?                          
         BE    MO40                 YES - MINUS THESE & COME BACK               
*                                                                               
         C     R4,BUYDEND          PAST END OF CURRENT LINE                     
         BH    MO30                YES                                          
*                                                                               
         MVC   0(2,R4),=C',-'                                                   
         OC    XOTOCODE,XOTOCODE   ANY SPECIAL -OTO CHARACTER?                  
         BZ    *+10                 NO                                          
         MVC   1(L'XOTOCODE,R4),XOTOCODE                                        
         AHI   R4,2                                                             
         B     MO20                SET NEXT DATE-SPOTNUM                        
*                                                                               
MO30     BRAS  RE,NEXTOUT          ADVANCE R4 TO NEXT LINE                      
         BE    MO14                AND CONTINUE                                 
*                                                                               
MO40     CLI   0(R2),0             TEST ANY MORE DATA                           
         BE    MO50                 NO                                          
         LA    RE,BDATA            SAVE DISPLACEMENT INTO BDATA                 
         SR    R2,RE                                                            
         STH   R2,SVDPNTRD                                                      
*                                                                               
MO50     CLI   SVREASON,SVRTMOTO   DON'T SET FOR TEST PASS                      
         BE    MO60                                                             
         OI    L_FLAGS,L_NOSTAT    DON'T UPDATE STAT1                           
         MVI   BYTE,X'40'          SET WIP                                      
         BRAS  RE,UPDINVS          UPDATE INVOICE KEYS/RECS                     
         BRAS  RE,UPDPASS          UPDATE INVOICE PASSIVES                      
         NI    L_FLAGS,X'FF'-L_NOSTAT                                           
*                                                                               
MO60     BRAS  RE,CALLBUY                                                       
*                                                                               
MOX      B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* GET BUYLINE # FROM BDATA AND STORE IN BBUYLIN AND ADDR PASSED   *             
* IN R4                                                           *             
* AND SET CURRENT ESTIMATE NUMBER IN BUYEST AND QBUYEST           *             
*=================================================================*             
         SPACE 1                                                                
PUTLIN   DS    0H                                                               
         L     R2,DPOINTER                                                      
         CLI   0(R2),C'B'          LINE NUMBER?                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BUYEST,1(R2)                                                     
*                                                                               
         MVC   BBUYLIN,2(R2)       BUYLINE NUMBER                               
         MVC   BYCBLNET,4(R2)      MOVE BUY CABLE NETWORK                       
         MVC   BBCBLNET,7(R2)      MOVE BINARY BUY CABLE NETWORK                
         AHI   R2,8                C'B'/EST/LIN/CBL/BINARY CBL                  
         ST    R2,DPOINTER                                                      
*                                                                               
         EDIT  BBUYLIN,(3,(R4)),ALIGN=LEFT                                      
         AR    R4,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
*=================================================================*             
* GET DATE FROM BDATA AND STORE IN ADDR PASSED IN R4              *             
*=================================================================*             
         SPACE 1                                                                
PUTDATE  LR    R0,RE                                                            
         L     R2,DPOINTER                                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(2,1(R2)),(4,(R4))                                  
         AHI   R2,3                ADJUST FOR C'D' DT DT                        
         ST    R2,DPOINTER                                                      
*                                                                               
         AHI   R4,5                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
*=================================================================*             
* GET SPOT NUMBER FROM BDATA AND STORE IN ADDR PASSED IN R4       *             
* SPOT NUMBER IS PRECEDED BY AN N OR O FOR OTO                    *             
*=================================================================*             
         SPACE 1                                                                
PUTSPOT  L     R2,DPOINTER                                                      
         EDIT  (1,1(R2)),(3,(R4)),ALIGN=LEFT                                    
         AR    R4,R0                                                            
         AHI   R2,2                                                             
         ST    R2,DPOINTER                                                      
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* SEND COMMENTS                                                   *             
*=================================================================*             
         SPACE 1                                                                
SENDCMTS NTR1  BASE=*,LABEL=*                                                   
         L     R2,DPOINTER                                                      
         CLI   0(R2),C'B'          LINE NUMBER?                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUYEST,1(R2)                                                     
         MVC   BBUYLIN,2(R2)       BUYLINE NUMBER                               
         MVC   BYCBLNET,4(R2)      MOVE BUY CABLE NETWORK                       
         MVC   BBCBLNET,7(R2)      MOVE BINARY BUY CABLE NETWORK                
*                                                                               
         BRAS  RE,GETBUY           RTFB                                         
*                                                                               
* GET 66 ELEMS & SEND TO FALINK                                                 
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         L     R6,AIO1             A(BUY)                                       
         AHI   R6,BDELEM-BUYREC                                                 
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   SCX                                                              
         LHI   R1,H0AQ                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
* MAKE SURE IF THERE'S A COMMENT 'MISSING' WE SEND A PLACEHOLDER                
         XR    R5,R5                                                            
         LHI   R2,1                COMMENT LINE NUMBER                          
*                                                                               
SC10     LHI   R1,1                FAMAP ELEM CODE                              
         CLM   R2,1,2(R6)          SAME COMMENT NUMBER?                         
         BE    SC20                 YES - SEND COMMENT                          
         XR    R5,R5                NO - JUST SEND PLACE HOLDER                 
         BRAS  RE,SENDD                                                         
         AHI   R2,1                NEXT COMMENT LINE NUMBER                     
         B     SC10                                                             
*                                                                               
SC20     IC    R5,1(R6)            GET COMMENT LENGTH                           
         AHI   R5,-4               1 EXTRA FOR EX                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    3(0,R6),SPACES      MAKE SURE NO BINARY 0'S                      
         AHI   R5,1                                                             
         LA    R4,3(R6)            R4=A(COMMENT DATA)                           
         BRAS  RE,SENDD            SEND IT                                      
         AHI   R2,1                NEXT COMMENT LINE NUMBER                     
         BRAS  RE,NEXTEL                                                        
         BE    SC10                                                             
*                                                                               
SCX      J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* TEST MK PROFILE FOR SKED IN LIEU OF OTO OPTION                  *             
* IF IT'S ON, COUNT SPOTS IN EACH WEEK AND SAVE IN BWKSPOTS       *             
* ON ENTRY DPOINTER POINTS AT BUY DATE/SPOT NUMBER LOOKING LIKE   *             
*                              C'D' BDAT BDAT  C'N' BSPNUM        *             
*=================================================================*             
         SPACE 1                                                                
CHKSKED  NTR1  BASE=*,LABEL=*                                                   
         MVI   SVSKNUM,0                                                        
         XC    SVSKDATE,SVSKDATE                                                
*                                                                               
         CLI   BSKED,C'O'          FORCE OTO?                                   
         BE    CSKXNEQ              YES - SET CC NEQ AND EXIT                   
*                                                                               
         CLI   BSKED,C'S'          FORCE SKED?                                  
         BE    CSK40                YES                                         
*                                                                               
         CLI   PROFMK,C'S'         FORCE SKED?                                  
         BE    CSK40                                                            
         CLI   PROFMK,C'Y'         NEITHER OPTION - FOLLOW PROFILE              
         BNE   CSKXNEQ                                                          
*                                                                               
CSK40    BRAS  RE,GETBUY           RTFB                                         
         L     R6,AIO1                                                          
         USING BUYRECD,R6                                                       
*                                                                               
* BUILD TABLE OF BUY RECORD WEEKS IN BLOCK                                      
         XC    BLOCK,BLOCK                                                      
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK                                    
         GOTO1 (RF),(R1),(3,BDEND),WORK+12                                      
*                                                                               
         LA    R5,BLOCK                                                         
*                                                                               
CSK50    GOTO1 VDATCON,DMCB,WORK,(2,(R5))                                       
         LA    R5,3(R5)                                                         
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+6,7                                        
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK+6(6),WORK+12   TEST PAST END DATE                           
         BNH   CSK50                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,WORK,(2,(R5))  ONE XTRA WEEK TO FIX SKED            
         MVC   3(2,R5),=X'FFFF'    SET EOL FLAG                                 
*                                                                               
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,12                                                        
         LA    R5,BLOCK                                                         
         LA    R6,BDELEM                                                        
*                                                                               
CSK60    BRAS  RE,NEXTEL                                                        
         BNE   CSK90                                                            
*                                                                               
CSK70    CLC   2(2,R6),0(R5)       SPOT DATE TO WEEK DATE                       
         BE    CSK80                                                            
         CLC   2(2,R6),3(R5)       BEFORE START OF NEXT WEEK                    
         BL    CSK80               YES                                          
         LA    R5,3(R5)                                                         
         B     CSK70                                                            
*                                                                               
CSK80    IC    R1,2(R5)                                                         
         LA    R0,1                                                             
         CLI   ELCDLO,11           TEST DOING POL                               
         BE    *+8                                                              
         IC    R0,7(R6)            GET NUMBER OF SPOTS                          
         TM    6(R6),X'80'         TEST MINUS                                   
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AR    R1,R0                                                            
         STC   R1,2(R5)                                                         
         B     CSK60                                                            
*                                                                               
CSK90    MVC   0(2,R4),=C'C,'                                                   
         AHI   R4,2                                                             
*                                                                               
         CLI   SVRCVEL+1,H36Q      IS THIS A MOVE SPOT REQ?                     
         BNE   CSK100               NO - USE DATE IN BDATA                      
         CLI   SVREASON,SVRPOTO    TEST PLUS OTO                                
         BNE   CSK100               NO - USE SPOT DATE IN BDATA                 
         XC    FULL,FULL                                                        
         GOTO1 VDATCON,DMCB,(3,BSDATE),(2,FULL+1)                               
         LA    R2,FULL             BE CAREFUL - DATE IS AT FULL+1               
         B     CSK110                                                           
*                                                                               
CSK100   L     R2,DPOINTER         DPOINTER SHOULD BE AT DATE                   
         CLI   0(R2),C'D'                                                       
         BE    CSK110                                                           
         CLI   0(R2),C'A'                                                       
         BE    CSK110                                                           
         DCHO                                                                   
*                                                                               
CSK110   LA    R6,BLOCK              START OF DATE TABLE                        
*                                                                               
CSK120   CLC   =X'FFFF',3(R6)                                                   
         BE    CSKERR                                                           
         CLC   1(2,R2),3(R6)       INPUT DATE TO START OF NEXT WEEK             
         BL    CSK130                                                           
         LA    R6,3(R6)                                                         
         B     CSK120                                                           
*                                                                               
CSK130   MVC   0(L'SKJAN01,R4),SKJAN01                                          
         GOTO1 VDATCON,DMCB,(2,(R6)),(4,2(R4))                                  
         AHI   R4,8                ADVANCE OUTPUT POINTER TO XX                 
         LA    R0,1                                                             
         CLI   SVREASON,SVRPOTO    TEST PLUS OTO                                
         BE    CSK140                                                           
         CLI   SVREASON,SVRTMOTO   TEST MINUS OTO TEST                          
         BE    *+14                                                             
         CLI   SVREASON,SVRMOTO    TEST MINUS OTO                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LNR   R0,R0                                                            
*                                                                               
CSK140   SR    RE,RE                                                            
         IC    RE,2(R6)            GET CURRENT SPOTS IN WEEK                    
         AR    RE,R0                                                            
         CHI   RE,99                                                            
         BH    TOOMANY                                                          
         LTR   RE,RE                                                            
         BM    TOOFEW                                                           
         STC   RE,2(R6)            SAVE NEW SPOT COUNT                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,2                ADVANCE OUTPUT POINTER                       
*                                                                               
         MVC   SVSKDATE,0(R6)      USED IF FORCED SKED                          
         MVC   SVSKNUM,2(R6)                                                    
*                                                                               
         CLI   SVRCVEL+1,H36Q      IS THIS A MOVE SPOT REQ?                     
         BE    CSKXEQ               YES - WE'RE DONE                            
*                                                                               
         CLI   SVRCVEL+1,H38Q      +OTO?                                        
         BNE   CSK170               NO                                          
         MVI   BYTE,C'Y'           SET NEED PRD                                 
         BRAS  RE,TSTPROD          ALLOCATE PRODUCT?                            
         BE    *+18                                                             
         MVI   BYTE,C'N'                                                        
         CLC   QCOST,SPACES        CHECK FOR COST OVERRIDE                      
         BNH   CSKXEQ               NONE                                        
*                                                                               
         BRAS  RE,NEXTOUT                                                       
         MVC   0(11,R4),=C'A,JAN99-99$'                                         
         GOTO1 VDATCON,DMCB,(2,SVSKDATE),(4,2(R4))                              
         AHI   R4,8                                                             
         ZIC   RE,SVSKNUM          SAVE NEW SPOT COUNT                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                ADVANCE OUTPUT POINTER                       
         CLC   QCOST,SPACES        IS THERE A COST OVERRIDE                     
         BNH   *+14                                                             
         MVC   0(L'QCOST,R4),QCOST                                              
         AHI   R4,L'QCOST                                                       
*                                                                               
         CLI   BYTE,C'Y'           NEED TO ALLOC PRD?                           
         BNE   CSK150                                                           
         CLI   0(R4),C' '          GET TO LAST CHARACTER                        
         BH    *+8                                                              
         BCT   R4,*-8                                                           
*                                                                               
         CLI   0(R4),C'$'          IF NO COST OVERRIDE, SET PRD HERE            
         BE    *+8                                                              
         AHI   R4,1                                                             
*                                                                               
         CLI   SVRCVEL+1,H36Q      THIS A MOVE SPOT?                            
         BNE   *+14                 NO                                          
         MVC   0(3,R4),RH6PRD       YES - USE REQ'D PRD (NO AFFIDS)             
         B     *+10                                                             
*                                                                               
         MVC   0(3,R4),APRD                                                     
         CLC   APRD2,SPACES        IS THERE A PIGGY?                            
         BNH   CSK150               NO                                          
         AHI   R4,2                                                             
         CLI   0(R4),C' '          MAY BE 2 CHAR PRD CODE                       
         BNH   *+8                                                              
         AHI   R4,1                                                             
         MVI   0(R4),C'-'                                                       
         MVC   1(3,R4),APRD2                                                    
*                                                                               
CSK150   LA    R2,=X'00'           MAKE SURE WE DON'T GET BACK HERE             
         B     CSKXEQ              AND EXIT                                     
*                                                                               
* R2 = C'D' BDAT BDAT C'N' SPNUM FOLLOWED BY NEXT DATE OR BUYLINE               
CSK170   CLI   5(R2),C'B'          OTHER -OTO'S FOR DIFFERENT LINE?             
         BNE   CSK180               NO                                          
         AHI   R2,5                 YES - ADVANCE TO IT                         
         B     CSKXEQ               AND EXIT                                    
*                                                                               
CSK180   MVC   HALF,1(R2)          SAVE OFF CURRENT WEEK DATE                   
         MVI   BYTE,C'D'                                                        
         BRAS  RE,NXTBDATA         ANY MORE -OTO'S?                             
         BNE   CSKXEQ               NO                                          
*                                                                               
* CHECK IF NEXT SPOT TO MINUS IS SAME DATE                                      
         CLC   HALF,1(R2)                                                       
         BNE   *+12                                                             
         AHI   R4,-L'SKJAN01       BACK UP POINTER                              
         B     CSK110                                                           
*                                                                               
* SEE IF NEED TO ADVANCE TO NEXT INPUT LINE                                     
         C     R4,BUYDEND          TEST PAST END OF CURRENT LINE                
         BH    CSK200              YES                                          
*                                                                               
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         B     CSK110                                                           
*                                                                               
CSK200   BRAS  RE,NEXTOUT                                                       
         BNE   CSKXEQ                                                           
         MVC   0(2,R4),=C'C,'                                                   
         AHI   R4,2                AND BUMP POINTER YOU FUCKING IDIOT!          
         B     CSK110                                                           
*                                                                               
CSKXNEQ  CR    RB,RE                SET CC NEQ AND EXIT                         
         B     *+6                                                              
*                                                                               
CSKXEQ   CR    RB,RB                EXIT CC EQ                                  
*                                                                               
         ST    R2,DPOINTER                                                      
         J     EXIT                                                             
*                                                                               
CSKERR   MVC   ERROR,=Y(BADSKDT)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
TOOMANY  DC    H'0'                                                             
*                                                                               
TOOFEW   MVC   ERROR,=Y(MISSSPOT)                                               
         GOTO1 SENDMSG                                                          
*                                                                               
SKJAN01  DC    C'SKJAN01=XX'                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* RETURN NEW BUY/MG DATA                                          *             
*=================================================================*             
         SPACE 1                                                                
SENDBMG  NTR1  BASE=*,LABEL=*                                                   
         LHI   R1,H06Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
         LHI   R1,11               SEND BUYLINE                                 
         LA    R4,BBUYLIN                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         CLI   SVRCVEL+1,H44Q      SEND MGCODE (IF THIS WAS A MG)               
         BE    *+12                                                             
         CLI   SVRCVEL+1,H35Q      SEND MGCODE (IF THIS WAS A MG)               
         BNE   SB10                                                             
         LHI   R1,13                                                            
         LHI   R5,2                                                             
         LA    R4,BMGCODE                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
SB10     LHI   R1,14               SEND START DATE                              
         LA    R4,BSTDATE                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LHI   R1,15               SEND END DATE                                
         LA    R4,BEDDATE                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LHI   R1,16               SEND NPW                                     
         LA    R4,BNPW                                                          
         BRAS  RE,SENDD                                                         
*                                                                               
         LHI   R1,17               SEND COST                                    
         LA    R4,BCOST                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         LHI   R1,9                SEND INV NTWK FOR CROSS NTWK MKGDS           
         LHI   R5,3                                                             
         LA    R4,IVCBLNET                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
* 10NOV04 - I'M NOT REALLY SURE WHY THIS CODE IS HERE, I THINK YOU CAN          
*           ONLY BE HERE FOR THESE MAP CODES. (I DON'T HAVE THE GUTS            
*            TO REMOVE IT!)                                                     
         CLI   SVRCVEL+1,H35Q      ONLY SEND SPOTS FOR NEW MG                   
         BE    SB11                                                             
         CLI   SVRCVEL+1,H44Q      ONLY SEND SPOTS FOR NEW MG                   
         BE    SB11                                                             
         CLI   SVRCVEL+1,H37Q      OR NEW BUY                                   
         BNE   SB100                                                            
*                                                                               
SB11     BRAS  RE,GETBUY           RTFB                                         
         L     R6,AIO1                                                          
*                                                                               
         MVC   HALF,BDNTAX         SAVE OFF TAX                                 
         XC    BDNTAX,BDNTAX        AND HAVE GETRATE IGNORE IT                  
         GOTO1 VGETRATE,DMCB,SPOTS,AIO1,0                                       
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+18                NO                                           
         TM    BDCIND,X'10'        TEST RATE TYPE = NET                         
         BZ    *+10                NO                                           
         MVC   GROSS,NET           YES - SET GROSS = NET                        
*                                                                               
         MVC   BDNTAX,HALF                                                      
         LHI   R1,32               SEND NET COST                                
         LA    R4,NET                                                           
         BRAS  RE,SENDD                                                         
         XC    LADATE,LADATE                                                    
*                                                                               
         CLI   SVCOST2,C'Y'        IF COS2 ON CLT REC=Y/O/T, SEND IT            
         BE    SB11A                                                            
         CLI   SVCOST2,C'O'                                                     
         BE    SB11A                                                            
         CLI   SVCOST2,C'T'                                                     
         BNE   SB11B                                                            
*                                                                               
SB11A    MVC   HALF,BDNTAX         SAVE OFF TAX                                 
         XC    BDNTAX,BDNTAX        AND HAVE GETRATE IGNORE IT                  
*                                                                               
         MVC   SPOTS(4),=C'COS2'                                                
         GOTO1 VGETRATE,DMCB,SPOTS,AIO1,0                                       
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+18                NO                                           
         TM    BDCIND,X'10'        TEST RATE TYPE = NET                         
         BZ    *+10                NO                                           
         MVC   GROSS,NET           YES - SET GROSS = NET                        
*                                                                               
         MVC   BDNTAX,HALF                                                      
*                                                                               
         LHI   R1,30               SEND COST 2                                  
         LA    R4,GROSS                                                         
         BRAS  RE,SENDD                                                         
         LHI   R1,31               SEND NET COST 2                              
         LA    R4,NET                                                           
         BRAS  RE,SENDD                                                         
*                                                                               
SB11B    LA    R1,H08Q             FIND SPOT DETAIL HEADER                      
         BRAS  RE,SENDH                                                         
*                                                                               
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,11                                                        
         LA    R6,BDELEM                                                        
         USING REGELEM,R6                                                       
*                                                                               
SB12     BRAS  RE,NEXTEL                                                        
         BNE   SB100                                                            
*                                                                               
         LA    R4,BLOCK                                                         
         XC    BLOCK,BLOCK                                                      
*                                                                               
         MVI   0(R4),SEMICOL       FOR SEQ NUM                                  
         AHI   R4,1                                                             
*                                                                               
         MVI   0(R4),C'Y'          SET MATCHED                                  
         TM    FLAGS,FLNOMAT       SKIP MATCH?                                  
         BZ    *+8                                                              
         MVI   0(R4),C'N'          SET NOT MATCHED                              
         BRAS  RE,SETDLMS                                                       
*                                                                               
         BRAS  RE,SETDLMS          FOR MG CODE                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(2,RDATE),(X'20',DUB)                               
         MVC   0(4,R4),DUB+2       MOVE MMDD                                    
         MVC   4(2,R4),DUB         MOVE YY                                      
         AHI   R4,5                                                             
         BRAS  RE,SETDLMS                                                       
*                                                                               
         CLC   LADATE,RDATE        SAME DATE AS LAST SPOT?                      
         BE    *+14                 YES                                         
         MVC   LADATE,RDATE                                                     
         MVI   SPOTNUM+1,0                                                      
         ZIC   R0,SPOTNUM+1                                                     
         AHI   R0,1                                                             
         STC   R0,SPOTNUM+1                                                     
         BRAS  RE,SETNUM                                                        
*                                                                               
         LA    R1,10(R6)                                                        
         CLI   ELCDLO,11           POL EL?                                      
         BE    *+12                 YES                                         
         L     R1,AIO1                                                          
         AHI   R1,BUYKPRD-BUYKEY                                                
         BRAS  RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)                                                    
         AHI   R4,2                POINT TO END                                 
         BRAS  RE,SETDLMS                                                       
*                                                                               
         CLI   ELCDLO,11                                                        
         BE    SB20                                                             
         CLI   RLEN,14             TEST PIGGYBACK                               
         BNH   SB20                 NO                                          
         LA    R1,14(R6)                                                        
         CLI   0(R1),0                                                          
         BE    SB20                                                             
         BRAS  RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)                                                    
*                                                                               
SB20     AHI   R4,2                                                             
         BRAS  RE,SETDLMS                                                       
*                                                                               
**NOP    CLI   QMED,C'N'           DON'T SEND COST OVERRIDES FOR MED N          
**NOP    BE    *+12                                                             
         TM    RSTATUS,X'20'       TEST COST OVERRIDE                           
         BO    SB30                                                             
         BRAS  RE,SETDLMS          IF NO OVRD, SEND DLM FOR GROSS               
         BRAS  RE,SETDLMS           AND ANOTHER FOR NET                         
         B     SB40                                                             
*                                                                               
SB30     L     RF,AIO1                                                          
         USING BUYRECD,RF                                                       
         MVC   BYTE,BDPURP                                                      
         OC    BUYKMKT,BUYKMKT     MKT 0 (NETWORK)?                             
         BNZ   *+16                 NO                                          
         TM    BDSTAT3,BDST3_CNNEW NETWORK COST OVERRIDES ALLOWED?              
         BZ    *+8                  NO                                          
         MVI   BDPURP,X'FD'                                                     
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETRATE,DMCB,(X'FF',SPOTS),AIO1,(R6)                            
*                                                                               
         L     RF,AIO1             A(BUY RECORD)                                
         USING BUYRECD,RF          BUY RECORD DSECT                             
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+18                NO                                           
         TM    BDCIND,X'10'        TEST RATE TYPE = NET                         
         BZ    *+10                NO                                           
         MVC   GROSS,NET           YES - SET GROSS = NET                        
         DROP  RF                  DROP BUY RECORD USING                        
*                                                                               
         L     RF,AIO1                                                          
         MVC   BDPURP-BUYRECD(1,RF),BYTE                                        
         ICM   R0,15,GROSS                                                      
         EDIT  (R0),(10,(R4)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R4,R0                                                            
         MVI   0(R4),SEMICOL                                                    
         LA    R4,1(R4)                                                         
         ICM   R0,15,NET                                                        
         EDIT  (R0),(10,(R4)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R4,R0                                                            
         MVI   0(R4),SEMICOL                                                    
         LA    R4,1(R4)                                                         
*                                                                               
SB40     BRAS  RE,SETDLMS          DLM FOR OTO                                  
         BRAS  RE,SETDLMS          DLM FOR PAID                                 
*                                                                               
         LA    R0,BLOCK                                                         
         SR    R0,R4                                                            
         LPR   R5,R0               SET OUTPUT LENGTH                            
         LA    R4,BLOCK                                                         
         LA    R1,H08SPDET         SPOT DETAIL DATA CODE                        
         BRAS  RE,SENDD                                                         
         B     SB12                                                             
*                                                                               
SB100    DS    0H                                                               
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* PUT DATA IN AIO TO GLOBBER AND PASS CONTROL TO SPOT BUY         *             
*=================================================================*             
         SPACE 1                                                                
CALLBUY  NTR1  BASE=*,LABEL=*                                                   
         MVI   SVXFROV,X'20'       RETURN CONTROL TO THIS OVLY                  
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'CLEAR'                                          
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
         GOTO1 (RF),(R1),=C'PUTD',BUYDATA1,78,GLVBUY1                           
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
         OC    BUYDATA2,BUYDATA2                                                
         BZ    CB20                                                             
         GOTO1 (RF),(R1),,BUYDATA2,,GLVBUY2                                     
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
         OC    BUYDATA3,BUYDATA3                                                
         BZ    CB20                                                             
         GOTO1 (RF),(R1),,BUYDATA3,,GLVBUY3                                     
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
         OC    BUYDATA4,BUYDATA4                                                
         BZ    CB20                                                             
         GOTO1 (RF),(R1),,BUYDATA4,,GLVBUY4                                     
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
CB20     BRAS  RE,GTPOLPRD         SEE IF POL EST OPEN                          
*                                                                               
         XC    BLOCK,BLOCK                                                      
         USING MAKDATAD,R1                                                      
         LA    R1,BLOCK                                                         
         MVC   MAKMED,QMED                                                      
         MVC   MAKBUYER,QBYR                                                    
         MVC   MAKCLT,QCLT                                                      
* GUESS WHICH PRODUCT TO PUT IN THE HEADLINES                                   
         MVC   MAKPRD,=C'POL'                                                   
         CLI   BPOLPRD,X'FF'       TEST POL EST OPEN                            
         BNE   CB22                 NO - USE BRAND                              
         CLC   QPRD2,SPACES        IS THERE A PIGGY?                            
         BH    CB24                 YES - ALWAYS USE POL                        
         CLI   SVCPROF+0,C'0'      TEST BRAND POL CLIENT                        
         BE    CB24                 NO - USE POL                                
*                                                                               
CB22     MVC   MAKPRD,QPRD         USE BRAND                                    
*                                                                               
CB24     SR    R0,R0                                                            
         IC    R0,BUYEST           GET CURRENT ESTIMATE NUMBER                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MAKEST,DUB                                                       
*                                                                               
         MVC   MAKSTA(5),QSTA                                                   
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   MAKSTA+4,C' '       DON'T SEND MEDIA N                           
         LA    RF,BYCBLNET         BUY CABLE NETWORK                            
         CLI   SVRCVEL+1,H44Q      ADD NEW MAKEGOOD ACROSS MONTH?               
         BE    CB25                YES                                          
         CLI   SVRCVEL+1,H35Q      ADD NEW MAKEGOOD?                            
         BE    CB25                YES                                          
         CLI   SVRCVEL+1,H37Q      NEW BUY?                                     
         BNE   *+8                 NO                                           
CB25     LA    RF,IVCBLNET         INVOICE CABLE NETWORK                        
         CLC   0(3,RF),SPACES      HAVE CABLE NETWORK?                          
         BNH   *+14                                                             
         MVI   MAKSTA+4,C'/'                                                    
         MVC   MAKSTA+5(3),0(RF)                                                
*                                                                               
         MVC   MAKOPTNG,BNOGOL     NO GOALS OPTION                              
         MVC   MAKOPTND,BNODEM     NO DEMOS OPTION                              
         MVC   MAKB0PRF,PROFB0+9   PURPOSE CODES REQUIRED                       
         MVC   MAKOPTID,QBUYID                                                  
         MVC   MAKREP,QREP                                                      
         LR    RE,RA                                                            
         AHI   RE,SVRCD-TWAD                                                    
         MVC   MAKRSNCD(L'SVRCD),0(RE)                                          
         DROP  R1                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BLOCK,MAKDATAL,GLVSMSG                    
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'MAK'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'BUY'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
CBX      J     EXIT                                                             
*                                                                               
GLBERR   DCHO                                                                   
*                                                                               
* KEEP THIS IN SYNC WITH SAME DSECT IN SPBUY00                                  
*                                                                               
MAKDATAD DSECT                                                                  
MAKMED   DS    CL1                                                              
MAKBUYER DS    CL12                                                             
MAKCLT   DS    CL3                                                              
MAKPRD   DS    CL3                                                              
MAKEST   DS    CL3                                                              
MAKSTA   DS    CL8                                                              
MAKPRD2  DS    CL3                                                              
MAKEST2  DS    CL3                                                              
MAKOPTNG DS    CL1                 Y=NOGOAL                                     
MAKB0PRF DS    CL1                 PROF_B0+9 - PURPOSE CODES REQUIRED           
MAKOPTID DS    CL12                CANADIAN ID                                  
MAKREP   DS    CL3                 REP                                          
MAKRSNCD DS    CL6                 REASON CODE                                  
MAKRSNTX DS    CL40                RC TEXT                                      
MAKOPTND DS    CL1                 Y=NODEMO                                     
MAKDATAL EQU   *-MAKDATAD                                                       
*                                                                               
T22820   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* FIND THIS RUN NOT ORDERED SPOT IN AN INVOICE RECORD AND UPDATE *              
* MATCHING DATA                                                  *              
*                                                                *              
* ON PASS 1, SKIP MATCHED INVOICE DETAIL ELEMS TO SEE IF WE FIND *              
* ANOTHER DETAIL THAT FITS.  IF NOT, USE IT ON PASS 2            *              
*                                                                *              
* AIO2 = INVOICE REOCRD                                          *              
* AIO1 = BUY REOCRD                                              *              
*                                                                *              
* CONVERT AFFID TO INVOICE DETAIL ELEMENT FORMAT                 *              
* DAY  = RELATIVE TO FIRST DAY OF INVOICE                        *              
* TIME = TIME IN MINUTES FROM 600A                               *              
* R2 POINTS TO AFFID IN BDATA:                                   *              
*  'A'DD 'T'TTPPCCCC 'I'INVOICENUM                               *              
*     TT    IIRROOOO                                             *              
*           MMD2SSSS                                             *              
*                                                                *              
* R5 POINTS TO REGEL TABLE  DATE(2)                              *              
* SPOTNUM = SPOTNUM BEING MATCHED TO AFFID                       *              
*                                                                *              
* TURN ON WIP FLAG IN FILE AND DIRECTORY STATUS BYTES            *              
*================================================================*              
         SPACE 1                                                                
SNVUPD   NTR1  BASE=*,LABEL=*                                                   
         CLI   SVRCVEL+1,H44Q      NEW MAKEGOOD ACROSS MONTHS?                  
         BE    SNVX                 YES - NO INVOICE/BUY TO UPDATE              
         MVI   PASSFLAG,PF1                                                     
* (NOTE - COMPUTE RELAFDAY AFTER PROCESS INVOICE HEADER)                        
         SR    R1,R1                                                            
         ICM   R1,3,4(R2)          GET AFFID TIME                               
         CHI   R1,600                                                           
         BNL   *+8                                                              
         AHI   R1,2400                                                          
         AHI   R1,-600             600A IS 0                                    
         SR    R0,R0                                                            
         D     R0,=F'100'          MIN IN R0, HRS IN R1                         
         MHI   R1,60                                                            
         AR    R0,R1               GIVES MINUTES FROM 6A                        
         STCM  R0,3,RELAFTIM                                                    
*                                                                               
SNV1     XC    KEY,KEY                                                          
         MVC   KEY(L'SNVKMAST),SVSNVKEY                                         
         CLI   SVRCVEL+1,H3CQ      UNMATCH?                                     
         BE    SNV1A                YES                                         
         CLI   SVRCVEL+1,H3DQ      IS THIS EMAIL RNO SPOTS?                     
         BE    SNV1A                YES                                         
         CLI   SVRCVEL+1,H47Q      IS THIS CHANGE RNO DATA?                     
         BNE   SNV2                 NO                                          
*                                                                               
SNV1A    LA    R1,12(R2)           NOW ALWAYS 2 PRDS                            
         CLI   0(R1),C'I'          MAKE SURE!                                   
         BE    *+6                                                              
         DCHO                                                                   
         MVC   KEY+SNVKINV-SNVKEY(10),1(R1)                                     
         OC    KEY+SNVKINV-SNVKEY(10),SPACES                                    
*                                                                               
SNV2     MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
         MVC   SAVEDA,KEY+(SNVDDA-SNVKEYD)                                      
         B     SNV6                                                             
*                                                                               
SNV4     GOTO1 SEQ                                                              
*                                                                               
SNV6     LA    R1,9                KEY COMPARE LEN (-1)                         
         CLI   SVRCVEL+1,H3CQ      IS THIS UNMATCH?                             
         BE    SNV6A                YES                                         
         CLI   SVRCVEL+1,H3DQ      IS THIS EMAIL RNO SPOTS?                     
         BE    SNV6A                NO                                          
         CLI   SVRCVEL+1,H47Q      IS THIS CHANGE RNO DATA?                     
         BNE   *+8                  NO                                          
SNV6A    LA    R1,21                                                            
*                                                                               
         EX    R1,*+8              0E03/A-M/CLT/STA/MOS                         
         B     *+10                 OR 0E03/A-M/CLT/STA/MOS/INV                 
         CLC   KEY(0),KEYSAVE                                                   
         BE    SNV8                                                             
*                                                                               
         TM    PASSFLAG,PF2        ALREADY ON PASS 2?                           
         BZ    *+6                                                              
         DCHO                       YES - DIE ON AFFID NOT FOUND                
         TM    PASSFLAG,PFFOUND     NO - A PRIOR MATCHED DET FIT?               
         BNZ   SNV7                  YES - GO BACK AND USE IT                   
         MVC   ERROR,=Y(BADNVDET)                                               
         GOTO1 SENDMSG                                                          
*                                                                               
SNV7     OI    PASSFLAG,PF2                                                     
         B     SNV1                                                             
*                                                                               
SNV8     L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         USING SNVKEYD,R6                                                       
         LA    R6,SNVELS                                                        
         B     SNV12                                                            
*                                                                               
SNV10    SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    SNV4                                                             
*                                                                               
SNV12    CLI   0(R6),SNVIDELQ      INVOICE DETAIL ELEM (X'40')                  
         BE    SNV30                                                            
         CLI   0(R6),SNVHDELQ      TEST HEADER ELEM (X'10')                     
         BE    SNV20                                                            
         B     SNV10                                                            
*                                                                               
         USING SNVHDELD,R6                                                      
SNV20    DS    0H                  PROCESS INVOICE HEADER ELEMENT               
         CLI   SVRCVEL+1,H47Q      IS THIS CHANGE RNO DATA?                     
         BNE   *+12                 NO                                          
         BRAS  RE,TESTPAID         TEST INVOICE PAID                            
         BRAS  RE,TESTESTL         TEST ESTIMATE LOCKED                         
*                                                                               
         MVC   INVHDPRD,SNVHDPRD   SAVE HEADER PRDS/EST                         
         MVC   INVHDPR2,SNVHDPR2                                                
         MVC   INVHDEST,SNVHDEST                                                
         MVI   INVCOSTY,C'G'       SET FOR GROSS COSTS                          
         TM    SNVHDCTL,SNVHDNTQ+SNVHDDNQ   TEST INVOICE IS AT NET              
         BZ    *+8                                                              
         MVI   INVCOSTY,C'N'                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(2,SNVHDSDT),INVSDATE GET START YYMMDD              
         GOTO1 (RF),(R1),(2,1(R2)),DUB            GET AFFID YYMMDD              
         GOTO1 VPERVERT,DMCB,INVSDATE,DUB                                       
         LH    R0,8(R1)                                                         
         BCTR  R0,0                                                             
         STC   R0,RELAFDAY                                                      
         B     SNV10                                                            
*                                                                               
         USING SNVIDELD,R6                                                      
SNV30    DS    0H                    PROCESS INVOICE DETAIL ELEMENT             
         CLC   RELAFDAY(3),SNVIDDAY  SAME DAY/TIME                              
         BE    SNV30A                YES                                        
         CLC   RELAFDAY,SNVIDDAY     SAME DAY?                                  
         BNE   SNV10                 NO                                         
         CLC   SNVIDTIM,=X'05A0'     <= 24 HOURS?                               
         BNH   SNV10                 YES                                        
         XR    RE,RE                 CLEAR RE                                   
         ICM   RE,3,RELAFTIM         AFFID TIME                                 
         AHI   RE,X'05A0'            ADD 24 HOURS (INV MANAGER/EASI)            
         STCM  RE,3,HALF             HALF = TIME + 24 HOURS                     
         CLC   SNVIDTIM,HALF         DO THE TIMES MATCH NOW?                    
         BNE   SNV10                 NO                                         
*                                                                               
* MAKE SURE SAME NETWORK IF THERE IS ONE                                        
SNV30A   LA    RF,INVBCNET           INVOICE CABLE NETWORK                      
         CLI   SVRCVEL+1,H35Q        ADD NEW MAKEGOOD?                          
         BE    *+8                   YES - USE INVOICE CABLE NETWORK            
         CLI   SVRCVEL+1,H37Q        NEW BUY?                                   
         BE    *+8                   YES - USE INVOICE CABLE NETWORK            
         CLI   SVRCVEL+1,H47Q        CHANGE RNO DATA?                           
         BE    *+8                   YES - USE INVOICE CABLE NETWORK            
         LA    RF,BBCBLNET           BUY CABLE NETWORK                          
         CLC   SNVIDNWK,0(RF)        SAME CABLE NETWORK?                        
         BNE   SNV10                 NO                                         
*                                                                               
         CLI   6(R2),0             TEST AFFID HAS PRODUCT                       
         BE    SNV32                NO                                          
* MATCH TO DETAIL OR HEADER PRD                                                 
         LA    RE,SNVIDPRD         POINT TO PRD IN DETAIL                       
         CLI   0(RE),0             IS IT THERE                                  
         BNE   *+8                 YES                                          
         LA    RE,INVHDPRD         POINT TO SAVED HEADER PRD                    
*                                                                               
* IF THIS IS AN UNMATCH, AND THE PRD IS POL, SEE IF THE AFFID                   
* DETAILS MATCH THE SPOT - IF SO, THEN THIS IS THE ONE!                         
         CLI   SVRCVEL+1,H3CQ      IS THIS UNMATCH?                             
         BNE   SNV31                NO                                          
*                                                                               
* IF IT'S AN UN-MATCH, AND THIS DETAIL POINTS TO THE SPOT, I DON'T              
* CARE WHAT THE DAMN PRODUCT IS (SINCE THE I2 WILL OPTIONALLY                   
* MATCH ON ALL BUT PRODUCT!)                                                    
**NOP         CLI   0(RE),X'FF'         IS IT A FUCKING POL AFFID               
**NOP         BNE   SNV31                NO                                     
         CLC   SNVIDBES,BUYEST     ESTIMATE NUMBER                              
         BNE   SNV31                NO, THIS ISN'T IT!                          
         OC    SNVIDTBL,SNVIDTBL   HAVE A 2-BYTE BUYLINE NUMBER?                
         BZ    *+14                NO                                           
         CLC   SNVIDTBL,BBUYLIN    MATCH ON 2-BYTE BUYLINE?                     
         B     *+10                                                             
         CLC   SNVIDBLN,BBUYLIN+1  MATCH ON 1 BYTE BUYLINE?                     
         BNE   SNV31                NO, THIS ISN'T IT!                          
         CLC   SNVIDBNO,SPOTNUM+1  SPOT NUMBER                                  
         BNE   SNV31                NO, THIS ISN'T IT!                          
         CLC   SNVIDBDT,0(R5)                                                   
         BE    SNV33                NO, THIS ISN'T IT!                          
*                                                                               
SNV31    CLC   6(1,R2),0(RE)       MATCH PRODUCT                                
         BNE   SNV10                                                            
*                                                                               
SNV32    CLI   SVREASON,SVRPASS1   TEST FIRST PASS                              
         BNE   *+6                 6/6/00 - THIS CAN'T HAPPEN (EJOR)            
         DCHO                                                                   
         OI    PASSFLAG,PFFOUND    SET THAT WE FOUND A MATCHING AFFID           
*                                                                               
* PASS 2 - MOVE MATCH DATA TO RUN NOT ORDERED SPOT                              
         CLI   SVRCVEL+1,H3DQ      IS THIS EMAIL RNO?                           
         BNE   SNV33                                                            
         TM    PASSFLAG,PF2        PASS 2?                                      
         BNZ   *+12                 YES - SKIP MATCHED TEST                     
         CLI   SNVIDBES,0          IS IT A MATCHED AFFID?                       
         BNE   SNV10                YES - FIND THE RIGHT ONE!                   
         OI    SNVIDCT2,SNVIDEMS   EMAIL SENT FLAG                              
         B     SNV36                                                            
*                                                                               
SNV33    CLI   SVRCVEL+1,H47Q      IS THIS CHANGE RNO DATA?                     
         BNE   SNV33K                                                           
         TM    PASSFLAG,PF2        PASS 2?                                      
         BNZ   *+12                 YES - SKIP MATCHED TEST                     
         CLI   SNVIDBES,0          IS IT A MATCHED AFFID?                       
         BNE   SNV10                YES - FIND THE RIGHT ONE!                   
*                                                                               
***      CLC   SNVIDCST,8(R2)      DOES THE COST MATCH?                         
         ICM   R1,15,SNVIDCST                                                   
         CLI   INVCOSTY,C'G'       INVOICE COST IN GROSS?                       
         BE    SNV33D               YES, LEAVE IT ALONE                         
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   SNV33DA             NO - GROSS UP NET$                           
         L     RF,AIO2             A(INVOICE RECORD)                            
         MVC   FULL(2),SNVKMOS-SNVKEY(RF) INVOICE MOS                           
         XC    FULL(2),=X'FFFF'    XC MOS TO GET COMPRESSED DATE                
         CLC   FULL(2),=X'F021'    JAN/2020 OR HIGHER?                          
         BNL   SNV33D              YES - DO NOT GROSS UP!                       
*                                                                               
SNV33DA  M     R0,=F'200'           ELSE GROSS IT UP (MM ONLY IN GROSS)         
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
SNV33D   CLM   R1,15,8(R2)         DOES THE COST MATCH?                         
         BNE   SNV10                NO - WRONG AGAIN!                           
*                                                                               
         CLI   AFCOST,X'FF'        DID WE GET A CHANGED AFFID COST?             
         BE    SNV33G               NO                                          
         ICM   R1,15,AFCOST                                                     
         CLI   INVCOSTY,C'G'       INVOICE COSS IN GROSS?                       
         BE    SNV33F               YES - LEAVE IT ALONE                        
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   SNV33FA             NO - GROSS UP NET$                           
         L     RF,AIO2             A(INVOICE RECORD)                            
         MVC   FULL(2),SNVKMOS-SNVKEY(RF) INVOICE MOS                           
         XC    FULL(2),=X'FFFF'    XC MOS TO GET COMPRESSED DATE                
         CLC   FULL(2),=X'F021'    JAN/2020 OR HIGHER?                          
         BNL   SNV33F              YES - DO NOT GROSS UP!                       
*                                                                               
SNV33FA  M     R0,=F'85'            ELSE NET DOWN COST RCVD FROM MM             
         AHI   R1,50                                                            
         D     R0,=F'100'          RESULT IN R1                                 
*                                                                               
SNV33F   ICM   R0,15,SNVIDCST      SAVE ORIGINAL AFFID COST                     
         STCM  R1,15,SNVIDCST      UPDATE THE AFFID COST                        
         SR    R1,R0                COMPUTE THE DIFFERENCE TO UPDATE            
         CVD   R1,COSTDIFF          HEADER COST AND SAVE PACKED                 
         OI    SNVIDCT2,SNVIDMMC   SET COST CHANGED BY MM                       
         CLI   SNVIDLEN,SNVIDL3Q   NEW LENGTH ELEM?                             
         BL    SNV33G               NO, NO ORIGINAL COST                        
         OC    SNVIDOR$,SNVIDOR$   IS THERE ALREADY AN ORIGINAL COST?           
         BNZ   SNV33G                                                           
         MVC   SNVIDOR$,=X'FFFFFFFF'                                            
         LTR   R0,R0               ORIGINAL COST = 0?                           
         BZ    SNV33G                                                           
         STCM  R0,15,SNVIDOR$                                                   
*                                                                               
SNV33G   BRAS  RE,SETOVR           SET OVERRIDE FLAGS                           
*                                                                               
         XC    SNVIDBES,SNVIDBES   CLEAR ANY MATCH INFO (JIC)                   
         XC    SNVIDBLN,SNVIDBLN                                                
         XC    SNVIDBDT,SNVIDBDT                                                
         XC    SNVIDBNO,SNVIDBNO                                                
         BRAS  RE,SAVECOST         RESTORE READ SEQ AND UPDATE COSTS            
         B     SNV38                                                            
*                                                                               
SNV33K   TM    FLAGS,FLNOMAT       SKIP MATCH?                                  
         BNZ   SNV50                YES                                         
*                                                                               
         CLI   SVRCVEL+1,H3CQ      IS THIS UNMATCH?                             
         BNE   SNV34                NO                                          
         CLI   SNVIDBES,0          IS IT A MATCHED AFFID?                       
         BE    SNV10                NO - FIND THE RIGHT ONE!                    
         XC    SNVIDBES,SNVIDBES                                                
         XC    SNVIDBLN,SNVIDBLN                                                
         XC    SNVIDBDT,SNVIDBDT                                                
         XC    SNVIDBNO,SNVIDBNO                                                
         B     SNV36                                                            
*                                                                               
SNV34    TM    PASSFLAG,PF2        PASS 2?                                      
         BNZ   *+12                 YES - SKIP MATCHED TEST                     
         CLI   SNVIDBES,0          IS IT A MATCHED AFFID?                       
         BNE   SNV10                YES - FIND THE RIGHT ONE!                   
         MVC   SNVIDBES,BUYEST     ESTIMATE NUMBER                              
         CLI   BBUYLIN,0           HAVE A REAL 2-BYTE BUYLINE?                  
         BNE   *+10                YES - DON'T EVEN SET 1-BYTE BUYLINE          
         MVC   SNVIDBLN,BBUYLIN+1  1-BYTE BUYLINE                               
         MVC   SNVIDTBL,BBUYLIN    2-BYTE BUYLINE                               
         MVC   SNVIDBDT,0(R5)      SPOT DATE                                    
         MVC   SNVIDBNO,SPOTNUM+1  SPOT NUMBER                                  
*                                                                               
         BRAS  RE,SETOVR           SET OVERRIDE FLAGS                           
         BNE   *+8                 NO FLAGS SET                                 
         BRAS  RE,SAVECOST         CALL TO DEAL WITH ACTIVITY                   
         DROP  R6                                                               
*                                                                               
SNV36    MVI   RDUPDATE,C'Y'                                                    
         GOTO1 PUTREC                                                           
         CLI   SVRCVEL+1,H3CQ      IS THIS UNMATCH?                             
         BE    SNVX                 YES - DONE!                                 
*                                                                               
SNV38    MVI   BYTE,X'40'          SET WIP FLAG                                 
         BRAS  RE,UPDINVS          UPDATE INVOICE KEYS/RECS                     
         BRAS  RE,UPDPASS          UPDATE PASSIVE POINTERS                      
         SPACE 1                                                                
*==============================================================*                
* NOW INSERT AFFIDAVIT ELEMENT IN BUYREC                       *                
*    ***** NOTE ***** ***** NOTE ***** ***** NOTE *****        *                
* IF THIS IS A NEW BUY/NEW MAKEGOOD,                           *                
* DON'T SKIP ADDING AFFID ELEM EVEN WHEN NOT MATCHING - WE     *                
* NEED THEM TO KNOW WHICH SPOTS NOT TO DELETE IN ADJSPOTS AND  *                
* WILL DELETE THEM THERE.                                      *                
*==============================================================*                
         SPACE 1                                                                
         USING SNVIDELD,R3                                                      
SNV50    LR    R3,R6               SAVE A(DETAIL ELEM)                          
         MVI   XSP,C'N'                                                         
         CLI   SVRCVEL+1,H3DQ      IS THIS EMAIL RNO?                           
         BE    SNVX                                                             
         CLI   SVRCVEL+1,H47Q      IS THIS CHANGE RNO DATA?                     
         BE    SNVX                                                             
         CLI   SVRCVEL+1,H33Q      IS THIS A CHANGE BUY?                        
         BNE   *+12                 NO MUST BE NEW BUY/NEW MG/+OTO              
         TM    FLAGS,FLNOMAT       SKIP MATCH?                                  
         BNZ   SNVX                 YES - DON'T ADD AFFID EL                    
*                                                                               
         CLI   BUYEST,0    <<<< TRAP THIS CONDITION                             
         BNE   *+6                                                              
         DCHO                                                                   
         BRAS  RE,GETBUY                                                        
*                                                                               
         L     R6,AIO                                                           
         LA    R6,BDELEM-BUYRECD(R6)                                            
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,12                                                        
*                                                                               
         USING REGELEM,R6                                                       
SNV52    BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DCHO                                                                   
         CLC   0(2,R5),2(R6)       MATCH DATE                                   
         BNE   SNV52                                                            
         SR    RF,RF                                                            
*                                                                               
SNV54    TM    6(R6),X'80'         TEST MINUS                                   
         BO    *+8                                                              
         AHI   RF,1                                                             
         CLM   RF,1,SPOTNUM+1      MATCH SPOTNUM                                
         BE    SNV56                                                            
         BRAS  RE,NEXTEL                                                        
         BE    SNV54                                                            
         DCHO                                                                   
*                                                                               
SNV56    TM    6(R6),X'40'         TEST MINUSED                                 
         BZ    *+6                                                              
         DC    H'0'                CAN'T MATCH A MISSED SPOT!                   
*                                                                               
         TM    FLAGS,FLNOMAT       IF THIS IS CREDIT FEATURE,                   
         BNZ   SNV60                ALL SPOTS HAVE SAME COST                    
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+12                 NO                                          
         CLI   QMED,C'N'            YES - NO OVERRIDES FOR MED N                
         BE    SNV60                                                            
*                                                                               
         L     RF,AIO                                                           
         LA    RF,BDNTAX-BUYRECD(RF)                                            
         MVC   HALF,0(RF)          SAVE OFF TAX                                 
         XC    0(L'BDNTAX,RF),0(RF)  AND HAVE GETRATE IGNORE IT!                
         GOTO1 VGETRATE,DMCB,SPOTS,AIO,0                                        
*                                                                               
         L     RF,AIO1             A(BUY RECORD)                                
         USING BUYRECD,RF          BUY RECORD DSECT                             
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+18                NO                                           
         TM    BDCIND,X'10'        TEST RATE TYPE = NET                         
         BZ    *+10                NO                                           
         MVC   GROSS,NET           YES - SET GROSS = NET                        
         DROP  RF                  DROP BUY RECORD USING                        
*                                                                               
         L     RF,AIO                                                           
         MVC   BDNTAX-BUYRECD(L'BDNTAX,RF),HALF                                 
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   SNV58                                                            
         CLI   QMED,C'N'           FOR NETWORK - DEFAULT IS DOLLARS             
         BNE   SNV57               UNLESS THE INDICATOR SAYS NOT                
         L     RF,AIO                                                           
         USING BUYRECD,RF                                                       
         OC    BUYKMKTN,BUYKMKTN   EXCEPT FOR LOCAL MARKET EXPLODED BUY         
         BNZ   SNV57         WHICH ARE IN PENNIES UNLESS INDICATOR SAYS         
         TM    BDCIND2,BDCRATPQ                                     NOT         
         BNZ   SNV58                                                            
         B     SNV57A                                                           
*                                                                               
SNV57    TM    BDCIND2,BDCNBRDQ    FOR OTHER MEDIA DEFAULT IS PENNIES           
         BZ    SNV58               UNLESS THE INDICATOR SAYS NOT                
         DROP  RF                                                               
*                                                                               
SNV57A   BRAS  RE,FIXCOST                                                       
*                                                                               
SNV58    MVC   BCOST,GROSS                                                      
         CLI   SVRCVEL+1,H48Q      OVERRIDE MATCH CRITERIA?                     
         BE    SNV60                                                            
         TM    FLAGS,FLNOOVR       SKIP COST OVERRIDE?                          
         BNZ   SNV60                YES                                         
         CLC   GROSS,8(R2)         IS THERE AN OVERRIDE COST?                   
         BE    SNV60                NO                                          
         OC    RPAY,RPAY           NO CHANGES TO COST IF PAID!                  
         BZ    *+14                                                             
         MVC   ERROR,=Y(MISSSPOT)                                               
         B     SNV58A                                                           
*                                                                               
         CLI   8(R2),0             MAKE SURE OVRD COST FITS IN 3 BYTES!         
         BE    SNV59                                                            
         MVC   ERROR,=Y(INVOCOST)                                               
SNV58A   GOTO1 SENDMSG                                                          
*                                                                               
SNV59    OI    RSTATUS,RSRATOVQ    SET RATE OVERRIDE (X'20')                    
         MVC   RPCOST,9(R2)        SET OVERRIDE COST IN REGEL                   
*                                                                               
SNV60    XC    DUB,DUB             BUILD AFFIDAVIT ELEMENT                      
         MVI   DUB,X'10'                                                        
         MVI   DUB+1,6                                                          
         MVC   DUB+2(2),1(R2)      AFFID DATE                                   
         MVC   DUB+4(2),4(R2)      AFFID TIME                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               ADD AFTER REGEL                              
         CLI   0(R6),X'10'         IS THERE A SF AFFID ELEM?                    
         BNE   SNV60A               NO - ADD IT                                 
         MVC   0(6,R6),DUB          ELSE OVERWRITE IT                           
         B     SNV61                                                            
*                                                                               
SNV60A   GOTO1 VRECUP,DMCB,(0,AIO),DUB,(C'R',(R6))                              
         CLI   8(R1),C'R'                                                       
         BE    SNV61                                                            
         MVC   ERROR,=Y(BUYFUL1)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
SNV61    CLI   PROFTI+1,C'Y'       $MAT & I2 FILMS POSTED?                      
         BNE   SNV70                NO                                          
*                                                                               
         XC    DUB,DUB             BUILD FILM NUMBER ELEMENT                    
         MVI   DUB,X'12'                                                        
         MVI   DUB+1,5                                                          
         SR    R1,R1                                                            
         ICM   R1,1,SNVIDCML                                                    
         BZ    SNV70               NO ELEM IF NO FILM                           
         BRAS  RE,GETFLM                                                        
         BNE   SNV70               DON'T POST ON INVALID FILM                   
         MVC   DUB+3(2),0(R1)                                                   
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SNVIDCM2       ANY SECOND FILM?                             
         BZ    SNV62                NO - JUST ADD ELEM WITH ONE FILM            
         BRAS  RE,GETFLM                                                        
         BNE   SNV70               DON'T POST ON INVALID FILM                   
         MVI   DUB+1,7             SET LENGTH FOR 2 FILMS                       
         MVC   DUB+5(2),0(R1)                                                   
*                                                                               
SNV62    SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               ADD AFTER REGEL                              
         GOTO1 VRECUP,DMCB,(0,AIO),DUB,(C'R',(R6))                              
         CLI   8(R1),C'R'                                                       
         BE    SNV70                                                            
         MVC   ERROR,=Y(BUYFUL2)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
SNV70    BRAS  RE,PUTBUY           PUT THE BUY RECORD USING GETBUY              
                                                                                
SNVX     MVI   XSP,C'N'                                                         
         J     EXIT                                                             
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* READ A BUY INTO AIO1                                            *             
*=================================================================*             
GETBUY   NTR1  BASE=*,LABEL=*,WORK=(R2,8)                                       
*                                                                               
         USING GETBUYD,R2          GETBUY DSECT                                 
         XC    0(GETBUYL,R2),0(R2) CLEAR GETBUY BLOCK                           
         MVC   GBYCOMF,ACOMFACS    A(COMFACS)                                   
*                                                                               
         CLI   BUY1OR2,0           ALREADY INITIALIZED GETBUY?                  
         BNE   GETB1               YES                                          
         MVI   GBYACT,GBYINIT      INIT                                         
         MVC   GBYAGY,QAGY         AGENCY                                       
*                                                                               
         GOTO1 VGETBUY,0(R2)       INIT CALL TO SET 1 OR 2 BYTE BUYLINE         
*                                                                               
         CLC   QAGY,=C'SJ'         AGENCY SJ?                                   
         BE    *+14                YES                                          
         CLC   QAGY,=C'T1'         AGENCY T1?                                   
         BNE   GETB0               NO                                           
         CLC   =X'CC2B',BCLT       CLIENT TBL?                                  
         BNE   GETB0               NO                                           
         MVI   GBY1OR2,2           BUYLINES IN 2-BYTE MODE                      
*                                                                               
GETB0    MVC   BUY1OR2,GBY1OR2     SAVE 1 OR 2-BYTE BUYLINE MODE                
*                                                                               
GETB1    BRAS  RE,GTPOLPRD                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),BAGYMD                                                    
         MVC   KEY+1(2),BCLT                                                    
         MVC   KEY+3(1),BPOLPRD                                                 
         MVC   KEY+4(5),BMKTSTA                                                 
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    GETB2                                                            
         CLI   KEY+6,X'E8'         TEST CABLE                                   
         BL    GETB2                                                            
*                                                                               
         LA    RF,INVBCNET         USE INVOICE CABLE NETWORK                    
         CLI   SVRCVEL+1,H44Q      ADD NEW MAKEGOOD ACROSS MONTH?               
         BE    *+8                 YES                                          
         CLI   SVRCVEL+1,H35Q      ADD NEW MAKEGOOD?                            
         BE    *+8                 YES                                          
         CLI   SVRCVEL+1,H37Q      NEW BUY?                                     
         BE    *+8                 YES                                          
         LA    RF,BBCBLNET         NO - USE BUY CABLE NETWORK                   
         OC    KEY+8(1),0(RF)      CABLE NETWORK                                
*                                                                               
GETB2    MVC   KEY+9(1),BUYEST                                                  
         MVC   KEY+11(2),BBUYLIN   USE 2-BYTE BUYLINE                           
*                                                                               
         LA    RE,KEYSAVE          RE = KEYSAVE                                 
         ST    RE,GBYKEYIN         STORE A(KEYSAVE)                             
         LA    RE,KEY              RE = KEY                                     
         ST    RE,GBYKEYOT         STORE A(KEY)                                 
         MVI   GBYACT,GBYHIGH      READ HIGH                                    
         MVC   KEYSAVE,KEY         SAVE OFF THE KEY                             
         MVC   GBYDMIN,DMINBTS     PASS DMINBTS                                 
         MVC   GBYDMOUT,DMOUTBTS   PASS DMOUTBTS                                
         LA    RE,DMWORK           DMWORK                                       
         ST    RE,GBYDMWRK         STORE A(DMWORK)                              
         OI    GBYDMIN,X'80'       READ FOR UPDATE                              
         MVC   GBY1OR2,BUY1OR2     1 OR 2-BYTE BUYLINE MODE                     
         GOTO1 VGETBUY,0(R2)       CALL GETBUY                                  
         CLC   KEY(13),KEYSAVE     FOUND THE BUY RECORD?                        
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
*                                                                               
         LA    RE,KEY+14           A(DISK ADDRESS)                              
         ST    RE,GBYDA            STORE A(DISK ADDRESS)                        
         MVC   AIO,AIO1            AIO=AIO1                                     
         MVC   GBYIOA,AIO1         PUT THE BUY RECORD IN AIO1                   
         MVI   GBYACT,GBYGET       GETREC                                       
         GOTO1 VGETBUY,0(R2)       CALL GETBUY                                  
         J     EXIT                EXIT                                         
         DROP  R2                  DROP GETBUY USING                            
*=================================================================*             
* PUT BUY RECORD USING GETBUY INTERFACE                           *             
*=================================================================*             
PUTBUY   NTR1  BASE=*,LABEL=*,WORK=(R2,8)                                       
*                                                                               
         USING GETBUYD,R2          GETBUY DSECT                                 
         XC    0(GETBUYL,R2),0(R2) CLEAR GETBUY BLOCK                           
         MVC   GBYCOMF,ACOMFACS    A(COMFACS)                                   
*                                                                               
         LA    RE,KEY+14           A(DISK ADDRESS)                              
         ST    RE,GBYDA            STORE A(DISK ADDRESS)                        
         LA    RE,DMWORK           DMWORK                                       
         ST    RE,GBYDMWRK         STORE A(DMWORK)                              
         MVC   GBYIOA,AIO1         A(BUY RECORD) IN AIO1                        
         MVI   GBYACT,GBYPUT       PUTREC                                       
         MVI   HALF2,0             MAKE SURE WE DON'T PASS A PRD LIST           
         LA    RE,HALF2            A(NULL PRD LIST)                             
         ST    RE,GBYPRDL          STORE A(PRD LIST)                            
         MVC   GBY1OR2,BUY1OR2     1 OR 2-BYTE BUYLINE MODE                     
         MVC   GBYDMIN,DMINBTS     PASS DMINBTS                                 
         MVC   GBYDMOUT,DMOUTBTS   PASS DMOUTBTS                                
         OI    GBYDMIN,X'80'       UPDATE                                       
         GOTO1 VGETBUY,0(R2)       CALL GETBUY FOR PUTREC                       
         J     EXIT                EXIT                                         
         DROP  R2                  DROP GETBUY USING                            
*=================================================================*             
* GET POL PRODUCT                                                 *             
*=================================================================*             
         SPACE 1                                                                
GTPOLPRD NTR1  BASE=*,LABEL=*                                                   
         CLI   BPOLPRD,0           DO WE KNOW IF POL EST ?                      
         BNE   GPPX                                                             
         MVI   BPOLPRD,X'FF'       ASSUME POL WILL BE THERE                     
         CLI   BPRD,X'FF'          ARE WE DOING POL ?                           
         BE    GPPX                                                             
* READ FOR POL ESTIMATE                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),BUYEST                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GPPX                                                             
         MVC   BPOLPRD,BPRD       ELSE USE PRD                                  
*                                                                               
GPPX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* UPDATE INVOICE KEYS/RECS WITH WIP/MATCHED FLAG               *                
*  BYTE = X'80' MATCHED                                        *                
*         X'40' WIP                                            *                
*         X'02' APPROVED                                       *                
*         X'FD' SET UN-APPROVED                                *                
*         X'7F' SET UNMATCHED                                  *                
*                                                              *                
*  STAT1= SNVMMFLM X'20' FILM ANALYSIS ERRORS                  *                
*         SNVMMHVE X'10' HORIZONTAL/VERTICAL ERRORS            *                
*         SNVMMSSE X'08' SECONDARY SEPARATION ERRORS           *                
*                                                              *                
* *** NOTE *** IF L_NOSTAT IS SET, IGNORE STAT1                *                
*                                                              *                
* ALSO REBUILDS CHECKSUM TABLE                                 *                
*==============================================================*                
         SPACE 1                                                                
UPDINVS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CHKSUMD,RF                                                       
         LR    RF,RA               CLEAR CHECKSUM VALUES                        
         AHI   RF,SVCHKSUM-TWAD                                                 
         LHI   RE,SVCHKL/CHKSUML   MAX ENTRIES IN TABLE                         
         XC    CHKSVAL,CHKSVAL     CLEAR ENTRY                                  
         LA    RF,CHKSUML(RF)                                                   
         BCT   RE,*-10                                                          
         DROP  RF                                                               
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'SNVKMAST),SVSNVKEY                                         
         MVI   XSP,C'Y'                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     UPDINV20                                                         
*                                                                               
* CALLING CHECKSUM HERE MAKES SURE THAT THE CHECKSUM GETS RE-CALCULATED         
* AFTER ANY POTENTIAL RECORD CHANGES                                            
UPDINV10 BRAS  RE,CHECKSUM         RE-COMPUTE CHECKSUMS                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
*                                                                               
         USING SNVKEYD,R6                                                       
UPDINV20 LA    R6,KEY                                                           
*                                                                               
         CLC   KEY(10),KEYSAVE     0E03/A-M/CLT/STA/MOS                         
         BNE   UPDINVX                                                          
*                                                                               
         CLI   BYTE,X'FD'                                                       
         BE    *+12                                                             
         CLI   BYTE,X'7F'                                                       
         BNE   *+14                                                             
         NC    SNVDSTAT+1(1),BYTE  RESET MATCHED/APPROVED FLAG IN DIR           
         B     *+10                                                             
         OC    SNVDSTAT+1(1),BYTE  SET WIP/MATCHED FLAG IN DIR                  
*                                                                               
         TM    L_FLAGS,L_NOSTAT                                                 
         BNZ   *+14                                                             
         MVC   SNVDSTAT(1),STAT1   SET ERROR STATUS BITS                        
         NI    SNVDSTAT,X'FF'-X'C0'  NEVER ALLOW DELETE/CLOSEOUT                
*                                                                               
         CLC   SNVKMINK,=X'FFFFFFFFFFFF'  ONLY NEED TO UPDATE LAST REC          
         BNE   *+8                                                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLC   SNVKMINK,=X'FFFFFFFFFFFF'  IF THIS ISN'T THE XFF REC             
         BNE   UPDINV10                    THERE ARE NO E8/EA ELS               
         L     R6,AIO                                                           
         LA    R6,SNVELS           UPDATE E8 (MM STATUS) ELEM                   
         SR    R0,R0                                                            
*                                                                               
UPDINV30 CLI   0(R6),SNVMMELQ      X'E8' ELEM                                   
         BE    UPDINV40                                                         
         CLI   0(R6),SNVMRELQ      X'EA' RECON ELEM                             
         BE    UPDINV60                                                         
         CLI   0(R6),0                                                          
         BE    UPDINV90                                                         
*                                                                               
UPDINV32 XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     UPDINV30                                                         
         DROP  R6                                                               
*                                                                               
         USING SNVMMELD,R6                                                      
UPDINV40 CLC   QPRD,SNVMMRP1       I2 RUN FOR THIS PRD?                         
         BNE   UPDINV10             NO                                          
         CLC   QPRD2,SNVMMRP2                                                   
         BNE   UPDINV10                                                         
*                                                                               
         CLI   SNVMMRE1,0          EST=NO?                                      
         BE    UPDINV50             YES - TAKE ALL ESTS                         
         CLC   BEST,SNVMMRE1        NO - MAKE SURE EST IN RANGE OF REQ          
         BE    UPDINV50                                                         
         BL    UPDINV10                                                         
         CLC   BEST,SNVMMRE2                                                    
         BH    UPDINV10                                                         
*                                                                               
UPDINV50 CLI   BYTE,X'FD'                                                       
         BE    *+12                                                             
         CLI   BYTE,X'7F'                                                       
         BNE   *+14                                                             
         NC    SNVMMMST,BYTE       RESET MATCHED/APPROVED IN E8                 
         B     *+10                                                             
         OC    SNVMMMST,BYTE       SET WIP/MATCHED FLAG IN E8                   
*                                                                               
         TM    L_FLAGS,L_NOSTAT                                                 
         BNZ   UPDINV52                                                         
         CLI   SNVMMLEN,SNVMML2Q   IS THERE ROOM IN EL FOR NEW STATUS?          
         BL    UPDINV52                                                         
         MVC   SNVMMERR,STAT1      SET ERROR STATUS BITS                        
         NI    SNVMMERR,X'FF'-X'C0'  NEVER ALLOW DELETE/CLOSEOUT                
*                                                                               
UPDINV52 L     RF,AIO              AND CHANGE KEY OF RECORD                     
         USING SNVKEYD,RF                                                       
         CLI   BYTE,X'FD'                                                       
         BE    *+12                                                             
         CLI   BYTE,X'7F'                                                       
         BNE   *+14                                                             
         NC    SNVRSTAT+1(1),BYTE  RESET MATCHED/APPROVED IN DIR                
         B     *+10                                                             
         OC    SNVRSTAT+1(1),BYTE  SET WIP/MATCHED FLAG IN DIR                  
*                                                                               
         TM    L_FLAGS,L_NOSTAT                                                 
         BNZ   *+14                                                             
         MVC   SNVRSTAT(1),STAT1   SET ERROR STATUS BITS                        
         NI    SNVDSTAT,X'FF'-X'C0'  NEVER ALLOW DELETE/CLOSEOUT                
         B     UPDINV32                                                         
         DROP  R6,RF                                                            
*                                                                               
         USING SNVMRELD,R6                                                      
UPDINV60 CLI   BYTE,X'80'          HERE TO SET MATCHED?                         
         BNE   UPDINV32             NO                                          
         GOTO1 VDATCON,DMCB,(5,0),(3,SNVMRLDT)                                  
         MVI   SNVMRLPC,100        SET 100% MATCHED                             
         B     UPDINV32                                                         
         DROP  R6                                                               
*                                                                               
UPDINV90 MVI   RDUPDATE,C'Y'                                                    
         GOTO1 WRITE                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 PUTREC                                                           
         B     UPDINV10                                                         
*                                                                               
UPDINVX  MVI   XSP,C'N'                                                         
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* UPDATE CHECKSUM TABLE                                           *             
*                                                                 *             
*=================================================================*             
         SPACE 1                                                                
         USING CHKSUMD,RF                                                       
CHECKSUM NTR1  BASE=*,LABEL=*                                                   
         LR    RF,RA                                                            
         AHI   RF,SVCHKSUM-TWAD                                                 
         LHI   RE,SVCHKL/CHKSUML   MAX ENTRIES IN TABLE                         
         L     R6,AIO                                                           
*                                                                               
CKS10    CLC   CHKSKEY,12(R6)      FIND THIS ENTRY                              
         BE    CKS20                                                            
         LA    RF,CHKSUML(RF)                                                   
         BCT   RE,CKS10                                                         
         DCHO                      INVOICE NOT FOUND???                         
*                                                                               
CKS20    XR    RE,RE                                                            
         ICM   RE,3,CHKSVAL        RE = CHECKSUM VALUE                          
         XR    R1,R1                                                            
         ICM   R1,3,SNVRLEN-SNVKEY(R6)  GET RECLEN                              
         JNZ   *+6                                                              
         DCHO                                                                   
         LR    R0,R6               RE = AREC                                    
*                                                                               
         CKSM  RE,R0               COMPUTE CHECKSUM                             
         BC    1,*-4               IN CASE INTERRUPTED!                         
         LR    R0,RE               THESE NEXT FEW INSTRUCTIONS CONVERT          
         SRDL  R0,16               A 32-BIT CKSUM TO 16-BIT                     
         ALR   R0,R1                                                            
         ALR   R0,RE                                                            
         SRL   R0,16                                                            
*                                                                               
         STCM  R0,3,CHKSVAL                                                     
         DROP  RF                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* DELETE ANY UNMATCHED SPOTS ON BUY JUST ADDED                 *                
*==============================================================*                
         SPACE 1                                                                
ADJSPOTS NTR1  BASE=*,LABEL=*                                                   
         CLI   SVRCVEL+1,H44Q      THIS A MG ACROSS MONTH?                      
         BE    ADJX                 YES - LEAVE IT ALONE                        
         MVI   BYTE,0              FLAG FOR CHANGED BUY                         
         BRAS  RE,GETBUY           RTFB                                         
*                                                                               
         L     R6,AIO                                                           
         USING BUYRECD,R6                                                       
*                                                                               
         MVI   ELCDLO,11                                                        
         MVC   ELCDHI,ELCDLO                                                    
*                                                                               
         LA    R6,BDELEM                                                        
         USING REGELEM,R6                                                       
*                                                                               
ADJ10    BRAS  RE,NEXTEL                                                        
         BNE   ADJ70                                                            
*                                                                               
ADJ20    CLI   ELCDLO,11           POL BUY?                                     
         BNE   ADJ40                NO                                          
         ZIC   R1,1(R6)                                                         
         AR    R1,R6                                                            
         CLI   0(R1),X'10'         THIS AN AFFID ELEM?                          
         BE    ADJ10                YES - SPOT BE OK                            
*                                                                               
ADJ30    MVI   BYTE,1              SET REC CHANGED                              
         GOTO1 VRECUP,DMCB,(0,AIO),(R6)  NO AFFID - DELETE SPOT                 
         BRAS  RE,NEXTEL2                                                       
         BNE   ADJ70                                                            
         B     ADJ20                                                            
*                                                                               
ADJ40    ZIC   R0,RNUM                                                          
         SR    R1,R1                                                            
*                                                                               
ADJ50    IC    R1,1(R6)                                                         
         AR    R1,R6                                                            
         CLI   0(R1),X'10'         AFFID ELEM?                                  
         BNE   ADJ60                                                            
         BCT   R0,ADJ50                                                         
         B     ADJ10               R0 = # SPOTS WITHOUT AFFIDS                  
*                                                                               
ADJ60    IC    R1,RNUM                                                          
         SR    R1,R0                                                            
         BZ    ADJ30               THERE ARE NO AFFIDS - DELETE REGEL           
         STC   R1,RNUM             ADJUST NUMBER OF SPOTS                       
         MVI   BYTE,1                                                           
         B     ADJ10                                                            
         DROP  R6                                                               
*                                                                               
ADJ70    TM    FLAGS,FLNOMAT       SKIP MATCH?                                  
         BZ    ADJ90                NO                                          
         L     R6,AIO               YES - REMOVE ALL AFFID ELEMS                
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   ADJ90                                                            
         MVI   BYTE,1                                                           
*                                                                               
ADJ80    GOTO1 VRECUP,DMCB,(0,AIO),(R6)  REMOVE AFFID                           
         BRAS  RE,NEXTEL2                                                       
         BNE   ADJ90                                                            
         B     ADJ80                                                            
*                                                                               
ADJ90    CLI   BYTE,0              ANY CHANGES TO BUY?                          
         BE    ADJ100                                                           
         BRAS  RE,PUTBUY           PUT THE BUY RECORD USING GETBUY              
*                                                                               
ADJ100   CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   ADJX                                                             
         CLI   QMED,C'N'           NETWORK?                                     
         BNE   ADJX                                                             
         BRAS  RE,ADJNET           ADJUST EXPLODED BUYS                         
*                                                                               
ADJX     J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
*==============================================================*                
* ADJUST NUMBER OF SPOTS ON EXPLODED STATION RECORDS BY        *                
* DELETING ALL 06/0B ELEMENTS, AND COPYING 06/0B ELEMENTS FROM *                
* NETWORK LEVEL BUY                                            *                
*                                                              *                
* EXPECTS NETWORK BUY RECORD IN AIO1                           *                
*  (BE CAREFUL WITH R0 - IT'S ALWAYS USED FOR IC'S             *                
*==============================================================*                
         SPACE 1                                                                
ADJNET   NTR1  BASE=*,LABEL=*,WORK=(R3,8)                                       
*                                                                               
         USING GETBUYD,R3          GETBUY DSECT                                 
         XC    0(GETBUYL,R3),0(R3) CLEAR GETBUY BLOCK                           
         MVC   GBYCOMF,ACOMFACS    A(COMFACS)                                   
*                                                                               
         L     R2,AIO1                                                          
         AHI   R2,BDELEM-BUYRECD                                                
         SR    R0,R0                                                            
*                                                                               
ADJN10   IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             READ BUYS FOR ALL LOCAL STATIONS             
         BE    ADJNX                                                            
         CLI   0(R2),X'68'                                                      
         BNE   ADJN10                                                           
*                                                                               
ADJN20   MVC   KEY+4(5),2(R2)      LOCAL STATION                                
         LA    RE,KEYSAVE          RE = KEYSAVE                                 
         ST    RE,GBYKEYIN         STORE A(KEYSAVE)                             
         LA    RE,KEY              RE = KEY                                     
         ST    RE,GBYKEYOT         STORE A(KEY)                                 
         MVI   GBYACT,GBYHIGH      READ HIGH                                    
         MVC   KEYSAVE,KEY         SAVE OFF THE KEY                             
         MVC   GBYDMIN,DMINBTS     PASS DMINBTS                                 
         MVC   GBYDMOUT,DMOUTBTS   PASS DMOUTBTS                                
         LA    RE,DMWORK           DMWORK                                       
         ST    RE,GBYDMWRK         STORE A(DMWORK)                              
         OI    GBYDMIN,X'80'       READ FOR UPDATE                              
         MVC   GBY1OR2,BUY1OR2     1 OR 2-BYTE BUYLINE MODE                     
         GOTO1 VGETBUY,0(R3)       CALL GETBUY                                  
         CLC   KEY(12),KEYSAVE     FOUND THE BUY RECORD?                        
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
*                                                                               
         LA    RE,KEY+14           A(DISK ADDRESS)                              
         ST    RE,GBYDA            STORE A(DISK ADDRESS)                        
         L     R6,AIO2             R6=AIO2                                      
         ST    R6,AIO              AIO=AIO2                                     
         MVC   GBYIOA,AIO2         PUT THE BUY RECORD IN AIO2                   
         MVI   GBYACT,GBYGET       GETREC                                       
         GOTO1 VGETBUY,0(R3)       CALL GETBUY                                  
*                                                                               
         USING BUYRECD,R6          R6 AT LOCAL STATION RECORD                   
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,11                                                        
*                                                                               
         LA    R6,BDELEM                                                        
         DROP  R6                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DCHO                      WHY ARE THERE NO REGELS?                     
*                                                                               
ADJN30   GOTO1 VRECUP,DMCB,(0,AIO),(R6)  REMOVE ALL REGELS                      
         BRAS  RE,NEXTEL2                                                       
         BE    ADJN30                                                           
*                                                                               
         L     R4,AIO                                                           
         AHI   R4,BDELEM-BUYRECD   FIND WHERE TO INSERT REGELS                  
*                                                                               
         SR    R0,R0                                                            
ADJN40   CLI   0(R4),0                                                          
         BE    ADJN50                                                           
         CLI   0(R4),3             ELCODE HIGHER THAN 3?                        
         BH    ADJN50               YES - THIS BE WHERE THEY GO                 
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ADJN40                                                           
*                                                                               
ADJN50   L     R6,AIO1             R6 AT NETWORK BUY                            
         AHI   R6,BDELEM-BUYRECD                                                
*                                                                               
ADJN60   BRAS  RE,NEXTEL                                                        
         BNE   ADJN70                                                           
         GOTO1 VRECUP,DMCB,(0,AIO),(R6),(R4)                                    
         NI    6(R4),X'DF'         UNSET COST OVRD FLAG                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ADJN60                                                           
*                                                                               
ADJN70   LA    RE,DMWORK           DMWORK                                       
         ST    RE,GBYDMWRK         STORE A(DMWORK)                              
         MVI   GBYACT,GBYPUT       PUTREC                                       
         MVI   HALF2,0             MAKE SURE WE DON'T PASS A PRD LIST           
         LA    RE,HALF2            A(NULL PRD LIST)                             
         ST    RE,GBYPRDL          STORE A(PRD LIST)                            
         GOTO1 VGETBUY,0(R3)       CALL GETBUY FOR PUTREC                       
         B     ADJN10                                                           
*                                                                               
ADJNX    J     EXIT                                                             
         DROP  R3                  DROP GETBUY USING                            
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* UPDATE PASSIVE POINTERS WITH WIP/MATCHED FLAG                *                
*  BYTE = X'80' MATCHED                                        *                
*         X'40' WIP                                            *                
*         X'02' APPROVED                                       *                
*         X'FD' SET UN-APPROVED                                *                
*         X'7F' SET UNMATCHED!                                 *                
*                                                              *                
*  STAT1= SNVMMFLM X'20' FILM ANALYSIS ERRORS                  *                
*         SNVMMHVE X'10' HORIZONTAL/VERTICAL ERRORS            *                
*         SNVMMSSE X'08' SECONDARY SEPARATION ERRORS           *                
*                                                              *                
* *** NOTE *** IF L_NOSTAT IS SET, IGNORE STAT1                *                
*                                                              *                
*                                                              *                
*==============================================================*                
         SPACE 1                                                                
K        USING SNVKEYD,KEY                                                      
*                                                                               
UPDPASS  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             SET TO READ SNV PASSIVE KEYS                 
         MVI   K.SNVPTYP,X'0E'                                                  
         MVI   K.SNVPSUB,X'83'                                                  
         MVC   K.SNVPAM,BAGYMD                                                  
         MVC   K.SNVPMKT(5),BMKTSTA                                             
*                                                                               
         CLI   PROFMK+4,C'Y'       USING BUBBLCD?                               
         BNE   *+10                                                             
         MVC   K.SNVPBYR,BUBBLCD                                                
*                                                                               
         LA    RE,SVSNVKEY+(SNVKMOS-SNVKEY)                                     
         MVC   K.SNVPMOS,0(RE)                                                  
         MVC   K.SNVPCLT,BCLT                                                   
         MVC   K.SNVPPRD,QPRD                                                   
         MVC   K.SNVPEST,BEST                                                   
         MVI   XSP,C'Y'                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE     0E83/A-M/MKT/STA/MOS/CLT/PRD/EST             
         BE    UPPAS20                                                          
         DCHO                                                                   
*                                                                               
UPPAS10  MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(16),KEYSAVE                                                  
         BNE   UPPASX                                                           
*                                                                               
*** I THINK THIS CODE HAS BEEN WRONG SINCE WE INCLUDED UNMATCH                  
***UPPAS20  ZIC   R1,BYTE                                                       
***         EX    R1,*+8                                                        
***         B     *+8                                                           
***         TM    K.SNVPSTAT+1,X'00'  TEST WIP/MATCHED FLAG ON                  
***         BO    UPPAS10                                                       
*                                                                               
UPPAS20  CLC   BEST,BEST2          ANY 2ND EST?                                 
         BE    *+14                 NO                                          
         CLC   K.SNVPEST2,BEST2    MATCH EST2                                   
         BNE   UPPAS10                                                          
         CLC   K.SNVPPRD2,QPRD2    MATCH PRD2                                   
         BNE   UPPAS10                                                          
*                                                                               
         CLI   BYTE,X'FD'                                                       
         BE    *+12                                                             
         CLI   BYTE,X'7F'                                                       
         BNE   *+14                                                             
         NC    K.SNVPSTAT+1(1),BYTE   RESET MATCHED/APPROVED                    
         B     *+10                                                             
         OC    K.SNVPSTAT+1(1),BYTE   SET WIP/MATCHED                           
*                                                                               
         TM    L_FLAGS,L_NOSTAT                                                 
         BNZ   *+14                                                             
         MVC   K.SNVPSTAT(1),STAT1    SET ERROR STATUS BITS                     
         NI    K.SNVPSTAT,X'FF'-X'C0'  NEVER ALLOW DELETE/CLOSEOUT              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 WRITE                                                            
         B     UPPAS10                                                          
*                                                                               
UPPASX   MVI   XSP,C'N'                                                         
         J     EXIT                                                             
         DROP  K                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* SAVE OFF BUY COMMENTS                                        *                
*==============================================================*                
         SPACE 1                                                                
SAVCMT   NTR1  BASE=*,LABEL=*                                                   
         TM    FLAGS,FLCMTRCV      ANY COMMENT ELEM RECEIVED?                   
         BZ    SAVCMTX              NO                                          
         LR    RE,RA                                                            
         AHI   RE,SVCOM-TWAD                                                    
         LHI   RF,RH6COML                                                       
         LA    R0,RH6COM1                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
SAVCMTX  J     EXIT                                                             
         EJECT                                                                  
*==================================================================*            
* GETBYDTA:                                                        *            
* CALL GLOBBER TO GET RETURN VALUES - EITHER ERROR CODE, NEW LINE, *            
* OR MG CODE, BDELEM, AND REGELS                                   *            
*==================================================================*            
         SPACE 1                                                                
GETBYDTA NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VGLOBBER,DMCB,=C'GETD',BUYDATA1,78,GLVBUY1                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),,BUYDATA2,,GLVBUY2                                     
         GOTO1 (RF),(R1),,BUYDATA3,,GLVBUY3                                     
         GOTO1 (RF),(R1),,BUYDATA4,,GLVBUY4                                     
*                                                                               
         GOTO1 (RF),(R1),=C'CLEAR'                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    BUYDATA1(2),BUYDATA1    ANY ERRORS?                              
         BNZ   GBXNEQ                                                           
*                                                                               
         MVC   BBUYLIN,BUYDATA1+4      SAVE BUYLINE NUMBER RETURNED             
         MVC   BMGCODE,BUYDATA1+6      AND MAKEGOOD CODE                        
         CLI   SVREASON,SVRPOTO        COMING BACK FROM A +OTO?                 
         BNE   *+10                    NO, DON'T CHANGE OTODATA!                
         MVC   OTODATA,BUYDATA1+8      AND +OTO DT/SPNUM                        
         MVC   SVBDELEM,BUYDATA1+11    SAVE OFF BDELEM(66)                      
*                                                                               
B        USING BDELEM,BUYDATA1+11  ONLY 67 BYTES OF BDELEM                      
         MVI   BCOST,0                                                          
         MVC   BCOST+1(3),B.BDCOST                                              
         TM    B.BDCIND2,BDCCANAQ  X'20' TEST CANADIAN BUY                      
         BZ    GB01                                                             
         CLI   QMED,C'N'           MEDIA N?                                     
         BNE   GB01                NO - SAME TEST AS US                         
         TM    B.BDCIND2,BDCRATPQ  X'01' TEST RATE IN PENNIES                   
         BO    GB10                                                             
         B     GB05                                                             
GB01     TM    B.BDCIND2,BDCNBRDQ  X'10' TEST RATE IN DOLLARS                   
         BZ    GB10                                                             
GB05     ICM   R0,15,BCOST                                                      
         MHI   R0,100                                                           
         STCM  R0,15,BCOST                                                      
*                                                                               
GB10     MVC   BNPW,B.BDNOWK                                                    
         GOTO1 VDATCON,DMCB,(3,B.BDSTART),(2,BSTDATE)                           
         GOTO1 (RF),(R1),(3,B.BDEND),(2,BEDDATE)                                
         DROP  B                                                                
*                                                                               
GBXEQ    CR    RE,RE               EXIT CC EQ                                   
         B     *+6                                                              
GBXNEQ   LTR   RE,RE               EXIT CC NEQ                                  
         J     EXIT                                                             
         EJECT                                                                  
*==================================================================*            
* MAKE SURE THAT ALL AFFIDS ARE INCLUDED IN THIS NEW BUY           *            
*==================================================================*            
         SPACE 1                                                                
CHKBUY   NTR1  BASE=*,LABEL=*                                                   
         ICM   R0,15,BCOST                                                      
         TM    SVBDELEM+(BDCIND2-BDELEM),X'10'   TEST RATE IN DOLLARS           
         BZ    CHKB2                                                            
         C     R0,MAXDOLS          TEST COST TO MAX THAT WORKS                  
         BL    CHKB2                                                            
         MVC   ERROR,=Y(INVCOST)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
CHKB2    SR    R0,R0               COUNT NUMBER OF DAYS IN ROTATION             
         ICM   R0,8,SVBDELEM+(BDDAY-BDELEM)                                     
         SLL   R0,1                SHIFT REG TILL NEGATIVE                      
         LTR   R0,R0                                                            
         BP    *-6                                                              
*                                                                               
         XR    RF,RF               CLEAR COUNTER                                
*                                                                               
         BCTR  RF,0                'ADD' ONE TO COUNT                           
         SLL   R0,1                SHIFT REG TILL ZERO                          
         LTR   R0,R0                                                            
         BNZ   *-8                                                              
*                                                                               
         LPR   R0,RF                                                            
         BCTR  R0,0                                                             
         ST    R0,NDAYS            SAVE NUMBER OF DAYS                          
         SPACE 1                                                                
*=================================================================*             
* HERE, BDATA CONTAINS AFFID DATES ONLY LOOKING LIKE              *             
*                    C'A' AFDATE(2)                               *             
*                    C'T' AFTIME(2)                               *             
*                         PRDCODE (TRUE POL)                      *             
*                         AFFID COST(4)                           *             
* WORK USAGE:                                                     *             
*   WORK(6)=AFFID DATE, WORK+6(6)=START DATE, WORK+12(6)=END DATE *             
*   WORK+18(2)=AFFID TIME                                         *             
*                                                                 *             
* BUYDATA2/3/4 CONTAINS DATES/NUMSPOTS                            *             
*=================================================================*             
         SPACE 1                                                                
         LA    R2,BDATA                                                         
*                                                                               
         CLI   0(R2),C'A'          IS FIRST DATA ITEM AN AFFID DATE?            
         BE    CHKB20               YES                                         
*                                                                               
CHKB10   MVI   BYTE,C'A'            GET AN AFFID DATE                           
         BRAS  RE,NXTBDATA                                                      
         BNE   CHKBX                NOT FOUND - DONE                            
*                                                                               
CHKB20   GOTO1 VDATCON,DMCB,(2,1(R2)),WORK     AFFID DATE                       
*                                                                               
         LA    R0,600              SET STANDARD START TIME                      
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    CHKB22              YES                                          
***      CLI   QMED,C'R'           US RADIO?                                    
***      BE    *+12                YES - SET 5AM                                
***      CLI   QMED,C'X'           US RADIO?                                    
***      BNE   *+8                 NO                                           
***      LA    R0,500              US STARTS AT 5AM                             
         LA    R0,300              US STARTS AT 3AM                             
*                                                                               
CHKB22   SR    RE,RE                                                            
         ICM   RE,3,4(R2)                      AFFID TIME                       
         CR    RE,R0                                                            
         BNL   *+8                                                              
         AHI   RE,2400                                                          
         STH   RE,WORK+18          SAVE AFFID END TIME                          
*                                                                               
CHKB30   LA    R5,BUYDATA2         R5 = A(REGELS)                               
         LHI   R0,1                                                             
         STH   R0,SPOTNUM          SET SPOT NUMBER                              
*                                                                               
CHKB40   CLI   2(R5),0             TEST ANY SPOTS LEFT THIS DATE                
         BNH   CHKB50               NO                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(2,(R5)),WORK+6   DECODE REGEL DATE                 
         L     R0,NDAYS                                                         
         GOTO1 VADDAY,DMCB,(C'D',WORK+6),WORK+12,(R0)                           
*                                                                               
         CLC   WORK(6),WORK+6      AFFID < START DATE?                          
         BL    CHKB50               YES - NEXT REGEL                            
         CLC   WORK(6),WORK+12     AFFID > END DATE?                            
         BH    CHKB50               YES - TRY NEXT REGEL                        
*                                                                               
* AFFID FITS REGEL DATE - NOW CHECK DAY                                         
         GOTO1 VGETDAY,DMCB,WORK,FULL      GET AFFID DAY IN FULL                
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         LTR   RF,RF                                                            
         BZ    CHKB60              INVALID DAY                                  
         IC    RF,DAYTAB-1(RF)     GET CORRESPONDING DAY BIT                    
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    SVBDELEM+(BDDAY-BDELEM),0                                        
         BZ    CHKB50              DAY DOESN'T FIT THIS REGEL - NEXT            
         B     CHKB42                                                           
DAYTAB   DC    X'40201008040201'   MO/TU/.../SU                                 
*                                                                               
* NOW CHECK TIME                                                                
CHKB42   SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R0,600              SET STANDARD START TIME                      
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    CHKB42A             YES                                          
***      CLI   QMED,C'R'           US RADIO?                                    
***      BE    *+12                YES - SET 5AM                                
***      CLI   QMED,C'X'           US RADIO?                                    
***      BNE   *+8                 NO                                           
***      LA    R0,500              US STARTS AT 5AM                             
         LA    R0,300              US STARTS AT 3AM                             
*                                                                               
CHKB42A  ICM   RE,3,SVBDELEM+(BDTIMST-BDELEM)   BDELEM START TIME               
         CR    RE,R0                                                            
         BNL   *+8                                                              
         AHI   RE,2400                                                          
*                                                                               
         ICM   RF,3,SVBDELEM+(BDTIMEND-BDELEM)  BDELEM END TIME                 
         BNZ   *+8                                                              
         ICM   RF,3,SVBDELEM+(BDTIMST-BDELEM)   ELSE USE START TIME             
         CR    RE,RF                            TEST START > END TIME           
         BNH   *+8                                                              
         AHI   RF,2400                                                          
***                                                                             
* WE SOMETIMES GIVE A "RNO'S NOT COVERED BY PURCHASED SPOTS" ERROR              
* THAT WE SHOULD NOT GIVE AND THE FOLLOWING IS THE LOGIC THAT                   
* ATTEMPTS TO CATCH THAT SCENARIO                                               
***                                                                             
         CHI   RE,2400                          BUY START TIME >= 2400          
         BNL   *+12                             YES                             
         CHI   RF,2400                          BUY END TIME >= 2400            
         BL    CHKB42B                          NO                              
         XR    R3,R3                            CLEAR R3                        
         ICM   R3,3,WORK+18                     AFFID TIME                      
         CR    R3,RE                            AFFID >= BUY START TIME         
         BNL   CHKB42B                          YES - DON'T ADD 2400            
         CHI   R3,2400                          AFFID TIME >=2400?              
         BNL   CHKB42B                          YES - DON'T ADD 2400            
         AHI   R3,2400                          NO - ADD 2400                   
         STH   R3,WORK+18                       AFFID TIME                      
*                                                                               
CHKB42B  OC    PROFI2Z+8(2),PROFI2Z+8   TIME LEEWAY EFF DATE?                   
         BZ    CHKB43                                                           
         CLC   BMOS,PROFI2Z+8      >= EFFECTIVE DATE?                           
         BL    CHKB43               NO                                          
         MVC   PROFI2(1),PROFI2Z+7  AND OVERRIDE PROFS                          
         MVC   PROFI2X+13(1),PROFI2Z+7                                          
*                                                                               
CHKB43   CLI   PROFI2,0            ANY LEEWAY TO ADJUST BY?                     
         BE    CHKB44               NO                                          
         SR    R0,R0                                                            
         LR    R1,RE                                                            
         D     R0,=F'100'          HOURS IN R1/MINS IN R0                       
         MHI   R1,60                                                            
         AR    R0,R1               TIME IN MINUTES                              
         SR    R1,R1                                                            
         IC    R1,PROFI2                                                        
         SR    R0,R1               LEEWAY-ADJUSTED TIME IN MINUTES              
         SRDL  R0,32                                                            
         D     R0,=F'60'           HOURS IN R1/MINS IN R0                       
         MHI   R1,100                                                           
         AR    R1,R0                                                            
         LR    RE,R1               ADJUSTED TIME (MILITARY)                     
*                                                                               
CHKB44   CLI   PROFI2X+13,X'FF'    0 LEEWAY?                                    
         BE    CHKB46               YES - NO ADJUSTMENT                         
         CLI   PROFI2X+13,0        NORMAL LEEWAY?                               
         BNE   *+12                 NO - ADJUST END TIME                        
         CLI   PROFI2,0            NO LEEWAY?                                   
         BE    CHKB46                                                           
*                                                                               
         SR    R0,R0                                                            
         LR    R1,RF                                                            
         D     R0,=F'100'          HOURS IN R1/MINS IN R0                       
         MHI   R1,60                                                            
         AR    R0,R1               TIME IN MINUTES                              
         SR    R1,R1                                                            
         ICM   R1,1,PROFI2X+13                                                  
         BNZ   *+8                                                              
         IC    R1,PROFI2                                                        
         AR    R0,R1               LEEWAY-ADJUSTED TIME IN MINUTES              
         SRDL  R0,32                                                            
         D     R0,=F'60'           HOURS IN R1/MINS IN R0                       
         MHI   R1,100                                                           
         AR    R1,R0                                                            
         LR    RF,R1               ADJUSTED TIME (MILITARY)                     
*                                                                               
CHKB46   CH    RE,WORK+18          BUY START TO AFFID                           
         BH    CHKB50                                                           
         CH    RF,WORK+18          BUY END TO AFFID                             
         BL    CHKB50                                                           
* ADJUST SPOT COUNTER                                                           
         IC    R0,2(R5)                                                         
         BCTR  R0,0                                                             
         STC   R0,2(R5)            SET SPOT NUMBER                              
*                                                                               
         CLI   SVREASON,SVRPASS2                                                
         BNE   *+8                                                              
         BRAS  RE,SNVUPD           UPDATE SNV RECORD                            
         B     CHKB10              AFFID FIT THIS REGEL - NEXT AFFID            
*                                                                               
* AFFID DOESN'T FIT THIS REGEL - TRY NEXT REGEL                                 
CHKB50   CLC   0(2,R5),3(R5)       TEST NEXT REGEL SAME DATE                    
         BE    *+10                                                             
         XC    SPOTNUM,SPOTNUM     ELSE RESET COUNTER                           
*                                                                               
         LH    RE,SPOTNUM                                                       
         LA    RE,1(RE)            BUMP ELEMENT COUNTER                         
         STH   RE,SPOTNUM                                                       
*                                                                               
         AHI   R5,3                NEXT REGEL                                   
         CLI   0(R5),0             ANY MORE REGELS?                             
         BNE   CHKB40               YES - TRY TO FIT AFFID TO NEXT              
*                                                                               
* AFFID DIDN'T FIT IN ANY REGELS - RETURN ERROR                                 
CHKB60   DS    0H                                                               
         MVC   ERROR,=Y(BADRNOS)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
CHKBX    XC    BUYDATA2,BUYDATA2                                                
         XC    BUYDATA3,BUYDATA3                                                
         XC    BUYDATA4,BUYDATA4                                                
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
MAXDOLS  DC    A(200000000)         SPOT MUST BE < 2,000,000.00                 
         EJECT                                                                  
         SPACE 1                                                                
*==================================================================*            
* GETBYWK:                                                         *            
* BUILD A TABLE OF BUY WEEKS IN BLOCK (BUY START --> EST END)      *            
* RH6DATE HAS BUYLINE S/E DATES MMM/DD-MMM/DD                      *            
*==================================================================*            
         SPACE 1                                                                
         USING ESTHDRD,R6                                                       
GETBYWK  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BRAS  RE,GETEST                                                        
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+6(5),RH6DATE                                                
         GOTO1 VDATVAL,DMCB,(1,WORK+6),WORK   RETURN 00MMDD                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   GBY10                                                            
         MVC   ERROR,=Y(PERERR)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
GBY10    XC    BLOCK,BLOCK                                                      
         LA    R5,BLOCK                                                         
*                                                                               
         MVC   WORK(2),ESTART                                                   
         CLC   ESTART(2),EEND      TEST ESTIMATE IS IN 1 YEAR                   
         BE    GBY20                                                            
         CLC   WORK+2(4),ESTART+2  NO-TEST INPUT MMDD L.T. EST ST.              
         BNL   *+10                                                             
         MVC   WORK(2),EEND        YES-DATE MUST BE IN END YEAR                 
*                                                                               
GBY20    GOTO1 VDATCON,DMCB,WORK,(2,(R5))                                       
         AHI   R5,2                                                             
         MVC   0(2,R5),=X'FFFF'    SET EOL FLAG                                 
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+6,7                                        
         CLC   WORK+6(6),EEND      TEST PAST END DATE                           
         BH    GBYX                                                             
         MVC   WORK(6),WORK+6                                                   
         B     GBY20                                                            
*                                                                               
GBYX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*==================================================================*            
* GETEST: READ AN ESTIMATE INTO AIO                                *            
* EPRD = OPTIONAL PRD (DON'T READ QPRD)                            *            
*==================================================================*            
GETEST   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             RTFE (READ EST)                              
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         OC    EPRD,EPRD                                                        
         BZ    *+10                                                             
         MVC   KEY+4(3),EPRD                                                    
         MVC   KEY+7(1),BUYEST                                                  
         CLI   EEST,0              HAVE OVERRIDE ESTIMATE?                      
         BE    *+10                NO                                           
         MVC   KEY+7(1),EEST       YES - USE OVERRIDE ESTIMATE                  
         MVI   XSP,C'N'                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DCHO                                                                   
         GOTO1 GETREC                                                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*==================================================================*            
* TESTLOCK:                                                        *            
* TEST IF BUY/INVOICE LOCKED BEFORE ALLOWING ANY CHANGES           *            
*==================================================================*            
         SPACE 1                                                                
L        USING LKKEYD,BLOCK                                                     
*                                                                               
TESTLOCK NTR1  BASE=*,LABEL=*                                                   
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,QAGY                                                   
         MVC   L.LOCKRTY,=C'NV'    INVOICE RECORDS                              
         MVC   L.LKNVMED,QMED                                                   
         MVC   L.LKNVCLT,QCLT                                                   
         MVC   L.LKNVSTA,QSTA                                                   
*                                                                               
         CLI   L.LKNVMED,C'X'      FOR MEDIA X                                  
         BNE   *+8                                                              
         MVI   L.LKNVSTA+4,C'X'    SET BAND X                                   
*                                                                               
         CLI   L.LKNVSTA+4,C' '    SET BAND IF NOT SET                          
         BH    TL2                                                              
         MVI   L.LKNVSTA+4,C'T'                                                 
*                                                                               
         CLI   L.LKNVSTA,C'0'      TEST CABLE                                   
         BL    TL2                                                              
         MVI   L.LKNVSTA+4,C'/'                                                 
*                                                                               
TL2      BRAS  RE,TESTIT                                                        
         BNE   TSTLKERR                                                         
         CLI   SVRCVEL+1,H3BQ      SET MATCHED?                                 
         BE    TLX                                                              
         CLI   SVRCVEL+1,H3DQ      AND EMAIL RNO SPOTS                          
         BE    TLX                                                              
         CLI   SVRCVEL+1,H40Q      AND SEND HISTORY                             
         BE    TLX                                                              
         CLI   SVRCVEL+1,H41Q      AND APPROVE                                  
         BE    TLX                                                              
         CLI   SVRCVEL+1,H42Q      AND UN-APPROVE                               
         BE    TLX                                                              
         CLI   SVRCVEL+1,H43Q      AND MAKEGOOD ANALYSIS                        
         BE    TLX                                                              
         CLI   SVRCVEL+1,H47Q      AND CHANGE RNO DATA                          
         BE    TLX                  DON'T CHECK BUY, NOT CHANGING               
*                                                                               
* TEST BRAND ALLOCATION RUNNING                                                 
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,QAGY                                                   
         MVC   L.LOCKRTY,=C'BA'    BRAND ALLOCATION                             
         MVC   L.LKBAMED,QMED                                                   
         MVC   L.LKBACLT,QCLT                                                   
*                                                                               
* FIRST FOR SPECIFIC ESTIMATE                                                   
         SR    R0,R0                                                            
         LA    R2,BDATA                                                         
         CLI   0(R2),C'B'          LINE NUMBER?                                 
         BNE   *+12                 NO - SEE IF WE HAVE ONE IN BUYEST           
         IC    R0,1(R2)                                                         
         B     TL20                                                             
*                                                                               
         ICM   R0,1,BUYEST         MAY BE SET IN MAK00                          
         BNZ   *+6                                                              
         DC    H'0'                WHAT EST???                                  
*                                                                               
TL20     CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LKBAEST,DUB                                                    
         MVC   DUB(3),L.LKBAEST    SAVE EST                                     
         BRAS  RE,TESTIT                                                        
         BNE   TSTLKERR                                                         
*                                                                               
* AND FOR ALL ESTIMATES                                                         
         MVC   L.LKBAEST,SPACES                                                 
         BRAS  RE,TESTIT                                                        
         BNE   TSTLKERR                                                         
*                                                                               
* FOR CANADA, IF MEDIA T OR N, ALSO TEST FOR MEDIA C BA LOCK                    
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   TLX                  NO                                          
         CLI   QMED,C'T'                                                        
         BE    *+12                                                             
         CLI   QMED,C'N'                                                        
         BNE   TLX                                                              
         MVI   L.LKBAMED,C'C'                                                   
         MVC   L.LKBAEST,DUB                                                    
         BRAS  RE,TESTIT                                                        
         BNE   TSTLKERR                                                         
         MVC   L.LKBAEST,SPACES                                                 
         BRAS  RE,TESTIT                                                        
         BNE   TSTLKERR                                                         
*                                                                               
TLX      J     EXIT                                                             
         SPACE 1                                                                
*                                                                               
TSTLKERR MVC   ERROR,=Y(DATALOCK)                                               
         GOTO1 SENDMSG                                                          
         SPACE 1                                                                
*                                                                               
TESTIT   LR    R0,RE                                                            
         L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',BLOCK),ACOMFACS                             
         LR    RE,R0                                                            
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TESTIT               YES                                         
         CLI   4(R1),0                                                          
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
         DROP  L                                                                
         LTORG                                                                  
         EJECT                                                                  
*==================================================================*            
* TESTI2:                                                          *            
* TEST IF I2 HAS BEEN RE-RUN BEFORE ALLOWING ANY CHANGES           *            
*==================================================================*            
         SPACE 1                                                                
*                                                                               
         USING SNVKEYD,R6                                                       
TESTI2   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   KEY(L'SNVKMAST),SVSNVKEY                                         
         MVC   SNVKMINK,=X'FFFFFFFFFFFF'  E8 ELEM ONLY IN LAST REC              
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   TI2ERR2                                                          
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R6,SNVELS                                                        
         SR    R0,R0                                                            
*                                                                               
TI210    CLI   0(R6),SNVMMELQ      MATCHMAKER STATUS ELEM (X'E8')               
         BE    TI220                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   TI210                                                            
         B     TI2ERR              NEW INVOICE SINCE I2 RUN?                    
*                                                                               
         USING SNVMMELD,R6                                                      
TI220    CLC   I2DATE,SNVMMDAT     DATE/TIME CHANGED SINCE WE UPLOADED?         
         BNE   TI2ERR                                                           
         CLC   I2TIME,SNVMMTIM                                                  
         BNE   TI2ERR                                                           
         DROP  R6                                                               
*                                                                               
* TEST IF ANY INVOICES IN GROUP HAVE CHANGED SINCE WE SENT TO PC                
         XR    R2,R2               R2 = CKSM VALUE                              
         XC    KEY,KEY             RE-RD FOR 1ST REC (MAY NOT BE X'FF')         
         MVC   KEY(L'SNVKMAST),SVSNVKEY                                         
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         B     TI240                                                            
*                                                                               
TI230    OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 SEQ                                                              
*                                                                               
TI240    CLC   KEY(SNVKINV-SNVKEY),KEYSAVE                                      
         BNE   TI2X                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 GETREC                                                           
         USING SNVKEYD,R6                                                       
*                                                                               
         TM    SNVRSTAT,X'80'      DON'T ADD INTO CKSM IF DELETED               
         BNZ   TI242                                                            
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,SNVRLEN        GET RECLEN                                   
         BNZ   *+6                                                              
         DCHO                                                                   
         LR    R0,R6               R0 = AREC                                    
*                                                                               
         CKSM  R2,R0               COMPUTE CHECKSUM                             
         BC    1,*-4               IN CASE INTERRUPTED!                         
*                                                                               
TI242    CLC   SNVKMINK,=X'FFFFFFFFFFFF'  IS THIS THE LAST REC?                 
         BNE   TI230                NO - KEEP SUMMING                           
*                                                                               
         LR    R0,R2               THESE NEXT FEW INSTRUCTIONS CONVERT          
         SRDL  R0,16               A 32-BIT CKSUM TO 16-BIT                     
         ALR   R0,R1                                                            
         ALR   R0,R2                                                            
         SRL   R0,16                                                            
         LR    R2,R0                                                            
*                                                                               
         USING CHKSUMD,RF          FIND ENTRY IN CHKSUM TABLE                   
         LR    RF,RA                                                            
         AHI   RF,SVCHKSUM-TWAD                                                 
         LHI   R0,SVCHKL/CHKSUML   MAX ENTRIES IN TABLE                         
*                                                                               
         CLC   CHKSKEY,SNVKINV                                                  
         BE    TI250                                                            
         LA    RF,CHKSUML(RF)                                                   
         BCT   R0,*-14                                                          
         TM    SNVRSTAT,X'80'      IF IT'S DELETED AND NOT IN TBL               
         BNZ   TI260                IT'S OK                                     
         B     TI2ERR2             INV NOT IN TBL (ADDED AFTER MM RUN!)         
*                                                                               
TI250    CLM   R2,3,CHKSVAL                                                     
         BNE   TI2ERR2                                                          
*                                                                               
TI260    XR    R2,R2                                                            
         B     TI230                                                            
*                                                                               
         DROP  R6,RF                                                            
*                                                                               
TI2X     MVI   XSP,C'N'                                                         
         J     EXIT                                                             
*                                                                               
TI2ERR2  MVC   ERROR,=Y(INVCHGD)                                                
         B     TI2ERRX                                                          
TI2ERR   MVC   ERROR,=Y(I2RERUN)                                                
TI2ERRX  GOTO1 SENDMSG                                                          
         EJECT                                                                  
         SPACE 1                                                                
*=================================================================*             
* GETFLM: GET INVOICE CMML CODE TRANSLATION ELEM TO GET 2 BYTE    *             
*  TRAFFIC SYSTEM SEQ NUMBER.                                     *             
* PASS R1=INTERNAL CMML CODE                                      *             
* RETURN  R1=TRAFFIC SYSTEM SEQ NUMBER                            *             
*=================================================================*             
         SPACE 1                                                                
         USING SNVKEYD,R6                                                       
GETFLM   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO2                                                          
         LA    R6,SNVELS                                                        
         B     GF20                                                             
*                                                                               
GF10     SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GFXNEQ                                                           
*                                                                               
GF20     CLI   0(R6),SNVCMELQ      CMML CODE XLAT ELEM (X'30')                  
         BNE   GF10                                                             
*                                                                               
         USING SNVCMELD,R6                                                      
         CLM   R1,1,SNVCMICD                                                    
         BNE   GF10                                                             
         LA    R1,SNVCMSEQ                                                      
*                                                                               
GFXEQ    CR    RE,RE               EXIT CC EQ                                   
         B     *+6                                                              
GFXNEQ   LTR   RE,RE               EXIT CC NEQ                                  
         XIT1  REGS=(R1)                                                        
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
         SPACE 1                                                                
*=================================================================*             
* SRTBDATA: SORT BDATA FOR NEW BUY/NEW MG TRANSACTIONS IN CASE    *             
*  AFFIDS NOT PASSED IN DATE SEQUENCE TO PREVENT DRAMA:           *             
*   A,OCT30CLU,NOV06CLU,NOV06-2CLU,OCT30CLU,OCT30-2CLU            *             
*     ||||||||                     |||||||| ||||||||||            *             
*      SPOT 1                      SHOULD BE SPOTS 2 & 3          *             
*                                                                 *             
* BDATA FOR NEW BUY LOOKS LIKE:  (TOTAL LENGTH 12)                *             
* 'A' AFFID DATE(2) 'T' AFFID TIME(2)/PRD(1)/PRD2(1)/COST(4)      *             
*                                                                 *             
* NOTE: WE NEED TO BE CAREFUL WITH NEW MAKEGOODS BECAUSE THE      *             
* AFFID INFO IN BDATA COULD BE PRECEEDED BY SPOT DATA (B,D, AND   *             
* N/O)                                                            *             
*                                                                 *             
*=================================================================*             
         SPACE 1                                                                
SRTBDATA NTR1  BASE=*,LABEL=*                                                   
         LA    R2,BDATA                                                         
         MVI   BYTE,C'A'                                                        
         LHI   R3,-1               SET COUNTER FOR FOUND 1ST AFFID              
         CLI   0(R2),C'A'          IF NOT AT FIRST AFFID (MG'S)...              
         BE    SRTB10                                                           
         BRAS  RE,NXTBDATA         ...GET THERE                                 
         BE    SRTB10                                                           
         DCHO                                                                   
*                                                                               
SRTB10   BRAS  RE,NXTBDATA         COUNT NUMBER OF AFFIDS                       
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
*                                                                               
         LPR   R3,R3                                                            
         CHI   R3,3                DON'T SORT LESS THAN 3 AFFIDS                
         BL    SRTBX                                                            
*                                                                               
         LA    R2,BDATA            POINT AT FIRST AFFID AGAIN                   
         CLI   0(R2),C'A'          IF NOT AT FIRST AFFID (MG'S)...              
         BE    SRTB20                                                           
         BRAS  RE,NXTBDATA         ...GET THERE                                 
         BE    SRTB20                                                           
         DCHO                                                                   
*                                                                               
SRTB20   GOTO1 VCALLOV,DMCB,0,X'D9000A50'   GET A(QSORT)                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DCHO                                                                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R2),(R3),12,2,1                                       
*                                                                               
SRTBX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*=================================================================*             
* SENDHIST: READ HIST REC FROM XSPFIL, PASS TO SPXBHIST TO FORMAT *             
*           AND U/L TO MM                                         *             
*=================================================================*             
         SPACE 1                                                                
         USING HISTRECD,R6                                                      
SENDHIST NTR1  BASE=*,LABEL=*                                                   
         L     R2,DPOINTER                                                      
         CLI   0(R2),C'B'          LINE NUMBER?                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUYEST,1(R2)                                                     
         MVC   BBUYLIN,2(R2)       BUYLINE NUMBER                               
         MVC   BYCBLNET,4(R2)      MOVE BUY CABLE NETWORK                       
         MVC   BBCBLNET,7(R2)      MOVE BINARY BUY CABLE NETWORK                
*                                                                               
         BRAS  RE,GTPOLPRD                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   HISTTYP,HISTTYQ                                                  
         MVI   HISTSTYP,HISTSTYQ                                                
         MVC   HISTBKAM,BAGYMD                                                  
         MVC   HISTBKCL,BCLT                                                    
         MVC   HISTBKPR,BPOLPRD                                                 
         MVC   HISTBKMK,BMKT                                                    
         MVC   HISTBKST,BSTA                                                    
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    SH10                                                             
         CLI   HISTBKST,X'E8'      TEST CABLE                                   
         BL    SH10                                                             
         OC    HISTBKST+2(1),BBCBLNET                                           
*                                                                               
SH10     MVC   HISTBKES,BUYEST                                                  
         MVC   HISTBKBY+1(1),BBUYLIN+1  THESE ARE DEAD!                         
         MVI   XSP,C'Y'                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(30),KEYSAVE                                                  
         BNE   SHX                                                              
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R1,H40Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
X        USING XBHISTD,WORK                                                     
* BUILD INITIAL PARAMETER LIST                                                  
         XC    WORK,WORK                                                        
         ST    R6,X.XBAHIST        A(HISTORY REC)                               
         L     RE,ACOMFACS                                                      
         ST    RE,X.XBACOMF                                                     
         MVC   X.XBSECAGY,QAGY                                                  
         LA    RE,BUYDATA1                                                      
         ST    RE,X.XBAOUT1                                                     
*                                                                               
SH20     GOTO1 =V(XBHIST),DMCB,WORK,RR=Y                                        
         TM    X.XBFLAGS,XBNOPRTQ  NO OUTPUT FROM XBHIST?                       
         BO    SH20                 NONE - NO OUTPUT                            
         OC    BUYDATA1,SPACES     NO NULLS!                                    
         LHI   R1,1                                                             
         LA    R4,BUYDATA1                                                      
         BRAS  RE,SENDD                                                         
         TM    X.XBFLAGS,XBENDQ    END OF HISTORY RECORD?                       
         BNO   SH20                                                             
*                                                                               
SHX      MVI   XSP,C'N'                                                         
         J     EXIT                                                             
         DROP  X                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*=================================================================*             
* GETMGA: CALL BLDMGA AND PASS OUTPUT TO MM                       *             
*=================================================================*             
         SPACE 1                                                                
GETMGA   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GTPOLPRD                                                      
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BRAS  RE,GETEST                                                        
         MVC   SVEDEMOS,EDEMLST-ESTHDRD(R6)                                     
*                                                                               
         LA    R2,MGABLK           CALL MGABLD TO BUILD TABLE                   
         USING MGABLKD,R2                                                       
         XC    0(MGALNQ,R2),0(R2)                                               
         MVI   MGAACT,MGAQBLD      SET ACTION - BUILD TABLE                     
         MVC   MGAACOM,ACOMFACS    SET A(COMFAS)                                
         MVC   MGATSAR,VTSAR                                                    
         MVC   MGAIO,AIO1                                                       
         MVC   MGUNTIME,VUNTIME                                                 
         MVC   MGSTAPAK,VSTAPACK                                                
         MVC   MGGETBUY,VGETBUY    SET A(GETBUY)                                
         MVC   MG1OR2,BUY1OR2      1 OR 2 BYTE BUYLINE FLAG                     
*                                                                               
         LA    R1,SVEDEMOS                                                      
         ST    R1,MGABRDEM                                                      
         ST    R1,MGADEM                                                        
*                                                                               
         OI    MGAOPT,MGONONC+MGONOPR    SKIP NO CHARGE/PRE-EMPT                
         MVC   MGAAGMD,BAGYMD      SET AGY/MED                                  
         MVC   MGACLT,BCLT             CLIENT                                   
         MVC   MGAPRD,BPOLPRD          PRODUCT                                  
         MVC   MGASTA(2),BMKT          MKT                                      
         MVC   MGASTA+2(3),BSTA        STA                                      
         MVC   MGAQAGY,QAGY            AGENCY                                   
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    GMGA2                                                            
         CLI   MGASTA+2,X'E8'      TEST CABLE                                   
         BL    GMGA2                                                            
         OC    MGASTA+4(1),BBCBLNET                                             
*                                                                               
GMGA2    MVC   MGAEST,BUYEST           ESTIMATE                                 
         OI    MGAOPT2,MGAOPT2_NODSK   SET DO NOT WRITE TO DISK                 
         MVI   MGATSRPGS,40        REQUEST 40 PAGES (18432 BYTES/PAGE)          
         OI    MGATSRPGS,X'80'     SET FLAG FOR BOTH TSAR BUFFERS               
*                                                                               
         GOTO1 VBLDMGN,MGABLKD                                                  
         CLI   MGAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MGAENTRY,MGAENTRY                                                
         XC    MGATSNUM,MGATSNUM                                                
         XC    BLOCK,BLOCK                                                      
         LA    R1,BLOCK                                                         
         ST    R1,MGALINE                                                       
*                                                                               
         TM    L_FLAGS,L_NOMGA     SUPPRESS MGA?                                
         BNZ   GMGA10              YES                                          
         LHI   R1,H43Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
GMGA10   MVI   MGAACT,MGAQNXT      SET ACTION - GET NEXT ENTRY                  
         GOTO1 VBLDMGN,MGABLKD                                                  
         CLI   MGAERR,0                                                         
         BE    GMGA20                                                           
         TM    L_FLAGS,L_NOMGA     SUPPRESS MGA?                                
         BNZ   GMGA50              YES - DIDN'T FIND MKGD/NTWK MATCH            
         CLI   MGAERR,MGAQEOF                                                   
         BE    GMGAX                                                            
         DCHO                                                                   
*                                                                               
GMGA20   MVI   MGAACT,MGAQPRNT     SET ACTION - SET UP PRINT LINE               
         GOTO1 VBLDMGN,MGABLKD                                                  
         CLI   MGAERR,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         L     R4,MGALINE                                                       
         OC    0(80,R4),SPACES     SPACES IS ONLY L'80                          
         OC    80(MGLINEQ-80,R4),SPACES                                         
         TM    L_FLAGS,L_NOMGA     SUPPRESS MGA?                                
         BZ    GMGA30              NO - SEND IT                                 
         USING MGLINED,R4          MGA DSECT                                    
         CLC   MGLCODE,QMGCD       MATCH ON MAKEGOOD CODE?                      
         BE    *+12                YES                                          
         BH    GMGA50              PAST THIS MAKEGOOD - ERROR                   
         B     GMGA40              BEFORE THIS MAKEGOOD - GET NEXT LINE         
         CLC   MGLNET,BYCBLNET     MATCH ON NETWORK?                            
         BE    GMGAX               YES - THIS MAKEGOOD IS OK                    
         B     GMGA40              GET THE NEXT LINE                            
         DROP  R4                  DROP MGA DSECT USING                         
*                                                                               
GMGA30   LHI   R1,1                MGA PRINT LINE CODE                          
         BRAS  RE,SENDD                                                         
GMGA40   XC    BLOCK,BLOCK         CLEAR MGLINE                                 
         B     GMGA10                                                           
*                                                                               
GMGA50   MVC   ERROR,=Y(MKGDCNF)   MAKEGOOD CODE NOT FOUND                      
         GOTO1 SENDMSG             SEND THE ERROR MESSAGE                       
*                                                                               
GMGAX    J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'*MGABLK*'                                                    
MGABLK   DS    CL(MGALNQ)                                                       
*                                                                               
         EJECT                                                                  
         SPACE 1                                                                
*=================================================================*             
* SAVECOST - RESTORE READ SEQUENCE AND UPDATE INV COSTS & F1 ELEM *             
*=================================================================*             
         SPACE 1                                                                
         USING SNVHDELD,R6                                                      
SAVECOST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* RE-READ THE KEY OF REC IN AIO2 TO RESTORE SEQUENCE                            
         L     R6,AIO2                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'SNVKEY),0(R6)                                              
         MVI   RDUPDATE,C'Y'                                                    
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(22),KEYSAVE                                                  
         BE    *+6                                                              
         DCHO                                                                   
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              AND DO GETREC TO AVOID PUTREC DRAMA          
         MVC   AIO,AIO2                                                         
*                                                                               
* RE-READ THE REC WITH D/A=SAVEDA, CHANGE THE HEADER ELEM COST                  
         CLI   AFCOST,X'FF'        DID WE CHANGE THE AFFID COST?                
         BE    SVC15                NO- ONLY NEED TO UPDATE ACTIVITY            
         CLC   SAVEDA,KEY+(SNVDDA-SNVKEYD)   DO WE HAVE THE FIRST REC?          
         BE    SVC10                          YES                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 PUTREC              SAVE THIS REC                                
         XC    KEY,KEY                                                          
         MVC   KEY(22),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(22),KEYSAVE                                                  
         BE    *+6                                                              
         DCHO                                                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
SVC10    L     R6,AIO2                                                          
         LA    R6,SNVELS-SNVKEYD(R6)                                            
         CLI   0(R6),SNVHDELQ      TEST HEADER ELEM (X'10')                     
         BE    *+6                                                              
         DCHO                                                                   
         USING SNVHDELD,R6                                                      
         AP    SNVHDTCS,COSTDIFF                                                
*                                                                               
* READ THE LAST REC OF THE MINIO SET TO UPDATE THE ACTIVITY ELEM                
SVC15    L     R6,AIO2                                                          
         USING SNVKEYD,R6                                                       
         CLC   SNVKMINK,=X'FFFFFFFFFFFF'                                        
         BE    SVC20                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 PUTREC              SAVE THIS REC                                
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(22),KEYSAVE                                                  
         MVC   SNVKMINK,=X'FFFFFFFFFFFF'                                        
         GOTO1 HIGH                                                             
         CLC   KEY(30),KEYSAVE                                                  
         BE    *+6                                                              
         DCHO                                                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
SVC20    XR    R0,R0                                                            
         L     R6,AIO2                                                          
         LA    R6,SNVELS-SNVKEYD(R6)                                            
*                                                                               
SVC30    CLI   0(R6),X'F1'         TEST ACTIVITY ELEM                           
         BE    SVC40                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   SVC30                                                            
         DCHO                                                                   
*                                                                               
         USING ACTVD,R6                                                         
SVC40    GOTO1 VDATCON,DMCB,(5,0),(3,ACTVCHDT)                                  
         MVC   ACTVCHID,TWAUSRID   MOVE IN ID (TWAORIG IN DDGENTWA)             
         XR    R1,R1                                                            
         IC    R1,ACTVCHNM                                                      
         AHI   R1,1                                                             
         STC   R1,ACTVCHNM                                                      
*                                                                               
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         BZ    *+14                                                             
         MVC   ACTVCHID,FAPASSWD   YES SO USE THIS ID                           
         OI    ACTVCHFL,X'80'                                                   
         DROP  R1                                                               
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 PUTREC              SAVE THIS REC                                
*                                                                               
SVCX     J     EXIT                                                             
         LTORG                                                                  
         DROP  R6,RB                                                            
         EJECT                                                                  
         SPACE 1                                                                
*=================================================================*             
* TEST IF CHANGES TO PAID INVOICES ALLOWED/TEST IF INVOICE PAID   *             
*=================================================================*             
         SPACE 1                                                                
         USING SNVHDELD,R6                                                      
TESTPAID NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SA0A'                                                 
         NI    WORK,X'BF'          CHANGE 'S' TO LOWER CASE                     
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
*                                                                               
         XC    WORK2,WORK2                                                      
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,WORK2,VDATAMGR                                    
         CLI   WORK2+14,C'Y'       NO PAID INVOICE CHANGE?                      
         BNE   TPX                  NO                                          
*                                                                               
         TM    SNVHDCTL,SNVHDPDQ                                                
         BZ    TPX                                                              
         MVC   ERROR,=Y(INVPAID)                                                
         GOTO1 SENDMSG                                                          
         DCHO                                                                   
*                                                                               
TPX      J     EXIT                                                             
         LTORG                                                                  
         DROP  R6,RB                                                            
         EJECT                                                                  
         SPACE 1                                                                
*=================================================================*             
* TEST IF CHANGES TO LOCKED ESTIMATES ALLOWED/TEST INV LOCKED     *             
*=================================================================*             
         SPACE 1                                                                
         USING SNVHDELD,R6                                                      
TESTESTL NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SI2P'                                                 
         NI    WORK,X'BF'          CHANGE 'S' TO LOWER CASE                     
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
*                                                                               
         XC    WORK2,WORK2                                                      
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,WORK2,VDATAMGR                                    
         CLI   WORK2+2,C'Y'        NO LOCKED ESTIMATE CHANGE?                   
         BNE   TELX                 NO                                          
*                                                                               
         CLI   SNVHDPRD,0          PROD IN HDR?                                 
         BE    TELX                 NO                                          
         CLI   SNVHDEST,0          EST IN HEADER?                               
         BE    TELX                 NO                                          
*                                                                               
         L     RF,AIO2                                                          
         MVC   FULL(2),SNVKMOS-SNVKEY(RF)                                       
         XC    FULL(2),=X'FFFF'    XOR INVOICE MOS TO GET READABLE DATE         
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   EPRD,=C'POL'                                                     
         MVC   EEST,SNVHDEST                                                    
         BRAS  RE,GETEST                                                        
         BAS   RE,ESTTEST                                                       
         BNE   TELERR                                                           
*                                                                               
TEL10    LR    R1,RA               CHECK EST FOR PRD 1                          
         AHI   R1,(SVCLTREC-TWAD)+(CLIST-CLTHDRD)  RF=A(CLIST)                  
         LHI   R0,220              MAX PRODUCTS                                 
                                                                                
         CLC   SNVHDPRD,3(R1)      MATCH ON PRODUCT?                            
         BE    *+14                 YES                                         
         AHI   R1,4                                                             
         BCT   R0,*-14                                                          
         DCHO                                                                   
*                                                                               
         MVC   EPRD,0(R1)                                                       
         BRAS  RE,GETEST                                                        
         BAS   RE,ESTTEST                                                       
         BNE   TELERR                                                           
*                                                                               
         CLI   SNVHDPR2,0          CHECK EST FOR PIGGY                          
         BE    TELX                 NONE                                        
         LR    R1,RA                                                            
         AHI   R1,(SVCLTREC-TWAD)+(CLIST-CLTHDRD)  RF=A(CLIST)                  
         LHI   R0,220              MAX PRODUCTS                                 
                                                                                
         CLC   SNVHDPR2,3(R1)      MATCH ON PRODUCT?                            
         BE    *+14                 YES                                         
         AHI   R1,4                                                             
         BCT   R0,*-14                                                          
         DCHO                                                                   
*                                                                               
         MVC   EPRD,0(R1)                                                       
         BRAS  RE,GETEST                                                        
         BAS   RE,ESTTEST                                                       
         BE    TELX                                                             
*                                                                               
TELERR   XC    EPRD,EPRD                                                        
         MVI   EEST,0                                                           
         MVC   ERROR,=Y(ESTLOCK)                                                
         GOTO1 SENDMSG                                                          
         DCHO                                                                   
*                                                                               
TELX     XC    EPRD,EPRD                                                        
         MVI   EEST,0                                                           
         MVC   AIO,AIO2                                                         
         J     EXIT                                                             
         DROP  R6                                                               
*=================================================================*             
* TEST IF ESTIMATE IS LOCKED OR PERIOD LOCKED                     *             
*  FULL=INVOICE MOS                                               *             
*  AIO1=ESTIMATE REC                                              *             
*  EXIT CC NEQ IF EST LOCKED                                      *             
*=================================================================*             
         USING SNVHDELD,R6                                                      
         USING ESTHDRD,R2                                                       
ESTTEST  NTR1                                                                   
         L     R2,AIO1                                                          
         TM    ECNTRL,X'08'        LOCKED?                                      
         BO    GNEX                 YES                                         
*                                                                               
         MVC   DUB(2),ELOCKYR      TEST PERIOD LOCKED                           
         OC    DUB(2),DUB                                                       
         BZ    GEX                                                              
         NI    DUB+1,X'3F'         DROP PRIOR/SUB FLAGS                         
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),WORK                                        
         GOTO1 VGETBROD,(R1),(1,WORK),WORK+6,VGETDAY,VADDAY                     
         GOTO1 VDATCON,(R1),WORK+6,(2,WORK2)                                    
         GOTO1 (RF),(R1),WORK+12,(2,WORK2+2)                                    
*                                                                               
         TM    ELOCKMON,X'80'      TEST MONTH & PRIOR                           
         BZ    *+10                                                             
         XC    WORK2(2),WORK2      CLEAR START DATE                             
         TM    ELOCKMON,X'40'      TEST MONTH & SUBSEQUENT                      
         BZ    *+10                                                             
         MVC   WORK2+2(2),=X'FFFF' SET HIGH END DATE                            
*                                                                               
         CLC   WORK2(2),FULL                                                    
         BH    GEX                                                              
         CLC   WORK2+2(2),FULL                                                  
         BNL   GNEX                                                             
*                                                                               
GEX      J     EQEXIT                                                           
GNEX     J     NEQEXIT                                                          
         DROP  R2,R6,RB                                                         
         LTORG                                                                  
*                                                                               
BLDALLOC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R5,R4               START OF BUYLINE                             
         LA    R6,3                ALLOCATE UP TO 3 LINES                       
         LA    R7,L'BUYDATA1       78 BYTE LINE                                 
         LR    R8,R4               START AT BUYDATA2                            
         XC    WORK2,WORK2         CLEAR WORK2                                  
         MVC   WORK2(2),=C'A,'     YES, NOW ALLOCATE PROD                       
         LA    R4,WORK2+2          START BUILDING ALLOCATION HERE               
         B     NB24                GET AFFID DATE                               
*                                                                               
NB23     LA    R4,WORK2            BUILD ONE ALLOCATION HERE                    
         XC    WORK2,WORK2         CLEAR WORK2                                  
*                                                                               
NB24     MVI   BYTE,C'A'           GET AFFID DATE                               
         CLI   0(R2),C'A'          TEST ALREADY POINTING AT IT                  
         BE    *+12                                                             
         BRAS  RE,NXTBDATA                                                      
         BNE   NB35                                                             
* ALLOC THE WEEK START DATE OF THE AFFID                                        
         BRAS  RE,GETBYWK                                                       
         LA    RF,BLOCK                                                         
NB25     CLC   1(2,R2),2(RF)       AFFID DATE < NEXT WEEK START DATE?           
         BL    NB25A                                                            
         AHI   RF,2                                                             
         CLC   =X'FFFF',0(RF)                                                   
         BNE   NB25                                                             
         DCHO                                                                   
*                                                                               
NB25A    MVC   ALDATE,0(RF)        ALLOCATE AFFID FOR THIS SPOT DATE            
         CLC   LADATE,ALDATE       HAS ALLOC DATE CHANGED?                      
         BE    NB25C               NO - BUY CAN NOW HANDLE W/O DATE!            
         SR    R3,R3               YES - RESET SPOT COUNTER                     
         MVC   LADATE,ALDATE       SAVE CURRENT DATE                            
*                                                                               
NB25B    GOTO1 VDATCON,DMCB,(2,ALDATE),(4,(R4))                                 
         AHI   R4,5                BUMP PAST DATE                               
         B     NB26                SPOT NUMBER 1 IS ASSUMED                     
*                                                                               
NB25C    LA    R1,1(R3)            COUNT IS ZERO RELATIVE                       
         EDIT  (R1),(2,FULL)       EDIT OUT SPOT COUNT                          
         LA    RE,1                SEE IF SPOT COUNT IS 1 OR DIGITS             
         LA    RF,FULL             SEE IF WE HAVE 1 OR 2 DIGITS                 
         CLI   0(RF),X'40'         HAVE A 1-DIGIT NUMBER?                       
         BNE   *+10                NO - WE HAVE A 2-DIGIT NUMBER                
         BCTR  RE,0                YES - DECREMENT BY 1                         
         AHI   RF,1                JUST MOVE THE 1 DIGIT                        
         EX    RE,*+8              EXECUTE THE MVC                              
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   0(0,R4),0(RF)       MOVE 1 OR 2 DIGIT SPOT NUMBER                
         LA    R4,1(RE,R4)         BUMP STRING POINTER BY 1 OR 2 BYTES          
*                                                                               
NB26     MVI   BYTE,C'T'           GET AFFID TIME/PRD                           
         BRAS  RE,NXTBDATA                                                      
         BE    *+6                                                              
         DCHO                                                                   
         LR    RF,RA                                                            
         AHI   RF,(SVCLTREC-TWAD)+(CLIST-CLTHDRD)  RF=A(CLIST)                  
         LHI   R0,220              MAX PRODUCTS                                 
*                                                                               
         CLC   3(1,R2),3(RF)       MATCH ON PRODUCT?                            
         BE    NB28                 YES                                         
         AHI   RF,4                                                             
         BCT   R0,*-14                                                          
         DCHO                                                                   
*                                                                               
NB28     MVC   0(3,R4),0(RF)       MOVE OUT PRD                                 
         AHI   R4,2                ALLOW FOR 2 CHAR PRD                         
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         AHI   R4,1                                                             
*                                                                               
         CLC   APRD2,SPACES        IS THERE A PIGGYBACK?                        
         BNH   NB29                 NO                                          
         CLI   4(R2),0              NOT NECESSARILY YES THOUGH...               
         BE    NB29                                                             
         LR    RF,RA                YES - FIND PIGGY PRD                        
         AHI   RF,(SVCLTREC-TWAD)+(CLIST-CLTHDRD)  RF=A(CLIST)                  
         LHI   R0,220              MAX PRODUCTS                                 
*                                                                               
         CLC   4(1,R2),3(RF)       MATCH ON PRODUCT?                            
         BE    NB28A                YES                                         
         AHI   RF,4                                                             
         BCT   R0,*-14                                                          
         DCHO                                                                   
*                                                                               
NB28A    MVI   0(R4),C'-'                                                       
         MVC   1(3,R4),0(RF)       MOVE OUT PRD                                 
         AHI   R4,3                ALLOW FOR 2 CHAR PRD                         
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         AHI   R4,1                                                             
*                                                                               
NB29     BRAS  RE,SETDLM           SET DELIMETER                                
         AHI   R3,1                INC SPOT COUNTER                             
         LA    RF,WORK2            START OF THE STRING                          
         SR    R4,RF               L'STRING                                     
         CR    R7,R4               STILL HAVE ROOM ON THIS LINE?                
         BNL   NB29A               YES                                          
*                                                                               
         CHI   R6,1                LAST LINE?                                   
         BNE   NB29B               NO                                           
         BCTR  R4,0                SUBTRACT FOR THE COMMA                       
         CR    R7,R4               HAVE ROOM WITHOUT THE COMMA?                 
         BL    NB29B               NO - PUT THIS ON THE NEXT INPUT LINE         
*                                                                               
NB29A    SR    R7,R4               SUBTRACT FROM TOTAL BUYLINE LENGTH           
         BCTR  R4,0                DECREMENT FOR EX                             
         EX    R4,*+8              MOVE STRING TO BUYLINE                       
         B     *+10                                                             
         MVC   0(0,R8),WORK2       STRING IS IN WORK2                           
         LA    R8,1(R4,R8)         INPUT NEXT ALLOCATION HERE                   
         B     NB23                GET NEXT ALLOCATION                          
*                                                                               
NB29B    BCT   R6,NB29C            ANY MORE LINES LEFT?                         
         MVC   ERROR,=Y(MANYRNO)   NO - ERROR                                   
         GOTO1 SENDMSG             SEND THE ERROR MESSAGE                       
*                                                                               
NB29C    AHI   R5,L'BUYDATA1       BUMP TO NEXT BUYLINE                         
         LR    R8,R5               POINT TO NEXT BUYLINE                        
         LA    R7,L'BUYDATA1       78 BYTE LINE                                 
         B     NB29A               ADD NEW ALLOCATION IN WORK2                  
*                                                                               
NB35     BCTR  R8,0                CLEAR LAST DELIMITER (IF ANY)                
         CLI   0(R8),C','                                                       
         BNE   *+8                                                              
         MVI   0(R8),C' '                                                       
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
         LTORG                                                                  
*=================================================================*             
* BACK UP TO LAST NONBLANK AND INSERT COMMA DELIMITER             *             
*=================================================================*             
         SPACE 1                                                                
SETDLM   CLI   0(R4),C' '                                                       
         JH    *+8                                                              
         BRCT  R4,SETDLM                                                        
         MVI   1(R4),C','                                                       
         AHI   R4,2                                                             
         BR    RE                                                               
         SPACE 2                                                                
*=================================================================*             
* BACK UP TO LAST NONBLANK AND INSERT SEMICOLON DELIMETER         *             
*=================================================================*             
         SPACE 1                                                                
SETDLMS  CLI   0(R4),C' '                                                       
         JH    *+8                                                              
         BRCT  R4,SETDLMS                                                       
         MVI   1(R4),SEMICOL                                                    
         AHI   R4,2                                                             
         BR    RE                                                               
         SPACE 2                                                                
*=================================================================*             
* CLEAR BUYDATA1 - 4                                              *             
*=================================================================*             
         SPACE 1                                                                
CLRBYDTA LA    R0,BUYDATA1         CLEAR BUYDATA LINES                          
         LHI   R1,BUYDATAL                                                      
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R0,R2                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* GET BDATA ITEM                                                  *             
* ON ENTRY BYTE CONTAINS ITEM WANTED                              *             
* RETURNS R2=A(DATA ITEM)                                         *             
* EXIT CC NEQ IF NOT FOUND                                        *             
*=================================================================*             
         SPACE 1                                                                
NXTBDATA CLI   0(R2),0             TEST NO MORE DATA ITEMS                      
         JNE   NXTB2                                                            
         LTR   RE,RE                NO MORE - EXIT CC NEQ                       
         BR    RE                                                               
*                                                                               
NXTB2    LA    R0,8                EST/BUY NOW 8 BYTES                          
         CLI   0(R2),C'B'          TEST BUY DATE                                
         JE    NXB10                                                            
         LA    R0,3                                                             
         CLI   0(R2),C'D'          TEST BUY DATE                                
         JE    NXB10                                                            
         CLI   0(R2),C'A'          TEST AFFID DATE                              
         JE    NXB10                                                            
         LA    R0,2                                                             
         CLI   0(R2),C'O'          TEST OTO                                     
         JE    NXB10                                                            
         CLI   0(R2),C'N'          TEST SPOTNUM                                 
         JE    NXB10                                                            
         LA    R0,9                T TYPES ALWAYS HAVE 9 CHARS NOW              
         CLI   0(R2),C'T'          TEST AFFID TIME/PRD                          
         JE    NXB10                                                            
         LA    R0,11                                                            
         CLI   0(R2),C'I'          TEST INVOICE NUMBER                          
         JE    NXB10                                                            
         DC    H'0'                                                             
*                                                                               
NXB10    AR    R2,R0                                                            
         CLC   BYTE,0(R2)          TEST FOUND ITEM WANTED                       
         BER   RE                   YES                                         
         J     NXTBDATA                                                         
         EJECT                                                                  
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*=================================================================*             
* TRANSMIT NUMERIC VALUE IN R0 AND INSERT SEMICOLON DELIMITER     *             
*=================================================================*             
         SPACE 1                                                                
*****    EDIT  (R0),(10,(R4)),ALIGN=LEFT                                        
SETNUM   DS    0H                                                               
         CVD   R0,DUB                                                           
         MVC   WORK(17),EDSAVE                                                  
         MVI   WORK+15,X'21'                                                    
         ED    WORK(17),DUB+2                                                   
         MVC   0(10,R4),WORK+17-(10)                                            
         LA    R0,10                                                            
         CLI   0(R4),C' '                                                       
         JNE   *+18                                                             
         MVC   0(10,R4),1(R4)                                                   
         MVI   10-1(R4),C' '                                                    
         BRCT  R0,*-18                                                          
         AR    R4,R0                                                            
         MVI   0(R4),SEMICOL                                                    
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         SPACE 1                                                                
*=================================================================*             
* FIND PRODUCT CODE IN CLIENT RECORD                              *             
*=================================================================*             
         SPACE 1                                                                
TSTPROD  LR    R0,RE                                                            
         CLI   SVCPROF+0,C'0'                                                   
         BNER  RE                                                               
*                                                                               
* TEST POL EST OPEN                                                             
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
*NOTE MVC  KEY+4(3),=C'POL'        FUN WITH BRAS                                
         BRAS  RF,*+8                                                           
         DC    CL4'POL '                                                        
         MVC   KEY+4(3),0(RF)                                                   
*                                                                               
         MVC   KEY+7(1),BUYEST                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         LR    RE,R0                                                            
         CLC   KEY(13),KEYSAVE     SET CC                                       
         BR    RE                                                               
         SPACE 1                                                                
*=================================================================*             
* FIND PRODUCT CODE IN CLIENT RECORD                              *             
*=================================================================*             
         SPACE 1                                                                
GETPRD   BRAS  RF,*+8              PUT A(C'***') IN RF                          
         DC    CL4'****'                                                        
         CLI   0(R1),0                                                          
         BER   RE                                                               
*                                                                               
         BRAS  RF,*+8              PUT A(C'POL') IN RF                          
         DC    CL4'POL '                                                        
         CLI   0(R1),X'FF'                                                      
         BER   RE                                                               
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,(SVCLTREC-TWAD)+(CLIST-CLTHDRD)  RF=A(CLIST)                  
*                                                                               
GETPRD2  CLC   0(1,R1),3(RF)                                                    
         BER   RE                                                               
         AHI   RF,4                                                             
         CLI   0(RF),C'A'                                                       
         JNL   GETPRD2                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
*=================================================================*             
* ADVANCE OUTPUT POINTER TO NEXT LINE                             *             
*=================================================================*             
         SPACE 1                                                                
NEXTOUT  DS    0H                                                               
         L     R4,BUYDSTR          GET ORIGINAL START OF LINE                   
         LA    R4,L'BUYDATA1(R4)   ADVANCE TO NEXT                              
         LA    RF,BUYDATAX                                                      
         CR    R4,RF                                                            
         BNLR  RE                  RETURN WITH CC NEQ                           
         ST    R4,BUYDSTR          SET NEW START                                
         LA    R0,L'BUYDATA1(R4)                                                
         AHI   R0,-11                                                           
         ST    R0,BUYDEND                                                       
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* SET/RESET VARIOUS OVERRIDE FLAGS ON AFFID ELEM                  *             
*                                                                 *             
*  ON ENTRY AFFID ELEM POINTS TO R6                               *             
*                                                                 *             
*  EXIT CC NEQ IF NO CHANGES                                      *             
*                                                                 *             
*=================================================================*             
         SPACE 1                                                                
         USING SNVIDELD,R6                                                      
SETOVR   OC    OVFLAGS,OVFLAGS                                                  
         JNZ   *+8                                                              
         CR    RE,RB                                                            
         BR    RE                                                               
*                                                                               
         CLI   OVFLM,C'Y'          SET IGNORE FILM FLAG?                        
         JNE   *+12                                                             
         OI    SNVIDCTL,SNVIDIFQ                                                
         J     *+16                                                             
         CLI   OVFLM,C'N'                                                       
         JNE   *+8                                                              
         NI    SNVIDCTL,X'FF'-SNVIDIFQ                                          
*                                                                               
         CLI   OVCST,C'Y'          SET IGNORE COST FLAG?                        
         JNE   *+12                                                             
         OI    SNVIDCTL,SNVIDICQ                                                
         J     *+16                                                             
         CLI   OVCST,C'N'                                                       
         JNE   *+8                                                              
         NI    SNVIDCTL,X'FF'-SNVIDICQ                                          
*                                                                               
         CLI   OVTIM,C'Y'          SET IGNORE TIME FLAG?                        
         JNE   *+12                                                             
         OI    SNVIDCTL,SNVIDITQ                                                
         J     *+16                                                             
         CLI   OVTIM,C'N'                                                       
         JNE   *+8                                                              
         NI    SNVIDCTL,X'FF'-SNVIDITQ                                          
*                                                                               
         CLI   OVSLN,C'Y'          SET IGNORE SPOT LENGTH FLAG?                 
         JNE   *+12                                                             
         OI    SNVIDCT2,SNVIDISQ                                                
         J     *+16                                                             
         CLI   OVSLN,C'N'                                                       
         JNE   *+8                                                              
         NI    SNVIDCT2,X'FF'-SNVIDISQ                                          
*                                                                               
         CLI   OVINT,C'Y'          SET IGNORE INTERVAL CHECKING?                
         JNE   *+12                                                             
         OI    SNVIDCTL,SNVIDSIQ                                                
         J     *+16                                                             
         CLI   OVINT,C'N'                                                       
         JNE   *+8                                                              
         NI    SNVIDCTL,X'FF'-SNVIDSIQ                                          
*                                                                               
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* TEST READ ONLY ID/FACPAK                                        *             
*=================================================================*             
         SPACE 1                                                                
TESTRO   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CXTRAINF-COMFACSD(RF)                                      
         BZR   RE                                                               
         USING XTRAINFD,RF                                                      
**NOP    TM    XIFLAG1,XITSTADV    THIS A TEST SYS?                             
**NOP    BNZR  RE                   YES - WHO CARES                             
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF                                
         BZR   RE                                                               
         DROP  RF                                                               
*                                                                               
         BRAS  RF,*+6              PUT A(=Y(NOUPDATE)) IN RF                    
         DC    Y(NOUPDATE)                                                      
         MVC   ERROR,0(RF)                                                      
         GOTO1 SENDMSG                                                          
         DC    H'0'                TAKE NO PRISONERS!                           
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PUT GROSS AND NET IN PENNIES (IF IN DOLLARS)                        *         
***********************************************************************         
*                                                                               
FIXCOST  DS    0H                                                               
         L     R0,GROSS                                                         
         MHI   R0,100                                                           
         ST    R0,GROSS                                                         
         L     R0,NET                                                           
         MHI   R0,100                                                           
         ST    R0,NET                                                           
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPMAKWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
NDAYS    DS    F                                                                
BUYDSTR  DS    A                                                                
BUYDEND  DS    A                                                                
SPOTNUM  DS    H                                                                
INVSDATE DS    CL6                                                              
INVHDPRD DS    X                   INVOICE HEADER PRD                           
INVHDPR2 DS    X                                                                
INVHDEST DS    X                                                                
INVCOSTY DS    C                                                                
RELAFDAY DS    X                                                                
RELAFTIM DS    XL2                                                              
SVBDELEM DS    CL66                                                             
ALDATE   DS    XL2                 ALLOC DATE                                   
LADATE   DS    XL2                 LAST ALLOC DATE                              
V10301   DS    CL4                                                              
V20000   DS    CL4                                                              
EDSAVE   DS    XL17                                                             
CHEKSUM  DS    XL2                                                              
PASSFLAG DS    X                                                                
PF1      EQU   X'01'                                                            
PF2      EQU   X'02'                                                            
PFFOUND  EQU   X'80'                                                            
EPRD     DS    CL3                 ALTERNATE PRD FOR GETEST                     
EEST     DS    XL1                 ALTERNATE EST FOR GETEST                     
SAVEDA   DS    F                                                                
COSTDIFF DS    PL8                                                              
L_FLAGS  DS    X                   LOCAL FLAGS                                  
L_NOSTAT EQU   X'80'                                                            
L_NOMGA  EQU   X'40'                                                            
*        EQU   X'20'                                                            
*        EQU   X'10'                                                            
*        EQU   X'08'                                                            
*        EQU   X'04'                                                            
*        EQU   X'02'                                                            
*        EQU   X'01'                                                            
*                                                                               
         DS    0D                                                               
BUYDATA1 DS    CL78                                                             
BUYDATA2 DS    CL78                                                             
BUYDATA3 DS    CL78                                                             
BUYDATA4 DS    CL78                                                             
BUYDATAL EQU   *-BUYDATA1                                                       
         ORG   *-1                                                              
BUYDATAX EQU   *                                                                
         ORG                                                                    
*                                                                               
TWAD     DSECT                                                                  
         ORG   SVOVDATA                                                         
SVCOM    DS    CL(RH6COML)                                                      
SVRCD    DS    CL(L'RH6RSNCD+L'RH6RSNTX)                                        
         ORG                                                                    
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LKNVMED  DS    XL1                                                              
LKNVCLT  DS    XL3                                                              
LKNVSTA  DS    XL5                                                              
*                                                                               
         ORG   LOCKKEY                                                          
LKBAMED  DS    XL1                                                              
LKBACLT  DS    XL3                                                              
LKBAEST  DS    XL3                                                              
*                                                                               
         ORG                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE FAFACTS                                                        
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENHIST                                                      
       ++INCLUDE SPXBHISTD                                                      
       ++INCLUDE SPMGADN                                                        
       ++INCLUDE SPGETBUYD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087SPMAK20   10/14/20'                                      
         END                                                                    
