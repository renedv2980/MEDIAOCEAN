*          DATA SET PPMBC00    AT LEVEL 054 AS OF 06/22/10                      
*PHASE T41800A                                                                  
*INCLUDE PUBFLOAT                                                               
*INCLUDE SRCHCALL                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41800 - PRINTPAK MULTI-BUY DISP/CHA VALIDATION'                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE 6/17/10  DELETE REFERENCE TO POWER CODES NO LONGER ON DDS SYSTEM         
*                                                                               
* KWAN 06/12/08 CHECK FOR FX PROFILE AT CLIENT VALIDATION                       
*                                                                               
* SMYE 10/21/05 - DEACTIVATE SPECIAL MINDSHARE PRD SECURITY IN EDTPRD           
*                                                                               
* BOBY 11/14/03 ALLOW 'CHAPLAY' AND 'CHGPLAY' AS ACTIONS                        
*                                                                               
* KWAN 06/12/02 REDUCE PROGRAM SIZE FOR CU LOOK UP FEATURE                      
*                                                                               
* SMYE 04/02    SPECIAL PRODUCT SECURITY FOR CERTAIN MINDSHARE ID'S             
*               AND PUBVAL AND PUBEDIT BECOME CORE-RESIDENT                     
*                                                                               
* SMYE 02/02    NEW SECURITY CHANGES                                            
*                                                                               
* KWAN 09/07/01 PRD GRP FILTERING (PRD=ALL AND KEYWORD IS PGR=)                 
*                                                                               
* KWAN 08/08/01 "NO TRAFFIC" (DATASW=W)                                         
*                                                                               
* KWAN 07/09/01 LEGAL WARNING CODES (DATASW=W)                                  
*                                                                               
* BPLA 04/01    DISALLOW ALL RATES IF SUPPRESSING COST                          
*               OPTION VALUE IS X                                               
*                                                                               
* BPLA 01/01    DISPLAY OF SERIAL NUMBER (LIKE UPID)                            
*                                                                               
* BPLA 01/01    USE COMFACS GETPROF (INSTEAD OF *INCLUDE)                       
*               *INCLUDE GETPROF CAN'T READ USERID PROFILES                     
*                                                                               
* SMYE 12/00    SAVE CLIENT DATA FOR "FROZEN-BY-DATE" FEATURE                   
*                                                                               
* SMYE 11/28/00 DISABLE (*NOP*) IMPS (IMPRESSIONS) - ALLOW ONLY EIMPS           
*               AND AIMPS (ESTIMATED AND ACTUAL IMPRESSIONS)                    
*                                                                               
* KWAN 05/00    ADD AIMPS (SIMILAR TO EIMPS WHICH IS SAME AS IMPS)              
*                                                                               
* KWAN 04/00    NEW DATA FIELD KEY WORD, UPID                                   
*                                                                               
* KWAN 03/00    IN LASTIO, PASS DELETED REC AND PROTECT DATA FIELD              
*                                                                               
* KWAN 03/00    ADD CODES FOR LAST INSERTION ORDER DISPLAY                      
*                                                                               
* KWAN 12/99    ADD MORE DATA KEY WORDS TO THE HELP SCREEN                      
*                                                                               
* KWAN 12/99    EXPAND REC STORAGE SIZE FROM 3000C TO 4000C                     
*                                                                               
* BPLA 11/99    DISALLOW ACTION MBC FOR RATES - ALLOWED                         
*               FOR INCH AND LINE RATES.                                        
*                                                                               
* BPLA 11/99    DATA LIMIT ACCESS FOR COST 2 FACTOR (X'80')                     
*               SAMES AS PLANNED COST, RATES                                    
*                                                                               
* BPLA 99/99    CHANGES FOR RATE CHANGES - SEE **RATES**                        
*               COPY OF PPMBC00/6 MADE 9/24/99                                  
*      10/99    ACCEPT ON-SALE AND CLOSING DATES FOR NEWSPAPERS                 
*                                                                               
* KWAN 06/99    SET DOLSW TO C'0' IF DOLLAR TYPE IS NOT ENTERED                 
*                                                                               
* BPLA 06/99    ADD IMPS TO DISPLAY OF AVAILABLE DATA                           
*                                                                               
* KWAN 04/99    NEW DATASW FOR IMPRESSION (DATASW=C'I')                         
*                                                                               
* SMYE 03/99    ADD LOGIC TO SET COST2SW FOR GETINS AT EDTO6V                   
*                                                                               
* SMYE 02/99    ADD COST 2 FACTOR (DATASW=C'B')                                 
*                                                                               
* SMYE 04/98    SAVE PCLTSTAT AT SVCLPROF+30 ("FROZEN" CLIENT INDIC.)           
*                                                                               
* BPLA 03/98    CHANGE FOR EXPANDED CONIO (USED BY GETADVC)                     
*               CONIO IN PPMBCWRK EXPANDED TO 6000 BYTES                        
*                                                                               
* BPLA 02/98    ALLOW WESTERN ID TO ONLY DISPLAY T/S STATUS                     
*                                                                               
* BPLA 01/98    ALLOW SJR TO CHANGE TEARSHEET STATUS                            
*               (I.E. NO LONGER TREAT AS A WESTERN AGENCY)                      
*               MX ADDED TO WESTERN AGENCY CHECK                                
*                                                                               
* BPLA 01/98    ADD BILLED AND PAID FILTERS, SJR CHECKS REMOVED                 
*                                                                               
* SMYE 10/97    ADD LOGIC FOR SFH (SPECIAL FINANCIAL HANDLING)                  
*               (DATASW=C'X')                                                   
*                                                                               
* SMYE 10/08/97 GETINS MADE CORE-RESIDENT                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41800   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 2550,T41800,RR=R9                                                
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         LA    R8,T41800+4095   NOTE USE R8 AND R9 AS BASE REGISTERS            
         LA    R8,1(R8)                                                         
         LA    R9,4095(R8)                                                      
         LA    R9,1(R9)                                                         
         USING T41800+4096,R8,R9                                                
*                                                                               
         USING T418FFD,RA                                                       
*                                                                               
         BAS   RE,INITL                                                         
*                                                                               
         NI    DMINBTS,X'7F'    SET OFF READ FOR UPDATE X'80'                   
*                                                                               
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         LH    RF,=Y(CONIO-GENOLD)                                              
         AR    RF,RC                                                            
         ST    RF,ACONIO                                                        
*                                                                               
         LH    RF,=Y(PRDTAB-GENOLD)                                             
         AR    RF,RC                                                            
         ST    RF,APRDTAB                                                       
*                                                                               
         L     RF,ACOMFACS         POINT TO COMFACS                             
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF) SAVE GLOBBER ADDRESS              
*                                                                               
         LH    RF,=Y(WRKREC-GENOLD)                                             
         AR    RF,RC                                                            
         ST    RF,AWRKREC                                                       
*                                                                               
         LH    RF,=Y(MYPUBIO-GENOLD)                                            
         AR    RF,RC                                                            
         ST    RF,APUBIO                                                        
*                                                                               
         LH    RF,=Y(SECBLK-GENOLD)                                             
         AR    RF,RC                                                            
         ST    RF,ASECBLK                                                       
*                                                                               
         MVC   FULL,=X'D9000AAB'                                                
         BRAS  RE,GETCORES                                                      
         MVC   VGETINS,DMCB        STORE GETINS ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AB8'                                                
         BRAS  RE,GETCORES                                                      
         MVC   VPUBVAL,DMCB        STORE PUBVAL ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AB9'                                                
         BRAS  RE,GETCORES                                                      
         MVC   VPUBEDIT,DMCB       STORE PUBEDIT ADDRESS                        
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,BTODAY)                                    
*                                                                               
         LA    R3,REC                                                           
         ST    R3,AREC                                                          
         XC    MBCEMSG,MBCEMSG                                                  
         FOUT  MBCEMSGH                                                         
         L     RF,=A(HEAD1)        POINT TO HEADING                             
         A     RF,RELO             RE-LOCATE ADDRESS                            
         MVC   MBCHD1,0(RF)        SET DEFAULT HEADINGS                         
         L     RF,=A(HEAD2)        POINT TO HEADING                             
         A     RF,RELO             RE-LOCATE ADDRESS                            
         MVC   MBCHD2,0(RF)        SET DEFAULT HEADINGS                         
         FOUT  MBCHD1H                                                          
         FOUT  MBCHD2H                                                          
         MVI   PVSW,1                                                           
*                                                                               
         BRAS  RE,LEGWHDR          SET LEGAL WARNINGS HEARD IF NEEDED           
*                                                                               
******************************************************************              
**  THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK                               
**  WHICH ARE "PERSONAL ID"                                                     
*                                                                               
PID      XC    SVMBCPID,SVMBCPID         PASSWORD ID NUMBER CLEARED             
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0),0,0                                          
         DROP  RF                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'          CHECK IF SECRET CODE IS THERE             
         BZ    *+10                                                             
         MVC   SVMBCPID,FAPASSWD      SAVE PASSWORD ID NUMBER                   
         DROP  R1                                                               
         SPACE 1                                                                
*                                                                               
*                                                                               
* CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE                      
* ASECBLK POINTS TO 1024 BYTES OF SAVED STORAGE                                 
*                                                                               
         L     R0,ASECBLK                                                       
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    NOSECRET                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
NOSECRET DS    0H                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECK GLOBBER TO SEE IF THIS IS CALL FROM MATCH                               
* IF SO GO FILL IN FIELDS ON SCREEN WITH THOSE FROM GLOBBER                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MBCGLB   DS    0H                                                               
         OC    VGLOBBER,VGLOBBER   MUST HAVE GLOBBER ADDRESS                    
         BZ    MBCGLBX                                                          
*                                                                               
* SKIP IF STILL PROCESSING CALL FROM INVOICE MATCH                              
*                                                                               
         CLI   INVMATSW,C'Y'       IF NOT DOING INV MATCH CALL                  
         BNE   MBCGLB10            CK GLOBBER AREA                              
*                                                                               
         L     RF,VTIOB            POINT TO INPUT AREA                          
         USING TIOBD,RF            ESTABLISH AREA                               
         CLI   TIOBAID,12          LOOK FOR PFKEY 12 HIT                        
         BE    *+8                                                              
         CLI   TIOBAID,24                                                       
         BNE   MBCGLBX             SKIP GLOBBER IF NOT FOUND                    
*                                                                               
         MVC   MBCID,=CL17'=SW '   SWAP BACK TO MATCH                           
         B     EXIT1                                                            
         DROP  RF                                                               
*                                                                               
* READ GLOBBER AREA FOR MBC DATA TYPES                                          
* IF FOUND WE ARE COMING FROM INVOICE MATCH ELSE NORMAL MBC                     
*                                                                               
MBCGLB10 DS    0H                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',MBCDATH,,GLVPRDTA                         
*                                                                               
         CLI   DMCB+8,GLEGNF       DONE IF NOT FOUND                            
         BE    MBCGLBX                                                          
*                                                                               
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FILL IN SCREEN FROM GLOBBER                                                   
*                                                                               
         BRAS  RE,FILSCR                                                        
*                                                                               
MBCGLBX  DS    0H                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LA    R2,MBCDATH          FIRST CHECK FOR ? IN DATA                    
         CLI   8(R2),C'?'          QUESTION MARK                                
         BE    DISOPTS             GO DISPLAY OPTIONS                           
*                                                                               
EDTMED   DS    0H                  EDIT MEDIA                                   
         LA    R2,MBCMDIAH         EDIT MEDIA                                   
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTCLT                                                           
         BAS   RE,UNVAL                                                         
         BAS   RE,ANY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),8(R2)                                                   
         MVI   KEY+3,X'01'                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BE    EDTM10                                                           
FLDERR   LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTM10   BAS   RE,GETREC                                                        
         MVC   SVAPROF,PAGYPROF                                                 
         FOUT  MBCMDESH,PAGYMED,10                                              
         MVC   SVMED,8(R2)                                                      
         OI    4(R2),X'20'         SET VALIDATED                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTCLT   DS    0H                  EDIT CLIENT                                  
         LA    R2,MBCCLTH                                                       
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTCLTX             MUST STILL SET MBCHD1 AND MBCHD2             
         BAS   RE,UNVAL                                                         
         BAS   RE,ANY                                                           
         CLI   8(R2),C'?'                                                       
         BE    FLDERR                                                           
         CLI   8(R2),C'*'          DOING OFFICES?                               
         BE    FLDERR                                                           
         CLC   8(3,R2),=C'ALL'     ALL CLIENT?                                  
         BE    FLDERR                                                           
*                                                                               
* MBC CANNOT HANDLE OFFICES AND ALL CLIENTS!                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),8(R2)                                                   
         OC    KEY+4(3),=3C' '                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+12                                                             
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
         BAS   RE,GETREC                                                        
         MVC   SVCLT,KEY+4                                                      
*                                                                               
         BRAS  RE,CKFXPROF                                                      
         JNE   ERR_EXIT                                                         
*                                                                               
ACCCHK   DS    0H                  CK FOR ANY LIMIT ACCESS                      
         MVC   SVCOFF,PCLTOFF      SAVE CLIENT OFFICE                           
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   ACCXCL                                                           
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    ACCX                NOTHING TO CHECK                             
*                                                                               
ACCXCL   DS    0H                  TEST FOR "IGNORE CHECK" ENTRY                
         CLI   6(RA),C'*'          OFFICE OR CLT GRP LIMIT ACCESS ?             
         BNE   ACCTST              NO                                           
         CLI   PCLTOFF+2,C'*'      IGNORE OFFICE CHECK ?                        
         BNE   ACCTST              NO                                           
         CLI   7(RA),C'A'          SEE IF CLT GRP (*AN)                         
         BL    ACCX                NO - MUST BE OFFICE - IGNORE CHECK           
         CLI   7(RA),C'Z'                                                       
         BH    ACCX                MUST BE OFFICE - IGNORE CHECK                
         CLI   8(RA),C'0'          SEE IF 3RD BYTE IS NUMERIC                   
         BL    ACCX                NO - NOT CLT GRP - IGNORE CHECK              
         CLI   8(RA),C'9'                                                       
         BH    ACCX                NOT CLT GRP - IGNORE CHECK                   
*                                                                               
ACCTST   DS    0H                                                               
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON ?                         
         BNE   ACCTSTD             NO                                           
         BRAS  RE,TRAFFACC         LOOK FOR CLIENT TRAFFIC OFFICE CODE          
         CLI   BYTE3,0             ANYTHING FOUND ?                             
         BE    ACCTSTD             NO                                           
         MVC   PCLTOFF,BYTE3       USE CLIENT TRAFFIC OFFICE CODE TO..          
*                                                                               
ACCTSTD  DS    0H                  TEST OFFICE SECURITY (ANY ACCESS)            
         BAS   RE,PPCLIVER                                                      
         BE    ACCX                                                             
*                                                                               
ACCE     LA    R3,ACCERR           NOT AUTHORIZED                               
         B     ERROR                                                            
*                                                                               
ACCX     DS    0H                                                               
         MVC   PCLTOFF,SVCOFF     "RESTORE" CLIENT OFFICE                       
*                                                                               
         FOUT  MBCCLTNH,PCLTNAME,20                                             
         MVC   SVCLPROF,PCLTPROF                                                
         MVC   SVCLPROF+30(1),PCLTSTAT                                          
*                                                                               
* CHECK FOR CLIENT FROZEN-BY-DATE                                               
* SET FOR NOT FROZEN WITH DATE                                                  
*                                                                               
         XC    SVCLPROF+27(3),SVCLPROF+27                                       
*                                                                               
         TM    SVCLPROF+30,X'02'   FROZEN CLIENT ?                              
         BNO   EDCLX               NO                                           
         TM    SVCLPROF+30,X'10'   FROZEN WITH DATE ?                           
         BNO   EDCLX               NO                                           
*                                                                               
* GET FREEZE STATUS ELEMENT (MUST BE THERE IF FROZEN WITH DATE)                 
* SAVE STATUS INDICATOR AND YM FREEZE DATE IN SVCLPROF+27(3)                    
*                                                                               
         LA    R6,PCLTREC+33                                                    
EDCLF5   CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN                            
         CLI   0(R6),X'47'                                                      
         BE    EDCLF8                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0               JUST IN CASE                                 
         BNZ   *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN                            
         AR    R6,R0                                                            
         B     EDCLF5                                                           
*                                                                               
* SAVE FRZ STATUS ELEM DATA IN SVCLPROF+27                                      
*                                                                               
EDCLF8   MVC   SVCLPROF+27(3),2(R6)                                             
*                                                                               
EDCLX    DS   0H                                                                
         XC    SADVDATA,SADVDATA                                                
         LA    R6,PCLTREC+33                                                    
EDTC12   CLI   0(R6),0                                                          
         BE    EDTC14                                                           
         CLI   0(R6),X'15'                                                      
         BE    EDTC13                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0               JUST IN CASE                                 
         BZ    EDTC14                                                           
         AR    R6,R0                                                            
         B     EDTC12                                                           
*                                                                               
EDTC13   MVC   SADVDATA(18),2(R6)  SAVE ADVERTISER DATA                         
*                                                                               
EDTC14   DS    0H                                                               
         OI    4(R2),X'20'                                                      
*                                                                               
* NOW READ MB PROFILE                                                           
*                                                                               
         XC    WORK+10(30),WORK+10                                              
         MVC   WORK+10(4),=C'P0MB'                                              
         MVC   WORK+14(2),AGYALPHA                                              
         MVC   WORK+16(1),SVMED                                                 
         MVC   WORK+17(3),SVCLT                                                 
         CLI   PCLTOFF,C' '                                                     
         BNH   ACCX5                                                            
         MVI   WORK+20,C'*'                                                     
         MVC   WORK+21,PCLTOFF                                                  
*                                                                               
ACCX5    DS    0H                                                               
         L     R5,VDATAMGR                                                      
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,(0,WORK+10),MBPROF,(0(RA),(R5))                    
         DROP  RF                                                               
*                                                                               
         MVI   OUT2OPT,C'N'                                                     
         CLI   MBPROF+2,0                                                       
         BE    *+10                                                             
         MVC   OUT2OPT,MBPROF+2                                                 
*                                                                               
* READ BY PROFILE REST OF WORK+10(11) STILL OK                                  
*                                                                               
         MVC   WORK+10(4),=C'P0BY'                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,(0,WORK+10),BYPROF,(0(RA),(R5))                    
         DROP  RF                                                               
*                                                                               
EDTCLTX  CLI   SVIOSW,0            SEE IF DISPLAYING LASTIO                     
         BE    EDTCLTX5                                                         
         MVC   MBCHD1+56(11),=C'   LAST I/O'                                    
         MVC   MBCHD2+56(11),=C'   --------'                                    
         B     EDTCLTXX                                                         
*                                                                               
EDTCLTX5 CLI   MBPROF+0,C'X'       SEE IF SUPPRESSING COST                      
         BE    EDTCLTX7            CLEAR COST HEADING                           
         CLI   MBPROF+0,C'Y'       SEE IF SUPPRESSING COST                      
         BNE   EDTCLTXX            CLEAR COST HEADING                           
*                                                                               
EDTCLTX7 XC    MBCHD1+63(4),MBCHD1+63                                           
         XC    MBCHD2+63(4),MBCHD2+63                                           
*                                                                               
EDTCLTXX DS    0H                                                               
         FOUT  MBCHD1H                                                          
         FOUT  MBCHD2H                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPRD   DS    0H                  EDIT PRODUCT                                 
         LA    R2,MBCPRDH                                                       
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTEST                                                           
         BAS   RE,UNVAL                                                         
         BAS   RE,ANY                                                           
         MVC   SVPRD,8(R2)                                                      
         CLI   8(R2),C'?'          DON'T ACCEPT Q MARK                          
         BE    FLDERR              INVALID                                      
*                                                                               
         B     EDTP1               BRANCH AROUND DISCONTINUED LOGIC             
* 10/21/05 - BELOW LOGIC DISCONTINUED AT AGENCY REQUEST                         
*                                                                               
         LR    RE,RA               SPECIAL MINDSHARE PRD SECURITY               
         USING TWAD,RE                                                          
         ZICM  RF,TWAUSRID,2                                                    
         DROP  RE                                                               
         MVI   AGYLIMIT,C' '       CLEAR                                        
         CHI   RF,10296            "MSSROC" USER ID ?                           
         BE    EDTP0B              YES                                          
         CHI   RF,10297            "MSSRYC" USER ID ?                           
         BE    EDTP0C                                                           
         B     EDTP1                                                            
*                                                                               
EDTP0B   DS    0H                                                               
         MVI   AGYLIMIT,C'6'       "SPECIAL" AGENCY (MSSROC)                    
         B     EDTP1                                                            
*                                                                               
EDTP0C   DS    0H                                                               
         MVI   AGYLIMIT,C'7'       "SPECIAL" AGENCY (MSSRYC)                    
*                                                                               
* 10/21/05 - ABOVE LOGIC DISCONTINUED AT AGENCY REQUEST                         
*                                                                               
EDTP1    CLC   8(3,R2),=C'ALL'                                                  
         BNE   EDTP2                                                            
*SMY*    CLI   AGYLIMIT,C' '       "SPECIAL" AGENCY ?                           
*SMY*    BNE   FLDERR              YES - ALL NOT ALLOWED IN PRODUCT             
         B     EDTP10                                                           
*                                                                               
EDTP2    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),SVCLT                                                   
         MVC   KEY+7(3),8(R2)                                                   
         OC    KEY+7(3),=3C' '                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    EDTP5                                                            
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
EDTP5    BAS   RE,GETREC                                                        
*                                                                               
         B     EDTP7               BRANCH AROUND DISCONTINUED LOGIC             
* 10/21/05 - BELOW LOGIC DISCONTINUED AT AGENCY REQUEST                         
*                                                                               
         CLI   AGYLIMIT,C' '       "SPECIAL" SECURITY ?                         
         BE    EDTP7               NO - CONTINUE                                
         CLI   AGYLIMIT,C'6'       "SPECIAL" SECURITY ? (MSSROC)                
         BNE   EDTP5B              NO                                           
         CLI   PPRDTRAF,C'6'       PRODUCT TRAFFIC OFFICE=6 ?                   
         BE    EDTP7               YES - OK                                     
         B     EDTP5NG                                                          
*                                                                               
EDTP5B   CLI   AGYLIMIT,C'7'       "SPECIAL" SECURITY ? (MSSRYC)                
         BE    *+6                 YES                                          
         DC    H'0'                MUST BE 6 OR 7 OR BLANK                      
         CLI   PPRDTRAF,C'7'       PRODUCT TRAFFIC OFFICE=7 ?                   
         BE    EDTP7               YES - OK                                     
*                                                                               
EDTP5NG  DS    0H                                                               
         LA    R3,PPESECLK         SECURITY LOCKOUT                             
         B     ERROR                                                            
*                                                                               
* 10/21/05 - ABOVE LOGIC DISCONTINUED AT AGENCY REQUEST                         
*                                                                               
EDTP7    DS    0H                                                               
         FOUT  MBCPRDNH,PPRDNAME,20                                             
         MVC   SVPRD,KEY+7                                                      
         B     EDTP12                                                           
*                                                                               
EDTP10   XC    MBCPRDN,MBCPRDN                                                  
         FOUT  MBCPRDNH                                                         
*                                                                               
EDTP12   OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTEST   DS    0H                  EDIT ESTIMATE                                
         LA    R2,MBCESTH                                                       
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTST                                                            
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,UNVAL                                                         
         BAS   RE,ANY                                                           
         XC    SVESTRT(12),SVESTRT CLEAR EST DATES                              
         XC    SVEST,SVEST                                                      
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   EDTE2                                                            
         MVC   SVEST,8(R2)                                                      
         B     EDTE10                                                           
*                                                                               
EDTE2    CLI   8(R2),C'?'                                                       
         BE    ESTINV                                                           
*                                                                               
EDTE3    XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(6),SVCLT      CLT AND PRD                                  
         CLC   SVPRD,=C'ALL'                                                    
         BNE   *+10                                                             
         MVC   KEY+7(3),=C'ZZZ'    TRY FOR ZZZ                                  
         BAS   RE,PACK                                                          
         CP    DUB,=P'0'                                                        
         BNE   EDTE5                                                            
ESTINV   LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTE5    STH   R0,HALF                                                          
         MVC   KEY+10(2),HALF                                                   
         MVC   SVESTB,HALF                                                      
*                                                                               
* PROBLEM FOR EST READ COULD BE WHERE TO BRANCH TO FROM                         
* HERE - EDTE9, OR EDTE10                                                       
*                                                                               
EDTE9    DS    0H                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(12),KEYSAVE                                                  
         BE    EDTE8                                                            
         CLC   SVPRD,=C'ALL'                                                    
         BE    EDTE10                                                           
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
EDTE8    BAS   RE,GETREC                                                        
         MVC   SVESTRT,PESTST                                                   
         MVC   SVEEND,PESTEND                                                   
         MVC   MBCESTN,PESTNAME                                                 
         B     EDTE12                                                           
*                                                                               
EDTE10   XC    MBCESTN,MBCESTN                                                  
EDTE12   FOUT  MBCESTNH                                                         
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTST    DS    0H                  EDIT START DATE                              
         LA    R2,MBCSTDEH                                                      
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTEND                                                           
         BAS   RE,UNVAL                                                         
         MVI   PRSW,0                                                           
         MVI   SUBSW,0                                                          
*                                                                               
         XC    SVSTRT,SVSTRT                                                    
         XC    SVEND,SVEND                                                      
         XC    SVSTRTB,SVSTRTB                                                  
         XC    SVENDB,SVENDB                                                    
         CLI   5(R2),0             ALLOW NO INPUT                               
         BE    EDTST10                                                          
         CLC   8(2,R2),=C'ES'                                                   
         BNE   EDTST5                                                           
         CLI   SVESTRT,0           SEE IF I HAVE EST DATES                      
         BNE   EDTST3              YES - OK                                     
         LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTST3   MVC   SVSTRT,SVESTRT      USE EST DATES                                
         MVC   SVEND,SVEEND                                                     
         MVI   PRSW,1              SET PRSW AND SUBSW FOR ES                    
         MVI   SUBSW,1                                                          
         CLC   MBCEDDE(2),=C'ES'                                                
         BE    EDTST4                                                           
         CLI   MBCEDDEH+5,0                                                     
         BE    EDTST4                                                           
         LA    R2,MBCEDDEH                                                      
         B     DATERR                                                           
*                                                                               
EDTST4   OI    MBCEDDEH+4,X'20'                                                 
         B     EDTST10                                                          
*                                                                               
EDTST5   GOTO1 VDATVAL,DMCB,(0,8(R2)),SVSTRT                                    
         OC    DMCB(4),DMCB        FIRST CK FOR MMDDYY                          
         BNZ   EDTST7                                                           
*                                                                               
         GOTO1 VDATVAL,DMCB,(2,8(R2)),SVSTRT                                    
         OC    DMCB(4),DMCB        IF ERROR TRY MMYY                            
         BNZ   EDTST7                                                           
*                                                                               
DATERR   LA    R3,DTEINV                                                        
         B     ERROR                                                            
*                                                                               
EDTST7   CLC   5(1,R2),DMCB+3                                                   
         BE    EDTST10             NO MORE INPUT                                
         LA    R4,8(R2)                                                         
         ZIC   R5,DMCB+3                                                        
         AR    R4,R5                                                            
         CLC   0(2,R4),=C'-P'                                                   
         BNE   DATERR                                                           
         MVI   PRSW,1                                                           
*                                                                               
EDTST10  OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTEND   DS    0H                  EDIT END DATE                                
         LA    R2,MBCEDDEH                                                      
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    EDTPUB                                                           
         MVI   SUBSW,0             SET OFF SUBSEQUENT SW                        
         BAS   RE,UNVAL                                                         
         MVC   SVEND,SVSTRT        SET END TO START                             
         CLI   5(R2),0             CK FOR INPUT                                 
         BE    EDTEND12            NO                                           
         CLC   8(2,R2),=C'ES'                                                   
         BNE   EDTEND5                                                          
         MVI   SUBSW,1             SET ON SUBSEQUENT SW                         
         MVC   SVEND,SVEEND        USE EST DATE                                 
         CLI   SVEND,0                                                          
         BNE   EDTEND12                                                         
         LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTEND5  GOTO1 VDATVAL,DMCB,(0,8(R2)),SVEND                                     
         OC    DMCB(4),DMCB        TRY FOR MMMDD/YY                             
         BNZ   EDTEND10                                                         
*                                                                               
         GOTO1 VDATVAL,DMCB,(2,8(R2)),SVEND                                     
         OC    DMCB(4),DMCB        IF ERROR - TRY MMM/YY                        
         BZ    DATERR                                                           
         MVC   SVEND+4(2),=C'31'   SET DAY TO 31 FOR MONTH                      
*                                                                               
EDTEND10 CLC   5(1,R2),DMCB+3                                                   
         BE    EDTEND12            NO MORE INPUT                                
         LA    R4,8(R2)                                                         
         ZIC   R0,DMCB+3                                                        
         AR    R4,R0                                                            
         CLC   0(2,R4),=C'-S'                                                   
         BNE   DATERR                                                           
         MVI   SUBSW,1                                                          
*                                                                               
EDTEND12 DS    0H                                                               
         CLC   SVSTRT,SVEND                                                     
         BNH   EDTEND20                                                         
         LA    R3,DATEERR          START AFTER END                              
         B     ERROR                                                            
*                                                                               
EDTEND20 OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPUB   DS    0H                  EDIT PUB                                     
         LA    R2,MBCPUBH                                                       
         TM    4(R2),X'20'                                                      
         BO    EDTOPT                                                           
         BAS   RE,UNVAL                                                         
         MVI   PVSW,0              UNVALIDATE PREV VAL SW                       
         XC    PREVKEY,PREVKEY                                                  
         XC    MBCPUBN,MBCPUBN                                                  
         BAS   RE,ANY                                                           
*                                                                               
* NAME SEARCH CALL                                                              
*                                                                               
         SR    R2,RA               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,SVMED                                                   
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       +        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         DROP  R3                                                               
*                                                                               
         LA    R2,MBCPUBH                                                       
*                                                                               
         CLI   8(R2),C'?'                                                       
         BE    EPUBERR                                                          
         XC    SVPUB,SVPUB                                                      
         XC    SVPUBL,SVPUBL       CLEAR PUBLIST FILTER                         
         MVI   SVPQMK,0                                                         
         MVC   SVPUB(3),8(R2)                                                   
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   EDTPUBL                                                          
*                                                                               
* PUB CAN'T BE 'ALL' IF CLT AND PRD AND EST  ARE                                
* NOTE - MUST CK INPUT FIELDS NOT SV FIELDS                                     
*                                                                               
         CLC   MBCCLT,=C'ALL'                                                   
         BE    EDTPUB0                                                          
         CLI   MBCCLT,C'*'         OR OFFICE                                    
         BNE   EDTPUB0X                                                         
EDTPUB0  CLC   MBCPRD,=C'ALL'                                                   
         BNE   EDTPUB0X                                                         
         CLI   SVEST+2,C'?'                                                     
         BE    EDTPUB0X                                                         
EDTPUB05 B     EPUBERR             NO ALL,ALL,ALL,ALL                           
*                                                                               
EDTPUB0X B     EDTPUB10                                                         
*                                                                               
EDTPUBL  DS    0H                                                               
         CLC   8(2,R2),=C'L='      PUBLIST                                      
         BNE   EDTPUB1                                                          
         XC    SVPUB,SVPUB                                                      
         MVC   SVPUB(3),=C'ALL'                                                 
         CLI   5(R2),5             MUST BE L=XXX                                
         BH    PUBERR                                                           
         MVC   SVPUBL,10(R2)                                                    
         OC    SVPUBL,=C'   '                                                   
         CLC   MBCCLT,=C'ALL'      CLIENT CAN'T BE ALL OR *                     
         BE    PUBERR                                                           
         CLI   MBCCLT,C'*'         CLIENT CAN'T BE ALL OR *                     
         BE    PUBERR                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'17'                                                      
         MVC   KEY+4(3),SVCLT                                                   
         MVC   KEY+7(3),SVPUBL                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    EDTPUB10                                                         
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
EDTPUB1  DS    0H                                                               
         ZIC   R4,5(R2)                                                         
         LA    R5,8(R2)                                                         
         CLI   SVPUB,C'?'          QUESTION MARK                                
         BNE   EDTPUB2                                                          
         MVI   SVPQMK,C'?'         QUESTION MARK                                
         XC    SVPUB,SVPUB                                                      
         CLI   5(R2),1                                                          
         BE    EDTPUB10                                                         
         CLC   SVPRD,=C'ALL'       PRD CAN'T BE ALL WITH START AT PUB           
         BE    EPUBERR                                                          
         BCTR  R4,0                                                             
         LA    R5,9(R2)                                                         
EDTPUB2  GOTO1 VPUBVAL,DMCB,((R4),0(R5)),(0,SVPUB)                              
         CLI   DMCB,X'FF'                                                       
         BNE   EDTPUB3                                                          
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         AHI   R0,-4                                                            
         BM    EPUBERR                                                          
         LA    R4,8(R2)                                                         
         AR    R4,R0                                                            
         CLC   0(4,R4),=C',ALL'    CK FOR ALL ZONES,EDTS                        
         BNE   EPUBERR                                                          
         CLI   SVPQMK,0            NO ALL ZONES,EDTS FOR QMARK                  
         BNE   EPUBERR                                                          
         GOTO1 VPUBVAL,DMCB,((R0),8(R2)),(0,SVPUB)                              
         CLI   0(R1),X'FF'                                                      
         BE    EPUBERR                                                          
         OC    SVPUB+4(2),SVPUB+4                                               
         BNZ   EPUBERR                                                          
         MVC   SVPUB+4(2),=X'FFFF'                                              
         B     EDTPUB3                                                          
EPUBERR  LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTPUB3  DS    0H                                                               
         CLI   SVPQMK,C'?'         QMARK                                        
         BE    EDTPUB10            BYPASS PUB READ FOR QMARK                    
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVMED                                                     
         MVC   KEY+1(6),SVPUB                                                   
         LA    R5,6                                                             
         CLI   SVPUB+4,X'FF'       SEE IF DOING ALL PUBS/EDTS                   
         BNE   EDTPUB3C                                                         
*                                                                               
EDTPUB3B DS    0H                                                               
         LA    R5,4                                                             
         XC    KEY+5(2),KEY+5                                                   
EDTPUB3C DS    0H                                                               
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
EDTPUB4  BAS   RE,HIGHPUB                                                       
EDTPUB4A EX    R5,*+8                                                           
         BNE   PUBERR                                                           
         CLC   KEY(0),KEYSAVE                                                   
         CLC   KEY+7(3),KEYSAVE+7  CK AGY                                       
         BE    EDTPUB8             HAVE FOUND A VALID PUB                       
*                                                                               
EDTPUB6  CLI   SVAPROF+16,C'0'                                                  
         BE    EDTPUB6B                                                         
         CLC   KEY+7(2),=C'ZZ'     SEE IF I FOUND DEFAULT                       
         BE    EDTPUB8                                                          
EDTPUB6B BAS   RE,SEQPUB                                                        
         B     EDTPUB4A                                                         
*                                                                               
EDTPUB8  DS    0H                                                               
         BAS   RE,GETPUB                                                        
         XC    MYWORK(L'MBCPUBN),MYWORK                                         
         GOTO1 =V(PUBFLOAT),DMCB,APUBIO,MYWORK,RR=RELO                          
         MVC   MBCPUBN(L'MBCPUBN),MYWORK                                        
*                                                                               
EDTPUB10 FOUT  MBCPUBNH                                                         
*                                                                               
* IF DISPLAY IS FOR ONE PUB SET PUBNOPT TO 'N'                                  
*                                                                               
         MVC   PUBNOPT,MBPROF+1    RESET PUBNOPT TO PROFILE                     
         CLI   PUBNOPT,0                                                        
         BNE   *+8                                                              
         MVI   PUBNOPT,C'N'                                                     
*                                                                               
         CLC   SVPUB(3),=C'ALL'                                                 
         BE    EDTPUB15                                                         
         CLC   SVPUB+4(2),=X'FFFF' SEE IF ALL ZONES/EDTS                        
         BE    EDTPUB15                                                         
         OC    SVPUB,SVPUB                                                      
         BZ    EDTPUB15                                                         
         MVI   PUBNOPT,C'N'                                                     
*                                                                               
EDTPUB15 DS    0H                                                               
         OI    4(R2),X'20'                                                      
         B     EDTOPT                                                           
*                                                                               
PUBERR   LA    R3,NOFNDERR         PUB NOT FOUND                                
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTOPT   DS    0H                  EDIT OPTIONAL DATA                           
         LA    R2,MBCDATH                                                       
         TM    4(R2),X'20'                                                      
         BO    EDTACT                                                           
         MVI   PVSW,0                                                           
         MVI   LACTION,0                                                        
         MVC   WORK(4),SVREP       SAVE OLD SPECIAL REP                         
         MVC   WORK+4(1),SVDTYP    SAVE OLD DATE TYPE                           
         MVC   WORK+5(1),SVDATYP   SAVE OLD DATA TYPE                           
*                                                                               
* IF OPTIONS CHANGED RESET PUBNOPT                                              
*                                                                               
         MVC   PUBNOPT,MBPROF+1    RESET PUBNOPT TO PROFILE                     
         CLI   PUBNOPT,0                                                        
         BNE   *+8                                                              
         MVI   PUBNOPT,C'N'                                                     
*                                                                               
         MVC   OUT2OPT,MBPROF+2    RESET OUT2OPT TO PROFILE                     
         CLI   OUT2OPT,0                                                        
         BNE   *+8                                                              
         MVI   OUT2OPT,C'N'                                                     
*                                                                               
         CLC   SVPUB(3),=C'ALL'                                                 
         BE    EDTO2                                                            
         CLC   SVPUB+4(2),=X'FFFF' ALL ZONES/EDTS?                              
         BE    EDTO2                                                            
         OC    SVPUB,SVPUB                                                      
         BZ    EDTO2                                                            
         MVI   PUBNOPT,C'N'                                                     
*                                                                               
EDTO2    DS    0H                                                               
         MVI   DOLSW,0                                                          
         MVI   DATESW,0                                                         
         MVI   DATASW,0                                                         
         MVI   GNOPT,0                                                          
         MVI   COST2SW,0                                                        
         MVI   SVIOSW,0            LAST I/O DISPLAY SWITCH                      
*                                                                               
         MVI   SVDLSW,0            DISP DELETED RECORDS SWITCH                  
*                                                                               
         XC    SVNV,SVNV           CLEAR NV LETTER FILTER                       
         XC    SVCON,SVCON         CLEAR CONTRACT NUMBER                        
         XC    SVCONL,SVCONL       CLEAR CONTRACT LEVEL IND                     
*                                                                               
         XC    SVREF,SVREF         CLEAR REFENENCE NUMBER FILTER                
*                                                                               
         XC    SVREP,SVREP         CLEAR SPECIAL REP                            
*                                  (FIRST 4 BYTES OF SVAPROF)                   
         XC    SVADC,SVADC         CLEAR ADCODE FILTER                          
*                                  (SVAPROF+4(6))                               
         XC    SVSTAT,SVSTAT       SAVED STATUS FILTER                          
*                                  SVAPROF+10(1)                                
         XC    SVBILL,SVBILL       CLEAR BILLED FILTER                          
         XC    SVPAID,SVPAID       CLEAR PAID FILTER                            
*                                                                               
         XC    SVTSTAT,SVTSTAT     SAVED T/S STATUS FILTER                      
         XC    SVSFH,SVSFH         SAVED SFH STATUS FILTER                      
         XC    SVOUT2,SVOUT2                                                    
         MVI   SVOUT2L,0                                                        
         XC    SVSPACE,SVSPACE                                                  
         MVI   SVSLEN,X'FF'        SET NO SPECIAL LENGTH COMPARE                
*                                                                               
         XC    SVPGRP,SVPGRP       PRDGRP FILTER                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    OPTMISS                                                          
         GOTO1 VSCANNER,DMCB,(20,MBCDATH),(6,SCNWRK)                            
         CLI   DMCB+4,0                                                         
         BNE   EDTO3                                                            
OPTERR   LA    R3,FLDINV                                                        
         B     ERROR                                                            
OPTMISS  LA    R3,MISSERR          OPTIONS MISSING                              
         B     ERROR                                                            
*                                                                               
OPTPERR  LA    R3,PRDERR           166 - PRODUCT MUST BE SPECIFIED              
         LA    R2,MBCPRDH          CURSOR TO PRODUCT                            
         B     ERROR                                                            
*                                                                               
EDTO3    DS    0H                                                               
         ZIC   R7,DMCB+4           SET FOR BCT                                  
         LA    R6,SCNWRK                                                        
*                                                                               
EDTO4    DS    0H                  CHECKING FOR KEYWORDS                        
         BRAS  RE,CKPGRP                                                        
         CLI   BYTE3,0             KEYWORD IS NOT PGRP?                         
         BE    EDTO4B                                                           
         CLI   BYTE3,99            NO ERROR FOUND?                              
         BE    EDTO14                                                           
         B     ERROR               ERROR MSG AND CURSOR ARE SET                 
*                                                                               
EDTO4B   CLC   12(3,R6),=C'JOB'                                                 
         BE    EDTO4C                                                           
         CLC   12(2,R6),=C'AD'                                                  
         BNE   EDTO5                                                            
EDTO4C   CLI   1(R6),0             CHK FOR SECOND FIELD                         
         BE    EDTO10                                                           
*                                                                               
* IF THERE IS A 2ND FIELD THIS IS A FILTER                                      
*                                                                               
         OC    SVADC,SVADC         SEE IF AD CODE FILTER ALREADY INPUT          
         BNZ   OPTERR                                                           
         CLI   1(R6),6                                                          
         BH    OPTERR                                                           
         CLC   SVPRD,=C'ALL'                                                    
         BE    OPTPERR             MUST BE ONE PRD FOR ADCODE REQUEST           
         MVC   SVADC,22(R6)                                                     
         OC    SVADC,=6C' '                                                     
         CLC   SVADC,=CL6'NONE  '                                               
         BE    EDTO14                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),SVCLT                                                   
         MVC   KEY+7(3),SVPRD                                                   
         MVC   KEY+10(6),SVADC                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BE    EDTO14                                                           
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
EDTO5    CLC   12(4,R6),=C'SREP'                                                
         BNE   EDTO6                                                            
         CLI   1(R6),0                                                          
         BE    OPTERR                                                           
         OC    SVREP,SVREP         SEE IF REP ALREADY INPUT                     
         BNZ   OPTERR                                                           
         TM    3(R6),X'80'         MUST BE NUMERIC                              
         BNO   OPTERR                                                           
         L     R0,8(R6)                                                         
         CHI   R0,9999             MAX REP                                      
         BH    OPTERR                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVREP,DUB                                                        
         B     EDTO14              NEXT FIELD                                   
*                                                                               
EDTO6    CLC   12(4,R6),=C'STAT'   STATUS FILTER/DATA?                          
         BNE   EDTO6B                                                           
         CLI   1(R6),0                                                          
         BE    EDTO10                                                           
*                                                                               
* IF THERE IS A 2ND FIELD THIS IS A FILTER                                      
*                                                                               
         OC    SVSTAT,SVSTAT       STATUS FILTER ALREADY INPUT?                 
         BNZ   OPTERR                                                           
         CLI   1(R6),4                                                          
         BH    OPTERR                                                           
         CLI   22(R6),C'L'         LIVE                                         
         BE    EDTO6A                                                           
         CLI   22(R6),C'T'         TEST                                         
         BNE   OPTERR                                                           
*                                                                               
EDTO6A   MVC   SVSTAT,22(R6)                                                    
         B     EDTO14                                                           
*                                  SFH (SPECIAL FINANCIAL HANDLING)             
EDTO6B   CLC   12(3,R6),=C'SFH'    SFH STATUS FILTER/DATA                       
         BNE   EDTO6C                                                           
         CLI   1(R6),0                                                          
         BE    EDTO10                                                           
*                                                                               
* IF THERE IS A 2ND FIELD THIS IS A FILTER                                      
*                                                                               
         OC    SVSFH,SVSFH         SFH STATUS FILTER ALREADY INPUT?             
         BNZ   OPTERR                                                           
         CLI   1(R6),4                                                          
         BH    OPTERR                                                           
         CLI   22(R6),C'H'         HELD                                         
         BE    EDTO6BX                                                          
         CLI   22(R6),C'R'         RELEASED (REL)                               
         BNE   OPTERR                                                           
*                                                                               
EDTO6BX  MVC   SVSFH,22(R6)                                                     
         B     EDTO14                                                           
*                                                                               
* CHECK FOR VARIOUS ALLOWABLE SPELLINGS OF TEARSHEET                            
*                                                                               
EDTO6C   CLC   12(4,R6),=C'TSTA'   STATUS FILTER/DATA                           
         BE    EDTO6C3                                                          
         CLC   12(4,R6),=C'TEAR'   STATUS FILTER/DATA                           
         BE    EDTO6C3                                                          
         CLC   12(4,R6),=C'T/S '   STATUS FILTER/DATA                           
         BE    EDTO6C3                                                          
         CLC   12(4,R6),=C'TS  '   STATUS FILTER/DATA                           
         BE    EDTO6C3                                                          
         CLC   12(4,R6),=C'T/SH'   STATUS FILTER/DATA                           
         BE    EDTO6C3                                                          
         CLC   12(4,R6),=C'T/ST'   STATUS FILTER/DATA                           
         BE    EDTO6C3                                                          
         CLC   12(4,R6),=C'TSHE'   STATUS FILTER/DATA                           
         BNE   EDTO6D                                                           
EDTO6C3  CLI   1(R6),0                                                          
         BE    EDTO10                                                           
*                                                                               
* IF THERE IS A 2ND FIELD THIS IS A FILTER                                      
*                                                                               
         OC    SVTSTAT,SVTSTAT     SEE IF T/S FILTER ALREADY INPUT              
         BNZ   OPTERR                                                           
         CLI   1(R6),1                                                          
         BH    OPTERR                                                           
         CLI   22(R6),C'N'         NOT APPROVED                                 
         BE    EDTO6C6                                                          
         CLI   22(R6),C'A'         APPROVED                                     
         BE    EDTO6C6                                                          
         CLI   22(R6),C'X'         BLANK OR NOT ENTERED                         
         BNE   OPTERR                                                           
         MVI   22(R6),C' '         CHANGE TO BLANK IN FILTER                    
*                                                                               
EDTO6C6  MVC   SVTSTAT,22(R6)      TEARSHEET STATUS FILTER                      
         B     EDTO14                                                           
*                                                                               
EDTO6D   CLC   12(2,R6),=C'NV'     NV FILTER/DATA                               
         BNE   EDTO6F                                                           
         CLI   1(R6),0                                                          
         BE    EDTO10                                                           
*                                                                               
* IF THERE IS A 2ND FIELD THIS IS A FILTER                                      
*                                                                               
         OC    SVNV,SVNV           SEE IF NV FILTER ALREADY INPUT               
         BNZ   OPTERR                                                           
         CLI   1(R6),3                                                          
         BH    OPTERR                                                           
         CLI   22(R6),C'N'         NO                                           
         BE    EDTO6E                                                           
         CLI   22(R6),C'Y'         YES                                          
         BNE   OPTERR                                                           
*                                                                               
EDTO6E   MVC   SVNV,22(R6)                                                      
         B     EDTO14                                                           
*                                                                               
EDTO6F   DS    0H                                                               
         CLC   12(4,R6),=C'CONT'                                                
         BE    EDTO6F3                                                          
         CLC   12(4,R6),=C'CON '                                                
         BNE   EDTO6L                                                           
EDTO6F3  CLI   1(R6),0                                                          
         BE    OPTERR                                                           
         TM    3(R6),X'80'         MUST BE NUMERIC                              
         BNO   OPTERR                                                           
         OC    8(4,R6),8(R6)       CAN'T BE ZERO                                
         BZ    OPTERR                                                           
         CLC   8(4,R6),=F'999'     MAX FOR CONTRACT NUMBER                      
         BH    OPTERR                                                           
         OC    SVCON,SVCON                                                      
         BNZ   OPTERR              CONTRACT ALREADY SPECIFIED                   
         MVC   SVCON,10(R6)        SAVE BINARY CONTRACT                         
*                                                                               
         CLI   SVPQMK,C'?'                                                      
         BE    OPTERR                                                           
         CLC   SVPUB,=C'ALL'                                                    
         BE    OPTERR                                                           
         CLC   SVPUB+4(2),=X'FFFF' NO ALL ZONES,EDTS                            
         BE    OPTERR                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'10'                                                      
         MVC   KEY+4(3),SVCLT                                                   
         CLI   SVCLPROF+5,C'2'     SEE IF SLAVE CLIENT                          
         BNE   *+10                                                             
         MVC   KEY+4(3),SVCLPROF+6 USE MASTER                                   
         MVC   KEY+7(6),SVPUB                                                   
         MVC   KEY+13(2),SVCON                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BE    EDTO6H                                                           
         LA    R3,NOFNDERR                                                      
         B     ERROR                                                            
*                                                                               
EDTO6H   MVC   AREC,ACONIO                                                      
         GOTO1 GETREC                                                           
         LA    RF,REC                                                           
         ST    RF,AREC             RESET TO REC                                 
         L     R5,ACONIO                                                        
         USING PCONRECD,R5                                                      
         CLI   PCONPRD,C'A'        SEE IF PRODUCT CONTRACT                      
         BL    EDTO6I                                                           
         CLC   SVPRD,PCONPRD       MUST MATCH                                   
         BNE   OPTERR                                                           
EDTO6I   CLI   MBCSTDEH+5,0        SEE IF START/END SPECIFIED                   
         BE    EDTO6J                                                           
         B     OPTERR                                                           
*                                                                               
EDTO6J   MVC   SVSTRTB,PCONSTRT                                                 
         MVC   SVENDB,PCONEDT                                                   
         GOTO1 VDATCON,DMCB,(3,SVSTRTB),(0,SVSTRT)                              
         GOTO1 VDATCON,DMCB,(3,SVENDB),(0,SVEND)                                
         LA    R2,PCONELEM                                                      
         MVI   ELCODE,X'20'                                                     
         CLI   0(R2),X'20'                                                      
         BE    EDTO6K                                                           
         BRAS  RE,NXTELEM                                                       
         BNE   EDTO14              NOT FOUND                                    
*                                                                               
EDTO6K   MVC   SVCONL,PRBLIND-PRBELEM(R2)                                       
         LA    R2,MBCDATH          RESET TO OPTIONS                             
         B     EDTO14                                                           
         DROP  R5                                                               
*                                                                               
EDTO6L   DS    0H                  SEE IF SPACE FILTER ENTERED                  
         CLC   12(5,R6),=C'SPACE'                                               
         BNE   EDTO6M                                                           
         CLI   1(R6),0             MUST ENTER SECOND FIELD                      
         BE    OPTERR                                                           
         OC    SVSPACE,SVSPACE     SEE IF SPACE FILTER ALREADY ENTERED          
         BNZ   OPTERR                                                           
         OC    SVOUT2,SVOUT2       SEE IF SPACE FILTER ALREADY ENTERED          
         BNZ   OPTERR                                                           
         CLI   1(R6),18                                                         
         BH    OPTERR                                                           
         CLI   22(R6),C'-'                                                      
         BE    *+8                                                              
         CLI   1(R6),17                                                         
         BH    OPTERR                                                           
         MVC   SVSPACE,22(R6)                                                   
         OC    SVSPACE,=CL18' '                                                 
         CLI   SVSPACE,C'-'        SEE IF NEAGTIVE FILTER                       
         BE    EDTO6L2                                                          
         MVC   SVSPACE,=CL18' '                                                 
*                                                                               
* IF NOT NEGATIVE - USE SVSPACE+1                                               
*                                                                               
         MVC   SVSPACE+1(17),22(R6)                                             
         B     EDTO6L2                                                          
*                                                                               
EDTO6L2  LA    R1,SVSPACE+17       SCAN BACKWARD FOR FIRST NON-SPACE            
         LA    RF,17                                                            
EDTO6L3  CLI   0(R1),C' '                                                       
         BH    EDTO6L4                                                          
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         B     EDTO6L3                                                          
*                                                                               
EDTO6L4  CLI   0(R1),C'*'                                                       
         BNE   EDTO14              NO SPECIAL LENGTH COMPARE                    
         CLI   SVSPACE,C'-'        SEE IF NEGATIVE FILTER                       
         BNE   EDTO6L6                                                          
         BCTR  RF,0                                                             
*                                                                               
EDTO6L6  DS    0H                                                               
         BCTR  RF,0                ADJUST FOR THE '*'                           
         BCTR  RF,0                ADJUST FOR THE EXECUTE                       
         LTR   RF,RF                                                            
         BM    OPTERR              CAN'T BE NEGATIVE                            
         STC   RF,SVSLEN           SAVE SPECIAL LENGTH COMPARE                  
         B     EDTO14                                                           
*                                                                               
EDTO6M   DS    0H                                                               
         CLC   12(4,R6),=C'NAME'   OVERRIDING  MBPROF PUB NAME OPT?             
         BE    EDTO6P                                                           
         CLC   12(4,R6),=C'PNAM'                                                
         BE    EDTO6P                                                           
         CLC   12(4,R6),=C'PUBN'                                                
         BNE   EDTO6Q                                                           
*                                                                               
EDTO6P   MVI   PUBNOPT,C'Y'        SET TO 'Y'                                   
         CLI   1(R6),0             IF NO SECOND FIELD INPUT                     
         BE    EDTO14                                                           
         CLI   22(R6),C'Y'                                                      
         BE    EDTO14                                                           
         CLI   22(R6),C'N'                                                      
         BNE   OPTERR                                                           
         MVI   PUBNOPT,C'N'                                                     
         B     EDTO14                                                           
*                                                                               
EDTO6Q   DS    0H                                                               
         CLC   12(6,R6),=C'BILLED' CK FOR BILLED FILTERS                        
         BNE   EDTO6Q2                                                          
         CLI   0(R6),6                                                          
         BNE   OPTERR                                                           
         MVI   SVBILL,C'B'                                                      
         B     EDTO6QX                                                          
*                                                                               
EDTO6Q2  DS    0H                  NO-OP TOTALLY BILLED FILTER FOR NOW          
******** CLC   12(7,R6),=C'TBILLED'                                             
******** BNE   EDTO6Q4                                                          
******** CLI   0(R6),7                                                          
******** BNE   OPTERR                                                           
******** MVI   SVBILL,C'T'                                                      
******** B     EDTO6QX                                                          
*                                                                               
EDTO6Q4  CLC   12(4,R6),=C'PAID'   CK FOR PAID FILTERS                          
         BNE   EDTO6Q6                                                          
         CLI   0(R6),4                                                          
         BNE   OPTERR                                                           
         MVI   SVPAID,C'P'                                                      
         B     EDTO6QX                                                          
*                                                                               
EDTO6Q6  DS    0H                  NO-OP TOTALLY PAID FILTER NOR NOW            
         B     EDTO6R                                                           
*                                                                               
******** CLC   12(5,R6),=C'TPAID'  CK FOR TOTALLY PAID FILTER                   
******** BNE   OPTERR                                                           
******** CLI   0(R6),5                                                          
******** BNE   OPTERR                                                           
******** MVI   SVPAID,C'T'                                                      
*                                                                               
EDTO6QX  CLI   1(R6),0             CK FOR SECOND FIELD                          
         BNE   OPTERR              NOT ALLOWED FOR BILLED/PAID FILTERS          
         B     EDTO14                                                           
*                                                                               
* REF AND OUTSPACE2 CAN BE FILTERS OR DATA,                                     
* IF NO SECOND FIELD - EDIT AS DATA                                             
* FUTURE FILTERS SHOULD BE ADDED SOME WHERE BEFORE THIS EDIT                    
*                                                                               
EDTO6R   DS    0H                                                               
         CLC   12(3,R6),=C'REF'                                                 
         BNE   EDTO6S                                                           
         OC    SVREF,SVREF         SEE IF REF FILTER ALREADY ENTERED            
         BNZ   OPTERR                                                           
         CLI   1(R6),0                                                          
         BE    EDTO6X                                                           
         CLI   1(R6),10                                                         
         BH    OPTERR                                                           
         CLC   AGYALPHA,=C'BS'                                                  
         BNE   EDTO6R5                                                          
         CLI   1(R6),6             MAX IS 6 CHARS FOR BACKER                    
         BH    OPTERR                                                           
*                                                                               
* IF SECOND FIELD ENTERED THIS IS A FILTER                                      
*                                                                               
EDTO6R5  MVC   SVREF,22(R6)                                                     
         OC    SVREF,=10C' '                                                    
         CLC   SVREF(6),=CL6'NONE  '                                            
         BNE   EDTO14                                                           
         MVI   SVREF,X'FF'                                                      
         B     EDTO14                                                           
*                                                                               
EDTO6S   DS    0H                                                               
         CLC   12(4,R6),=C'OUT2'                                                
         BE    EDTO6S3                                                          
         CLC   12(7,R6),=C'OUTSPC2'                                             
         BE    EDTO6S3                                                          
         CLC   12(5,R6),=C'OUTS2'                                               
         BE    EDTO6S3                                                          
         CLC   12(9,R6),=C'OUTSPACE2'                                           
         BNE   EDTO6T                                                           
*                                                                               
EDTO6S3  DS    0H                                                               
         OC    SVOUT2,SVOUT2       SEE IF OUT2 FILTER ALREADY ENTERED           
         BNZ   OPTERR                                                           
         CLI   MBCMDIA,C'O'        MUST BE OUTDOOR                              
         BNE   OPTERR                                                           
*                                                                               
         MVI   OUT2OPT,C'Y'                                                     
         CLI   1(R6),0             CK FOR FILTER INPUT IN SECOND FIELD          
         BE    EDTO14                                                           
         CLI   1(R6),10                                                         
         BH    OPTERR                                                           
*                                                                               
* IF SECOND FIELD ENTERED THIS IS A FILTER                                      
*                                                                               
         CLC   22(2,R6),=C'NO'                                                  
         BNE   EDTO6S7                                                          
         CLI   1(R6),2             SEE IF LENGTH IS 2                           
         BNE   EDTO6S8                                                          
         MVI   OUT2OPT,C'N'                                                     
         B     EDTO14                                                           
*                                                                               
EDTO6S7  CLC   22(3,R6),=C'YES'                                                 
         BNE   EDTO6S8                                                          
         CLI   1(R6),3             SEE IF LENGTH IS 3                           
         BNE   EDTO6S8                                                          
         MVI   OUT2OPT,C'Y'                                                     
         B     EDTO14                                                           
*                                                                               
EDTO6S8  OC    SVSPACE,SVSPACE     SEE SPACE FILTER ALREADY ENTERED             
         BNZ   OPTERR                                                           
         MVC   SVOUT2,22(R6)                                                    
         MVC   SVOUT2L,1(R6)       SAVE LENGTH OF FILTER                        
         OC    SVOUT2,=10C' '                                                   
         B     EDTO14                                                           
*                                                                               
EDTO6T   DS    0H                  CK KEYWORD FOR LAST INS ORDER                
         CLC   12(6,R6),=C'LASTIO'                                              
         BE    EDTO6T5                                                          
         CLC   12(7,R6),=C'LASTI/O'                                             
         BNE   EDTO6U                                                           
*                                                                               
EDTO6T5  DS    0H                                                               
         CLI   SVIOSW,0            SEE IF SVIOSW ALREADY SET                    
         BNE   OPTERR                                                           
         MVI   SVIOSW,C'Y'         SET SVIOSW TO SOMETHING                      
*                                                                               
         CLI   1(R6),0                                                          
         BNE   OPTERR              NO NEED FOR 2ND SCANNER FIELD                
         CLI   0(R6),7                                                          
         BH    OPTERR              1ST SCANNER FIELD IS 7 MAX                   
*                                                                               
         B     EDTO9F              GO DISPLAY LASTIO HEADER                     
*                                                                               
EDTO6U   DS    0H                                                               
         CLC   12(7,R6),=C'DELETED'                                             
         BE    EDTO6U5                                                          
         CLC   12(6,R6),=C'CHADEL'                                              
         BE    EDTO6U10                                                         
         BNE   EDTO6V                                                           
*                                                                               
EDTO6U5  DS    0H                                                               
         CLI   SVDLSW,0            SEE IF SVDLSW ALREADY SET                    
         BNE   OPTERR                                                           
         MVI   SVDLSW,C'D'         SET SVDLSW TO 'D'                            
*                                                                               
         CLI   0(R6),7                                                          
         BNE   OPTERR              1ST SCANNER FIELD IS 7                       
         B     EDTO14                                                           
*                                                                               
EDTO6U10 DS    0H                                                               
         LR    R1,RA                                                            
         USING TWAD,R1                                                          
         CLI   TWAOFFC,C'*'        DDS ONLY                                     
         BNE   OPTERR                                                           
         DROP  R1                                                               
*                                                                               
         CLI   SVDLSW,0            SEE IF SVDLSW ALREADY SET                    
         BNE   OPTERR                                                           
         MVI   SVDLSW,C'C'         SET SVDLSW TO 'D'                            
*                                                                               
         CLI   0(R6),6                                                          
         BNE   OPTERR              1ST SCANNER FIELD IS 6                       
*                                                                               
         B     EDTO14                                                           
*                                                                               
EDTO6V   DS    0H                                                               
         CLC   12(4,R6),=C'OPEN'                                                
         BE    EDTO6V3                                                          
         CLC   12(4,R6),=C'COS2'                                                
         BE    EDTO6V3                                                          
         CLC   12(5,R6),=C'COST2'                                               
         BNE   EDTO6X                                                           
*                                                                               
EDTO6V3  DS    0H                                                               
         CLI   COST2SW,0           SEE IF COST2SW ALREADY SET                   
         BNE   OPTERR                                                           
         MVI   COST2SW,C'O'        SET COST2SW                                  
*                                                                               
         CLI   1(R6),0             CK FOR FILTER INPUT IN SECOND FIELD          
         BNE   OPTERR                                                           
         CLI   0(R6),5                                                          
         BH    OPTERR                                                           
         B     EDTO14                                                           
*                                                                               
EDTO6X   CLI   1(R6),0             CK FOR NO SECOND FIELD                       
         BE    EDTO10              OK IF JUST INPUTTING DATA TYPE               
         LA    R3,TAB1                                                          
         LA    R1,DOLSW                                                         
         CLI   12(R6),C'$'                                                      
         BE    EDTO7                                                            
         LA    R3,TAB2                                                          
         LA    R1,DATESW                                                        
         CLC   12(4,R6),=C'DATE'                                                
         BE    EDTO7                                                            
         CLC   12(3,R6),=C'DTE'                                                 
         BE    EDTO7                                                            
         LA    R3,TAB3                                                          
         LA    R1,DATASW                                                        
         MVI   FULL,1                                                           
         CLC   12(4,R6),=C'DATA'                                                
         BE    EDTO7                                                            
         B     OPTERR                                                           
*                                                                               
EDTO7    ZIC   R5,1(R6)                                                         
         BCTR  R5,0                                                             
EDTO8    CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    OPTERR                                                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),22(R6)                                                   
         BE    EDTO9                                                            
         LA    R3,10(R3)                                                        
         B     EDTO8                                                            
*                                                                               
EDTO9    CLI   0(R1),0             SEE IF SWITCH ALREADY SET                    
         BE    EDTO9C                                                           
         B     OPTERR              YES - ERROR                                  
*                                                                               
EDTO9C   MVC   0(1,R1),9(R3)       SAVE CODE                                    
         CLC   22(4,R6),=C'ACNET'                                               
         BNE   *+8                                                              
         MVI   GNOPT,C'Y'          TO GROSS-UP BUYS WITH NET RATES              
*                                                                               
         CLI   MBPROF+0,C'X'       SUPPRESSING COST + NO RATE ACCESS            
         BNE   EDTO9F                                                           
         CLI   DATASW,C'$'         RATES                                        
         BE    AACTERR                                                          
         CLI   DATASW,C'N'         LINE RATE                                    
         BE    AACTERR                                                          
         CLI   DATASW,C'M'         INCH RATE                                    
         BE    AACTERR                                                          
*                                                                               
* RESTORE LAST I/O HEADER                                                       
*                                                                               
EDTO9F   CLI   SVIOSW,0                                                         
         BE    EDTO9G              NOT DOING LASTIO, CONTINUE OTHER CHK         
         CLI   DOLSW,0                                                          
         BNE   OPTERR              CAN'T HAVE COST AND LASTIO TOGETHER          
*                                                                               
         MVC   MBCHD1+49(18),=CL18' '                                           
         MVC   MBCHD2+49(18),=CL18' '                                           
         MVC   MBCHD1+56(11),=C'   LAST I/O'                                    
         MVC   MBCHD2+56(11),=C'   --------'                                    
         FOUT  MBCHD1H                                                          
         FOUT  MBCHD2H             FRESH AND CLEAN "LASTIO" HEADER              
         B     EDTO14                                                           
*                                                                               
* RESTORE COST TO HEADING IF $= ENTERED                                         
*                                                                               
EDTO9G   CLI   DOLSW,0                                                          
         BE    EDTO14                                                           
         CLI   SVIOSW,0                                                         
         BNE   OPTERR              CAN'T HAVE COST AND LASTIO TOGETHER          
*                                                                               
         CLI   MBPROF+0,C'Y'       SUPPRESSING COST?                            
         BE    EDTO09K             DON'T RESTORE HEADING                        
         CLI   MBPROF+0,C'X'       SUPPRESSING COST?                            
         BE    EDTO09K             DON'T RESTORE HEADING                        
*                                                                               
         CLI   DATASW,C'W'         LEGAL WARNIGNS?                              
         BE    EDTO09K             DON'T RESTORE HEADING                        
*                                                                               
         MVC   MBCHD1+49(18),=CL18' '                                           
         MVC   MBCHD2+49(18),=CL18' '                                           
         MVC   MBCHD1+63(04),=C'COST'                                           
         MVC   MBCHD2+63(04),=C'----'                                           
EDTO09K  FOUT  MBCHD1H                                                          
         FOUT  MBCHD2H             FRESH AND CLEAN "COST" HEADER                
         B     EDTO14                                                           
*                                                                               
EDTO10   LA    R3,TAB3             EDIT AS DATA TYPE                            
         MVC   22(9,R6),12(R6)                                                  
         MVC   1(1,R6),0(R6)       MOVE LENGHT                                  
         LA    R1,DATASW                                                        
         B     EDTO7                                                            
*                                                                               
EDTO14   LA    R6,42(R6)           NEXT SCANNER LINE                            
         BCT   R7,EDTO4                                                         
*                                                                               
EDTO14C  CLI   DATASW,0            SEE IF DATA SPECIFIED                        
         BNE   EDTO14K             YES                                          
         OC    SVNV,SVNV           SEE IF NV FILTER SET                         
         BZ    *+8                                                              
         MVI   DATASW,C'L'                                                      
         OC    SVADC,SVADC         SEE IF ADCODE FILTER SET                     
         BZ    EDTO14D                                                          
         CLI   DATASW,0            NV AND ADCODE FILTERS SET - ERROR            
         BNE   OPTMISS                                                          
         MVI   DATASW,C'A'         SET FOR ADCODE DISPLAY                       
*                                                                               
EDTO14D  OC    SVSFH,SVSFH         SEE IF SFH STATUS FILTER SET                 
         BZ    EDTO14D3                                                         
         CLI   DATASW,0            IF MORE THAN ONE FILTER SET                  
         BNE   OPTMISS             SEND ERROR                                   
         MVI   DATASW,C'X'         SET FOR SFH DISPLAY                          
*                                                                               
EDTO14D3 OC    SVSTAT,SVSTAT       SEE IF STATUS FILTER SET                     
         BZ    EDTO14D5                                                         
         CLI   DATASW,0            IF MORE THAN ONE FILTER SET                  
         BNE   OPTMISS             SEND ERROR                                   
         MVI   DATASW,C'S'                                                      
*                                                                               
EDTO14D5 OC    SVREF,SVREF         SEE IF REF FILTER SET                        
         BZ    EDTO14D7                                                         
         CLI   DATASW,0            IF MORE THAN ONE FILTER SET                  
         BNE   OPTMISS             SEND ERROR                                   
         MVI   DATASW,C'R'         SET FOR REF DISPLAY                          
*                                                                               
EDTO14D7 OC    SVTSTAT,SVTSTAT     SEE IF T/S FILTER SET                        
         BZ    EDTO14E                                                          
         CLI   DATASW,0            IF MORE THAN ONE FILTER SET                  
         BNE   OPTMISS             SEND ERROR                                   
         MVI   DATASW,C'E'         SET FOR T/S STATUS DISPLAY                   
         B     EDTO14K                                                          
*                                                                               
EDTO14E  CLI   DATASW,0            SEE IF DATA STILL NOT SPECIFIED              
         BNE   EDTO14K             SPECIFIED - DONE                             
*                                                                               
         CLI   DATESW,0                                                         
         BE    EDTO14F                                                          
         CLI   DATESW,C'I'         INSERTION - DEFAULT                          
         BE    EDTO14F             NEEDS DATA TYPE                              
*                                                                               
* IF DATE FILTER SPECIFIED AND NO DATA                                          
* ASSUME DATA TO BE DATE SPECIFIED                                              
*                                                                               
         MVI   DATASW,C'1'         ON-SALE                                      
         CLI   DATESW,C'O'                                                      
         BE    EDTO14K                                                          
         MVI   DATASW,C'2'         BILLABLE                                     
         CLI   DATESW,C'B'                                                      
         BE    EDTO14K                                                          
         MVI   DATASW,C'3'         PAYABLE                                      
         CLI   DATESW,C'P'                                                      
         BE    EDTO14K                                                          
         MVI   DATASW,C'4'         CLOSING                                      
         CLI   DATESW,C'C'                                                      
         BE    EDTO14K                                                          
         MVI   DATASW,C'5'         MATERIALS                                    
         CLI   DATESW,C'M'                                                      
         BE    EDTO14K                                                          
         MVI   DATASW,C'H'         SHIP DATE                                    
         CLI   DATESW,C'S'                                                      
         BE    EDTO14K                                                          
*                                                                               
         MVI   DATASW,0                                                         
         B     OPTMISS                                                          
*                                                                               
EDTO14F  CLI   DATASW,0            NO FILTERS SET - SEND ERROR                  
         BE    OPTMISS                                                          
*                                                                               
EDTO14K  DS    0H                                                               
*                                                                               
EDTO14L  DS    0H                                                               
*                                                                               
* SJR RESTRICTION FOR PAGEVIEWS AND CLICKTHRUS WAS HERE                         
*                                                                               
EDTO14M  DS    0H                                                               
         CLI   MBPROF+0,C'Y'       SEE IF SUPPRESSING COST                      
         BE    EDTO14M5            THEN DON'T SET DEFAULT                       
         CLI   MBPROF+0,C'X'       SEE IF SUPPRESSING COST                      
         BE    EDTO14M5            THEN DON'T SET DEFAULT                       
*                                                                               
         CLI   DOLSW,0                                                          
         BNE   *+8                                                              
         MVI   DOLSW,C'0'          NOT ENTERED                                  
EDTO14M5 CLI   DATESW,0                                                         
         BNE   *+8                                                              
         MVI   DATESW,C'I'         DEFAULT TO INSERTION DATE                    
*                                                                               
         OC    SVCON,SVCON         SEE IF I HAVE CONTRACT                       
         BNZ   EDTO14O             YES                                          
         CLI   MBCSTDEH+5,0        SEE IF I HAVE DATES                          
         BNE   EDTO14O                                                          
         LA    R2,MBCSTDEH                                                      
         LA    R3,MISSERR          MISSING                                      
         BAS   RE,UNVAL                                                         
         B     ERROR                                                            
*                                                                               
EDTO14O  OC    SVCON,SVCON         SEE IF DOING CONTRACT DATES                  
         BZ    EDTO14P                                                          
         CLI   DATESW,C'I'         MUST USE INSERTION DATES                     
         BNE   OPTERR                                                           
*                                                                               
EDTO14P  CLI   SVMED,C'N'                                                       
         BNE   EDTO14Q                                                          
*                                                                               
         B     EDTO16              ALLOW CLOSING & ON-SALE DATE FOR "N"         
*                                                                               
         CLI   DATASW,C'4'                                                      
         BE    OPTERR              NO CLOSING FOR NEWSPAPERS                    
         CLI   DATESW,C'C'                                                      
         BE    OPTERR              NO CLOSING FOR NEWSPAPERS                    
         CLI   DATASW,C'1'                                                      
         BE    OPTERR              NO ON-SALE FOR NEWSPAPERS                    
         CLI   DATESW,C'O'                                                      
         BE    OPTERR              NO ON-SALE FOR NEWSPAPERS                    
         B     EDTO15                                                           
*                                                                               
EDTO14Q  DS    0H                                                               
         CLI   DATASW,C'M'         NO INCH RATE FOR NON-NEWS                    
         BE    OPTERR                                                           
         CLI   DATASW,C'N'         NO LINE RATE FOR NON-NEWS                    
         BE    OPTERR                                                           
*                                                                               
EDTO15   CLI   SVMED,C'O'          SEE IF OUTDOOR                               
         BNE   EDTO16                                                           
         CLI   DATASW,C'1'                                                      
         BE    OPTERR              NO ON-SALE FOR OUTDOOR                       
         CLI   DATESW,C'O'                                                      
         BE    OPTERR              NO ON-SALE FOR OUTDOOR                       
*                                                                               
EDTO16   DS    0H                                                               
         CLI   DATASW,C'D'         SEE IF DLC                                   
         BNE   EDTO16D                                                          
         CLI   SVMED,C'O'          MUST BE OUTDOOR                              
         BNE   OPTERR                                                           
*                                                                               
EDTO16D  DS    0H                                                               
         CLI   DATASW,C'T'         SEE IF RPT                                   
         BNE   EDTO17                                                           
         CLI   SVMED,C'O'          MUST BE OUTDOOR                              
         BNE   OPTERR                                                           
*                                                                               
EDTO17   DS    0H                                                               
         CLI   DATASW,C'F'         SEE IF FSI                                   
         BNE   EDTO18                                                           
         CLI   SVMED,C'O'          CAN'T BE OUTDOOR                             
         BE    OPTERR                                                           
*                                                                               
EDTO18   DS    0H                                                               
         CLI   DATASW,C'B'         SEE IF COST 2 FACTOR                         
         BNE   EDTO19              NO                                           
         TM    SVCLPROF+30,X'08'   COST 2 FACTOR CLIENT ?                       
         BNO   OPTERR              NO - SEND ERROR                              
*                                                                               
EDTO19   DS    0H                                                               
*                                                                               
EDTO20   DS    0H                  NOW CHECK FOR FUNCTION LIMIT ACCESS          
EDTO40   OI    4(R2),X'20'         VALIDATE OPTIONS                             
         XC    MBCHD1+49(18),MBCHD1+49                                          
         XC    MBCHD2+49(18),MBCHD2+49                                          
*                                                                               
         MVC   MBCHD1+59(8),=C'    COST'                                        
         MVC   MBCHD2+59(8),=C'    ----'                                        
         CLI   SVIOSW,0                                                         
         BE    EDTO41                                                           
         MVC   MBCHD1+59(8),=C'LAST I/O'                                        
         MVC   MBCHD2+59(8),=C'--------'                                        
         B     EDTO41X                                                          
*                                                                               
EDTO41   CLI   MBPROF+0,C'X'       SEE IF SUPPRESSING COST                      
         BE    EDTO41B                                                          
         CLI   MBPROF+0,C'Y'       SEE IF SUPPRESSING COST                      
         BNE   EDTO41X                                                          
EDTO41B  MVC   MBCHD1+59(8),=CL18' '                                            
         MVC   MBCHD2+59(8),=CL18' '                                            
EDTO41X  FOUT  MBCHD1H                                                          
         FOUT  MBCHD2H                                                          
*                                                                               
EDTO45   CLC   LDOLSW,DOLSW        SEE IF SAME DOLSW                            
         BNE   EDTACT                                                           
         MVI   PVSW,0              SO I'LL CONTINUE DISPLAY                     
         EJECT                                                                  
*                                                                               
EDTACT   DS    0H                  EDIT ACTION                                  
         CLI   PVSW,0              HAS ANY OTHER FIELD BEEN CHANGED             
         BNE   EDTA1               NO                                           
         NI    MBCACTH+4,X'DF'     YES - UNVALIDATE ACTION                      
*                                                                               
EDTA1    LA    R2,MBCACTH                                                       
         CLI   5(R2),0                                                          
         BE    ACTMISS                                                          
         CLI   8(R2),C'?'                                                       
         BE    DISACTS                                                          
         TM    4(R2),X'20'         SEE IF PREVIOUSLY VALIDATED                  
         BNO   EDTA2                                                            
         CLI   ACTION,C'M'                                                      
         BNE   EDTX                                                             
         BAS   RE,MULTICHA                                                      
         CLI   FULL,0                                                           
         BE    EDTX                                                             
         B     ACTINV              SOME DATA FLD OTHER THAN 1ST CHANGED         
*                                                                               
EDTA2    LA    R3,ACTTAB                                                        
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
EDTA3    CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    ACTINV              INVALID ACTION                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),8(R2)                                                    
         BE    EDTA5                                                            
         LA    R3,10(R3)                                                        
         B     EDTA3                                                            
*                                                                               
EDTA5    MVC   ACTION,9(R3)        SAVE CODE                                    
*                                                                               
         CLI   DATASW,C'U'         SEE IF DISPLAYING UPID                       
         BNE   *+12                                                             
         CLI   ACTION,C'D'                                                      
         BNE   ACTINV              UPID ONLY ALLOW DISPLAY ACTION               
*                                                                               
         CLI   DATASW,C'#'         SEE IF DISPLAYING SERIAL NUMBER              
         BNE   *+12                                                             
         CLI   ACTION,C'D'                                                      
         BNE   ACTINV              ALLOW DISPLAY ACTION                         
*                                                                               
         CLI   ACTION,C'M'         MBC                                          
         BNE   EDTA5C                                                           
         CLI   DATASW,C'$'         DISALLOW FOR RATES                           
         BE    ACTINV                                                           
*                                                                               
EDTA5C   DS    0H                                                               
         CLI   ACTION,C'D'                                                      
         BE    EDTA7X                                                           
*                                                                               
         CLI   DATASW,C'E'         SEE IF T/S STATUS                            
         BNE   EDTA5X                                                           
         CLC   AGYALPHA,=C'WI'     AND WESTERN                                  
         BE    AACTERR                                                          
         CLC   AGYALPHA,=C'WJ'     AND WESTERN - TEST                           
         BE    AACTERR                                                          
         CLC   AGYALPHA,=C'WT'     AND WESTERN - TEST                           
         BE    AACTERR                                                          
         B     EDTA5X                                                           
*                                                                               
* ACTION NOT DISPLAY - CK FOR DATA TYPE LIMIT ACCESS                            
*                                                                               
EDTA5X   LA    R3,ACCTAB                                                        
EDTA6    CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID DATASW                               
         CLC   0(1,R3),DATASW                                                   
         BE    EDTA7                                                            
         LA    R3,3(R3)                                                         
         B     EDTA6                                                            
*                                                                               
EDTA7    DS    0H                  CK FOR FUNCTION LIMIT ACCESS                 
         MVC   HALF(2),1(R3)       MOVE LIMIT ACCESS BYTES                      
         NC    HALF(2),T418FFD+12                                               
         BZ    EDTA7X                                                           
*                                                                               
AACTERR  LA    R2,MBCACTH                                                       
         LA    R3,FACCERR          NOT AUTHORIZED FOR THIS FUNCTION             
         NI    MBCACTH+4,X'DF'     YES - UNVALIDATE ACTION                      
         MVI   LACTION,0           ALSO CLEAR LAST ACTION                       
         B     ERROR                                                            
*                                                                               
EDTA7X   DS    0H                                                               
         CLI   ACTION,C'D'         DISPLAY                                      
         BNE   EDTA9B                                                           
         CLI   PVSW,1              AND NO OTHER FIELD CHANGED                   
         BE    EDTAX2              LEAVE PVSW = 1 TO CONTINUE DISPLAY           
*                                                                               
EDTA9B   CLI   ACTION,C'M'         MULTI-CHA                                    
         BE    EDTA9C                                                           
         CLI   ACTION,C'C'         IF ACTION IS CHANGE PREVIOUS ACTION          
         BNE   EDTAX               MUST BE CHANGE OR                            
EDTA9C   CLI   LACTION,C'D'                                                     
         BNE   ACTERR                                                           
         CLI   ACTION,C'M'                                                      
         BNE   EDTAX2                                                           
         BAS   RE,MULTICHA                                                      
         CLI   FULL,0                                                           
         BE    EDTAX2                                                           
         B     ACTINV              ERROR WAS DETECTED IN MULTICHA               
*                                                                               
EDTAX    MVI   PVSW,0                                                           
         B     EDTAX2                                                           
EDTAX1   MVI   PVSW,1                                                           
EDTAX2   OI    4(R2),X'20'                                                      
         MVC   LACTION,ACTION                                                   
         B     EDTX                                                             
*                                                                               
ACTERR   LA    R3,NODSPERR         MUST DISPLAY BEFORE CHANGE                   
         NI    MBCDATH+4,X'DF'                                                  
         B     ERROR                                                            
ACTINV   LA    R3,FLDINV                                                        
         NI    MBCDATH+4,X'DF'                                                  
         B     ERROR                                                            
ACTMISS  LA    R3,MISSERR          ACTION MISSING                               
         NI    MBCDATH+4,X'DF'                                                  
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
EDTX     CLI   ACTION,C'C'                                                      
         BE    EDTX1                                                            
         CLI   ACTION,C'M'         OR MULTI-CHANGE                              
         BE    EDTX1                                                            
         BAS   RE,CLRSCN                                                        
*                                                                               
EDTX1    DS    0H                  CALL 01 PHASE TO PROCESS DATA                
         CLI   SVSTRT,0                                                         
         BE    EDTX5                                                            
         GOTO1 VDATCON,DMCB,(0,SVSTRT),(3,SVSTRTB)                              
         GOTO1 VDATCON,DMCB,(0,SVEND),(3,SVENDB)                                
EDTX5    XC    DMCB(4),DMCB                                                     
         MVI   DMCB,1              NEW CLT/PRD/EST DISPLAY - BUYS               
*                                                                               
         CLC   WORK(4),SVREP       EE IF SPECIAL REP CHANGED                    
         BE    VALMAXZ                                                          
*                                                                               
VALMAXZ  OC    SVREP,SVREP         SEE IF SPECIAL REP SPECIFIED                 
         BZ    CALLOL              NO                                           
         CLC   MBCPUB(3),=C'ALL'                                                
         BNE   CALLOL                                                           
         LA    R2,MBCDATH          SPECIAL REP ONLY FOR ONE PUB                 
         LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
EDTX25   OC    SVREP,SVREP                                                      
         BZ    CALLOL                                                           
         LA    R2,MBCDATH          SPECIAL REP NOT ALLOWED                      
         LA    R3,FLDINV           FOR OVERLAYS 2,3,4                           
         B     ERROR                                                            
*                                                                               
CALLOL   DS    0H                                                               
         GOTO1 VCALLOV,DMCB,,(RA)                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
EXIT1    J     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPCLIVER NTR1                                                                   
*                                                                               
         XC    WORK,WORK           WORK MUST BE AT LEAST 48 BYTES               
         LA    R1,WORK             (LENGTH OF OFFICED IS 48 BYTES)              
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF                                                   
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         MVC   OFCSECD,ASECBLK     SET A(SECRET BLOCK)                          
         DROP  R1                                                               
                                                                                
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),255           PROBLEM CALLING OFFICER?                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
*                                                                               
JUMPXIT1 XIT1                                                                   
X_R2R3   XIT1  REGS=(R2,R3)        ERROR MSG AND CURSOR POSITION                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTELEM  ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R2),0                                                          
         JNE   NXTELEM                                                          
         LTR   R2,R2               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
GETCORES ST    RE,DUB                                                           
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),FULL      CORE-RESIDENT PHASE TO BE CALLED             
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RE,DUB                                                           
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UNVAL    NTR1                                                                   
         MVI   PVSW,0              CANCEL PREV VAL SW                           
         LA    R4,MBCDATH                                                       
UNV2     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R4                                                            
         BH    UNVX                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    UNV2                YES - SKIP                                   
         NI    4(R2),X'DF'         UNVALIDATE                                   
         B     UNV2                                                             
UNVX     J     JUMPXIT1                                                         
*                                                                               
CLRSCN   NTR1                      CLEAR SCREEN                                 
         XC    MBCDAN1,MBCDAN1                                                  
         XC    MBCDAN2,MBCDAN2                                                  
         FOUT  MBCDAN1                                                          
         FOUT  MBCDAN2                                                          
*                                                                               
         LA    R5,MBCOT01H                                                      
         LA    R6,MBCDT01H                                                      
         LA    R7,16                                                            
*                                                                               
CLRS1    XC    8(67,R5),8(R5)                                                   
         XC    8(11,R6),8(R6)                                                   
         OI    6(R5),X'80'                                                      
         OI    6(R6),X'80'                                                      
         LA    R5,LDISP(R5)                                                     
         LA    R6,LDISP(R6)                                                     
         BCT   R7,CLRS1                                                         
         J     JUMPXIT1                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FOR MULTICHANGES FIND FIRST UNPROTECTED FIELD AND SAVE ITS INPUT              
* THEN PUT SAVED INPUT IN ALL OTHER UNPROTECTED FLIEDS                          
* IF THEY TRY TO CHANGE 2 OR MORE FIELDS - ERROR                                
* IF NO UNPROTECTED FIELDS FOUND - ERROR                                        
* SETS FULL TO 1 ON ERROR                                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MULTICHA NTR1                                                                   
         MVI   FULL,0                                                           
         SR    R4,R4                                                            
         OC    SVDISC(4),SVDISC                                                 
         BZ    MULTICE             ERROR                                        
         LA    R5,MBCDT01H                                                      
         LA    R7,SVDISC                                                        
         LA    R6,16                                                            
*                                                                               
* SET FIRST DATA FIELD IN ALL OTHERS SO T41801 WILL ACT                         
* AS IF CHANGE WAS MADE TO EVERY FIELD                                          
*                                                                               
MULTIC3  OC    0(4,R7),0(R7)                                                    
         BZ    MULTIC10                                                         
         TM    1(R5),X'20'         SEE IF PROTECTED                             
         BO    MULTIC8             YES - SKIP                                   
         CLI   8(R5),C'*'                                                       
         BE    MULTIC8                                                          
         LTR   R4,R4               FIRST UNPROTECTED FLD?                       
         BNZ   MULTIC6                                                          
         LR    R4,R5               SAVE IT'S ADDR                               
         B     MULTIC8                                                          
*                                                                               
MULTIC6  CLI   8(R5),C'*'          MEANS DON'T CHANGE THIS BUY                  
         BE    MULTIC8             SKIP                                         
         TM    4(R5),X'80'         IS FIELD INPUT THIS TIME                     
         BO    MULTICE             YES, ERROR-ONLY FIRST FLD CAN BE             
         NI    4(R5),X'DF'         SET TO ALTERED                               
         MVC   5(1,R5),5(R4)       LENGTH OF FIRST UNPROTECTED FLD              
         MVC   8(L'MBCDT01,R5),8(R4)                                            
         FOUT  (R5)                                                             
*                                                                               
MULTIC8  LA    R7,4(R7)                                                         
         LA    R5,LDISP(R5)                                                     
         CLI   PUBNOPT,C'Y'        SEE IF DISPLAYING PUB NAME                   
         BNE   MULTIC9                                                          
         AHI   R6,-1                                                            
         LTR   R6,R6                                                            
         BNP   MULTIC10                                                         
         LA    R5,LDISP(R5)        BUMP PAST PUB NAME LINE                      
*                                                                               
MULTIC9  BCT   R6,MULTIC3                                                       
MULTIC10 LTR   R4,R4                                                            
         BZ    MULTICE             NO UNPROTECTED LFDS FOUND                    
*                                  SEND ERROR MSG                               
         B     MULTICX                                                          
*                                                                               
MULTICE  MVI   FULL,1              SET FOR ERROR                                
*                                                                               
MULTICX  J     JUMPXIT1                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISOPTS  DS    0H                  MBCOT## COL 1-68, MBCDT## COL 69-80          
         BAS   RE,CLRSCN                                                        
*                                                                               
******** BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON?                          
******** BNE   *+12                                                             
******** BRAS  RE,TRAFFSCR         DIFFERENT HELP SCR FOR TRAFFIC USERS         
******** BE    DISOPTSX                                                         
*                                                                               
         FOUT  MBCOT01H,=C'CHANGEABLE DATA-',16                                 
         OI    MBCDT01H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT02H,=C'ADCODE,CASH DISC,AGY COMM,STATUS,NV,CU,PLANN+        
               ED $,FSI,RPT,REFNO,T/S,',67                                      
         OI    MBCDT02H+1,X'20'    MUST PROTECT FIELD!                          
         FOUT  MBCDT02H,=C'PAGEVIEWS',9                                         
*                                                                               
         FOUT  MBCOT03H,=C'CLICKTHRU,EIMPS,AIMPS,RATE,LINE RATE,INCH RA+        
               TE,LEGALW,TRAFFIC',61                                            
         OI    MBCDT03H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT04H,=C'CHANGEABLE DATES-',17                                
         OI    MBCDT04H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT05H,=C'BILLABLE,PAYABLE,CLOSING,ON-SALE,MATERIALS,S+        
               HIP,EXDATE',53                                                   
         OI    MBCDT05H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT06H,=C'SPACE FILTER (SPACE=), PRD GROUP FILTER (PGR+        
               P=)',47                                                          
         OI    MBCDT06H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT07H,=C'DATE FILTER TYPE (DATE=)',24                         
         OI    MBCDT07H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT08H,=C'INSERTION(DEFAULT),BILLABLE,PAYABLE,ON-SALE,+        
               CLOSING,MATERIALS,SHIP',66                                       
         OI    MBCDT08H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT09H,=C'ADCODE FILTER (ADCODE=)',23                          
         OI    MBCDT09H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT10H,=C'STATUS FILTER (STATUS=LIVE,TEST)',32                 
         OI    MBCDT10H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT11H,=C'NV FILTER (NV=YES,NO)',21                            
         OI    MBCDT11H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT12H,=C'T/S STATUS FILTER (T/S=A,N,X(NOT ENTERED))',+        
               42                                                               
         OI    MBCDT12H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT13H,=C'OUTSPC2 (OUTSPC2=YES,NO, OR FILTER)',35              
         OI    MBCDT13H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT14H,=C'COST TYPE ($=GROSS(DEFAULT),NET,G-CD,N-CD,CD+        
               )',45                                                            
         OI    MBCDT14H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT15H,=C'PUBNAME (NAME=YES OR NO)',24                         
         OI    MBCDT15H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT16H,=C'DELETED (VIEW DELETED RECS)',27                      
         OI    MBCDT16H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
DISOPTSX XC    PREVKEY,PREVKEY                                                  
         BAS   RE,UNVAL                                                         
         B     EXIT                                                             
*                                                                               
DISACTS  DS    0H                                                               
         BAS   RE,CLRSCN                                                        
         FOUT  MBCOT01H,=C'VALID ACTIONS:',14                                   
         FOUT  MBCOT02H,=C'DISPLAY',7                                           
         FOUT  MBCOT03H,=C'CHANGE',6                                            
         FOUT  MBCOT04H,=C'MBC   -SEE BELOW',16                                 
         FOUT  MBCOT06H,=C'TO CHANGE ALL DISPLAYED BUYS TO THE SAME DAT+        
               A:',46                                                           
         FOUT  MBCOT07H,=C'AFTER DISPLAY, ENTER ''MBC'' IN ACTION',36           
         FOUT  MBCOT08H,=C'AND THE COMMON DATA IN THE 1ST AVAILABLE LIN+        
               E',45                                                            
         FOUT  MBCOT09H,=C'   ** NOTE ** TO PREVENT THIS ACTION FROM CH+        
               ANGING A BUY',56                                                 
*                                                                               
         FOUT  MBCOT10H,=C'   ENTER ''*'' IN ITS DATA FIELD',30                 
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,UNVAL                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MISSERR  EQU   1                                                                
FACCERR  EQU   96                                                               
PRDERR   EQU   166                 PRODUCT MUST BE SPECIFIED                    
NODSPERR EQU   142                 MUST DISPLAY BEFORE CHA                      
*                                                                               
* INITIALISATION CODE                                                           
*                                                                               
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         ST    R2,VTIOB            V(TIOB)                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD          A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD          A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2)        NUMBER OF FIELDS                             
         ST    R3,VTWA             A(TWA)                                       
         MVC   VDATAMGR(44),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA)       TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LA    R3,IOAREA                                                        
         ST    R3,AREC                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CHI   R5,250                                                           
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         AHI   R5,-250                                                          
         B     CLEARWRK                                                         
*                                                                               
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
*                                                                               
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                                                                               
* FARMABLE CODE                                                                 
*                                                                               
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
*                                                                               
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
*                                                                               
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                OR NON NUMERIC                               
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         CLI   SVDLSW,0            PASS DELETED RECS WHEN DOING DELETE          
         BE    *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
*                                                                               
         CLI   SVIOSW,0            PASS DELETED RECS WHEN DOING LASTIO          
         BE    *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
*                                                                               
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             +        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
*                                                                               
         CLI   SVDLSW,0            PASS DELETED RECS WHEN DOING LASTIO          
         BE    *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
*                                                                               
         CLI   SVIOSW,0            PASS DELETED RECS WHEN DOING LASTIO          
         BE    *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
*                                                                               
         B     FILE                                                             
*                                                                               
FILE     NTR1                                                                   
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            +        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBDIR)                                      
*                                                                               
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
*                                                                               
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
*                                                                               
PUBDIRY  NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             +        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBFILE)                                     
*                                                                               
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
*                                                                               
PUBFILE  NTR1                                                                   
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            +        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         J     JUMPXIT1                                                         
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT1                          
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
*                                                                               
GET_ETXT LR    R0,RE               SAVE RETURN ADDRESS                          
         XC    MBCEMSG,MBCEMSG                                                  
         MVI   ERRAREA,X'FF'                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ERR_EXIT BRAS  RE,GET_ETXT                                                      
         MVI   ERRAREA,X'FF'                                                    
         J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MYWORK   DS    CL80                                                             
*                                                                               
HEAD1    DC    CL67'PRD PUBLICATION      EST  DATE        SPACE        +        
                           COST'                                                
HEAD2    DC    CL67'--- -----------      ---  --------    -----        +        
                           ----'                                                
*                                                                               
TAB1     DC    CL9'GROSS',C'1'                                                  
         DC    CL9'NET',C'3'                                                    
         DC    CL9'GROSS-CD',C'2'                                               
         DC    CL9'G-CD',C'2'                                                   
         DC    CL9'GLCD',C'2'                                                   
         DC    CL9'NET-CD',C'4'                                                 
         DC    CL9'N-CD',C'4'                                                   
         DC    CL9'NLCD',C'4'                                                   
         DC    CL9'NET/NET',C'4'                                                
         DC    CL9'NN',C'4'        NET/NET                                      
         DC    CL9'NETNET',C'4'    NET/NET                                      
         DC    CL9'CDISCOUNT',C'5'                                              
         DC    CL9'CASH DISC',C'5'                                              
         DC    X'FF'                                                            
*                                                                               
TAB2     DC    CL9'BILLING  ',C'B'                                              
         DC    CL9'BILLABLE ',C'B'                                              
         DC    CL9'BL DATE  ',C'B'                                              
         DC    CL9'BILL DATE',C'B'                                              
         DC    CL9'BILLDATE ',C'B'                                              
         DC    CL9'BILL DTE ',C'B'                                              
         DC    CL9'PAYABLE  ',C'P'                                              
         DC    CL9'PAY DATE ',C'P'                                              
         DC    CL9'PAYDATE  ',C'P'                                              
         DC    CL9'PAYING   ',C'P'                                              
         DC    CL9'PAY DTE  ',C'P'                                              
         DC    CL9'PY DATE  ',C'P'                                              
         DC    CL9'CLOSING  ',C'C'                                              
         DC    CL9'CL DATE  ',C'C'                                              
         DC    CL9'ON-SALE  ',C'O'                                              
         DC    CL9'ONSALE   ',C'O'                                              
         DC    CL9'OS DATE  ',C'O'                                              
         DC    CL9'OS DTE   ',C'O'                                              
         DC    CL9'INSERTION',C'I'                                              
         DC    CL9'MATERIALS',C'M'                                              
         DC    CL9'MATCLOSE ',C'M'                                              
         DC    CL9'MCLOSE   ',C'M'                                              
         DC    CL9'SHIPDATE ',C'S' SHIP DATE                                    
         DC    CL9'SDATE    ',C'S' SHIP DATE                                    
         DC    X'FF'                                                            
*                                                                               
         DC    X'FF'                                                            
*                                                                               
ACTTAB   DC    CL9'DISPLAY  ',C'D'                                              
         DC    CL9'CHANGE   ',C'C'                                              
         DC    CL9'CHAPLAY  ',C'C'                                              
         DC    CL9'CHG      ',C'C'                                              
         DC    CL9'CHGPLAY  ',C'C'                                              
         DC    CL9'CHAALL   ',C'M' TO CHANGE ALL DISPLAYED BUYS                 
         DC    CL9'CHGALL   ',C'M' TO DATA IN FIRST LINE                        
         DC    CL9'ALLCHA   ',C'M'                                              
         DC    CL9'ALLCHG   ',C'M'                                              
         DC    CL9'MBCCHG   ',C'M'                                              
         DC    CL9'MBCCHA   ',C'M'                                              
         DC    CL9'MBCPLAY  ',C'M'                                              
         DC    CL9'MCHG     ',C'M'                                              
         DC    CL9'MCHANGE  ',C'M'                                              
         DC    CL9'NEXT     ',C'N'                                              
         DC    X'FF'                                                            
*                                                                               
* LIMIT ACCESS TABLE                                                            
*                                                                               
ACCTAB   DC    C'A',X'1000'        AD CODE                                      
         DC    C'C',X'2000'        CONTRACT UNIT                                
         DC    C'L',X'4000'        NV LETTER STATUS                             
         DC    C'P',X'8000'        PLANNED COST                                 
         DC    C'S',X'0100'        STATUS                                       
         DC    C'1',X'0200'        ON-SALE DATE                                 
         DC    C'2',X'0400'        BILLABLE DATE                                
         DC    C'3',X'0800'        PAYABLE DATE                                 
         DC    C'4',X'0010'        CLOSING DATE                                 
         DC    C'5',X'0020'        MATERIALS CLOSING DATE                       
         DC    C'6',X'0000'        ACTUAL IMPRESSION                            
         DC    C'7',X'0040'        CASH DISCOUNT                                
         DC    C'8',X'0080'        AGY COMMISSION                               
         DC    C'9',X'0020'        EXTENSION DATE                               
         DC    C'D',X'0000'        DLC                                          
         DC    C'F',X'0000'        FSI                                          
         DC    C'R',X'0000'        REF NUMBER                                   
         DC    C'T',X'0000'        RPT NUMBER                                   
         DC    C'H',X'0010'        SHIP DATE                                    
         DC    C'I',X'0000'        IMPRESSION (ESTIMATED)                       
         DC    C'E',X'0000'        TEARSHEET STAT (COULD HAVE A VALUE)          
         DC    C'V',X'0000'        PAGE VIEWS                                   
         DC    C'K',X'0000'        CLICK THRUS                                  
         DC    C'X',X'0000'        SPECIAL FINANCIAL HANDLING                   
         DC    C'B',X'8000'        COST 2 FACTOR                                
         DC    C'$',X'8000'        RATE (EXCEPT INCH AND LINE)                  
         DC    C'M',X'8000'        INCH RATES (NEWSPAPERS)                      
         DC    C'N',X'8000'        LINE RATES (NEWSPAPERS)                      
         DC    C'U',X'0000'        UPLOAD ID                                    
         DC    C'W',X'1000'        LEGAL WARNINGS                               
         DC    C'Z',X'1000'        "NO TRAFFIC"                                 
         DC    C'#',X'0000'        SERIAL NUMBER                                
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DOTTED MEANS IT HAS BEEN USED ALREADY                                         
*                                                                               
* USED ALPHAS: A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z              
*              . . . . . .   . .   . . . .   .   . . . . . . .   .              
*                                                                               
* USED DIGITS: 1,2,3,4,5,6,7,8,9,0                                              
*              . . . . . . . . .                                                
*                                                                               
* USED OTHERS: !,@,#,$,%,^,&,*,(,),-,+,=,{,,º,},\,|,;,:,',",<,>,?,/            
*                  . .                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TAB3     DC    CL9'ADCODE   ',C'A'                                              
         DC    CL9'ACOMM    ',C'8'                                              
         DC    CL9'ACNET    ',C'8' WILL SET GNOPT TO 'Y'                        
         DC    CL9'AGCY COMM',C'8'                                              
         DC    CL9'AGENCYCOM',C'8'                                              
         DC    CL9'AGY COMM ',C'8'                                              
         DC    CL9'BL DATE  ',C'2'                                              
         DC    CL9'BILL DATE',C'2'                                              
         DC    CL9'BILLDATE ',C'2'                                              
         DC    CL9'BILL DTE ',C'2'                                              
         DC    CL9'BILLABLE ',C'2'                                              
         DC    CL9'BDATE    ',C'2'                                              
         DC    CL9'CASH DISC',C'7'                                              
         DC    CL9'CASH DSCT',C'7'                                              
         DC    CL9'CASH DSC ',C'7'                                              
         DC    CL9'CLOSING  ',C'4'                                              
         DC    CL9'CL DATE  ',C'4'                                              
         DC    CL9'CDISCOUNT',C'7'                                              
*                                                                               
         DC    CL9'CDATE    ',C'4'                                              
         DC    CL9'CUNITS   ',C'C' CONTRACT UNITS                               
         DC    CL9'CONUNITS ',C'C'                                              
         DC    CL9'CNUNITS  ',C'C'                                              
         DC    CL9'CLICKTHRO',C'K' CLICK THROUGHS                               
         DC    CL9'CLICKTHRU',C'K' CLICK THROUGHS                               
         DC    CL9'CLICK THR',C'K' CLICK THROUGHS                               
         DC    CL9'CLICKS   ',C'K' CLICK THROUGHS                               
         DC    CL9'CTHRUS   ',C'K' CLICK THROUGHS                               
         DC    CL9'CTHROUGHS',C'K' CLICK THROUGHS                               
         DC    CL9'C2FACTOR ',C'B' COST 2 FACTOR                                
         DC    CL9'C2 FACTOR',C'B' COST 2 FACTOR                                
         DC    CL9'DLC      ',C'D' DLC (DAILY EFFECTIVE CIRC, OUTDOOR)          
*                                                                               
         DC    CL9'FSI      ',C'F' FSI (FREE STANDING INSERTS), NOT "O"         
*                                                                               
         DC    CL9'JOBCODE  ',C'A' SAME AS AD CODE                              
*                                                                               
         DC    CL9'LINERATE ',C'N' LINE RATE (NEWS)                             
         DC    CL9'LINE RATE',C'N' LINE RATE (NEWS)                             
         DC    CL9'LNRATE   ',C'N' LINE RATE (NEWS)                             
         DC    CL9'LRATE    ',C'N' LINE RATE (NEWS)                             
*                                                                               
         DC    CL9'MATERIALS',C'5'                                              
         DC    CL9'MATCLOSE ',C'5'                                              
         DC    CL9'MCLOSE   ',C'5'                                              
         DC    CL9'MCDATE   ',C'5'                                              
         DC    CL9'NV LETTER',C'L'                                              
         DC    CL9'NVLETTER ',C'L'                                              
         DC    CL9'OS DATE  ',C'1'                                              
         DC    CL9'OS DTE   ',C'1'                                              
         DC    CL9'ON-SALE  ',C'1'                                              
         DC    CL9'ONSALE   ',C'1'                                              
         DC    CL9'PY DATE  ',C'3'                                              
         DC    CL9'PAY DATE ',C'3'                                              
         DC    CL9'PAYDATE  ',C'3'                                              
         DC    CL9'PAY DTE  ',C'3'                                              
         DC    CL9'PAYABLE  ',C'3'                                              
         DC    CL9'PDATE    ',C'3'                                              
         DC    CL9'PVIEWS   ',C'V' PAGEVIEWS (FOR WEBSITES)                     
         DC    CL9'PAGEVIEWS',C'V' PAGEVIEWS (FOR WEBSITES)                     
         DC    CL9'PAGE VIEW',C'V' PAGEVIEWS (FOR WEBSITES)                     
         DC    CL9'PAGEVUS  ',C'V' PAGEVIEWS (FOR WEBSITES)                     
*                                                                               
         DC    CL9'EST IMPRS',C'I' ESTIMATED IMPS (SAME AS IMP)                 
         DC    CL9'EST IMPS ',C'I' ESTIMATED IMPS (SAME AS IMP)                 
         DC    CL9'EIMPS    ',C'I'                                              
         DC    CL9'EIMPRS   ',C'I'                                              
*                                                                               
         DC    CL9'ACT IMPRS',C'6' ACTUAL IMPRESSIONS                           
         DC    CL9'ACT IMPS ',C'6' ACTUAL IMPRESSIONS                           
         DC    CL9'AIMPS    ',C'6'                                              
         DC    CL9'AIMPRS   ',C'6'                                              
*                                                                               
         DC    CL9'IRATE    ',C'M' INCH RATE (NEWS)                             
         DC    CL9'INCHRATE ',C'M' INCH RATE (NEWS)                             
         DC    CL9'INCH RATE',C'M' INCH RATE (NEWS)                             
         DC    CL9'INRATE   ',C'M' INCH RATE (NEWS)                             
*                                                                               
         DC    CL9'PCOST    ',C'P' PLANNED COST                                 
         DC    CL9'PLANNED $',C'P' PLANNED COST                                 
         DC    CL9'PLANNED C',C'P' PLANNED COST                                 
*                                                                               
         DC    CL9'RATE     ',C'$' RATES (NON-NEWS)                             
*                                                                               
         DC    CL9'REFERENCE',C'R' REFERNECE NUMBER                             
         DC    CL9'REFNUMBER',C'R' REFERNECE NUMBER                             
         DC    CL9'REF NO   ',C'R' REFERNECE NUMBER                             
         DC    CL9'REFNO    ',C'R' REFERNECE NUMBER                             
         DC    CL9'RPT      ',C'T' RPT (REPAINTS - OUTDOOR)                     
         DC    CL9'REPAINTS ',C'T' RPT (REPAINTS - OUTDOOR)                     
         DC    CL9'SCD      ',C'4'                                              
         DC    CL9'SCLOSE   ',C'4'                                              
         DC    CL9'SDATE    ',C'H' SHIP DATE                                    
         DC    CL9'SERIAL#  ',C'#' SERIAL NUMBER                                
         DC    CL9'SERIAL # ',C'#' SERIAL NUMBER                                
         DC    CL9'SHIPDATE ',C'H' SHIP DATE                                    
         DC    CL9'SFH      ',C'X' SPECIAL FINANCIAL HANDLING                   
         DC    CL9'STATUS   ',C'S'                                              
         DC    CL9'TEARSHEET',C'E' TEARSHEET STATUS                             
*                                                                               
         DC    CL9'TOTALRATE',C'$' RATE (T RATES FOR NEWS)                      
         DC    CL9'TRATE    ',C'$' RATE (T RATES FOR NEWS)                      
*                                                                               
         DC    CL9'T/STATUS ',C'E' TEARSHEET STATUS                             
         DC    CL9'T/S STATU',C'E' TEARSHEET STATUS                             
         DC    CL9'T/SHEET  ',C'E' TEARSHEET STATUS                             
         DC    CL9'TSHEET   ',C'E' TEARSHEET STATUS                             
         DC    CL9'TSTATUS  ',C'E' TEARSHEET STATUS                             
*                                                                               
         DC    CL9'UPID     ',C'U' UPID                                         
*                                                                               
         DC    CL9'LEGALW   ',C'W' LEGAL WARNING                                
*                                                                               
         DC    CL9'TRAFFIC  ',C'Z' "TRAFFIC?"                                   
*                                                                               
         DC    CL9'EXDATE   ',C'9' EXTENSION DATE                               
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FILL IN FIELDS ON SCREEN FROM GLOBBER AREA                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FILSCR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* CLEAR ALL UNPROTECTED FIELDS NO SCREEN                                        
*                                                                               
         LA    R2,MBCMDIAH         POINT TO FIRST FIELD ON SCREEN               
         SR    RF,RF                                                            
*                                                                               
FCLCLRLP DS    0H                                                               
         TM    1(R2),X'20'         SKIP IF PROTECTED FIELD                      
         BO    FCLCLRCN                                                         
*                                                                               
         ICM   RF,1,0(R2)          TOTAL FIELD LENGTH                           
         BZ    FCLCLRDN            DONE IF SCREEN END REACHED                   
*                                                                               
         AHI   RF,-8               HEADER LENGTH                                
         TM    1(R2),X'02'         SKIP IF NOT EXTENDED HEADER                  
         BNO   *+8                                                              
         AHI   RF,-8               EXTENSION LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
*                                                                               
         FOUT  (R2)                FORCE RE-TRANSMISSION                        
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
*                                                                               
FCLCLRCN DS    0H                                                               
         IC    RF,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         LA    R2,0(RF,R2)                                                      
         B     FCLCLRLP                                                         
*                                                                               
FCLCLRDN DS    0H                                                               
         MVI   INVMATSW,C'Y'       INDICATE CALLED BY MATCH                     
*                                                                               
         L     RF,VGLOBBER         POINT TO GLOBBER                             
*                                                                               
* FILL IN HEADLINE FLDS, MEDIA, CLT, PRD, EST, PUB, PEROID                      
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',MBCMDIAH,,GLVPRMD                             
         GOTO1 (RF),(R1),,MBCCLTH,,GLVPRCLT                                     
         GOTO1 (RF),(R1),,MBCPRDH,,GLVPRPRD                                     
         GOTO1 (RF),(R1),,MBCESTH,,GLVPREST                                     
         GOTO1 (RF),(R1),,MBCPUBH,,GLVPRPUB                                     
         GOTO1 (RF),(R1),=C'GETD',TEMP,12,GLVPRPER                              
*                                                                               
         GOTO1 VDATCON,DMCB,TEMP,(17,MBCSTDE)                                   
         MVI   MBCSTDEH+4,0        KILL INPUT INDICATORS                        
         MVI   MBCSTDEH+5,8        SET DATA LENGTH                              
*                                                                               
         GOTO1 VDATCON,DMCB,TEMP+6,(17,MBCEDDE)                                 
         MVI   MBCEDDEH+4,0        KILL INPUT INDICATORS                        
         MVI   MBCEDDEH+5,8        SET DATA LENGTH                              
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',MBCDATH,,GLVPRDTA                         
*                                                                               
         MVC   MBCACT(7),=C'DISPLAY'                                            
         MVI   MBCACTH+4,0         KILL INPUT INDICATORS                        
         MVI   MBCACTH+5,7         SET DATA LENGTH                              
*                                                                               
         FOUT  MBCMDIAH            FORCE RE-TRANSMISSION OF MEDIA               
         FOUT  MBCCLTH             FORCE RE-TRANSMISSION OF CLIENT              
         FOUT  MBCPRDH             FORCE RE-TRANSMISSION OF PRODUCT             
         FOUT  MBCESTH             FORCE RE-TRANSMISSION OF ESTIMATE            
         FOUT  MBCPUBH             FORCE RE-TRANSMISSION OF PUB                 
         FOUT  MBCSTDEH            FORCE RETRANSMISSION  OF START DTE           
         FOUT  MBCEDDEH            FORCE RETRANSMISSION  OF END   DTE           
         FOUT  MBCDATH             FORCE RE-TRANSMISSION OF MBC DATA            
         FOUT  MBCACTH             FORCE RE-TRANSMISSION OF ACTION              
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'DELE'                                           
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,16,GLVXCTL                           
*                                                                               
         GOTO1 (RF),(R1),=C'DELE',,,GLVXCTL  DELETE FROM GLOBBER                
*                                                                               
FILSCRX  J     JUMPXIT1                                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPGRP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   BYTE3,0             SWTICH FOR RETURNING ERROR CODES             
*                                                                               
         CLC   12(03,R6),=C'PGR'   MATCHING KEYWORDS?                           
         BE    CKPG20                                                           
         CLC   12(04,R6),=C'PGRP'                                               
         BE    CKPG20                                                           
         CLC   12(06,R6),=C'PGROUP'                                             
         BE    CKPG20                                                           
         CLC   12(07,R6),=C'PRD GRP'                                            
         BE    CKPG20                                                           
         CLC   12(06,R6),=C'PRDGRP'                                             
         BNE   CKPGRPX                                                          
*                                                                               
CKPG20   CLI   1(R6),0             SECOND FIELD (PGRP FILTER) PRESENT?          
         BE    CKPG90              IT IS NEEDED FOR PGRP KEYWORD                
*                                                                               
         OC    SVPGRP,SVPGRP       PGRP FILTER ALREADY ENTERED?                 
         BNZ   CKPG91              ALREADY EXIST, INVALID ERROR                 
         CLI   1(R6),5                                                          
         BH    CKPG91              MAX OF 5 CHARS FOR VALID PGRP FILTER         
         CLC   SVPRD,=C'ALL'                                                    
         BNE   CKPG92              TO USE PGRP FILTER, PRD MUST BE ALL          
*                                                                               
         CLI   22(R6),C'A'         FIRST CHAR IS ALPHA?                         
         BL    CKPG91                                                           
         CLI   22(R6),C'I'                                                      
         BNH   CKPG30              A,B,C,D,E,F,G,H,I                            
         CLI   22(R6),C'J'                                                      
         BL    CKPG91                                                           
         CLI   22(R6),C'R'                                                      
         BNH   CKPG30              J,K,L,M,N,O,P,Q,R                            
         CLI   22(R6),C'S'                                                      
         BL    CKPG91                                                           
         CLI   22(R6),C'Z'                                                      
         BH    CKPG91              S,T,U,V,W,X,Y,Z                              
*                                                                               
CKPG30   LA    RE,23(R6)           POINT TO 1 BEYOND SCHEME CODE                
         LA    RF,1                INPUT COUNTER                                
         ZIC   R3,1(R6)                                                         
*                                                                               
         CHI   R3,1                ALPHA PORTION OF SCHEME CODE ONLY?           
         BE    CKPG70              YES                                          
*                                                                               
CKPG40   CLI   0(RE),C'0'          NUMERIC?                                     
         BL    CKPG91                                                           
         CLI   0(RE),C'9'          NUMERIC?                                     
         BH    CKPG91                                                           
         LA    RE,1(RE)            POINT TO NEXT CHAR IN INPUT                  
         AHI   RF,1                INCREMENT FOR CHAR COUNTER                   
         CR    R3,RF                                                            
         BE    CKPG50              VALID SCHEME CODE ENTERED                    
         B     CKPG40                                                           
*                                                                               
CKPG50   AHI   RF,-2               MINUS 1 FOR ALPHA AND 1 FOR EX               
         CHI   RF,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD LENGTH!                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVPGRP+1(0),23(R6)  NUMERIC PORTION OF SCHEME CODE               
*                                                                               
CKPG70   MVC   SVPGRP+0(1),22(R6)  ALPHA PART OF SCHEME CODE                    
         B     CKPG99                                                           
*                                                                               
CKPG90   LA    R2,MBCDATH                                                       
         LA    R3,MISSERR          PRD GRP FILTER IS MISSING                    
         MVI   BYTE3,1                                                          
         B     CKPGRPX                                                          
*                                                                               
CKPG91   LA    R2,MBCDATH                                                       
         LA    R3,FLDINV           PRD GRP FILTER ENTERED IS INVALID            
         MVI   BYTE3,1                                                          
         B     CKPGRPX                                                          
*                                                                               
CKPG92   LA    R2,MBCPRDH                                                       
         LA    R3,183              PRD HAS TO BE ALL FOR PGRP FILTERING         
         MVI   BYTE3,1                                                          
         B     CKPGRPX                                                          
*                                                                               
CKPG99   MVI   BYTE3,99                                                         
*                                                                               
CKPGRPX  J     X_R2R3              ERROR MSG & CURSOR POSITION ARE SET          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LEGWHDR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   DATASW,C'W'         LEGAL WARNINGS?                              
         BNE   LEGWHDRX                                                         
         MVC   MBCHD1,LWHEAD1      SET LEGAL WARNINGS HEADER                    
         MVC   MBCHD2,LWHEAD2      SET LEGAL WARNINGS HEADER DASHES             
         FOUT  MBCHD1H                                                          
         FOUT  MBCHD2H                                                          
*                                                                               
LEGWHDRX J     JUMPXIT1                                                         
*                                                                               
LWHEAD1  DC    CL67'PRD PUBLICATION      EST  DATE        SPACE        +        
                     DEFAULT LW'                                                
LWHEAD2  DC    CL67'--- -----------      ---  --------    -----        X        
                     ----------'                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         L     R0,AWRKREC                                                       
         LHI   R1,400*4                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         L     R4,AWRKREC                                                       
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                
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
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFACC NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC LIMIT ACCESS            
*                                                                               
         MVI   BYTE3,0             FOR RETURN CODE                              
*                                                                               
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                GOT TO HAVE AT LEAST ONE ELEM!               
         CLC   ELCODE,0(R2)                                                     
         BE    *+12                FOUND IN FIRST ELEM                          
         BRAS  RE,NXTELEM                                                       
         BNE   TRACCX              NO CLIENT TRAFFIC OFFICE CODE ELEM           
         MVC   BYTE3,2(R2)         SAVE CLIENT TRAFFIC OFFICE CODE              
*                                                                               
TRACCX   J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFSCR NTR1  BASE=*,LABEL=*      TRAFFIC USERS HAVE LIMITED CHOICES           
*                                                                               
         FOUT  MBCOT01H,=C'CHANGEABLE DATA-',16                                 
         OI    MBCDT01H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT02H,=C'ADCODE,LEGALW',13                                    
         OI    MBCDT02H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
******** FOUT  MBCOT03H,=C'     NOT YET USED       ',24                         
         OI    MBCDT03H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT04H,=C'CHANGEABLE DATES-',17                                
         OI    MBCDT04H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT05H,=C'SHIP,EXDATE',11                                      
         OI    MBCDT05H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
******** FOUT  MBCOT06H,=C'     NOT YET USED       ',24                         
         OI    MBCDT06H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT07H,=C'DATE FILTER TYPE (DATE=)',24                         
         OI    MBCDT07H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT08H,=C'INSERTION(DEFAULT),BILLABLE,PAYABLE,ON-SALE,+        
               CLOSING,MATERIALS,SHIP',66                                       
         OI    MBCDT08H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT09H,=C'ADCODE FILTER (ADCODE=)',23                          
         OI    MBCDT09H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT10H,=C'STATUS FILTER (STATUS=LIVE,TEST)',32                 
         OI    MBCDT10H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT11H,=C'NV FILTER (NV=YES,NO)',21                            
         OI    MBCDT11H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT12H,=C'T/S STATUS FILTER (T/S=A,N,X(NOT ENTERED))',+        
               42                                                               
         OI    MBCDT12H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT13H,=C'OUTSPC2 (OUTSPC2=YES,NO, OR FILTER)',35              
         OI    MBCDT13H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT14H,=C'COST TYPE ($=GROSS(DEFAULT),NET,G-CD,N-CD,CD+        
               )',45                                                            
         OI    MBCDT14H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
         FOUT  MBCOT15H,=C'PUBNAME (NAME=YES OR NO)',24                         
         OI    MBCDT15H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
******** FOUT  MBCOT16H,=C'     NOT YET USED       ',24                         
         OI    MBCDT16H+1,X'20'    MUST PROTECT FIELD!                          
*                                                                               
TRSCRX   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
TRSCRERR LTR   RB,RB               NOT EQUAL (NOT TRAFFIC USER)                 
         J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKFXPROF NTR1  BASE=*,LABEL=*      CHECK FOREIGN EXCHANGE PROFILE               
*                                                                               
         XC    TEMP,TEMP           FOREIGN EXCHANGE PROFILE VALUES              
         MVC   WORK+00(04),=C'P0FX'                                             
         MVC   WORK+04(2),AGYALPHA                                              
         MVC   WORK+06(1),SVMED                                                 
         MVC   WORK+07(3),SVCLT    NOTE: NO CLT OFFICE FOR FX PROFILE           
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'90',WORK),TEMP,VDATAMGR                             
         CLI   TEMP+00,C'Y'                                                     
         BNE   CKFXP_X                                                          
*                                                                               
         LA    R3,NOUPDTER         UPDATES NOT AVAILABLE                        
         LTR   RB,RB               SET CC NOT EQUAL                             
         J     X_R2R3                                                           
*                                                                               
CKFXP_X  CR    RB,RB               SET CC EQUAL                                 
         J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FATWA                                                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPERREQUS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENGRP                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PPMBCWRK                                                       
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054PPMBC00   06/22/10'                                      
         END                                                                    
*                                                                               
