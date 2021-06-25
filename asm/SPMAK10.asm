*          DATA SET SPMAK10    AT LEVEL 103 AS OF 09/16/20                      
*PHASE T22810A                                                                  
*                                                                               
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT SPEC-49246   09/16/20 SEND FLAG FOR PURPOSE CODE VS BUY-ID     *         
* AKAT SPEC-48927   08/19/20 READ STW EST ONLY IF NEW I2N PROFILE = Y *         
* AKAT SPEC-46400   05/19/20 INV DEMO MOVING FROM SNVIDDEM TO SNVIDDM4*         
* AKAT SPEC-44847   04/06/20 HANDLE SNVIDDEM VALUES > X'3FFF'         *         
* AKAT SPEC-31824   07/15/19 TWO DECIMAL IMPRESSIONS SUPPORT          *         
* AKAT SPEC-31662   07/15/19 CANADIAN NET$ SUPPORT                    *         
* AKAT MOXSYS-150   10/03/16 SUPPORT COMSCORE DEMOS                   *         
***********************************************************************         
*                                                               *               
* 21NOV13 097 SUPPORT CROSS NETWORK MAKEGOODS                   *               
* 07OCT13 096 CALL BLDMGN TO CONVERT MAKEGOOD CODES             *               
* 27AUG13 095 INCREASE TSPAGN TO 40                             *               
* 05JUL13 094 FIX FOR DEFERRED SPOTS                            *               
* 25OCT12 093 DOLLARS ALREADY IN PENNIES AFTER GETRATE          *               
* 09SEP11 092 NEW PROFILE VALUE & SUPPORT PROGRAM NAME FEATURE  *               
*         --- NEW DOWNLOAD OF SUBSTITUTE BRANDS FROM CMML REC   *               
* 27APR11 091 INCREASE TSPAGN TO 28                             *               
* 04APR11 090 2-BYTE BUYLINE SUPPORT                            *               
* 03MAR11 089 BROADCAST FIX FOR NO COMMCIAL TIMES               *               
* 25FEB11 088 BROADCAST SUPPORT FOR COMMERCIAL TIMES            *               
* 18JAN11 087 SEND NEW PROFILE VALUE                            *               
* 15DEC10 086 REMOVE BROADCAST LOGIC FOR CMML TIMES MATCHING    *               
* 10SEP10 085 NETWORK EXPLODED BUYLINES CAN NOW BE IN DOLLARS   *               
* 19FEB10 084 MATCH ON CMML LENGTH                              *               
* 03JUN09 083 FIX SEARCH SPOTS FOR $0 COSTS  DAYS               *               
* 08MAY09 082 HANDLE TIM-FLM CHECKING ACROSS DAYS               *               
* 13FEB09 081 SUPPORT RATE TYPES                                *               
* 24OCT08 080 ADD CMLSEQ NUMBER AND STATUS TO DOWNLOAD          *               
* 27AUG08 079 SEND UNALLOCATED SPOTS                            *               
* 10JUN08 078 SKIP BUYS WITH MATCH=NO IN COMMENT                *               
* 09MAY08 077 SEND TAX AT THE LINE LEVEL AS WELL                *               
* 13AUG07 075 ALLOW 12 CMML PRODS                               *               
*         --- I2N PROFILE                                       *               
*         --- SEND TAX FOR EACH SPOT                            *               
* 27MAR07 074 INCREASE TSPAGN TO 12 (MAX I THINK!)              *               
* 28OCT06 073 SEND INVOICE MANAGER ID ELEM                      *               
* 04MAY06 072 SEND BUYLINE ORBIT INFO                           *               
*         --- AUTO I2 PROFILE                                   *               
*         --- SPOT LENGTH LEEWAY                                *               
* 20APR06 071 COST OVERRIDES FOR CANADA AT THE NETWORK LEVEL    *               
* 06DEC05 070 PROGRAM EXCHANGE FEATURES                         *               
* 02JAN06 069 FIX COST OVERRIDE BUG                             *               
* 30AUG05 068 ADDED VALUE PHASE 2                               *               
* 15JUL05 067 SUPPORT ADDED VALUE SPOTS                         *               
* 25MAR05 066 INCREASE TSPAGN TO 10                             *               
* 01MAR05 065 NEED TO SEND COSTS AS NEGATIVE IF THEY ARE!       *               
* 22NOV04 064 NEW MK PROFILE FIELD FOR CHANGE RNO COST          *               
*         --- SEND RNO COST CHANGED FLAG                        *               
* 01DEC04 063 DON'T ADJUST US$ - GETRATE ALWAYS RETURNS PENNIES *               
* 09JUL04 062 EARNED DISCOUNT/COST2                             *               
*         --- 2 DECIMAL DEMOS                                   *               
*         --- ALWAYS SEND COSTS IN $                            *               
* 21JUN04 061 SUPPORT AD-ID                                     *               
* 07MAY04 060 NO CABLE FOR CANADA!                              *               
* 23MAR04 059 NEW TESTS FOR CABLE                               *               
* 18DEP03 058 NO MORE PRISONERS ON BAD PRD CODES!               *               
* 12AUG03 057 GIVE ERROR ON DUP SPOTS - IT'S JUST TOO MANY      *               
* 13MAY03 056 VERSION 3.1                                       *               
* 11MAR03 055 FIX SAVE OF BUYMSTA                               *               
* 04FEB03 054 INVOICES BY BUYER CODE                            *               
* 24JUN02 053 VERSION 3.0                                       *               
*         --- IF ACTION IS APPROVE/UN-APPROVE, DON'T SEND DATA  *               
*         --- NEW MK PROFILE FIELD                              *               
*         --- FIX PROCNV CALL TO FIND MATCHNG SPOT              *               
*         --- DELETE BRAND CODE                                 *               
*         --- FIX CODE FOR DEFERRED SPOTS                       *               
* 15JUL02 049 TELL SPOTIO NOT TO SKIP STW/BAR ESTS              *               
* 21MAY02 048 SEND I2Z TIME LEEWAY IF SET                       *               
* 02MAY02 047 GIVE ERROR MESSAGE FOR MISSING CMML RECORD        *               
* 06MAR02 046 ERROR MESSAGE ON TSAR BUFFER OVERFLOW             *               
*         --- FIX SF SEMICOL CODE!                              *               
* 20NOV01 045 TRY TO DO SOMETHING ABOUT DEMOS...                *               
*         --- SEND SOME MORE PROFILI                            *               
*         --- SEND I2 DATE AND TIME                             *               
*         --- HELP OUT DEFERRED SPOTS A LITTLE BIT              *               
* 01OCT01 044 BUILD TABLE OF INVOICE CHECKSUMS                  *               
* 04JUN01 043 DISABLE MM FOR BRAND CLTS                         *               
* 22MAY01 042 SENDING WRONG RELATIVE INVOICE NUMBER             *               
* 17MAY01 041 MAKE SURE PASSIVE STILL AROUND BEFORE DETAIL DL   *               
*                                                               *               
*===============================================================*               
*                                                                               
T22810   TITLE 'SPMAK10 - MATCHMAKER - BUILD BUY/SPOT/INVOICE TABLES'           
T22810   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMK10**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         LA    R3,SPTBLK                                                        
         USING SPTBLKD,R3                                                       
*                                                                               
         USING TSARD,TSARBLK                                                    
*                                                                               
         MVC   SVTSRKEY(1),BAGYMD  SAVE NEW TSAR DATA KEY                       
         MVC   SVTSRKEY+1(2),BCLT                                               
         MVC   SVTSRKEY+3(1),BPRD                                               
         MVC   SVTSRKEY+4(2),BMKT                                               
         MVC   SVTSRKEY+6(3),BSTA                                               
         MVC   SVTSRKEY+9(1),BEST                                               
         XC    SVBUYCNT,SVBUYCNT   CLEAR BUY COUNTER                            
         L     R0,ATIA             CLEAR SAVED DEMO AREA                        
         LHI   R1,255*L'EDEMLST                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   SVRCVEL+1,H3FQ      SEARCH REQUEST?                              
         BE    M10                  YES - LEAVE SAVED INVOICE KEY ALONE         
         XC    SVSNVKEY,SVSNVKEY   CLEAR FIRST INVOICE KEY                      
         XC    I2DATE,I2DATE                                                    
         XC    I2TIME,I2TIME                                                    
         XC    SVEDEMOS,SVEDEMOS                                                
*                                                                               
         LR    R0,RA               AND CLEAR CHECKSUM TABLE                     
         AHI   R0,SVCHKSUM-TWAD                                                 
         LHI   R1,SVCHKL                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
M10      MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,BTKEYX-BTKEY                                              
         MVI   TSPAGN,TSPEXPN      REQUEST 40 PAGES                             
         OI    TSRECI,TSRTSARB+TSRXTN                                           
         OI    TSINDS,TSINODSK     TEMPEST IS IN USE BY FALINK !!!              
         OI    TSINDS,TSIXTTWA     AND IT HAS BIG PAGES !                       
         LHI   R0,BTRECX-TSARREC   BE SURE TO CHOOSE THE LONGEST !              
         STH   R0,TSRECL                                                        
         LA    R1,BTREC                                                         
         ST    R1,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
* THESE MASKS FOR EDIT WHEN HAVE NO BASE REG                                    
         MVC   EDSAVEP(17),=X'4040404040402020202020202020202020'               
         MVC   EDSAVEM(17),=X'4040404040202020202020202020202060'               
         MVC   EDSAVE,EDSAVEP                                                   
*                                                                               
         MVI   INVHDSEQ,0                                                       
*                                                                               
         BRAS  RE,CKPASS           TEST PASSIVE KEY NOT DELETED                 
         CLI   SVRCVEL+1,H3FQ      SEARCH REQUEST?                              
         BNE   *+12                 NO                                          
         BRAS  RE,SAVEFILT          YES - SAVE FILTERS                          
         B     *+8                   AND DON'T SEND PROFILI                     
         BRAS  RE,SENDPROF         SEND CLT LEVEL PROFILES                      
         BAS   RE,CALLSPIO                                                      
         BRAS  RE,SENDSPTS                                                      
*                                                                               
         CLI   SVRCVEL+1,H3FQ      SEARCH REQUEST?                              
         BE    EXIT                 YES - DON'T NEED DEMOS                      
         BRAS  RE,SENDPDEM         SEND PRIMARY DEMO                            
*                                                                               
         CLI   PROFB0+7,0          REASON CODES REQUIRED?                       
         BE    EXIT                 NO                                          
         BRAS  RE,SENDRC                                                        
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
         J     EXIT                                                             
         EJECT                                                                  
CALLSPIO NTR1                                                                   
         MVC   SBCOMFAC,ACOMFACS                                                
         MVC   SBAIO1(12),AIO1                                                  
         LA    R1,IOHOOK           SET ADDRESS OF IOHOOK                        
         ST    R1,SBIOHOOK                                                      
         MVC   SBAIO1(12),AIO1         PASS 3 I/O AREAS                         
         OI    SBQSKIP,X'FF'-SBQSKBUY  SKIP ALL EXCEPT BUYS                     
         OI    SBQREAD,SBQRDINV        READ INVOICES                            
         CLI   PROFI2N+11,C'Y'         PROCESS STW ESTIMATES?                   
         BNE   *+8                     NO                                       
         MVI   SBQETYPE,C'*'       DON'T SKIP STW/BAR ESTS                      
*                                                                               
         CLI   SVRCVEL+1,H3FQ      SEARCH SPOT REQUEST?                         
         BNE   *+8                                                              
         NI    SBQREAD,X'FF'-SBQRDINV  YES - DON'T READ INVOICES                
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BPER),SBQSTART                                   
         GOTO1 (RF),(R1),(2,BPER+2),SBQEND                                      
         GOTO1 VADDAY,DMCB,SBQSTART,WORK,-7                                     
         GOTO1 VDATCON,DMCB,(0,WORK),(2,BPERDEF)                                
*                                                                               
         MVC   SBQREQST,SBQSTART                                                
         MVC   SBQREQND,SBQEND                                                  
         LA    R1,BPER                 2 BYTE START/END DATES                   
         ST    R1,SBADATE                                                       
         MVC   SBNDATES,=F'1'                                                   
         MVC   SBQAGY,QAGY                                                      
         MVC   SBQMED,QMED                                                      
         MVC   SBQCLT,QCLT                                                      
         MVC   SBQPRD,QPRD                                                      
         MVC   SBQBPRD,BPRD                                                     
         MVC   SBQBPRD2,BPRD2                                                   
         CLI   SVRCVEL+1,H3FQ      FIND SPOTS REQUEST?                          
         BNE   CS10                 NO                                          
         MVC   SBQPRD,FPRD          YES - USE ALTERNATE PRD                     
         MVC   SBQBPRD,FBPRD                                                    
         MVC   SBQBPRD2,FBPRD2                                                  
*                                                                               
CS10     MVC   SBQMKT,QMKT                                                      
         MVC   SBQSTA,QSTA                                                      
         MVC   SBBAGYMD,BAGYMD                                                  
         CLI   QSTA,C'0'           TEST CABLE                                   
         BL    *+10                                                             
         MVC   SBQCNET,QSTA+5       YES - FILTER ON NET (IF ANY)                
*                                                                               
         SR    R0,R0                                                            
         CLI   SVRCVEL+1,H3FQ      FIND SPOTS REQUEST?                          
         BNE   CS12                                                             
         IC    R0,FBEST                                                         
         STC   R0,SBQEST                                                        
         B     CS14                                                             
*                                                                               
CS12     IC    R0,BEST                                                          
         STC   R0,SBQEST                                                        
         CLI   BEST2,0                                                          
         BE    *+8                                                              
         IC    R0,BEST2                                                         
CS14     STC   R0,SBQESTND                                                      
*                                                                               
         LTR   R0,R0               IF REQUESTED EST =0,                         
         BNZ   *+12                                                             
         MVI   SBQEST,1                                                         
         MVI   SBQESTND,255        SET REQUEST TO 001/255                       
*                                                                               
         LR    R0,RA                                                            
         AHI   R0,SVCLTREC-TWAD                                                 
         ST    R0,SBACLTRC                                                      
*                                                                               
         XC    SBAGYREC,SBAGYREC                                                
         LA    RE,SBAGYREC                                                      
         AHI   RE,AGYPROF-AGYHDRD                                               
         MVC   0(16,RE),SVAPROF    MOVE AGYPROF FOR F...ING SPOTIO              
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   CS100                                                            
         CLI   QMED,C'N'           MEDIA N?                                     
         BNE   CS100                                                            
         OI    SBQCAN,SBQCBYM0     YES - READ MARKET ZERO BUYS                  
*                                                                               
CS100    GOTO1 VSPOTIO,DMCB,SBLOCK                                              
*                                                                               
CSX      J     EXIT                                                             
         EJECT                                                                  
IOHOOK   NTR1                                                                   
         CLI   SBMODE,SBPROCSP     PROCESS A BUY                                
         BNE   *+12                                                             
         BRAS  RE,PROCBUY                                                       
         B     IOHOOKX                                                          
*                                                                               
         CLI   SBMODE,SBPROCNV     PROCESS AN INVOICE                           
         BNE   *+12                                                             
         BRAS  RE,PROCNV                                                        
         B     IOHOOKX                                                          
*                                                                               
         CLI   SBMODE,SBPROCPR     PROCESS A  PRODUCT                           
         BNE   *+12                                                             
         BAS   RE,PROCPR                                                        
         B     IOHOOKX                                                          
*                                                                               
         CLI   SBMODE,SBPROCES     PROCESS AN ESTIMATE                          
         BNE   *+12                                                             
         BAS   RE,PROCES                                                        
         B     IOHOOKX                                                          
*                                                                               
         CLI   SBMODE,SBPROCCL     PROCESS A  CLIENT                            
         BNE   *+12                                                             
         BAS   RE,PROCCL                                                        
         B     IOHOOKX                                                          
*                                                                               
         CLI   SBMODE,SBPROCST     PROCESS A  STATION                           
         BNE   *+12                                                             
         BRAS  RE,PROCST                                                        
         B     IOHOOKX                                                          
*                                                                               
IOHOOKX  J     EXIT                                                             
         EJECT                                                                  
PROCCL   DS    0H                                                               
         CLI   SVRCVEL+1,H3FQ      SEARCH REQUEST?                              
         BE    IOHOOKX              YES - CLT DETAILS ALREADY SENT              
*                                                                               
         MVC   SVCNAME,SBCLTNM                                                  
         MVC   SVCPROF,SBCPROF                                                  
         MVC   SVCXTRA,SBCEXTRA                                                 
*                                                                               
         LA    R1,H03Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R1,10                                                            
         LA    R4,SVCNAME                                                       
         BRAS  RE,SENDD                                                         
         B     IOHOOKX                                                          
         EJECT                                                                  
PROCPR   DS    0H                  GET PRODUCT DATA                             
         CLI   SVRCVEL+1,H3FQ      SEARCH REQUEST?                              
         BE    IOHOOKX              YES - PRD DETAILS ALREADY SENT              
*                                                                               
         LA    R1,H03Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R1,11                                                            
         LA    R4,SVPNAME                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         CLI   BPRD2,0             TEST POL PIG                                 
         BE    IOHOOKX                                                          
         LA    R1,18                                                            
         LA    R4,SVPNAME2                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         B     IOHOOKX                                                          
*                                                                               
         EJECT                                                                  
PROCES   DS    0H                  DO ESTIMATE DATA                             
         CLI   SVRCVEL+1,H3FQ      SEARCH REQUEST?                              
         BE    PROCES10             YES - EST DETAILS ALREADY SENT              
*                                                                               
         LA    R1,H05Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R4,SBBEST                                                        
         LA    R1,1                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,SBESTNM                                                       
         LA    R1,2                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,SBESTSTB                                                      
         LA    R1,3                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,SBESTNDB                                                      
         LA    R1,4                                                             
         BRAS  RE,SENDD                                                         
* SEND DEMO NAMES                                                               
PROCES10 L     R6,SBAIO1                                                        
         USING ESTHDRD,R6                                                       
         XC    SVEDEMOS,SVEDEMOS                                                
         BRAS  RE,GDEMCAT          GET DEMO CATEGORIES                          
         BNE   PROCES20                                                         
* SAVE DEMO CATEGORIES IN TIA                                                   
         SR    RF,RF               INDEX INTO SAVED DEMO TABLE                  
         IC    RF,SBBEST                                                        
         BCTR  RF,0                                                             
         MHI   RF,L'EDEMLST                                                     
         L     RE,ATIA                                                          
         AR    RE,RF                                                            
         MVC   0(L'EDEMLST,RE),EDEMLST                                          
*                                                                               
         CLI   SVRCVEL+1,H3FQ      SEARCH REQUEST?                              
         BE    PROCES40             YES - EST DETAILS ALREADY SENT              
*                                                                               
         LA    R4,BLOCK                                                         
         LA    R5,7                SET LENGTH                                   
*                                                                               
PROCES16 LA    R1,5                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,7(R4)                                                         
         BCT   R7,PROCES16                                                      
*                                                                               
PROCES20 DS    0H                                                               
         CLI   EOWSDAY,0                                                        
         BE    PROCES24                                                         
         LA    R4,EOWSDAY          OUT OF WEEK START DAY NUMBER                 
         LA    R1,6                MAP CODE                                     
         BRAS  RE,SENDD                                                         
*                                                                               
PROCES24 DS    0H                                                               
         CLI   ERATE,0                                                          
         BE    PROCES30                                                         
         LA    R4,ERATE            ESTIMATE RATE TYPE                           
         LA    R1,7                                                             
         BRAS  RE,SENDD                                                         
         SPACE 1                                                                
*=================================================================*             
* CHECK FOR I2 COMMENT DATA                                                     
*=================================================================*             
         SPACE 1                                                                
X        USING XCOMRECD,KEY                                                     
*                                                                               
PROCES30 XC    KEY,KEY                                                          
         MVC   X.COMI2K,=X'0D0C'                                                
         MVC   X.COMI2KAM,BAGYMD                                                
         MVI   X.COMI2KTY,C'I'                                                  
         MVC   X.COMI2KCL,BCLT                                                  
         MVC   X.COMI2KPR,QPRD                                                  
         MVC   X.COMI2KES,BEST                                                  
         MVC   X.COMI2KP2,QPRD2                                                 
         CLC   BEST,BEST2                                                       
         BE    *+10                                                             
         MVC   X.COMI2KE2,BEST2                                                 
         MVC   X.COMI2KST,BSTA                                                  
         MVC   X.COMI2KYM,BMOS                                                  
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   PROCES40                                                         
         DROP  X                                                                
*                                                                               
         L     R8,AIO2                                                          
         ST    R8,AIO                                                           
         USING XCOMRECD,R8                                                      
*                                                                               
         MVI   XSP,C'Y'                                                         
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,XCOMEL                                                        
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,X'15'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   PROCES40                                                         
*                                                                               
         LA    R1,H03Q                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
PROCES32 SR    R5,R5                                                            
         IC    R5,1(R6)                                                         
         AHI   R5,-2               GIVES COMMENT LENGTH                         
         LA    R1,17                                                            
         LA    R4,2(R6)            POINT TO COMMENT DATA                        
         BRAS  RE,SENDD                                                         
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    PROCES32                                                         
*                                                                               
PROCES40 MVI   XSP,C'N'                                                         
         B     IOHOOKX                                                          
         DROP  R6                                                               
         EJECT                                                                  
PROCBUY  NTR1  BASE=*,LABEL=*                                                   
         L     R8,SBAIO1                                                        
         USING BUYRECD,R8                                                       
         TM    BUYRCNTL,BUYRLN2    2-BYTE BUYLINE?                              
         BNZ   PB00                YES - NO NEED TO CONVERT IT                  
         LLC   R0,10(R8)           GET 1-BYTE LINE NUMBER                       
         STCM  R0,3,10(R8)         AND SET AS 2-BYTE LINE NUMBER                
*                                                                               
* TEST EXCLUDE THIS BUY FROM MATCH                                              
PB00     MVI   ELCDLO,CMCODEQ      X'66'                                        
         MVI   ELCDHI,CMCODEQ                                                   
         LA    R6,BDELEM                                                        
*                                                                               
PBA      BRAS  RE,NEXTEL                                                        
         BNE   PB1                                                              
         USING COMELEM,R6                                                       
         CLC   =C'MATCH=NO',CMDATA                                              
         BE    PBX                                                              
         B     PBA                                                              
         DROP  R6                                                               
*                                                                               
* TEST MASTER PRODUCT(S) SIMILAR                                                
PB1      CLI   SVCPROF+0,C'0'      TEST TRUE POL CLIENT                         
         BE    PB2                                                              
         CLI   BPRD,X'FF'          IF REQ FOR POL, IGNORE PRDS                  
         BE    PB2                                                              
         CLC   BPRD,BDMASPRD       ELSE MATCH MASPRDS                           
         BNE   PBX                                                              
         CLC   BPRD2,BDMASPRD+1                                                 
         BNE   PBX                                                              
* SET BUY ELEMENT CODES                                                         
PB2      MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         MVC   BUYELS,ELCDLO       SAVE BUY ELEMENT CODES                       
* SEE IF ANY SPOTS IN REQ MONTH                                                 
         LA    R6,BDELEM                                                        
PB3A     BRAS  RE,NEXTEL                                                        
         BNE   PB4                                                              
         USING REGELEM,R6                                                       
         CLC   RDATE,BPER          TEST PRIOR TO MOS START                      
         BL    PB3A                                                             
         CLC   RDATE,BPER+2        TEST AFTER MOS END                           
         BNH   PB5                                                              
         B     PB3A                                                             
* SEE IF REQUEST PERIOD IS IN BUY DESCRIPTION PERIOD                            
PB4      CLC   BDSTART,QENDB                                                    
         BH    PBX                                                              
         CLC   BDEND,QSTARTB                                                    
         BL    PBX                                                              
*                                                                               
* AT LEAST SOME OVERLAP - ADD TSAR BUY SEQUENCE RECORD                          
*                                                                               
PB5      CLI   SVRCVEL+1,H3FQ      SEARCH REQUEST?                              
         BNE   *+12                 NO                                          
         BRAS  RE,FILTBUY                                                       
         BNE   PBX                 REJECT THIS LINE                             
*                                                                               
         LH    R0,SVBUYCNT                                                      
         AHI   R0,1                                                             
         STH   R0,SVBUYCNT                                                      
*                                                                               
         MVI   CBLNET,0                                                         
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    PB6                                                              
         CLI   BUYMSTA+2,X'E8'       TEST CABLE                                 
         BL    PB6                                                              
         MVC   CBLNET,BUYMSTA+4    SAVE CABLE NETWORK (ONE BYTE)                
         NI    CBLNET,X'7F'                                                     
*                                                                               
PB6      XC    BLOCK,BLOCK         CLEAR OUTPUT AREA                            
         LA    R4,BLOCK                                                         
*                                                                               
         SR    R0,R0                                                            
         LH    R0,SVBUYCNT                                                      
         BRAS  RE,SETNUM                                                        
*                                                                               
         LA    R1,BUYKPRD                                                       
         BRAS  RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)                                                    
         AHI   R4,2                POINT TO END                                 
         BRAS  RE,SETDLM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVI   BUYPRD2,0           CLEAR PIGGYBACK PARTNER                      
         MVI   0(R4),SEMICOL                                                    
         MVI   1(R4),SEMICOL                                                    
         AHI   R4,2                                                             
         B     PB8                                                              
*                                                                               
PB8      SR    R0,R0                                                            
         ICM   R0,3,BUYKBUY                                                     
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDDAY            CONVERT DAY BITS TO DECIMAL !                
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         CR    R0,R1               TEST OUT-OF-WEEK ROT                         
         BNH   *+8                 NO                                           
         AHI   R1,7                CALC DAYS TO END OF ROT                      
         SR    R1,R0                                                            
         LR    R0,R1                                                            
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BDTIMST                                                     
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BDTIMST+2                                                   
         BRAS  RE,SETNUM                                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(3,BDSTART),(X'20',(R4))                            
         AHI   R4,5                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         GOTO1 (RF),(R1),(3,BDEND),(X'20',(R4))                                 
         AHI   R4,5                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         BRAS  RE,SETNUM                                                        
*                                                                               
         ICM   R0,3,BUYMSTA        SAVE MARKET                                  
         CLI   QMED,C'N'                                                        
         BNE   PB10                                                             
         OC    BUYMSTA(2),BUYMSTA                                               
         BNZ   PB10                                                             
         MVI   BUYMSTA+1,1                                                      
*                                                                               
PB10     MVC   HALF,BDNTAX         SAVE OFF TAX                                 
         XC    BDNTAX,BDNTAX        AND HAVE GETRATE IGNORE IT                  
*                                                                               
         MVC   BYTE,BDCIND         SAVE RATE TYPE                               
         LR    RF,RA                                                            
         AHI   RF,(SVCLTREC-TWAD)+(COPT1-CLTHDRD)  RF=A(COPT1)                  
         TM    0(RF),COP1GMI       CHILD SPOT?                                  
         BZ    *+8                  NO                                          
         MVI   BDCIND,BDCGROSQ      YES - SEND COST AS GROSS                    
*                                                                               
         GOTO1 VGETRATE,DMCB,SPOTS,SBAIO1,0                                     
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+18                NO                                           
         TM    BDCIND,X'10'        TEST RATE TYPE = NET                         
         BZ    *+10                NO                                           
         MVC   GROSS,NET           YES - SET GROSS = NET                        
*                                                                               
         MVC   BDNTAX,HALF                                                      
         MVC   BDCIND,BYTE                                                      
         STCM  R0,3,BUYMSTA                                                     
*                                                                               
*                  *** NOTE *** NOTE *** NOTE ***                               
*                                                                               
* THE CODE FROM HERE TO PB16 SEEMS WRONG BUT I'M AFRAID TO CHANGE IT.           
* AS BEST I CAN TELL, GETRATE ALWAYS RETURNS COSTS IN PENNIES FOR               
* ANYTHING OTHER THAN CANADIAN NETWORK (AND EVEN THEN I THINK IT IS             
* STILL IN PENNIES)                                                             
*                                                                               
* THE ONLY POSSIBLE REASON FOR THIS CODE TO LIVE IS THAT THERE IS CODE          
* IN GETRATE TO DEAL WITH MARKET 0 FOR CAN NTWK WHICH WILL NEVER                
* EXECUTE BECAUSE WE CHANGED THE MKT TO 1 A FEW INSTRUCTIONS ABOVE.             
*                                                                               
* GOOD LUCK IF YOU EVER DECIDE TO TRY TO FIX THIS MESS (EJOR 25OCT12)           
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   PB16                                                             
         CLI   QMED,C'N'           FOR NETWORK - DEFAULT IS DOLLARS             
**NOP    BNE   PB12                UNLESS THE INDICATOR SAYS NOT                
         BNE   PB16                                                             
*                                                                               
         OC    BUYKMKTN,BUYKMKTN   EXCEPT FOR LOCAL MARKET EXPLODED BUY         
         BNZ   PB12                ARE IN PENNIES UNLESS INDICATOR SAYS         
*                                                                   NOT         
         TM    BDCIND2,BDCRATPQ                                                 
         BNZ   PB16                                                             
         B     PB14                                                             
*                                                                               
PB12     TM    BDCIND2,BDCNBRDQ    FOR OTHER MEDIA DEFAULT IS PENNIES           
         BZ    PB16                UNLESS THE INDICATOR SAYS NOT                
*                                                                               
PB14     BRAS  RE,FIXCOST                                                       
*                                                                               
PB16     L     R0,GROSS                                                         
         MVC   EDSAVE,EDSAVEM      GROSS & NET CAN BE MINUS                     
         BRAS  RE,SETNUM                                                        
*                                                                               
         L     R0,NET                                                           
         BRAS  RE,SETNUM                                                        
         MVC   EDSAVE,EDSAVEP                                                   
*                                                                               
         BAS   RE,SETRATYP                                                      
*                                                                               
         MVC   0(17,R4),BDPROGRM                                                
         LR    RE,R4                                                            
         LHI   RF,17                                                            
*                                                                               
         CLI   0(RE),SEMICOL       AND MAKE SURE IT DOESN'T HAVE ANY            
         BNE   *+8                                                              
         MVI   0(RE),C' '          DAMN SEMICOLONS!                             
         AHI   RE,1                SFI!                                         
         BCT   RF,*-16                                                          
*                                                                               
         AHI   R4,16                                                            
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVI   ELCDLO,X'66'        SET COMMENTS FLAG                            
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         MVI   0(R4),C'Y'                                                       
         BRAS  RE,SETDLM                                                        
*                                                                               
         OC    BDREP,BDREP                                                      
         BZ    PB18A                                                            
         GOTO1 VCALLOV,DMCB,,X'D9000ABC'    GET A(RCPACK)                       
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(C'U',BDREP),0(R4)                                     
PB18A    AHI   R4,3                POINT TO END                                 
         BRAS  RE,SETDLM                                                        
*                                                                               
         BRAS  RE,SETADJ                                                        
*                                                                               
         MVC   0(1,R4),BDWKIND                                                  
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   0(1,R4),BDDAYPT                                                  
         BRAS  RE,SETDLM                                                        
*                                                                               
         CLI   CBLNET,0            TEST CABLE                                   
         BNE   PB18B                                                            
         MVI   0(R4),SEMICOL                                                    
         AHI   R4,1                                                             
         B     PB19                                                             
*                                                                               
PB18B    LA    R1,BUYMSTA                                                       
         BRAS  RE,STAUNPK                                                       
         MVC   0(3,R4),WORK+5                                                   
         AHI   R4,2                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
PB19     MVC   0(2,R4),BDMGDATE                                                 
         AHI   R4,2                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         TM    BDSTAT2,X'80'       TEST DAILY SKED                              
         BZ    *+8                                                              
         MVI   0(R4),C'Y'                                                       
         AHI   R4,1                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVI   ELCDLO,X'70'        SEND BUY ID (CONTRACT NUMBER)                
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+14                                                             
         MVC   0(12,R4),3(R6)                                                   
         AHI   R4,11                                                            
         BRAS  RE,SETDLM                                                        
*                                                                               
         SR    R0,R0               IF COS2 ON CLT REC=Y/O/T, SEND IT            
         CLI   SVCOST2,C'Y'                                                     
         BE    PB19A                                                            
         CLI   SVCOST2,C'O'                                                     
         BE    PB19A                                                            
         CLI   SVCOST2,C'T'                                                     
         BNE   PB19B                                                            
*                                                                               
PB19A    MVC   HALF,BDNTAX         SAVE OFF TAX                                 
         XC    BDNTAX,BDNTAX        AND HAVE GETRATE IGNORE IT                  
*                                                                               
         MVC   SPOTS(4),=C'COS2'                                                
         GOTO1 VGETRATE,DMCB,SPOTS,SBAIO1,0                                     
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+18                NO                                           
         TM    BDCIND,X'10'        TEST RATE TYPE = NET                         
         BZ    *+10                NO                                           
         MVC   GROSS,NET           YES - SET GROSS = NET                        
*                                                                               
         MVC   BDNTAX,HALF                                                      
         L     R0,GROSS                                                         
         L     R1,NET                                                           
*                                                                               
PB19B    LTR   R0,R0                                                            
         BNZ   PB19C                                                            
         BRAS  RE,SETDLM                                                        
         BRAS  RE,SETDLM                                                        
         B     PB19D                                                            
*                                                                               
PB19C    MVC   EDSAVE,EDSAVEM      GROSS & NET CAN BE MINUS                     
         BRAS  RE,SETNUM                                                        
         LR    R0,R1               SEND NET COST 2                              
         BRAS  RE,SETNUM                                                        
         MVC   EDSAVE,EDSAVEP                                                   
*                                                                               
* NOW CALL GETRATE TO GET TAX AMOUNTS                                           
PB19D    XC    SPOTS,SPOTS                                                      
         GOTO1 VGETRATE,DMCB,SPOTS,SBAIO1,0                                     
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+18                NO                                           
         TM    BDCIND,X'10'        TEST RATE TYPE = NET                         
         BZ    *+10                NO                                           
         MVC   GROSS,NET           YES - SET GROSS = NET                        
*                                                                               
         ICM   R0,15,TAX                                                        
         BNZ   *+12                                                             
         BRAS  RE,SETDLM                                                        
         B     PB19E                                                            
*                                                                               
         MVC   EDSAVE,EDSAVEM                                                   
         BRAS  RE,SETNUM                                                        
         MVC   EDSAVE,EDSAVEP                                                   
*                                                                               
PB19E    MVI   ELCDLO,X'70'        BUY ID OR PURPOSE CODE                       
         MVI   ELCDHI,X'70'        BUY ID OR PURPOSE CODE                       
         LA    R6,BDELEM           A(FIRST BUY RECORD ELEMENT)                  
         BRAS  RE,NEXTEL           DO WE HAVE A X'70' ELEMENT?                  
         BNE   PB19F               NO, SEND NULL FIELD                          
*                                                                               
         MVI   0(R4),C'B'          FLAG AS BUY-ID                               
         CLI   PROFB0+9,C'Y'       "REQUIRE PURPOSE CODES?" = Y?                
         BE    *+12                YES                                          
         CLI   PROFB0+9,C'O'       "REQUIRE PURPOSE CODES?" = O?                
         BNE   PB19E1              NO, THIS IS A BUY-ID                         
         LR    RE,R6               A(X'70') ELEMENT                             
         LA    R6,KEY              R6 = KEY                                     
         XC    KEY,KEY             CLEAR THE KEY                                
         USING PRPRECD,R6          PURPOSE CODE RECORD DSECT                    
         MVI   PRPKTYP,PRPKTYPQ    X'0D'                                        
         MVI   PRPKSUB,PRPKSUBQ    X'19'                                        
         MVC   PRPKAGY,QAGY        AGENCY                                       
         MVC   PRPKMED,QMED        MEDIA                                        
         USING IDELEM,RE           ID ELEMENT DSECT                             
         MVC   PRPCODE,IDCONNO     PURPOSE CODE OR BUY-ID                       
         OC    PRPCODE,SPACES      SPACE PAD                                    
         DROP  R6,RE               DROP USINGS                                  
*                                                                               
         GOTO1 HIGH                READ HI (SPOTIO WILL RESET READ SEQ)         
*                                                                               
         CLC   KEY(10),KEYSAVE     DOES PURPOSE CODE EXIST?                     
         BNE   *+8                 NO - LEAVE FLAG AS BUY-ID                    
         MVI   0(R4),C'P'          YES - FLAG AS PURPOSE CODE                   
PB19E1   AHI   R4,1                BUMP R4 BY 1                                 
*                                                                               
PB19F    BRAS  RE,SETDLM           SET FLAG & DELIMITER                         
*                                                                               
         LA    R1,H06Q             LOCATE BUYDATA HEADER                        
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R1,H06BDATA         GET BUYLINE DATA ELEMENT                     
         LA    R5,BLOCK                                                         
         SR    R5,R4               GIVES OUTPUT DATA LENGTH                     
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         EJECT                                                                  
*================================================================*              
* DEMOS                                                          *              
*================================================================*              
         SPACE 1                                                                
         SR    RF,RF               INDEX INTO SAVED DEMO TABLE                  
         IC    RF,BUYKEST                                                       
         BCTR  RF,0                                                             
         MHI   RF,L'EDEMLST                                                     
         L     RE,ATIA                                                          
         AR    RE,RF                                                            
         MVC   SVEDEMOS,0(RE)                                                   
         OC    SVEDEMOS(3),SVEDEMOS TEST NO DEMOS                               
         BZ    PB30                                                             
*                                                                               
         LA    R6,BDELEM           COUNT DEMOS IN DEMO EL                       
         SR    R0,R0               AND NEVER SEND HABES MORE                    
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R6,DEMELADR                                                      
         IC    R0,1(R6)            GET LENGTH                                   
         AHI   R0,-24                                                           
         SRL   R0,3                SET FOR NUMBER OF DEMOS PRESENT              
         LTR   R0,R0                                                            
         BNP   PB30                                                             
         STC   R0,DEMELADR         SAVE NUMBER OF DEMOS                         
*                                                                               
         LR    R6,R0               SET BFCT REGISTER                            
         LA    R7,SVEDEMOS                                                      
*                                                                               
PB20     BAS   RE,GETDEM                                                        
         CLI   DUB+5,C'U'          TEST DEMO VALUE IS USER DEFINED              
         BNE   PB22                                                             
         LA    R1,29               SET USER DEMO FLAG                           
         LA    R4,DUB+5                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
PB22     LA    R1,2                SET DEMO DATA CODE                           
         LA    R4,DUB                                                           
         LA    R5,4                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
         CLI   DUB+4,100           TEST SVI VALUE = 100                         
         BE    PB24                                                             
*                                                                               
         LA    R1,3                SEND SVI DATA CODE                           
         LA    R4,DUB+4                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
PB24     LA    R7,3(R7)                                                         
         OC    0(3,R7),0(R7)                                                    
         BZ    *+8                                                              
         BCT   R6,PB20             DO ONLY FOR DEMO EL COUNT                    
*=================================================================*             
* SEND ORBITS                                                     *             
*=================================================================*             
         SPACE 1                                                                
         MVI   ELCDLO,X'67'        GET ORBIT ELEM                               
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   PB30                                                             
         LHI   R1,H07Q                                                          
         BRAS  RE,SENDH                                                         
         XR    R5,R5               CLEAR OVERRIDE LENGTH                        
*                                                                               
         USING ORBELEM,R6                                                       
         XR    R7,R7                                                            
         IC    R7,ORBLEN           GET NUMBER OF SHOWS IN ORB                   
         AHI   R7,-4                                                            
         SRL   R7,4                                                             
         LA    R6,ORBDAY                                                        
         USING ORBDAY,R6                                                        
*                                                                               
PB26     LHI   R1,1                                                             
         LA    R4,ORBDAY                                                        
         BRAS  RE,SENDD                                                         
*                                                                               
         LHI   R1,2                                                             
         LA    R4,ORBTIME                                                       
         BRAS  RE,SENDD                                                         
         LHI   R1,3                                                             
         LA    R4,ORBTIME+2                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LHI   R1,4                                                             
         LA    R4,ORBDESC                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,8,ORBDEM         GET OVERRIDE & 2 DEC BITS IN HOB             
         ICM   R0,3,ORBDEM                                                      
         N     R0,=X'C0003FFF'                                                  
         ST    R0,DUB                                                           
         BRAS  RE,ADJPREC                                                       
*                                                                               
         LA    R4,DUB                                                           
         LHI   R1,5                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R6,16(R6)                                                        
         BCT   R7,PB26                                                          
*=================================================================*             
* SEND COMMENTS IF HABS WANTS THEM                                *             
*=================================================================*             
         SPACE 1                                                                
PB30     TM    FLAGS,FLSNDCMT      SEND COMMENTS?                               
         BZ    PB40                                                             
         MVI   ELCDLO,X'66'        SET COMMENTS FLAG                            
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   PB40                                                             
         LHI   R1,H0AQ                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
* MAKE SURE IF THERE'S A COMMENT 'MISSING' WE SEND A PLACEHOLDER                
         XR    R5,R5                                                            
         LHI   R7,1                COMMENT LINE NUMBER                          
*                                                                               
PB32     LHI   R1,1                FAMAP ELEM CODE                              
         CLM   R7,1,2(R6)          SAME COMMENT NUMBER?                         
         BE    PB34                 YES - SEND COMMENT                          
         XR    R5,R5                NO - JUST SEND PLACE HOLDER                 
         BRAS  RE,SENDD                                                         
         AHI   R7,1                NEXT COMMENT LINE NUMBER                     
         B     PB32                                                             
*                                                                               
PB34     IC    R5,1(R6)            GET COMMENT LENGTH                           
         AHI   R5,-4               ONE EXTRA FOR EX                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    3(0,R6),SPACES      MAKE SURE NO BINARY 0'S                      
         AHI   R5,1                                                             
         LA    R4,3(R6)            R4=A(COMMENT DATA)                           
         BRAS  RE,SENDD            SEND IT                                      
         AHI   R7,1                NEXT COMMENT LINE NUMBER                     
         BRAS  RE,NEXTEL                                                        
         BE    PB32                                                             
         SPACE 1                                                                
*=================================================================*             
* NOW BUILD TSAR RECORDS OF SPOTS                                 *             
*=================================================================*             
         SPACE 1                                                                
PB40     CLI   BUYKPRD,X'FF'       TEST POL BUY                                 
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         LA    R6,BDELEM                                                        
         MVC   ELCDLO(2),BUYELS                                                 
         XC    SPOTDATE,SPOTDATE   CLEAR                                        
*                                                                               
PB50     BRAS  RE,NEXTEL                                                        
         BNE   PBX                                                              
*                                                                               
         USING REGELEM,R6                                                       
         CLC   RDATE,BPERDEF       TEST PRIOR TO MOS START DEFERRED             
         BL    PB50                                                             
         CLC   RDATE,BPER+2        TEST AFTER MOS END                           
         BH    PBX                                                              
*                                                                               
         CLC   RDATE,SPOTDATE      TEST SAME DATE AS PREVIOUS                   
         BE    *+10                                                             
         XC    SPOTNUM,SPOTNUM                                                  
         MVC   SPOTDATE,RDATE      SAVE SPOT DATE                               
*                                                                               
         TM    RSTATUS,X'80'       TEST MINUS                                   
         BNZ   PB50                YES - SKIP                                   
*                                                                               
         LH    RE,SPOTNUM          BUMP SPOTNUM THIS DATE                       
         AHI   RE,1                                                             
         STH   RE,SPOTNUM                                                       
*                                                                               
         CLC   RDATE,BPER          TEST PRIOR TO MOS START                      
         BNL   PB60                 NO - IT'S NOT A DEFERRED SPOT               
*                                                                               
* IF THIS IS AN OOW, AND A SPOT IN THE LAST WEEK HAS AN AFFID IN THIS           
* MONTH, SEND IT                                                                
*                                                                               
         SR    R0,R0               TEST OOW ROTATOR                             
         IC    R0,BDSEDAY                                                       
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         CR    R0,R1               TEST OUT-OF-WEEK ROT                         
         BNH   PB50                 NO - SKIP IT                                
*                                                                               
         XR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AR    RF,R6                                                            
         CLI   0(RF),X'10'         AFFID ELEMENT?                               
         BNE   PB60                 NO - ASSUME DEFERRED IF NOT MATCHED         
*                                                                               
         USING AFFELEM,RF                                                       
         CLC   ADATE,BPER          IS AFFID DATE IN THIS MOS?                   
         BL    PB50                 NO - SKIP IT                                
         DROP  RF                                                               
*                                                                               
PB60     CLI   SVRCVEL+1,H3FQ      SEARCH SPOTS REQUEST?                        
         BNE   PB61                 NO                                          
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AR    RE,R6                                                            
         CLI   0(RE),X'10'         AFFID ELEMENT?                               
         BE    PB50                 YES - DON'T SEND MATCHED SPOTS              
*                                                                               
PB61     CLI   RLEN,10             TEST ALLOCATED                               
         BH    *+12                 YES - ALWAYS PROCESS                        
         CLI   PROFMK+11,C'Y'      SEND UNALLOCATED?                            
         BNE   PB50                 NO                                          
*                                                                               
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BE    PB64                YES - PROCESS ALL SPOTS                      
*                                                                               
         CLI   RLEN,14             TEST ONE PRODUCT IN SPOT                     
         BH    PB62                HIGH IS PIGGYBACK                            
* ONE PRODUCT IN SPOT                                                           
         CLI   BPRD2,0             TEST PIGGYBACK REQUEST                       
         BNE   PB50                YES - SKIP                                   
         CLC   BPRD,10(R6)         MATCH PRODUCT                                
         BNE   PB50                                                             
         B     PB64                                                             
* PIGGYBACK SPOT                                                                
PB62     CLI   BPRD2,0             TEST PIGGYBACK REQUEST                       
         BE    PB50                NO - SKIP                                    
         CLC   BPRD,10(R6)         MATCH PRD1                                   
         BNE   PB50                                                             
         CLC   BPRD2,14(R6)        MATCH PRD2                                   
         BNE   PB50                                                             
*                                                                               
PB64     XC    TSARREC,TSARREC     CLEAR OUTPUT AREA                            
         MVI   BTTYPE,C'S'                                                      
         MVC   BTEST,BUYKEST                                                    
         MVC   BTLIN,BUYRLIN                                                    
         MVC   BTCBLNET,CBLNET                                                  
         MVC   BTSEQ,SVBUYCNT      SET BUY SEQUENCE NUMBER                      
         MVC   BTDATE,RDATE                                                     
         MVC   BTSPNUM,SPOTNUM+1                                                
         TM    RSTATUS,X'40'       TEST MINUS SPOT                              
         BZ    *+8                                                              
         XI    BTSPNUM,X'FF'       COMPLEMENT THE SPOT NUMBER IF -              
         MVI   BTPRD,X'FF'         SET UNALLOCATED TO POL                       
         CLI   RLEN,10                                                          
         BNH   *+10                                                             
         MVC   BTPRD,10(R6)        PRODUCT 1                                    
         CLI   RLEN,14             TEST PIGGYBACK                               
         BNH   *+10                                                             
         MVC   BTPRD2,14(R6)       PRODUCT 2                                    
*                                                                               
**NOP    CLI   QMED,C'N'           DON'T SEND COST OVERRIDES FOR MED N          
**NOP    BE    PB66                                                             
         TM    6(R6),X'20'         TEST COST OVERRIDE                           
         BZ    PB66                NO                                           
         BRAS  RE,CHKCOV           CHECK COS2 ONLY OVERRIDE                     
         BNE   PB66                YES - IGNORE                                 
         OI    BTINDS,BTIND_COSTOV                                              
*                                                                               
         MVC   BYTE,BDPURP         SAVE OFF VALUE                               
         OC    BUYKMKT,BUYKMKT     MKT 0 (NETWORK)?                             
         BNZ   *+16                 NO                                          
         TM    BDSTAT3,BDST3_CNNEW NETWORK COST OVERRIDES ALLOWED?              
         BZ    *+8                  NO                                          
         MVI   BDPURP,X'FD'                                                     
*                                                                               
         MVC   HALF,BDNTAX         SAVE OFF TAX                                 
         XC    BDNTAX,BDNTAX        AND MAKE GETRATE IGNORE IT                  
         GOTO1 VGETRATE,DMCB,(X'FF',SPOTS),SBAIO1,(R6)                          
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+18                NO                                           
         TM    BDCIND,X'10'        TEST RATE TYPE = NET                         
         BZ    *+10                NO                                           
         MVC   GROSS,NET           YES - SET GROSS = NET                        
*                                                                               
         MVC   BTGROSS,GROSS                                                    
         MVC   BTNET,NET                                                        
         MVC   BDNTAX,HALF         RESTORE TAX & CALL GTRT FOR TAX              
         GOTO1 VGETRATE,DMCB,(X'FF',SPOTS),SBAIO1,(R6)                          
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+18                NO                                           
         TM    BDCIND,X'10'        TEST RATE TYPE = NET                         
         BZ    *+10                NO                                           
         MVC   GROSS,NET           YES - SET GROSS = NET                        
*                                                                               
         MVC   BTTAX,TAX                                                        
         MVC   BDPURP,BYTE                                                      
*                                                                               
PB66     CLI   SVRCVEL+1,H3FQ      SEARCH SPOTS REQUEST?                        
         BNE   PB70                 NO                                          
         OC    FILTCOST,FILTCOST                                                
         BZ    PB70                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,BDCOST         TEST MATCH ON BUYLINE COST                   
         TM    BTINDS,BTIND_COSTOV                                              
         BZ    *+8                                                              
         L     RE,GROSS            OR COST OVERRIDE IF PRESENT                  
         CLC   FILTCOST,=F'-1'     SPECIAL FLAG FOR $0                          
         BNE   *+14                                                             
         LTR   RE,RE                                                            
         BNZ   PB50                                                             
         B     PB70                                                             
*                                                                               
         C     RE,FILTCOST                                                      
         BNE   PB50                                                             
*                                                                               
PB70     OC    RPAY,RPAY                                                        
         BZ    *+14                                                             
         MVC   BTPAIDDT,RPAY                                                    
         OI    BTINDS,BTIND_PAID                                                
*                                                                               
         CLI   RCODE,X'0C'                                                      
         BNE   *+8                                                              
         OI    BTINDS,BTIND_OTO                                                 
*                                                                               
         TM    RSTATUS,X'40'       TEST MINUSSED                                
         BZ    *+8                                                              
         OI    BTINDS,BTIND_MISSED                                              
*                                                                               
         CLI   RLEN,10             TEST ALLOCATED                               
         BNH   PB90                 NO, CAN'T HAVE MAKEGOOD CODE                
         CLI   13(R6),0            TEST ANY MAKEGOOD CODE                       
         BE    PB90                NO                                           
         BAS   RE,TRANSCD                                                       
         MVC   BTMKGD,HALF                                                      
*                                                                               
PB90     MVI   TSACTN,TSAADD                                                    
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    PB50                NO ERROR                                     
         TM    TSERRS,TSEEOF+TSEDUP  OVERFLOW/DUPLICATE ERROR?                  
         BNZ   *+6                  YES - GIVE ERROR                            
         DCHO                       ELSE DIE                                    
         MVC   ERROR,=Y(BUFFOVER)                                               
         GOTO1 SENDMSG                                                          
         EJECT                                                                  
*                                                                               
PBX      J     EXIT                                                             
         EJECT                                                                  
*================================================================*              
* ON ENTRY R7 POINTS TO A 3 BYTE DEMO CODE                       *              
* FIND DEMO IN DEMO ELEMENT AND RETURN VALUE IN DUB  (4)         *              
*                                        SVI IN DUB+4(1)         *              
*                                        U(SER) DUB+5(1)         *              
*================================================================*              
         SPACE 1                                                                
GETDEM   NTR1                                                                   
         XC    DUB,DUB             CLEAR OUTPUT AREA                            
         MVI   DUB+4,100           SET SVI VALUE                                
*                                                                               
         L     R6,DEMELADR                                                      
         SR    R0,R0                                                            
         IC    R0,DEMELADR         GET NUMBER OF DEMOS                          
*                                                                               
         LA    R6,24(R6)                                                        
*                                                                               
GETDEM2  CLC   0(3,R7),0(R6)                                                    
         BE    GETDEM10                                                         
         LA    R6,8(R6)                                                         
         BCT   R0,GETDEM2                                                       
         B     GETDEMX                                                          
*                                                                               
GETDEM10 MVC   DUB(4),4(R6)                                                     
         NI    DUB,X'7F'           DROP OVERRIDE IND                            
         MVC   DUB+4(1),3(R6)      MOVE SVI VALUE                               
         CLI   1(R6),X'21'         TEST USER DEMO                               
         BNE   GETDEMX                                                          
         MVI   DUB+5,C'U'                                                       
*                                                                               
GETDEMX  BRAS  RE,ADJPREC                                                       
         J     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
*        TRANSLATE MAKEGOOD CODE FROM SPOT ELEMENT                              
*=================================================================*             
                                                                                
TRANSCD  LR    R0,RE                                                            
         LA    R5,BLOCK                                                         
         USING MGABLKD,R5                                                       
                                                                                
         XC    0(MGALNQ,R5),0(R5)                                               
                                                                                
         MVI   MGAACT,MGAQTRNS     ACTION TRANSLATE                             
         MVC   MGAACOM,ACOMFACS    SET A(COMFAS)                                
         MVC   MGAIO,SBAIO1        A(BUY RECORD)                                
         ST    R6,MGAELEM          A(SPOT ELEMENT)                              
                                                                                
         GOTO1 VBLDMGN,MGABLKD                                                  
                                                                                
         MVC   HALF,MGAENTRY+MGECODE-MGENTRYD                                   
         DROP  R5                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* BACK UP TO LAST NONBLANK AND INSERT SEMICOLON DELIMITER         *             
*=================================================================*             
         SPACE 1                                                                
SETDLM   CLI   0(R4),C' '                                                       
         JH    *+8                                                              
         BRCT  R4,SETDLM                                                        
         MVI   1(R4),SEMICOL                                                    
         AHI   R4,2                                                             
         BR    RE                                                               
         SPACE 1                                                                
*=================================================================*             
* TRANSMIT NUMERIC VALUE IN R0 AND INSERT SEMICOLON DELIMITER     *             
*=================================================================*             
         SPACE 1                                                                
*****    EDIT  (R0),(11,(R4)),ALIGN=LEFT                                        
SETNUM   DS    0H                                                               
         CVD   R0,DUB                                                           
         MVC   WORK(17),EDSAVE                                                  
         ED    WORK(17),DUB+2                                                   
         MVC   0(11,R4),WORK+17-(11)                                            
         LA    R0,11                                                            
         CLI   0(R4),C' '                                                       
         JNE   *+18                                                             
         MVC   0(11,R4),1(R4)                                                   
         MVI   11-1(R4),C' '                                                    
         BRCT  R0,*-18                                                          
         AR    R4,R0                                                            
         MVI   0(R4),SEMICOL                                                    
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         SPACE 1                                                                
*=================================================================*             
* CONVERT RATE TYPE IND TO CHARACTER                              *             
*=================================================================*             
         SPACE 1                                                                
SETRATYP MVI   BYTE,C'C'                                                        
         TM    BDCIND2,X'80'                                                    
         JO    SETRATX                                                          
         MVI   BYTE,C' '                                                        
         TM    BDCIND,X'20'                                                     
         JO    SETRATX                                                          
         MVI   BYTE,C'F'                                                        
         TM    BDCIND,X'80'                                                     
         JO    SETRATX                                                          
         MVI   BYTE,C'Q'                                                        
         TM    BDCIND,X'40'                                                     
         JO    SETRATX                                                          
         MVI   BYTE,C'N'                                                        
         TM    BDCIND,X'10'                                                     
         JO    SETRATX                                                          
         MVI   BYTE,C'V'                                                        
         TM    BDCIND,X'08'                                                     
         JO    SETRATX                                                          
         MVI   BYTE,C'S'                                                        
         TM    BDCIND,X'04'                                                     
         JO    SETRATX                                                          
         MVI   BYTE,C'X'                                                        
         TM    BDCIND,X'02'                                                     
         JO    SETRATX                                                          
         MVI   BYTE,C'P'                                                        
SETRATX  MVC   0(1,R4),BYTE                                                     
         J     SETDLM                                                           
         SPACE 1                                                                
*================================================================*              
* SET ADJACENCY CODE                                             *              
*================================================================*              
         SPACE 1                                                                
SETADJ   XC    HALF,HALF           SFI!                                         
         CLI   SVCPROF+9,C'0'                                                   
         JE    SETADJX                                                          
         MVC   HALF(1),BDPROGT                                                  
         MVI   HALF+1,C' '                                                      
         CLI   SVCPROF+9,C'1'      TEST ALPHA ADJ                               
         JE    SETADJX             YES                                          
         SR    R0,R0                                                            
         IC    R0,BDPROGT                                                       
         SRDL  R0,4                                                             
         STC   R0,HALF                                                          
         OI    HALF,X'F0'                                                       
         SRL   R1,28                                                            
         STC   R1,HALF+1                                                        
         OI    HALF+1,X'F0'                                                     
SETADJX  MVC   0(2,R4),HALF                                                     
         AHI   R4,1                                                             
         J     SETDLM                                                           
         EJECT                                                                  
*=================================================================*             
* FIND PRODUCT CODE IN CLIENT RECORD                              *             
*=================================================================*             
         SPACE 1                                                                
*                                                                               
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
         L     RF,SBACLTRC                                                      
         AHI   RF,CLIST-CLTHDR                                                  
*                                                                               
GETPRD2  CLC   0(1,R1),3(RF)                                                    
         BER   RE                                                               
         AHI   RF,4                                                             
         CLI   0(RF),C'A'                                                       
         JNL   GETPRD2                                                          
*                                                                               
* IT ISN'T SAFE TO USE A LTORG HERE!                                            
         BRAS  RF,*+6              PUT A(BADPRD) IN RF                          
         DC    Y(BADPRD)                                                        
         MVC   ERROR,0(RF)                                                      
         GOTO1 SENDMSG                                                          
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
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
PROCNV   NTR1  BASE=*,LABEL=*                                                   
         L     R8,SBAIO1                                                        
         USING SNVKEYD,R8                                                       
*                                                                               
* ADD ENTRY TO CHKSUM TABLE                                                     
         USING CHKSUMD,RF                                                       
         LR    RF,RA                                                            
         AHI   RF,SVCHKSUM-TWAD                                                 
         LR    RE,RF                                                            
         AHI   RE,SVCHKL           RE = A(EOT)                                  
*                                                                               
PNVA     CLI   CHKSKEY,0           FIND NEXT AVAILABLE ENTRY                    
         BE    PNVC                                                             
         CLC   CHKSKEY,12(R8)      OR THIS ENTRY                                
         BE    PNVC                                                             
         LA    RF,CHKSUML(RF)                                                   
         B     PNVA                                                             
*                                                                               
PNVC     CR    RF,RE               MAKE SURE NO OVERFLOW                        
         BL    *+6                                                              
         DCHO                      TOO MANY INVOICES!                           
         MVC   CHKSKEY,12(R8)      SAVE INVOICE NUMBER                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,CHKSVAL        RE = CHECKSUM VALUE                          
         XR    R1,R1                                                            
         ICM   R1,3,SNVRLEN        GET RECLEN                                   
         BNZ   *+6                                                              
         DCHO                                                                   
         LR    R0,R8               R0 = AREC                                    
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
*                                                                               
         LA    R6,SNVELS                                                        
         B     PNV4                                                             
*                                                                               
PNV2     SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
PNV4     CLI   0(R6),SNVHDELQ      TEST HEADER ELEM (X'10')                     
         BE    PNV20                                                            
         OC    INVSDATE,INVSDATE   TEST HEADER PASSED FILTERS                   
         BZ    PNVX                                                             
         CLI   0(R6),SNVEOELQ      EASI INVOICE ORIGIN (X'13')                  
         BE    PNV26                                                            
         CLI   0(R6),SNVCMELQ      CMML TRANSLATION ELEM (X'30')                
         BE    PNV30                                                            
         CLI   0(R6),SNVIDELQ      INVOICE DETAIL ELEM (X'40')                  
         BE    PNV40                                                            
         CLI   0(R6),SNVMMELQ      MATCHMAKER STATUS ELEM (X'E8')               
         BE    PNV60                                                            
         CLI   0(R6),0                                                          
         BE    PNVX                                                             
         B     PNV2                ELSE TRY NEXT                                
         EJECT                                                                  
*===============================================================*               
* PROCESS AN INVOICE HEADER                                     *               
*===============================================================*               
         SPACE 1                                                                
         USING SNVHDELD,R6                                                      
*                                                                               
PNV20    MVI   INVSW,C'N'          SET HEADER NOT SENT                          
         XC    INVSDATE,INVSDATE                                                
         XC    INVEOUID(INVEOQ),INVEOUID                                        
*                                                                               
         MVC   INVHDPRD,SNVHDPRD   SAVE HEADER PRDS/EST                         
         MVC   INVHDPR2,SNVHDPR2                                                
         MVC   INVHDEST,SNVHDEST                                                
         MVC   INVHDINV,SNVKINV                                                 
         MVC   INVHDEZS,SNVHDEZS                                                
         MVC   INVHDCON,SNVHDCON                                                
         MVC   INVHDREP,SNVHDREP                                                
*                                                                               
         TM    SNVHDCTL,SNVHDMCQ   SKIP MIDFLIGHT CLEARANCES                    
         BNZ   PNV100                                                           
*                                                                               
         CLI   BPRD,X'FF'          REQUEST PRODUCT POL                          
         BE    PNV22               YES - PROCESS                                
         CLI   INVHDPRD,0          TEST PRODUCT IN HEADER                       
         BE    PNV22               NO                                           
         CLC   INVHDPRD,BPRD       MATCH REQUESTED PRODUCT                      
         BNE   PNV100                                                           
         CLC   INVHDPR2,BPRD2      MATCH REQUESTED PRODUCT                      
         BNE   PNV100                                                           
*                                                                               
PNV22    SR    R0,R0                                                            
         ICM   R0,1,INVHDEST                                                    
         BZ    *+12                IF NO EST, SKIP THIS TEST                    
         BAS   RE,FLTREST          FILTER ON ESTIMATE                           
         BNE   PNV100              SKIP                                         
*                                                                               
         MVI   INVCOSTY,C'G'       SET FOR GROSS COSTS                          
         TM    SNVHDCTL,SNVHDNTQ+SNVHDDNQ   TEST INVOICE IS AT NET              
         BZ    *+8                                                              
         MVI   INVCOSTY,C'N'                                                    
         GOTO1 VDATCON,DMCB,(2,SNVHDSDT),INVSDATE SAVE YYMMDD STRT DT           
*                                                                               
         L     R4,AIO3             CLEAR FILM SAVE AREA                         
         LHI   R5,LENIO                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
*                                                                               
         B     PNV2                                                             
         EJECT                                                                  
*===============================================================*               
* PROCESS AN EASI INVOICE ORIGIN ELEMENT                        *               
*===============================================================*               
         SPACE 1                                                                
         USING SNVEOD,R6                                                        
PNV26    MVC   INVEOUID(INVEOQ),SNVEOUID   +SNVEOSTA +SNVEODAT                  
         B     PNV2                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*===============================================================*               
* PROCESS A COMMERCIAL SEQUENCE ELEMENT                         *               
*===============================================================*               
         SPACE 1                                                                
         USING SNVCMELD,R6                                                      
         USING FLMTABD,RE                                                       
PNV30    L     RE,AIO3             CMML CODE SAVE AREA                          
         LLC   RF,SNVCMICD                                                      
         BCTR  RF,0                ZERO RELATIVE                                
         MHI   RF,FLMTABLQ                                                      
         AR    RE,RF                                                            
         MVC   FLMCOD,SNVCMCD                                                   
         OC    FLMCOD,SPACES                                                    
         MVC   FLMSEQ,SNVCMSEQ                                                  
         MVC   FLMSTAT,SNVCMFLG                                                 
         OC    SNVCMSEQ,SNVCMSEQ   TEST INVALID FILM                            
         BNZ   *+8                  VALID                                       
         OI    FLMSTAT,FLMSINV     SET INVALID FILM FLAG                        
         B     PNV2                                                             
*                                                                               
         DROP  R6,RE                                                            
         EJECT                                                                  
*===============================================================*               
* PROCESS AN AFFIDAVIT DETAIL ELEMENT                           *               
*===============================================================*               
         SPACE 1                                                                
         USING SNVIDELD,R6                                                      
PNV40    DS    0H                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    PNV41               NFC (NO CABLE)                               
         CLI   BSTA,X'E8'          TEST CABLE?                                  
         BL    PNV41                NO                                          
         MVC   BYTE,BSTA+2                                                      
*                                                                               
         NI    BYTE,X'7F'          TURN OFF HOB                                 
         CLI   BYTE,0              SEE IF FOR SPECIFIC NETWORK                  
         BE    PNV41                                                            
         CLC   BYTE,SNVIDNWK       IS THIS FOR REQ'D NWK?                       
         BNE   PNV2                                                             
*                                                                               
PNV41    CLI   SNVIDPRD,0          TEST PRODUCT IN DETAIL                       
         BE    PNV42               NO - MUST HAVE MATCHED IN HDR                
*                                                                               
         CLI   BPRD,X'FF'          REQUESTED PRD POL                            
         BE    PNV42                                                            
         CLC   SNVIDPRD,BPRD       ELSE MATCH PRD NOW                           
         BNE   PNV2                                                             
         CLC   SNVIDPR2,BPRD2      MATCH 2ND PRD                                
         BNE   PNV2                                                             
*                                                                               
PNV42    CLI   SNVIDBES,0          IS THE AFFID MATCHED                         
         BE    PNV50               NO                                           
*                                                                               
         XC    TSARREC,TSARREC     FIND TSAR RECORD FOR THIS SPOT               
         MVI   BTTYPE,C'S'                                                      
         MVC   BTEST,SNVIDBES      ESTIMATE                                     
*                                                                               
PNV43    SR    R0,R0               TEST TO PROCESS THIS EST                     
         ICM   R0,1,BTEST                                                       
         BZ    PNV44               ALWAYS SHOW IF NO EST ON INV???              
         BAS   RE,FLTREST          FILTER ON ESTIMATE                           
         BNE   PNV2                SKIP                                         
*                                                                               
PNV44    BRAS  RE,SENDH0C                                                       
         MVC   BTLIN,SNVIDTBL      2-BYTE BUYLINE NUMBER                        
         OC    SNVIDTBL,SNVIDTBL   HAVE A 2-BYTE BUYLINE NUMBER?                
         BNZ   *+10                YES                                          
         MVC   BTLIN+1(1),SNVIDBLN NO - USE THE 1 BYTE BUYLINE NUMBER           
         MVC   BTCBLNET,SNVIDNWK   CABLE NETWORK                                
         MVC   BTDATE,SNVIDBDT     SPOT DATE                                    
         MVC   BTSPNUM,SNVIDBNO    SPOT NUMBER WITHIN DATE                      
         MVC   BTKEYSV,BTKEY       SAVE KEY OF READHI                           
*                                                                               
         MVI   TSACTN,TSARDH                                                    
         BRAS  RE,CALLTSAR                                                      
         BE    PNV45               NO ERROR                                     
         CLI   TSERRS,TSERNF       TREAT NOT FOUND AS OK                        
         BE    PNV45                                                            
         TM    TSERRS,TSEEOF+TSERNF  EOF OR NOT FOUND?                          
         BNZ   PNV45A                                                           
         DCHO                       ELSE DIE                                    
*                                                                               
PNV45    CLC   BTKEYSV(8),BTKEY    MATCH ALL BUT SEQNUM                         
         BE    PNV46               FOUND IT                                     
*                                                                               
PNV45A   MVC   BTKEY,BTKEYSV       RESTORE KEY                                  
         OC    BTCBLNET,BTCBLNET   WAS THIS FOR CABLE?                          
         BZ    PNV50                NO, IT'S JUST UNMATCHED                     
         MVI   BTCBLNET,0          SEE IF THERE'S A BUY W/OUT NET               
         MVC   BTKEYSV,BTKEY       SAVE KEY OF READHI                           
         BRAS  RE,CALLTSAR                                                      
         BE    PNV45               NO ERROR                                     
         CLI   TSERRS,TSERNF       TREAT NOT FOUND AS OK                        
         BE    PNV45               NO ERROR                                     
         TM    TSERRS,TSEEOF+TSERNF EOF OR NOT FOUND?                           
         BNZ   PNV50               NOT THERE - TREAT AS UNMATCHED               
         DC    H'0'                                                             
*                                                                               
* UPDATE SPOT WITH MATCH DATA                                                   
PNV46    MVC   DUB(6),INVSDATE                                                  
         SR    R0,R0                                                            
         ICM   R0,1,SNVIDDAY       GET DAYS FROM INV START                      
         BZ    PNV47                                                            
         GOTO1 VADDAY,DMCB,INVSDATE,DUB,(R0)                                    
*                                                                               
PNV47    GOTO1 VDATCON,DMCB,DUB,(2,BTAFFDT)                                     
*                                                                               
         SR    R0,R0                                                            
         XC    DUB,DUB                                                          
         ICM   R0,15,SNVIDDM4      HAVE THE NEW 4 BYTE DEMO?                    
         BNZ   PNV47AA             YES - USE THAT                               
         ICM   R0,3,SNVIDDEM                                                    
***      N     R0,=X'0000C000'                                                  
         N     R0,=X'00004000'     ONLY KEEP THE X'40' FLAG                     
         SLL   R0,16               SHIFT OVERRIDE & 2 DEC BITS                  
         ICM   R0,3,SNVIDDEM                                                    
***      N     R0,=X'C0003FFF'                                                  
         N     R0,=X'4000BFFF'     DEMO VALUE CAN BE UP TO 491.51               
PNV47AA  ST    R0,DUB                                                           
         BRAS  RE,ADJPREC                                                       
         MVC   BTAFDEMO,DUB                                                     
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SNVIDTIM                                                    
         BAS   RE,CNVIDTIM                                                      
         STCM  R0,3,BTAFFTM                                                     
         MVC   BTAFFSEQ,SNVIDSEQ                                                
         MVC   BTAFFORB,SNVIDBOR   MATCHED ORBIT LINE NUM                       
         MVC   BTAFFSLN,SNVIDSLN                                                
         MVC   BTAFFILM,SNVIDCML                                                
         MVC   BTAFFLM2,SNVIDCM2                                                
*                                                                               
         TM    SNVIDCTL,SNVIDMGQ   TEST AFFID IS A MAKEGOOD                     
         BZ    *+8                                                              
         OI    BTAFFIND,AFIND_MG   SET FLAG                                     
         TM    SNVIDCTL,SNVIDSIQ   SKIP INTERVAL CHECK                          
         BZ    *+8                                                              
         OI    BTAFFIND,AFIND_SIQ  SET FLAG                                     
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE ITEM                                
         BZ    *+8                                                              
         OI    BTAFFIND,AFIND_NEG  SET FLAG                                     
         TM    SNVIDCT2,SNVIDEMS   CHECK EMAIL SENT                             
         BZ    *+8                                                              
         OI    BTAFFIND,AFIND_EM   SET FLAG                                     
         TM    SNVIDCT2,SNVIDMMC   CHECK COST CHANGED                           
         BZ    *+8                                                              
         OI    BTAFFIND,AFIND_CH   SET FLAG                                     
         TM    SNVIDCTL,SNVIDICQ   CHECK IGNORE COST                            
         BZ    *+8                                                              
         OI    BTAFFIND,AFIND_IGC  SET FLAG                                     
         TM    SNVIDCTL,SNVIDITQ   CHECK IGNORE TIME                            
         BZ    *+8                                                              
         OI    BTAFFIND,AFIND_IGT  SET FLAG                                     
         TM    SNVIDCTL,SNVIDIFQ   CHECK IGNORE FILM                            
         BZ    *+8                                                              
         OI    BTAFFIND,AFIND_IGF  SET FLAG                                     
         TM    SNVIDCT2,SNVIDISQ   CHECK IGNORE SPOT LENGTH                     
         BZ    *+8                                                              
         OI    BTAFFIN2,AFIN2_IGS  SET FLAG                                     
*                                                                               
         L     RE,AIO3             CMML CODE SAVE AREA                          
         LR    R0,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,1,SNVIDCML                                                    
         BZ    PNV47A                                                           
         BCTR  RF,0                ZERO RELATIVE                                
         MHI   RF,FLMTABLQ                                                      
         AR    RE,RF                                                            
         MVC   BTTRFLM,FLMSEQ-FLMTABD(RE)                                       
*                                                                               
PNV47A   LR    RE,R0                                                            
         ICM   RF,1,SNVIDCM2                                                    
         BZ    PNV48                                                            
         BCTR  RF,0                ZERO RELATIVE                                
         MHI   RF,FLMTABLQ                                                      
         AR    RE,RF                                                            
         MVC   BTTRFLM2,FLMSEQ-FLMTABD(RE)                                      
*                                                                               
PNV48    MVC   BTINVSEQ,INVHDSEQ                                                
*                                                                               
         ICM   R1,15,SNVIDCST      SEND GROSS $                                 
         CLI   INVCOSTY,C'G'                                                    
         BE    PNV48A                                                           
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   PNV48AA             NO - GROSS UP NET$                           
         MVC   FULL(2),SNVKMOS     INVOICE MOS                                  
         XC    FULL(2),=X'FFFF'    XC MOS TO GET COMPRESSED DATE                
         CLC   FULL(2),=X'F021'    JAN/2020 OR HIGHER?                          
         BNL   PNV48A              YES - DO NOT GROSS UP!                       
* GROSS UP NET DOLLAROS                                                         
PNV48AA  M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
PNV48A   STCM  R1,15,BTAFGRS                                                    
*                                                                               
         MVC   BTAFEST,SNVIDEST    SEND AFFID EST                               
         CLI   SNVIDEST,0          TEST EST IN DETAIL                           
         BNE   *+10                YES                                          
         MVC   BTAFEST,INVHDEST                                                 
*                                                                               
         MVI   TSACTN,TSAWRT                                                    
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     PNV2                                                             
*                                                                               
FLTREST  CLI   BEST,0              TEST NO EST FILTER                           
         BER   RE                                                               
         CLM   R0,1,BEST                                                        
         BLR   RE                                                               
         CLM   R0,1,BEST2                                                       
         BHR   RE                                                               
FLTRESEQ CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
         EJECT                                                                  
*===============================================================*               
* ADD TSAR RECORD FOR UNMATCHED AFFIDAVIT                       *               
*  TEMP CODE - IF PROFI2A+14=Y, AGY DOESN'T MATCH ADDED VALUE   *               
*              AFFIDS, SO DON'T SEND THEM TO PC                 *               
*                                                               *               
*  AN ADDED VALUE AFFID IS ONE THAT HAS COST=0, AND IS LESS     *               
*  THAN 10 SECONDS, BUT NOT 5 OR 7 SECONDS.                     *               
*===============================================================*               
         SPACE 1                                                                
PNV50    DS    0H                                                               
         CLC   VERSION(2),=X'0303' IF VERSION >= 3.3, PC WILL DECIDE            
         BNL   PNV51                                                            
         CLI   PROFI2A+14,C'Y'     DON'T MATCH ADDED VALUE?                     
         BNE   PNV51                                                            
         OC    SNVIDCST,SNVIDCST   IS THE COST $0?                              
         BNZ   PNV51                NO - NOT ADDED VALUE SPOT                   
         CLI   SNVIDSLN,10         LESS THAN 10 SECONDS?                        
         BNL   PNV51                NO                                          
         CLI   SNVIDSLN,5           OR 5 SECONDS?                               
         BE    PNV51                 YES                                        
         CLI   SNVIDSLN,7           OR 7 SECONDS                                
         BNE   PNV2                  NO - IT'S ADDED VALUE!                     
*                                                                               
PNV51    XC    TSARREC,TSARREC                                                  
         MVI   AFTYPE,C'U'                                                      
         MVC   DUB,INVSDATE                                                     
         SR    R0,R0                                                            
         ICM   R0,1,SNVIDDAY       GET DAYS FROM INV START                      
         BZ    PNV52                                                            
         GOTO1 VADDAY,DMCB,INVSDATE,DUB,(R0)                                    
                                                                                
PNV52    GOTO1 VDATCON,DMCB,DUB,(2,AFDATE)                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SNVIDTIM                                                    
         BAS   RE,CNVIDTIM                                                      
         STCM  R0,3,AFTIME                                                      
*                                                                               
         MVC   AFSEQ,SNVIDSEQ                                                   
         MVC   AFSLN,SNVIDSLN                                                   
         MVC   AFAFFORB,SNVIDBOR   MATCHED ORBIT LINE NUM                       
         MVC   AFFILM,SNVIDCML                                                  
         MVC   AFFILM2,SNVIDCM2                                                 
*                                                                               
         TM    SNVIDCTL,SNVIDMGQ   TEST AFFID IS A MAKEGOOD                     
         BZ    *+8                                                              
         OI    AFINDS,AFIND_MG     SET FLAG                                     
         TM    SNVIDCTL,SNVIDSIQ   SKIP INTERVAL CHECK                          
         BZ    *+8                                                              
         OI    AFINDS,AFIND_SIQ    SET FLAG                                     
         TM    SNVIDCTL,SNVIDNGQ   NEGATIVE ITEM                                
         BZ    *+8                                                              
         OI    AFINDS,AFIND_NEG    SET FLAG                                     
         TM    SNVIDCT2,SNVIDEMS   CHECK EMAIL SENT                             
         BZ    *+8                                                              
         OI    AFINDS,AFIND_EM     SET FLAG                                     
         TM    SNVIDCT2,SNVIDMMC   CHECK COST CHANGED                           
         BZ    *+8                                                              
         OI    AFINDS,AFIND_CH     SET FLAG                                     
         TM    SNVIDCTL,SNVIDICQ   CHECK IGNORE COST                            
         BZ    *+8                                                              
         OI    AFINDS,AFIND_IGC    SET FLAG                                     
         TM    SNVIDCTL,SNVIDITQ   CHECK IGNORE TIME                            
         BZ    *+8                                                              
         OI    AFINDS,AFIND_IGT    SET FLAG                                     
         TM    SNVIDCTL,SNVIDIFQ   CHECK IGNORE FILM                            
         BZ    *+8                                                              
         OI    AFINDS,AFIND_IGF    SET FLAG                                     
         TM    SNVIDCT2,SNVIDISQ   CHECK IGNORE SPOT LENGTH                     
         BZ    *+8                                                              
         OI    AFIND2,AFIN2_IGS    SET FLAG                                     
*                                                                               
         MVC   AFPRD,SNVIDPRD                                                   
         MVC   AFPRD2,SNVIDPR2                                                  
*                                                                               
         CLI   SNVIDPRD,0          TEST PRD IN DETAIL                           
         BNE   PNV54               YES                                          
         MVC   AFPRD,INVHDPRD      ELSE USE SAVED HDR DATA                      
         MVC   AFPRD2,INVHDPR2                                                  
*                                                                               
PNV54    MVC   AFEST,SNVIDEST                                                   
         CLI   SNVIDEST,0          TEST EST IN DETAIL                           
         BNE   *+10                YES                                          
         MVC   AFEST,INVHDEST                                                   
*                                                                               
         SR    R0,R0               TEST TO PROCESS THIS EST                     
         ICM   R0,1,AFEST                                                       
         BZ    PNV56               ALWAYS SHOW IF NO EST ON INV???              
         BAS   RE,FLTREST          FILTER ON ESTIMATE                           
         BNE   PNV2                SKIP                                         
*                                                                               
PNV56    BRAS  RE,SENDH0C                                                       
         MVC   AFCBLNET,SNVIDNWK   CABLE NETWORK                                
         SR    R0,R0                                                            
         XC    DUB,DUB                                                          
         ICM   R0,15,SNVIDDM4      HAVE THE NEW 4 BYTE DEMO?                    
         BNZ   PNV57               YES - USE THAT                               
         ICM   R0,3,SNVIDDEM                                                    
***      N     R0,=X'0000C000'                                                  
         N     R0,=X'00004000'     ONLY KEEY THE X'40' FLAG                     
         SLL   R0,16               SHIFT OVERRIDE & 2 DEC BITS                  
         ICM   R0,3,SNVIDDEM                                                    
***      N     R0,=X'C0003FFF'                                                  
         N     R0,=X'4000BFFF'     DEMO VALUE CAN BE UP TO 491.51               
PNV57    ST    R0,DUB                                                           
         BRAS  RE,ADJPREC                                                       
         MVC   AFDEMO,DUB                                                       
*                                                                               
* 29MAR00 - ALWAYS SEND GROSS SPOT COST IN AFGROSS                              
         ICM   R1,15,SNVIDCST                                                   
         CLI   INVCOSTY,C'G'                                                    
         BE    PNV58                                                            
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   PNV58AA             NO - GROSS UP NET$                           
         MVC   FULL(2),SNVKMOS     INVOICE MOS                                  
         XC    FULL(2),=X'FFFF'    XC MOS TO GET COMPRESSED DATE                
         CLC   FULL(2),=X'F021'    JAN/2020 OR HIGHER?                          
         BNL   PNV58               YES - DO NOT GROSS UP!                       
* GROSS UP NET DOLLAROS                                                         
PNV58AA  M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
PNV58    ST    R1,AFGROSS                                                       
*                                                                               
         L     RE,AIO3             GET TRAFFIC CMML SEQ                         
         LR    R0,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,1,SNVIDCML                                                    
         BZ    PNV58A                                                           
         BCTR  RF,0                ZERO RELATIVE                                
         MHI   RF,FLMTABLQ                                                      
         AR    RE,RF                                                            
         MVC   AFTRFLM,FLMSEQ-FLMTABD(RE)                                       
*                                                                               
PNV58A   LR    RE,R0                                                            
         ICM   RF,1,SNVIDCM2                                                    
         BZ    PNV59                                                            
         BCTR  RF,0                ZERO RELATIVE                                
         MHI   RF,FLMTABLQ                                                      
         AR    RE,RF                                                            
         MVC   AFTRFLM2,FLMSEQ-FLMTABD(RE)                                      
*                                                                               
PNV59    MVC   AFINVSEQ,INVHDSEQ                                                
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         BRAS  RE,CALLTSAR                                                      
         BE    PNV2                NO TSAR ERROR                                
*                                                                               
         MVC   ERROR,=Y(DUPINV)    DUPLICATE INVOICE                            
         TM    TSERRS,TSEDUP                                                    
         BNZ   PNV59A                                                           
         MVC   ERROR,=Y(BUFFOVER)                                               
         TM    TSERRS,TSEEOF       OVERFLOW ERROR?                              
         BNZ   PNV59A               YES - GIVE ERROR                            
         DCHO                      DIE ON ANY OTHER TSAR ERROR                  
*                                                                               
PNV59A   GOTO1 SENDMSG                                                          
*                                                                               
         EJECT                                                                  
*===============================================================*               
* CONVERT AFFIDAVIT TIMES IN MINUTES TO MILITARY TIME                           
*===============================================================*               
         SPACE 1                                                                
CNVIDTIM SRDL  R0,32                                                            
         D     R0,=F'60'           CONVERT MINUTES FROM 6A TO HHMM              
         MHI   R1,100              HOURS X 100                                  
         AR    R0,R1               + MINUTES                                    
         AHI   R0,600              MILITARY TIME BASE IS 0, NOT 600             
         CHI   R0,2400                                                          
         BL    *+8                                                              
         AHI   R0,-2400                                                         
         BR    RE                                                               
         EJECT                                                                  
*===============================================================*               
* PROCESS A MATCHMAKER STATUS ELEMENT                           *               
*===============================================================*               
         USING SNVMMELD,R6                                                      
PNV60    CLI   SVSNVKEY,0          TEST SNVKEY SAVED YET                        
         BE    PNV80                NO, DON'T NEED MM STATUS                    
         CLC   SNVMMBK,SPACES                                                   
         BNH   PNV64                                                            
*                                                                               
         MVC   DUB(4),SNVMMBK      SEND BOOK AS MMM/YY                          
         MVC   DUB+4(2),=C'01'                                                  
         LA    R5,4                                                             
         LA    R4,DUB                                                           
         CLC   DUB(4),=C'LAT '                                                  
         BE    PNV63                                                            
         CLC   DUB(4),=C'ACT '                                                  
         BE    PNV63                                                            
         CLC   DUB(4),=C'ACT-'                                                  
         BNE   *+18                                                             
         AHI   R5,2                                                             
         MVC   DUB+4(2),=C'NO'                                                  
         B     PNV63                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(0,DUB),(6,WORK)                                    
         LA    R5,6                                                             
         LA    R4,WORK                                                          
*                                                                               
PNV63    LA    R1,2                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
PNV64    XC    WORK,WORK           SEND BILLER NAME                             
         LA    R4,WORK                                                          
         USING GETBUBLD,R4                                                      
*                                                                               
         MVC   GBCOMFAC,ACOMFACS                                                
         MVC   GBIOA,AIO2                                                       
         XC    WORK2,WORK2         DUMMY FLD HDR                                
         MVI   WORK2,X'1C'         SET LEN=28 (20 BYTES INP + HDR)              
         LA    R1,WORK2                                                         
         ST    R1,GBNAMFLD                                                      
         MVC   GBAGY,QAGY                                                       
         MVC   GBMEDEBC,QMED                                                    
         MVC   GBCLTEBC,QCLT                                                    
         MVC   GBOFFICE,SVOFFC                                                  
         MVC   GBAGYMD,BAGYMD                                                   
         MVC   GBCLT,BCLT                                                       
         MVC   GBPRD,BPRD                                                       
         MVC   GBEST,BEST                                                       
         MVC   GBMKT(5),BMKTSTA                                                 
         MVI   GBTYPE,C'I'         SET FOR BILLER NAME                          
         GOTO1 VGETBUBL,DMCB,(R4)                                               
*                                                                               
         CLI   GBERR,0                                                          
         BNE   PNV70               DON'T SEND ON ERROR                          
         LA    R1,3                                                             
         LA    R4,WORK2+8                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
PNV70    OC    I2DATE,I2DATE       I2 DATE/TIME ALREADY SET?                    
         BNZ   PNV80                YES                                         
         MVC   I2DATE,SNVMMDAT                                                  
         MVC   I2TIME,SNVMMTIM                                                  
         LHI   R1,8                                                             
         LA    R4,I2DATE                                                        
         BRAS  RE,SENDD                                                         
         GOTO1 VHEXOUT,DMCB,I2TIME,WORK,2,=C'TOG'                               
         LHI   R1,9                                                             
         LA    R4,WORK                                                          
         BRAS  RE,SENDD                                                         
*                                                                               
PNV80    B     PNV2                                                             
         DROP  R4                                                               
         EJECT                                                                  
*===============================================================*               
* END OF INVOICE RECORD PROCESSING                              *               
*===============================================================*               
PNV100   CLC   SNVKMINK,=6X'FF'    TEST LAST RECORD OF MINIO SET                
         BNE   PNVX                                                             
*                                                                               
PNVX     J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* READ SPOT DATA IN TSAR BUFFER AND SEND                          *             
*=================================================================*             
         SPACE 1                                                                
SENDSPTS NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO3             CLEAR FILM SAVE AREA                         
         LHI   R5,LENIO                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
*                                                                               
         L     R4,AIO2             AND BRAND SUBSTITUTION AREA                  
         LHI   R5,LENIO                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
*                                                                               
         MVI   TSACTN,TSAGET                                                    
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
*                                                                               
         BRAS  RE,CALLTSAR         CHECK FOR NO RECORDS                         
         TM    TSERRS,TSEEOF                                                    
         BO    SENDX                                                            
         MVI   TSACTN,TSANXT                                                    
         B     SEND14                                                           
*                                                                               
SEND12   BRAS  RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    SEND200                                                          
*                                                                               
SEND14   CLI   BTTYPE,C'S'         TEST SPOT RECORD                             
         BNE   SEND50              NO - MUST BE UNMATCHED AFFID                 
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         SR    R0,R0                                                            
         ICM   R0,3,BTSEQ                                                       
         BRAS  RE,SETNUM                                                        
*                                                                               
         OC    BTAFFDT,BTAFFDT     TEST MATCHED                                 
         BZ    *+8                 DEFAULT = NO                                 
         MVI   0(R4),C'Y'                                                       
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   0(2,R4),BTMKGD                                                   
         AHI   R4,1                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BTDATE),(X'20',DUB)                              
         MVC   0(4,R4),DUB+2       MOVE MMDD                                    
         MVC   4(2,R4),DUB         MOVE YY                                      
         AHI   R4,5                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BTSPNUM                                                       
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVC   FLMPRD,BTPRD                                                     
         LA    R1,BTPRD                                                         
         BRAS  RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)                                                    
         AHI   R4,2                POINT TO END                                 
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   FLMPIG,BTPRD2                                                    
         LA    R1,BTPRD2                                                        
         CLI   0(R1),0                                                          
         BE    SEND22                                                           
         BRAS  RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)                                                    
*                                                                               
SEND22   AHI   R4,2                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         TM    BTINDS,BTIND_COSTOV                                              
         BO    SEND24                                                           
         BRAS  RE,SETDLM           IF NO OVRD, SEND DLM FOR GROSS               
         BRAS  RE,SETDLM            AND ANOTHER FOR NET                         
         B     SEND26                                                           
*                                                                               
SEND24   ICM   R0,15,BTGROSS                                                    
***         BRAS  RE,SETNUM     NEED TO SEND '0', NOT BLANK                     
         EDIT  (R0),(10,(R4)),ALIGN=LEFT,ZERO=NOBLANK,MINUS=YES                 
         AR    R4,R0                                                            
         MVI   0(R4),SEMICOL                                                    
         LA    R4,1(R4)                                                         
*                                                                               
         ICM   R0,15,BTNET                                                      
***         BRAS  RE,SETNUM     NEED TO SEND '0', NOT BLANK                     
         EDIT  (R0),(10,(R4)),ALIGN=LEFT,ZERO=NOBLANK,MINUS=YES                 
         AR    R4,R0                                                            
         MVI   0(R4),SEMICOL                                                    
         LA    R4,1(R4)                                                         
*                                                                               
SEND26   TM    BTINDS,BTIND_OTO                                                 
         BZ    *+8                                                              
         MVI   0(R4),C'Y'                                                       
         TM    BTINDS,BTIND_MISSED                                              
         BZ    *+8                                                              
         MVI   0(R4),C'-'          BUT BEING MISSED IS MORE BETTER              
         BRAS  RE,SETDLM                                                        
*                                                                               
         TM    BTINDS,BTIND_PAID                                                
         BZ    *+8                                                              
         MVI   0(R4),C'Y'                                                       
         BRAS  RE,SETDLM                                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BTPAIDDT),(X'20',DUB)                            
         MVC   0(4,R4),DUB+2       MOVE MMDD                                    
         MVC   4(2,R4),DUB         MOVE YY                                      
         AHI   R4,5                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
* SEND TAX                                                                      
         ICM   R0,15,BTTAX                                                      
***         BRAS  RE,SETNUM     NEED TO SEND '0', NOT BLANK                     
         EDIT  (R0),(10,(R4)),ALIGN=LEFT,ZERO=NOBLANK,MINUS=YES                 
         AR    R4,R0                                                            
         MVI   0(R4),SEMICOL                                                    
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R1,H08Q             FIND SPOT DETAIL HEADER                      
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R0,BLOCK                                                         
         SR    R0,R4                                                            
         LPR   R5,R0               SET OUTPUT LENGTH                            
         LA    R4,BLOCK                                                         
         LA    R1,H08SPDET         SPOT DETAIL DATA CODE                        
         BRAS  RE,SENDD                                                         
*=================================================================*             
         SPACE 1                                                                
         OC    BTAFFDT,BTAFFDT     TEST MATCHED                                 
         BZ    SEND12              NO - GET NEXT                                
*                                                                               
         LA    R1,H09Q             GET AFFID DATA HEADER                        
         BRAS  RE,SENDH                                                         
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVI   0(R4),SEMICOL       SEND MATCHED=Y (DEFAULT)                     
         AHI   R4,1                                                             
*                                                                               
* THERE REALLY DOESN'T SEEM TO BE A LOGICAL PLACE FOR THIS INSTRUCTION          
         MVC   FLMSLN,BTAFFSLN                                                  
*                                                                               
         MVC   FLMDATE,BTAFFDT                                                  
         GOTO1 VDATCON,DMCB,(2,BTAFFDT),(X'20',DUB)                             
         MVC   0(4,R4),DUB+2       MOVE MMDD                                    
         MVC   4(2,R4),DUB         MOVE YY                                      
         AHI   R4,5                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   FLMTIME,BTAFFTM                                                  
         SR    R0,R0                                                            
         ICM   R0,3,BTAFFTM                                                     
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVI   0(R4),SEMICOL                                                    
         AHI   R4,1                                                             
*                                                                               
         LA    R1,BTPRD                                                         
         BRAS  RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)                                                    
         LA    R4,2(R4)            POINT TO LAST CHAR                           
         BRAS  RE,SETDLM                                                        
*                                                                               
         LA    R1,BTPRD2                                                        
         CLI   0(R1),0                                                          
         BE    SEND30                                                           
         BRAS  RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)                                                    
SEND30   AHI   R4,2                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         ICM   R0,15,BTAFGRS                                                    
         TM    BTAFFIND,AFIND_NEG                                               
         BZ    *+6                                                              
         LNR   R0,R0               MAKE THE DAMN THING NEGATIVE!                
         MVC   EDSAVE,EDSAVEM      GROSS & NET CAN BE MINUS                     
         BRAS  RE,SETNUM           SET GROSS COST                               
         MVC   EDSAVE,EDSAVEP                                                   
         MVI   0(R4),SEMICOL       DO NOT SEND NET                              
         AHI   R4,1                                                             
*                                                                               
         MVC   0(2,R4),BTMKGD                                                   
         AHI   R4,1                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         ICM   R0,15,BTAFDEMO                                                   
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BTAFFILM         POINT TO 1 BYTE FILM SEQ                     
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BTAFFLM2                                                      
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVI   0(R4),SEMICOL       SKIP CABLE NETWORK                           
         AHI   R4,1                                                             
*                                                                               
         MVI   0(R4),C'N'                                                       
         TM    BTAFFIND,AFIND_MG   TEST AFFID IS A MAKEGOOD                     
         BZ    *+8                                                              
         MVI   0(R4),C'Y'                                                       
         AHI   R4,1                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BTINVSEQ                                                      
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0               SEND AFFID EST                               
         ICM   R0,1,BTAFEST                                                     
         BZ    *+12                                                             
         BRAS  RE,SETNUM                                                        
         B     *+12                                                             
         MVI   0(R4),SEMICOL       DON'T SEND ESTIMATE '0'                      
         AHI   R4,1                                                             
*                                                                               
         LA    R2,BTTRFLM          SET FOR COMMON CODE BELOW                    
         B     SEND120                                                          
*                                                                               
SEND50   CLI   AFTYPE,C'U'         TEST UNMATCHED AFFID                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,H09Q             GET AFFID DATA HEADER                        
         BRAS  RE,SENDH                                                         
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVI   0(R4),C'N'                                                       
         BRAS  RE,SETDLM           SEND MATCHED=N                               
*                                                                               
         MVC   FLMDATE,AFDATE                                                   
         GOTO1 VDATCON,DMCB,(2,AFDATE),(X'20',DUB)                              
         MVC   0(4,R4),DUB+2       MOVE MMDD                                    
         MVC   4(2,R4),DUB         MOVE YY                                      
         AHI   R4,5                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   FLMTIME,AFTIME                                                   
         SR    R0,R0                                                            
         ICM   R0,3,AFTIME                                                      
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVC   FLMSLN,AFSLN                                                     
         SR    R0,R0                                                            
         ICM   R0,1,AFSLN                                                       
         BRAS  RE,SETNUM                                                        
*                                                                               
         MVC   FLMPRD,AFPRD                                                     
         LA    R1,AFPRD                                                         
         BRAS  RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)                                                    
         LA    R4,2(R4)            POINT TO LAST CHAR                           
         BRAS  RE,SETDLM                                                        
*                                                                               
         MVC   FLMPIG,AFPRD2                                                    
         LA    R1,AFPRD2                                                        
         CLI   0(R1),0                                                          
         BE    SEND52                                                           
         BRAS  RE,GETPRD                                                        
         MVC   0(3,R4),0(RF)                                                    
SEND52   AHI   R4,2                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         L     R0,AFGROSS                                                       
         TM    AFINDS,AFIND_NEG                                                 
         BZ    *+6                                                              
         LNR   R0,R0               MAKE THE DAMN THING NEGATIVE!                
         MVC   EDSAVE,EDSAVEM      GROSS & NET CAN BE MINUS                     
         BRAS  RE,SETNUM           SET GROSS COST                               
*                                                                               
         L     R0,AFNET                                                         
         TM    AFINDS,AFIND_NEG                                                 
         BZ    *+6                                                              
         LNR   R0,R0               MAKE THE DAMN THING NEGATIVE!                
         BRAS  RE,SETNUM           SET NET COST                                 
         MVC   EDSAVE,EDSAVEP                                                   
*                                                                               
         MVI   0(R4),SEMICOL       SKIP MG                                      
         AHI   R4,1                                                             
*                                                                               
         ICM   R0,15,AFDEMO                                                     
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,AFFILM           POINT TO 1 BYTE FILM SEQ                     
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,AFFILM2          POINT TO 1 BYTE FILM SEQ                     
         BRAS  RE,SETNUM                                                        
*                                                                               
         CLI   AFCBLNET,0          TEST CABLE                                   
         BE    SEND54              NO                                           
*                                                                               
         XC    WORK+32(8),WORK+32  GET NETWORK CALL LETTERS                     
         MVC   WORK+32(5),BMKTSTA                                               
         OC    WORK+36(1),AFCBLNET                                              
         LA    R1,WORK+32                                                       
         BRAS  RE,STAUNPK                                                       
         MVC   0(3,R4),WORK+5                                                   
SEND54   AHI   R4,3                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         TM    AFINDS,AFIND_MG                                                  
         BZ    *+8                                                              
         MVI   0(R4),C'Y'                                                       
         AHI   R4,1                                                             
         BRAS  RE,SETDLM                                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,AFINVSEQ                                                      
         BRAS  RE,SETNUM                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,AFEST                                                       
         BZ    *+12                                                             
         BRAS  RE,SETNUM                                                        
         B     *+12                                                             
         MVI   0(R4),SEMICOL       DON'T SEND ESTIMATE '0'                      
         AHI   R4,1                                                             
*                                                                               
         LA    R2,AFTRFLM          SET FOR COMMON CODE BELOW                    
*                                                                               
SEND120  LA    R0,BLOCK                                                         
         SR    R0,R4                                                            
         LPR   R5,R0                                                            
         LA    R4,BLOCK                                                         
         LA    R1,H09AFFID                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
* COMMON CODE FOR AFFIDS (MAT/UNM)  ---  R2=A(TRF FLM CODE)                     
         BRAS  RE,FILMCK           TEST RECALL/RELEASE DATES                    
         LA    R2,L'AFTRFLM(R2)    R2=A(TRF FLM CODE 2)                         
         MVC   FLMPRD,FLMPIG       SET FILMCK PRD TO PIGGY                      
         BRAS  RE,FILMCK                                                        
         LA    R2,L'AFTRFLM2(R2)   R2=A(AFFIND BYTE)                            
*                                                                               
         LA    R4,=C'Y'                                                         
         TM    0(R2),AFIND_EM      TEST EMAIL SENT                              
         BZ    *+12                                                             
         LHI   R1,22                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
         TM    0(R2),AFIND_SIQ     TEST NO SEPARATION CHECKING                  
         BZ    *+12                                                             
         LHI   R1,23                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
         TM    0(R2),AFIND_CH      TEST COST CHANGED BY MM                      
         BZ    *+12                                                             
         LHI   R1,25                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
         TM    0(R2),AFIND_IGF     TEST IGNORE FILM CODE FOR MATCHING           
         BZ    *+12                                                             
         LHI   R1,28                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
         TM    0(R2),AFIND_IGC     TEST COST FOR MATCHING                       
         BZ    *+12                                                             
         LHI   R1,29                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
         TM    0(R2),AFIND_IGT     TEST IGNORE TIME FOR MATCHING                
         BZ    *+12                                                             
         LHI   R1,30                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R2,L'AFINDS(R2)     R2=A(AFFIN2 BYTE)                            
*                                                                               
         TM    0(R2),AFIN2_IGS     TEST IGNORE TIME FOR MATCHING                
         BZ    *+12                                                             
         LHI   R1,32                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
         B     SEND12                                                           
*                                                                               
SEND200  L     R2,AIO2             BRAND SUBSTITUTION SAVE AREA                 
         LR    R6,R2                                                            
         AHI   R6,LENIO            R6=A(TABLE END)                              
         USING CMLSUBD,R2                                                       
*                                                                               
SEND210  OC    CMLSSEQ,CMLSSEQ     ANY MORE TABLE ENTRIES?                      
         JZ    SENDX                NO                                          
         LHI   R1,H11Q                                                          
         BRAS  RE,SENDH                                                         
         LHI   R1,01                                                            
         LA    R4,CMLSSEQ                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R8,CMLSSUBS         NOW SEND 02/03/04 FOR EA TBL ENT             
         USING CMLSSUBS,R8                                                      
         LHI   R0,CMLSPRQ                                                       
*                                                                               
SEND220  LHI   R1,02               SEND PRD, START AND END DATES                
         LA    R4,CMLSPRD                                                       
         BRAS  RE,SENDD                                                         
         LHI   R1,03                                                            
         LA    R4,CMLSSDT                                                       
         BRAS  RE,SENDD                                                         
         LHI   R1,04                                                            
         LA    R4,CMLSEDT                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R8,CMLSSUBQ(R8)     NEXT BRAND IN TBL                            
         CLC   0(L'CMLSPRD,R8),SPACES                                           
         JNH   SEND240                                                          
         JCT   R0,SEND220                                                       
*                                                                               
SEND240  LA    R2,CMLSUBLN(R2)     NEXT TABLE ENTRY                             
         CRJL  R2,R6,SEND210                                                    
         DCHO                                                                   
*                                                                               
SENDX    J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*=================================================================*             
* SENDPDEM:  SEND PRIMARY DEMO FOR LOWEST EST OR REQUESTED EST    *             
*            FOR EACH PRODUCT                                     *             
*=================================================================*             
         SPACE 1                                                                
SENDPDEM NTR1  BASE=*,LABEL=*                                                   
         CLC   SBQPRD,=C'POL'      POL REQ?                                     
         BNE   SPDX                 NO - DON'T NEED TO SEND                     
*                                                                               
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVI   KEY+4,C'A'          FORCE SKIP CLT REC                           
*                                                                               
SPD10    GOTO1 HIGH                GET FIRST/NEXT PRD                           
         MVC   KEY+7(1),SBQEST     SET EST START                                
         GOTO1 HIGH                GET FIRST REQ EST FOR PRD                    
*                                                                               
SPD20    CLC   KEY+1(1),BAGYMD     A-M/CLT                                      
         BNE   SPDX                                                             
         CLC   KEY+2(2),BCLT                                                    
         BNE   SPDX                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      A-M/CLT/PRD                                  
         BNE   SPD40               NEXT PRD                                     
*                                                                               
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   SPD50                YES - GET NEXT EST                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
* MAKE ESTIMATE DATES Y2K COMPATIBLE                                            
         GOTO1 VDATCON,DMCB,ESTART,ESTART                                       
         GOTO1 (RF),(R1),EEND,EEND                                              
*                                                                               
         CLC   QPER(6),EEND        REQ START AFTER EST END                      
         BH    SPD50                                                            
         CLC   QPER+6(6),ESTART    REQ END BEFORE EST START                     
         BL    SPD50                                                            
*                                                                               
         XC    SVEDEMOS,SVEDEMOS                                                
         BRAS  RE,GDEMCAT          GET DEMO CATEGORIES                          
         BNE   SPD40                                                            
*                                                                               
         LA    R1,H0DQ                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R1,01                                                            
         LA    R4,EKEYPRD                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,02                                                            
         LA    R4,EKEYEST                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,3                                                             
         LA    R4,BLOCK            A(DEMO LIST) - ONLY 1ST DEMO                 
         BRAS  RE,SENDD                                                         
*                                                                               
SPD40    MVC   KEY+7(6),=6X'FF'    NEXT PRD                                     
         B     SPD10                                                            
*                                                                               
SPD50    CLC   SBQEST,SBQESTND     SINGLE EST REQ?                              
         BE    SPD40                YES - NEXT PRD                              
         MVC   KEY+8(5),=5X'FF'    GET NEXT EST                                 
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      STILL ON SAME PRD?                           
         BNE   SPD10                NO                                          
         CLC   KEY+7(1),SBQESTND   HIGHER THAN REQ'D EST?                       
         BH    SPD40                YES - NEXT PRD                              
         B     SPD20                                                            
*                                                                               
SPDX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* SENDPROF:  SEND H1 DATA (CLT LEVEL PROFILES)                    *             
*=================================================================*             
         SPACE 1                                                                
SENDPROF NTR1  BASE=*,LABEL=*                                                   
         LA    R1,H01Q             LOCATE 01 HEADER                             
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R1,H01A001          GET A0 PROFILE DATA EL                       
         LA    R4,PROFA0                                                        
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01AG06          GET AGY PROFILE DATA EL                      
         LA    R4,SVAPROF+5                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01MK01          GET MK PROFILE                               
         LA    R4,PROFMK+0                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01CL10          GET CLT PROFILE ADJCENCY CODE                
         LA    R4,SVCPROF+9                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01CL15          GET CLT PROFILE RATE CONTROL                 
         LA    R4,SVCPROF+14                                                    
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2X7          MG BUY TO MG INVOICE ITEM ONLY               
         LA    R4,PROFI2X+6                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2X13         SHOW SPOT SEQ NUMBERS                        
         LA    R4,PROFI2X+12                                                    
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2Y5          PRINT SPECIAL REP NAME                       
         LA    R4,PROFI2Y+4        THIS IS NOT WHAT LCON THOUGHT, SO...         
         LA    R4,=C'Y'            ...ALWAYS FORCE SHOW REP CODE                
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01AG08          GET AGY PROFILE DATA EL                      
         LA    R4,SVAPROF+7        CANADIAN?                                    
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01B004          MATCH BY CONTRACT NUMBER                     
         LA    R4,PROFB0+3                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I200          TIME LEEWAY                                  
         LA    R4,PROFI2                                                        
         OC    PROFI2Z+8(2),PROFI2Z+8   TIME LEEWAY EFF DATE?                   
         BZ    SP10                                                             
         CLC   BMOS,PROFI2Z+8      >= EFFECTIVE DATE?                           
         BL    SP10                 NO                                          
         LA    R4,PROFI2Z+7        SEND I2Z LEEWAY                              
*                                                                               
SP10     BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2X14         TIME LEEWAY AFTER                            
         LA    R4,PROFI2X+13                                                    
         OC    PROFI2Z+8(2),PROFI2Z+8   TIME LEEWAY EFF DATE?                   
         BZ    SP20                                                             
         CLC   BMOS,PROFI2Z+8      >= EFFECTIVE DATE?                           
         BL    SP20                 NO                                          
         LA    R4,PROFI2Z+14       SEND I2Z LEEWAY AFTER                        
*                                                                               
SP20     BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I205          H/V CHECKING %                               
         LA    R4,PROFI2+4                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2X10         H/V CHECKING % WEEK                          
         LA    R4,PROFI2X+9                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2S1          SEPARATION CHECKING                          
         LA    R4,PROFI2S                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2S2          SEPARATION CHECKING - PRIMARY INT            
         LA    R4,PROFI2S+1                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2S3          SEPARATION CHECKING - SECONDARY INT          
         LA    R4,PROFI2S+2                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,PROFTI+10                                                     
         OC    PROFTI+11(2),PROFTI+11                                           
         BZ    SP26                                                             
         CLC   BMOS,PROFTI+11      PROFILES 9/11 VALID?                         
         BNL   SP26                 YES - USE PROFILE SETTINGS                  
         LA    R4,=C'N'             NO - FORCE INVALID FILMS NOT DISC           
SP26     LA    R1,H01TI11          INVALID FILMS DISCREPANT                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01B007          REASON CODE SCHEME                           
         LA    R4,PROFB0+7                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01B009          REQUIRE PURPOSE CODES                        
         LA    R4,PROFB0+9                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01MK09          APPROVE INVOICES                             
         LA    R4,PROFMK+8                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01PE            PROGRAM EXCHANGE                             
         LA    R4,=C'N'                                                         
         LR    RF,RA                                                            
         AHI   RF,(SVCLTREC-TWAD)+(COPT1-CLTHDRD)  RF=A(COPT1)                  
         TM    0(RF),COP1GMI                                                    
         BZ    *+8                                                              
         LA    R4,=C'Y'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01DAR7          TRADE SPECIAL REP DIGIT 1-3                  
         LA    R4,PROFDAR+6                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01DAR16         ADD COMM RATE TYPE FOR TRADE                 
         LA    R4,PROFDAR+15                                                    
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2A6          MATCH ON REP CODE                            
         LA    R4,PROFI2A+5                                                     
         OC    PROFI2A+6(2),PROFI2A+6   EFFECTIVE Y/M                           
         BZ    SP30                                                             
         CLC   BMOS,PROFI2A+6      >= EFFECTIVE DATE?                           
         BNL   SP30                 YES                                         
         LA    R4,=C'N'                                                         
SP30     BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H010010                                                       
         LA    R4,PROF00+9         2 DECIMAL RATINGS                            
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01CLC2                                                       
         LA    R4,SVCOST2          COST 2                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01MK10          CHANGE INVOICE COSTS                         
         LA    R4,PROFMK+9                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2A15         SEPARATE ADDED VALUE - $0                    
         LA    R4,PROFI2A+14                                                    
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2A16         5,7,10 SEC $0 ARE ADDED VALUE                
         LA    R4,PROFI2A+15                                                    
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01MK11          AUTO I2 ON MATCH                             
         LA    R4,PROFMK+10                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2B10         SPOT LENGTH LEEWAY - BEFORE                  
         LA    R4,PROFI2B+9                                                     
         OC    PROFI2B+11(2),PROFI2B+11  EFFECTIVE Y/M                          
         BZ    SP40                                                             
         CLC   BMOS,PROFI2B+11     >= EFFECTIVE DATE?                           
         BL    SP50                 YES                                         
*                                                                               
SP40     BRAS  RE,SENDD                                                         
         LA    R1,H01I2B11         SPOT LENGTH LEEWAY - AFTER                   
         LA    R4,PROFI2B+10                                                    
         BRAS  RE,SENDD                                                         
         B     SP60                                                             
*                                                                               
SP50     LA    R4,=X'00'           IF OUTSIDE EFFECTIVE DATE, SEND 0            
         BRAS  RE,SENDD                                                         
         LA    R1,H01I2B11                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
SP60     LA    R1,H01I2N4                                                       
         LA    R4,PROFI2N+3        EST=NO INVALID FOR UPD I2                    
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2N6                                                       
         LA    R4,PROFI2N+5        EST=NO UPDATIVE I2 ONLY                      
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01RTYPE                                                      
         LAY   R4,RTYPES                                                        
         LHI   R5,RTYPESLQ                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2C7          CMML TIMES ARE B'CAST DAY                    
         LA    R4,PROFI2C+6                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2C8          CHECK CMML LENGTH                            
         LA    R4,PROFI2C+7                                                     
         OC    PROFI2C+8(2),PROFI2C+8   EFFECTIVE Y/M                           
         BZ    SP70                                                             
         CLC   BMOS,PROFI2C+8      >= EFFECTIVE DATE?                           
         BNL   SP70                 YES                                         
         LA    R4,=C'N'                                                         
SP70     BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2C6          FILM MATCH DSCRPCY = FILM ERR                
         LA    R4,PROFI2C+5                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01I2C11         SPOT COST TOLERANCE                          
         LA    R4,PROFI2C+10                                                    
         OC    PROFI2C+11(2),PROFI2C+11  EFFECTIVE Y/M                          
         BZ    SP80                                                             
         CLC   BMOS,PROFI2C+11     >= EFFECTIVE DATE?                           
         BNL   SP80                 YES                                         
         LA    R4,=X'00'                                                        
SP80     BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H01MK14          ONLY USE DAR REP FOR NEW BUYS                
         LA    R4,PROFMK+13                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,H012DIMP         MAPCODE 42 TWO DECIMAL IMPRESSIONS           
         LA    R4,DEC2IMPS         2 DECIMAL IMPRESSIONS FROM 00A PROF          
         BRAS  RE,SENDD            SEND THE PROFILE IN FIELD 42                 
*                                                                               
SPX      J     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* CALL TO SEND H0C (INVOICE) DATA (IF NOT ALREADY SENT            *             
*=================================================================*             
         SPACE 1                                                                
         USING SNVKEYD,R8                                                       
SENDH0C  NTR1  BASE=*,LABEL=*                                                   
         CLI   INVSW,C'Y'          TEST HEADER SENT YET                         
         BE    SEND0CX                                                          
*                                                                               
         CLI   SVSNVKEY,0          TEST SNVKEY SAVED YET                        
         BNE   *+10                                                             
         MVC   SVSNVKEY,0(R8)      SAVE FIRST 0E03 KEY WE SEND DATA FOR         
*                                                                               
         ZIC   RE,INVHDSEQ                                                      
         AHI   RE,1                                                             
         STC   RE,INVHDSEQ                                                      
*                                                                               
* SEND HEADER                                                                   
         LA    R1,H0CQ             HDR 0C/DATA 01 IS INVOICE                    
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R1,1                                                             
         LA    R4,INVHDINV                                                      
         LA    R5,10                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
         OC    INVHDEZS,INVHDEZS                                                
         BZ    SEND0C10                                                         
         LA    R1,4                                                             
         LA    R4,=C'Y'                                                         
         BRAS  RE,SENDD                                                         
         LA    R1,14                                                            
         LA    R4,INVHDEZS                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
SEND0C10 CLC   INVHDCON,SPACES                                                  
         BNH   SEND0C12                                                         
         LA    R1,5                                                             
         LA    R4,INVHDCON                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
SEND0C12 CLC   INVHDREP,SPACES                                                  
         BNH   SEND0C20                                                         
         LHI   R1,10                                                            
         LA    R4,INVHDREP                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         USING FLMTABD,R4                                                       
SEND0C20 L     R4,AIO3             FILM CODE SAVE AREA                          
         MVI   BYTE,0                                                           
*                                                                               
SEND0C22 CLC   FLMCOD,SPACES                                                    
         BNH   SEND0C50                                                         
*                                                                               
         LA    R1,6                                                             
         BRAS  RE,SENDD                                                         
         ST    R4,FULL             SAVE R4                                      
*                                                                               
         TM    FLMSTAT,FLMSINV     TEST INVALID FILM CODE                       
         BZ    SEND0C24                                                         
         LA    R4,=C'Y'                                                         
         LA    R1,7                                                             
         BRAS  RE,SENDD                                                         
         B     SEND0C26                                                         
*                                                                               
SEND0C24 LA    R4,FLMSEQ                                                        
         LA    R1,12                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
         CLC   FLMCOD+8(4),SPACES  THIS AD-ID?                                  
         BNH   SEND0C26             NO                                          
         LA    R4,=X'01'           01=AD-ID, 02=HI-DEF                          
         TM    FLMSTAT,SNVCMHDQ                                                 
         BZ    *+8                                                              
         LA    R4,=X'02'                                                        
         LA    R1,13                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
SEND0C26 L     R4,FULL             RESTORE R4                                   
         AHI   R4,FLMTABLQ                                                      
         B     SEND0C22                                                         
         DROP  R4                                                               
*                                                                               
SEND0C50 LA    R4,SNVELS           SEND INVOICE ID EL (FOR IM)                  
         XR    R0,R0                                                            
*                                                                               
SEND0C52 CLI   0(R4),SNVIMELQ      IM ID ELEM (X'15')                           
         BE    SEND0C54                                                         
         ICM   R0,1,1(R4)                                                       
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    SEND0C60                                                         
         B     SEND0C52                                                         
*                                                                               
SEND0C54 LA    R1,11                                                            
         AHI   R4,2                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
SEND0C60 OC    INVEOUID(INVEOQ),INVEOUID  SEND EASI INVOICE ORIGIN DATA         
         BZ    SEND0CX                                                          
         LA    R1,15                                                            
         LA    R4,INVEOUID                                                      
         BRAS  RE,SENDD                                                         
         LA    R1,16                                                            
         LA    R4,INVEOSTA                                                      
         BRAS  RE,SENDD                                                         
         LA    R1,17                                                            
         LA    R4,INVEODAT                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
SEND0CX  MVI   INVSW,C'Y'          SET FLAG THAT HEADER HAS BEEN SENT           
         J     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*=================================================================*             
* GDEMCAT                                                         *             
*  PASS R6 = A(EST REC)                                           *             
*  EXIT CC NEQ IF NO DEMOS                                        *             
*  RETURN R7 = # DEMO CATEGORIES                                  *             
*=================================================================*             
         SPACE 1                                                                
GDEMCAT  NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO4             ** INITIALIZE DBLOCK                         
         XC    0(256,RE),0(RE)     YSFI                                         
         USING DBLOCKD,RE                                                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE(3),=C'TP '                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'                                                   
         BNE   GDC10                                                            
         CLI   SVCXTRA,C'U'        TEST US DEMOS                                
         BE    GDC10                                                            
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
         USING ESTHDRD,R6                                                       
GDC10    OC    SVEDEMOS,SVEDEMOS   DEMO CATS FROM LOWER EST?                    
         BNZ   *+10                 YES - USE THEM                              
         MVC   SVEDEMOS,EDEMOS     SAVE DEMO NUMBERS                            
*                                                                               
         SR    R7,R7               COUNT NUMBER OF DEMOS                        
         LA    R0,20                                                            
         LA    R1,SVEDEMOS                                                      
GDC20    OC    0(3,R1),0(R1)                                                    
         BZ    GDC30                                                            
         LA    R1,3(R1)                                                         
         BCTR  R7,0                DECREMENT COUNT                              
         BCT   R0,GDC20                                                         
*                                                                               
GDC30    XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,,X'D9000AE0'    GET A(DEMOCON)                      
         L     RF,0(R1)                                                         
*                                                                               
         LPR   R7,R7               SET NUMBER OF DEMOS                          
         BZ    GBXNEQ                                                           
         CLC   ELEN,=AL2(ESTHDRLN) HAVE COMSCORE DEMOS?                         
         BL    *+12                NO                                           
         LA    R4,ENONTDMS         YES - R4 = A(COMSCORE DEMOS)                 
         ST    R4,DMCB+16          PASS COMSCORE DEMOS IN P5                    
         XC    BLOCK,BLOCK         CLEAR OUTPUT BLOCK                           
         GOTO1 (RF),DMCB,((R7),SVEDEMOS),(2,BLOCK),(C'S',AIO4),EUSRNMS          
*                                                                               
GBXEQ    CR    RE,RE               EXIT CC EQ                                   
         B     *+6                                                              
GBXNEQ   LTR   RE,RE               EXIT CC NEQ                                  
         XIT1  REGS=(R7)                                                        
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* FILMCK: READ TRAFFIC CMML REC, ADD FILM & RECALL/RELEASE DATES, *             
*  START/END TIMES AND MATCHING PERIODS TO TABLE. BUILD BRAND     *             
*  SUBSTITUTION TABLE IN AIO2                                     *             
*                                                                 *             
*                                                                 *             
* PASS R2=A(TRF CMML SEQ #)                                       *             
*      FLMDATE = AFFID RUN DATE                                   *             
*      FLMPRD  = AFFID PRD OR PIGGY                               *             
*      FLMTIME = AFFID TIME                                       *             
*      FLMSLN  = AFFID SLN                                        *             
*                                                                 *             
* RETURN RECORD 9 MAPCODE                                         *             
*      21 IF FILM RUN OUTSIDE REC/REL DATES                       *             
*      26 IF AIRED (INV) OUTSIDE ENHANCED FLIGHT DATES            *             
*      27 IF AIRED (INV) PRD NOT ONE OF 1ST 3 FILM PRDS           *             
*      31 IF AIRED (INV) TIME IS OUTSIDE FILM START/END TIME      *             
*      35 IF AIRED (INV) SLN <> FILM SLN                          *             
*                                                                 *             
*                                                                 *             
*=================================================================*             
         SPACE 1                                                                
FILMCK   NTR1  BASE=*,LABEL=*                                                   
         OC    0(2,R2),0(R2)       ANY FILM CODE?                               
         BZ    FILMCKX              NO                                          
         L     R3,AIO3                                                          
         LR    RE,R3                                                            
         AHI   RE,LENIO-CMLTABLN                                                
         USING CMLTABLD,R3                                                      
*                                                                               
RR10     OC    CMLTSEQ,CMLTSEQ                                                  
         BZ    RR20                                                             
         CLC   CMLTSEQ,0(R2)       MAP CODE WE'RE LOOKING FOR?                  
         BE    FC100                                                            
         LA    R3,CMLTABLN(R3)                                                  
         CR    R3,RE               MAKE SURE WE DON'T BLOW CMLTAB               
         BL    RR10                                                             
         DC    H'0'                                                             
*                                                                               
RR20     MVC   CMLTSEQ,0(R2)       SET CMML SEQ IN TABLE                        
         MVI   XSP,C'T'            SET READ TRAFFIC                             
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLRECD,R4                                                       
         MVC   CMLKID(2),=X'0AA1'                                               
         MVC   CMLPAM,BAGYMD                                                    
         MVC   CMLPCLT,BCLT                                                     
         MVC   CMLPSEQ+1(2),CMLTSEQ                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DID WE FIND IT?                              
         BE    FC30                                                             
         MVC   ERROR,=Y(BADCMML)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
FC30     GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         CLI   CMLDTAEL,X'10'                                                   
         BE    *+6                                                              
         DCHO                                                                   
         GOTO1 VDATCON,DMCB,(3,CMLRLSE),(2,CMLTREL)                             
         GOTO1 (RF),(R1),(3,CMLRCL),(2,CMLTRCL)                                 
         MVC   CMLTSLN,CMLSLN                                                   
         DROP  R4                                                               
*                                                                               
         USING CMLSUBD,R5                                                       
         L     R5,AIO2                                                          
         LR    R6,R5                                                            
         AHI   R6,LENIO                                                         
         LA    R4,CMLDTAEL-CMLKEY(R4)   R4=A(FIRST EL)                          
*                                                                               
FC40     CLI   0(R4),0                                                          
         BE    FC100                                                            
         CLI   0(R4),X'20'         CMLPRDEL                                     
         BE    FC50                                                             
         CLI   0(R4),CMLMATEQ      X'B0'                                        
         BE    FC60                                                             
         CLI   0(R4),CMLPRSEQ      X'B1'                                        
         BE    FC70                                                             
         BH    FC100                                                            
*                                                                               
FC42     LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     FC40                                                             
*                                                                               
         USING CMLPRDEL,R4                                                      
FC50     XR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AHI   RF,-3                                                            
         CHI   RF,11               MAX OF 12 PRODUCTS                           
         BNH   *+8                                                              
         LHI   RF,11                                                            
         EX    RF,*+8                                                           
         B     FC42                                                             
         MVC   CMLTPRDS(0),CMLPRDS                                              
         DROP  R4                                                               
*                                                                               
         USING CMLMATEL,R4                                                      
FC60     MVC   CMLTFLDT(L'CMLTFLDT*CMLTFLDQ),CMLMPER1                           
         MVC   CMLTFLAG,CMLMFLAG                                                
         MVC   CMLTSTIM,CMLMSTIM                                                
         MVC   CMLTETIM,CMLMETIM                                                
         B     FC42                                                             
         DROP  R4                                                               
*                                                                               
         USING CMLPRSUB,R4                                                      
BRDS     USING CMLSSUBS,RE                                                      
FC70     LA    RE,CMLSSUBS                                                      
         LHI   RF,CMLSPRQ                                                       
*                                                                               
FC72     CLC   BMOS,CMLPREDT       TEST ELEM EXPIRES BEFORE MOS                 
         JH    FC74                 YES - NEXT ELEM                             
         CLC   BMOS,CMLPRSDT       TEST ELEM STARTS AFTER MOS                   
         JL    FC74                 YES - NEXT ELEM                             
         MVC   BRDS.CMLSPRD,CMLPRSPR                                            
         MVC   BRDS.CMLSSDT,CMLPRSDT                                            
         MVC   BRDS.CMLSEDT,CMLPREDT                                            
         LA    RE,CMLSSUBQ(RE)     NEXT SLOT IN PRD TABLE                       
         DROP  BRDS                                                             
*                                                                               
FC74     LLC   R0,CMLPRSLN         PROCESS ALL X'B1' ELEMS HERE                 
         AR    R4,R0                                                            
         CLI   CMLPRSEL,CMLPRSEQ   ANY MORE B1 ELEMS?                           
         JNE   FC76                 NO                                          
         JCT   RF,FC72                                                          
         DC    H'0'                                                             
*                                                                               
FC76     OC    CMLSSUBS,CMLSSUBS   ANY ENTRIES?                                 
         JZ    FC40                 NO                                          
         MVC   CMLSSEQ,CMLTSEQ                                                  
         LA    R5,CMLSUBLN(R5)                                                  
         CRJL  R5,R6,FC40          COMPARE REGISTER JUMP LOW                    
         DCHO                                                                   
         DROP  R4,R5                                                            
*                                                                               
FC100    XR    R5,R5               CLEAR LENGTH OVERRIDE                        
         CLI   PROFI2B+0,C'Y'      CHECK COMML PRODUCT                          
         BNE   FC110                                                            
         OC    PROFI2B+1(2),PROFI2B+1   IS THERE AN EFFECTIVE DATE?             
         BZ    *+14                      NO - IT'S ALWAYS EFFECTIVE             
         CLC   BMOS,PROFI2B+1      >= EFFECTIVE DATE?                           
         BL    FC110                NO                                          
         CLI   CMLTPRDS,X'FF'      ALL PRODUCTS?                                
         BE    FC110                YES                                         
         XR    R0,R0                                                            
         LHI   R0,CMLTPRDQ                                                      
         LA    RE,CMLTPRDS                                                      
*                                                                               
         CLC   FLMPRD,0(RE)                                                     
         BE    FC110                                                            
         LA    RE,L'CMLTPRDS(RE)                                                
         BCT   R0,*-14                                                          
*                                                                               
         LA    R4,=C'Y'                                                         
         LA    R1,27                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
FC110    MVI   BYTE,0              FLAG TO SKIP REC/REL DATE TEST               
         OC    CMLTFLDT(L'CMLTFLDT*CMLTFLDQ),CMLTFLDT  ANY CMML FLTS?           
         BZ    FC130                                    NO                      
         CLI   PROFI2B+3,C'Y'      CHECK COMML FLIGHTS                          
         BNE   FC130                                                            
         OC    PROFI2B+4(2),PROFI2B+4  IS THERE AN EFFECTIVE DATE?              
         BZ    *+14                     NO - IT'S ALWAYS EFFECTIVE              
         CLC   BMOS,PROFI2B+4      >= EFFECTIVE DATE?                           
         BL    FC130                NO                                          
         XR    R0,R0                                                            
         LHI   R0,CMLTFLDQ                                                      
         LA    RE,CMLTFLDT                                                      
*                                                                               
FC112    OC    0(L'CMLTFLDT,RE),0(RE)                                           
         BZ    FC114                                                            
         MVI   BYTE,1              SET TO SKIP REC/REL DATES                    
         CLC   FLMDATE,0(RE)       BEFORE RELEASE DATE?                         
         BL    FC114                YES - CHECK NEXT FLT PER                    
         CLC   FLMDATE,2(RE)       AFTER RECALL DATE?                           
         BNH   FC130                NO - IT FITS                                
*                                                                               
FC114    LA    RE,L'CMLTFLDT(RE)                                                
         BCT   R0,FC112                                                         
*                                                                               
         LA    R4,=C'Y'                                                         
         LA    R1,26                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
FC130    CLI   BYTE,0              NON-ZERO MEANS FLIGHT DATES USED             
         BNE   FC140                                                            
         CLI   PROFTI+9,C'Y'       CHECK CMML RECALL/RELEASE                    
         BNE   FC140                                                            
         CLC   FLMDATE,CMLTREL     BEFORE RELEASE DATE?                         
         BL    *+14                 YES - SEND MAP CODE 21                      
         CLC   FLMDATE,CMLTRCL     AFTER RECALL DATE?                           
         BNH   FC140                NO - IT FITS                                
*                                                                               
         LA    R4,=C'Y'                                                         
         LA    R1,21                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
FC140    OC    CMLTSTIM(4),CMLTSTIM  TEST START/END TIME PRESENT                
         BZ    FC160                                                            
         CLI   PROFI2B+6,C'Y'      CHECK COMML TIMES                            
         BNE   FC160                                                            
         OC    PROFI2B+7(2),PROFI2B+7  IS THERE AN EFFECTIVE DATE?              
         BZ    *+14                     NO - IT'S ALWAYS EFFECTIVE              
         CLC   BMOS,PROFI2B+7      >= EFFECTIVE DATE?                           
         BL    FC160                NO                                          
*                                                                               
*  FLMSTIME = CMLTSTIM                                                          
*  FLMETIME = CMLTETIM                                                          
*  FLMSTAT  = CMLTFLAG                                                          
*  FLMSDLY  = CMLTFDAY                                                          
*  IDAT     = FLMDATE                                                           
*  FLMRLSE  = CMLTREL                                                           
*  FLMRCL   = CMLTRCL                                                           
*  DUB      = FLMTIME                                                           
*                                                                               
         CLC   CMLTSTIM,CMLTETIM   START TIME <= END TIME?                      
         BNH   FC142               YES - DOES NOT CROSS MIDNIGHT                
*                                                                               
* COMML TIMES - HANDLE PERIODS THAT CROSS MIDNIGHT (9P-2A)                      
*                                                                               
         TM    CMLTFLAG,CMLTFDAY   CHECK TIMES DAILY?                           
         BZ    FC141A              NO                                           
         CLC   FLMTIME,CMLTSTIM    INVOICE ON OR AFTER START TIME?              
         BNL   FC160               YES - VALID (COVERS 9P-MIDNIGHT)             
         CLC   FLMTIME,CMLTETIM    INVOICE ON OR BEFORE END TIME?               
         BNH   FC160               VALID (COVERS MIDNIGHT-2A)                   
         B     FCTIMERR            NO - INVALID!                                
*                                                                               
FC141A   CLI   PROFI2C+6,C'Y'      COMML TIMES ARE BCAST DAY?                   
         BNE   FC141D              NO - CALENDAR TIMES                          
         CLC   FLMDATE,CMLTREL     INVOICE DETAIL AIRED ON RELEASE DATE         
         BNE   FC141B              NO                                           
         CLC   FLMTIME,=H'2400'    INV TIME = 12M?                              
         BE    FC160               YES - MIDNIGHT ALWAYS MATCHES                
         CLC   FLMTIME,=H'459'     INV TIME IS BETWEEN 12:01AM-4:59AM           
         BNH   FC160               YES - ALWAYS MATCHES                         
         CLC   FLMTIME,CMLTSTIM    INV TIME BEFORE CMML START?                  
         BL    FCTIMERR            YES - ERROR                                  
*                                                                               
FC141B   CLC   FLMDATE,CMLTRCL     INVOICE DETAIL AIRED ON RECALL DATE          
         BNE   FC160               NO                                           
         CLC   CMLTETIM,=H'500'    FILM END TIME FROM 12:01AM-4:59AM?           
         BNL   FC141C              NO                                           
         CLC   FLMTIME,=H'500'     INV TIME BEFORE 5AM?                         
         BNL   FC160               NO - 5AM-12M IS VALID                        
         CLC   FLMTIME,CMLTETIM    INV TIME AFTER CMML END?                     
         BH    FCTIMERR            YES - INVALID!                               
         B     FC160               VALID                                        
*                                                                               
FC141C   CLC   FLMTIME,=H'500'     INV TIME BEFORE 5AM?                         
         BL    FCTIMERR            YES - INVALID                                
         CLC   FLMTIME,CMLTETIM    INV TIME AFTER CMML END?                     
         BH    FCTIMERR            YES - INVALID!                               
         B     FC160               VALID                                        
*                                                                               
FC141D   CLC   FLMDATE,CMLTREL     INVOICE DETAIL AIRED ON RELEASE DATE         
         BNE   *+14                NO                                           
         CLC   FLMTIME,CMLTSTIM    INV TIME BEFORE CMML START?                  
         BL    FCTIMERR            YES - INVALID!                               
         CLC   FLMDATE,CMLTRCL     INVOICE DETAIL AIRED ON RECALL DATE          
         BNE   FC160               NO                                           
         CLC   FLMTIME,CMLTETIM    INV TIME AFTER CMML END?                     
         BH    FCTIMERR            YES - INVALID!                               
         B     FC160               VALID                                        
*                                                                               
* COMML TIMES - HANDLE PERIODS THAT DO NOT CROSS MIDNIGHT (9A-3P)               
*                                                                               
FC142    TM    CMLTFLAG,CMLTFDAY   CHECK TIMES DAILY                            
         BO    FC144                                                            
         CLC   FLMDATE,CMLTREL     INVOICE DETAIL AIRED ON RELEASE DATE         
         BNE   FC146                                                            
         CLI   PROFI2C+6,C'Y'      COMML TIMES ARE BCAST DAY?                   
         BNE   FC144               NO - CALENDAR TIMES                          
         CLC   FLMTIME,=H'459'     INV TIME IS BETWEEN 12:01AM-4:59AM           
         BH    FC144               NO                                           
         CLC   CMLTSTIM,=H'459'    CML START TIME BEFORE 5AM?                   
         BH    FC160               NO - INV TIME IS VALID                       
FC144    CLC   FLMTIME,CMLTSTIM    CHECK START TIME                             
         BL    FCTIMERR                                                         
*                                                                               
FC146    TM    CMLTFLAG,CMLTFDAY   CHECK TIMES DAILY                            
         BO    FC147                                                            
         CLC   FLMDATE,CMLTRCL     INVOICE DETAIL AIRED ON RECALL DATE          
         BNE   FC160                                                            
         CLI   PROFI2C+6,C'Y'      COMML TIMES ARE BCAST DAY?                   
         BNE   FC147               NO - CALENDAR TIMES                          
         OC    CMLTSTIM,CMLTSTIM   HAVE A START TIME?                           
         BNZ   *+14                YES                                          
         CLC   CMLTETIM,=X'FFFF'   ANY FILM END TIME                            
         BE    FC160               NO - NO FILM TIMES IS VALID                  
         CLC   CMLTETIM,=H'500'    FILM END TIME FROM 12:01AM-4:59AM?           
         BNL   FC146A              NO                                           
         CLC   FLMTIME,=H'500'     INV TIME BEFORE 5AM?                         
         BNL   FC160               NO - 5AM-12M IS VALID                        
         B     FC147               TEST END TIME                                
*                                                                               
FC146A   CLC   FLMTIME,=H'500'     INV TIME BEFORE 5AM?                         
         BL    FCTIMERR            YES - INVALID                                
*                                                                               
FC147    CLC   FLMTIME,CMLTETIM    CHECK END TIME                               
         BNH   FC160                                                            
*                                                                               
FCTIMERR LA    R4,=C'Y'                                                         
         LA    R1,31                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
FC160    CLI   PROFI2C+7,C'Y'      CHECK COMML LENGTH                           
         BNE   FC170                                                            
         OC    PROFI2C+8(2),PROFI2C+8   IS THERE AN EFFECTIVE DATE?             
         BZ    *+14                      NO - IT'S ALWAYS EFFECTIVE             
         CLC   BMOS,PROFI2C+8      >= EFFECTIVE DATE?                           
         BL    FC170                NO                                          
         CLC   FLMSLN,CMLTSLN                                                   
         BE    FC170                                                            
         LA    R4,=C'Y'                                                         
         LA    R1,35                                                            
         BRAS  RE,SENDD                                                         
*                                                                               
FC170    DS    0H                                                               
*                                                                               
FILMCKX  MVI   XSP,C'N'            SET READ SPOT                                
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
*=================================================================*             
* ROUTINE TO SAVE SPOT SEARCH FILTERS                             *             
*=================================================================*             
         SPACE 1                                                                
SAVEFILT NTR1  BASE=*,LABEL=*                                                   
         MVI   FILTDAY,0                                                        
         MVI   FILTDPT,0                                                        
         MVI   FILTPRGL,0                                                       
         XC    FILTPROG,FILTPROG                                                
         XC    FILTTIM,FILTTIM                                                  
         XC    FILTCOST,FILTCOST                                                
*                                                                               
         OC    RH6DAYS,RH6DAYS                                                  
         BZ    SF10                                                             
         LA    R1,RH6DAYS          GET DAYS IN SAME FORMAT AS BDDAY             
         SR    R3,R3                                                            
*                                                                               
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         AHI   R1,1                                                             
         BCT   R3,*-12                                                          
*                                                                               
         LPR   R3,R3                                                            
         GOTO1 VCALLOV,DMCB,0,X'D9000A03'  GET ADDRESS OF DAYPAK                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R3),RH6DAYS),FILTDAY,BYTE                            
*                                                                               
SF10     OC    RH6TIME,RH6TIME                                                  
         BZ    SF20                                                             
         LA    R1,RH6TIME          GET DAYS IN SAME FORMAT AS BDDAY             
         SR    R3,R3                                                            
*                                                                               
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         AHI   R1,1                                                             
         BCT   R3,*-12                                                          
*                                                                               
         LPR   R3,R3                                                            
         GOTO1 VTIMVAL,DMCB,((R3),RH6TIME),FILTTIM                              
*                                                                               
         SR    RE,RE                                                            
         LA    R0,500              SET RADIO START TIME                         
         CLI   QMED,C'R'                                                        
         BE    *+16                                                             
         CLI   QMED,C'X'                                                        
         BNE   *+8                                                              
         LA    R0,600              OTHER MEDIA START AT 6AM                     
*                                                                               
         ICM   RE,3,FILTTIM        ADJUST FILTER TIMES IF NECESSARY             
         CR    RE,R0                                                            
         BNL   *+12                                                             
         AHI   RE,2400                                                          
         STCM  RE,3,FILTTIM                                                     
*                                                                               
         ICM   RE,3,FILTTIM+2                                                   
         CR    RE,R0                                                            
         BNL   *+12                                                             
         AHI   RE,2400                                                          
         STCM  RE,3,FILTTIM+2                                                   
*                                                                               
SF20     MVC   FILTDPT,RH6DPT                                                   
         MVC   FILTPROG,RH6PROG                                                 
         MVC   FILTPRGL,RH6PROGL                                                
*                                                                               
         OC    QCOST,QCOST                                                      
         BZ    SFX                                                              
         CLC   =C'ZERO',QCOST                                                   
         BNE   *+14                                                             
         MVC   FILTCOST,=F'-1'                                                  
         B     SFX                                                              
*                                                                               
         LHI   R3,10               GET QCOST INPUT LENGTH                       
         LA    RE,QCOST                                                         
         AHI   RE,L'QCOST-1        RE=A(END OF QCOST)                           
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R3,*-10                                                          
*                                                                               
         CLI   0(RE),C'.'          IF LAST CHARACTER IS A '.'                   
         BNE   *+10                                                             
         MVI   0(RE),0             CLEAR IT AND DECREMENT INPUT LENGTH          
         BCTR  R3,0                                                             
*                                                                               
         LTR   R3,R3                                                            
         BNP   SFX                                                              
         GOTO1 VCASHVAL,DMCB,(2,QCOST),(R3)                                     
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DCHO                                                                   
         MVC   FILTCOST,4(R1)                                                   
*                                                                               
SFX      J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* CHECK THAT PASSIVE KEY HASN'T BEEN DELETED BY NEW I2 RUN        *             
*=================================================================*             
         SPACE 1                                                                
K        USING SNVKEYD,KEY                                                      
*                                                                               
CKPASS   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             SET TO READ SNV PASSIVE KEYS                 
         MVI   K.SNVPTYP,X'0E'                                                  
         MVI   K.SNVPSUB,X'83'                                                  
         MVC   K.SNVPAM,BAGYMD                                                  
         MVC   K.SNVPMKT(5),BMKTSTA                                             
*                                                                               
         CLI   PROFMK+4,C'Y'       USE BUBBLCD?                                 
         BNE   *+10                                                             
         MVC   K.SNVPBYR,BUBBLCD                                                
*                                                                               
         MVC   FULL(2),BMOS        GET MOS                                      
         MVI   FULL+2,1                                                         
         GOTO1 VDATCON,DMCB,(3,FULL),(2,HALF)                                   
         XC    HALF,=X'FFFF'                                                    
         MVC   K.SNVPMOS,HALF                                                   
*                                                                               
         MVC   K.SNVPCLT,BCLT                                                   
         MVC   K.SNVPPRD,QPRD                                                   
         MVC   K.SNVPEST,BEST                                                   
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
*                                                                               
CKP10    CLC   KEY(16),KEYSAVE     0E83/A-M/MKT/STA/MOS/CLT/PRD/EST             
         JE    EXIT                                                             
         MVC   ERROR,=Y(I2CHG)     I2 CHANGED - UPDATE MATCH DETAILS            
         GOTO1 SENDMSG                                                          
*                                                                               
         DROP  K                                                                
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* SEND REASON CODES                                               *             
*=================================================================*             
         SPACE 1                                                                
SENDRC   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,KEY                                                           
         USING RSNRECD,R2                                                       
         XC    KEY,KEY                                                          
         MVI   RSNKTYP,RSNKTYPQ                                                 
         MVI   RSNKSUB,RSNKSUBQ                                                 
         MVC   RSNKAGY,QAGY                                                     
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      SAME REC/AGY?                                
         BNE   SRCX                                                             
         LA    R1,H0EQ                                                          
         BRAS  RE,SENDH                                                         
         B     SRC20                                                            
*                                                                               
SRC10    GOTO1 SEQ                                                              
*                                                                               
SRC20    CLC   KEY(4),KEYSAVE      STILL SAME REC/AGENCY?                       
         BNE   SRCX                                                             
         DROP  R2                                                               
*                                                                               
         USING RSNRECD,R6                                                       
         GOTO1 GETREC                                                           
*                                                                               
         LHI   R1,1                SEND REASON CODE                             
         LA    R4,RSNCODE                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
         CLI   RSNELINP,C'Y'       USER INPUT REQUIRED?                         
         BNE   SRC30                NO - DON'T SEND                             
         LHI   R1,2                                                             
         LA    R4,RSNELINP         USER INPUT REQUIRED (Y/N)                    
         BRAS  RE,SENDD                                                         
*                                                                               
SRC30    B     SRC10                                                            
*                                                                               
SRCX     J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    CLI   SVRCVEL+1,H41Q                                                   
         BER   RE                                                               
         CLI   SVRCVEL+1,H42Q                                                   
         BER   RE                                                               
*                                                                               
         LR    R0,RE                                                            
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
SENDD    CLI   SVRCVEL+1,H41Q                                                   
         BER   RE                                                               
         CLI   SVRCVEL+1,H42Q                                                   
         BER   RE                                                               
*                                                                               
         LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*==============================================================*                
* FOR SPOT ELEM AT R6, CHECK IF COST OVRD APPLIES TO COST1     *                
* EXIT WITH CC NEQ   IF OVERRIDE APPLIES TO COS2 ONLY          *                
*==============================================================*                
         SPACE 1                                                                
CHKCOV   LR    RF,R6                                                            
         SR    R0,R0                                                            
*                                                                               
CHKCOV2  IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),X'10'                                                      
         JL    CKEQXIT                                                          
         CLI   0(RF),X'13'                                                      
         JL    CHKCOV2                                                          
         JH    CKEQXIT                                                          
* X'13' ELEMENT FOUND                                                           
         USING SXTELEM,RF                                                       
         TM    SXTFLAG,SXTFLAG_C2ONLY                                           
         JNZ   CKNEQXIT            SET CC NEQ TO IGNORE OVERRIDE                
*                                                                               
CKEQXIT  CR    RB,RB                                                            
         BR    RE                                                               
CKNEQXIT LTR   RB,RB                                                            
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
*==============================================================*                
* TEST BUY AGAINST FILTERS                                     *                
*==============================================================*                
         SPACE 1                                                                
         USING BUYRECD,R8                                                       
FILTBUY  CLI   FILTDAY,0           IS THERE A DAY FILTER?                       
         JE    FB10                 NO                                          
         SR    R1,R1                                                            
         IC    R1,FILTDAY          TEST MATCH ON DAY                            
*                                                                               
         BASR  RF,0                DON'T SEPERATE THESE INSTRUCTIONS!           
         EX    R1,8(RF)                                                         
*                                                                               
         J     *+8                                                              
         TM    BDDAY,0             *** EXECUTED ***                             
         JZ    FBNEQXIT            SET CC NEQ TO SKIP BUY                       
*                                                                               
FB10     OC    FILTTIM,FILTTIM                                                  
         JZ    FB20                                                             
*                                                                               
         SR    R1,R1                                                            
         SR    RF,RF                                                            
         LA    R0,500              SET RADIO START TIME                         
         CLI   QMED,C'R'                                                        
         JE    *+16                                                             
         CLI   QMED,C'X'                                                        
         JNE   *+8                                                              
         LA    R0,600              OTHER MEDIA START AT 6AM                     
*                                                                               
         ICM   R1,3,BDTIMST                                                     
         CR    R1,R0                                                            
         JNL   *+8                                                              
         AHI   R1,2400                                                          
*                                                                               
         ICM   RF,3,BDTIMEND                                                    
         JNZ   *+8                                                              
         ICM   RF,3,BDTIMST                                                     
         CR    R1,RF                   TEST START > END TIME                    
         JNH   *+8                                                              
         AHI   RF,2400                                                          
*                                                                               
         CLM   RF,3,FILTTIM        END TIME BEFORE FILTER START?                
         JL    FBNEQXIT             YES - SET CC NEQ TO SKIP BUY                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,FILTTIM+2      END TIME                                     
         JNZ   *+8                                                              
         ICM   R0,3,FILTTIM        SINGLE TIME - USE START TIME                 
*                                                                               
         CR    R1,R0               START TIME AFTER FILTER END?                 
         JH    FBNEQXIT             YES - SET CC NEQ TO SKIP BUY                
*                                                                               
FB20     CLI   FILTDPT,0           ANY DAYPART FILTER?                          
         JE    FB30                 NO                                          
         CLC   FILTDPT,BDDAYPT                                                  
         JNE   FBNEQXIT            SET CC NEQ TO SKIP BUY                       
*                                                                               
FB30     OC    FILTPROG,FILTPROG   ANY PROGRAM FILTER?                          
         JZ    FBEQXIT              NO                                          
         IC    R1,FILTPRGL                                                      
*                                                                               
         BASR  RF,0                DON'T SEPERATE THESE INSTRUCTIONS!           
         EX    R1,8(RF)                                                         
*                                                                               
         J     *+10                                                             
         CLC   BDPROGRM(0),FILTPROG     *** EXECUTED ***                        
         JNE   FBNEQXIT            SET CC NEQ TO SKIP BUY                       
*                                                                               
FBEQXIT  CR    RB,RB                                                            
         BR    RE                                                               
FBNEQXIT LTR   RB,RB                                                            
         BR    RE                                                               
         DROP  R8                                                               
         EJECT                                                                  
*==============================================================*                
* INTERFACE TO STAPACK TO GET STATION CALL LETTERS             *                
* ON ENTRY R1 POINTS TO PACKED MKT/STA                         *                
*==============================================================*                
         SPACE 1                                                                
STAUNPK  NTR1  BASE=*,LABEL=*                                                   
         XC    STABLK,STABLK                                                    
         LA    R4,STABLK                                                        
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
*                                                                               
         MVI   STAPCTRY,C'U'                                                    
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         MVC   STAPMKST,0(R1)      MOVE MARKET/STATION                          
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(5),STAPQSTA                                                 
         MVC   WORK+5(3),STAPQNET                                               
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* SEND CABLE NETWORK LIST FOR CROSS NETWORK MAKEGOOD SUPPORT          *         
*=====================================================================*         
PROCST   NTR1  BASE=*,LABEL=*                                                   
         CLI   SVRCVEL+1,H3FQ      SEARCH REQUEST?                              
         JE    EXIT                YES                                          
         CLI   QSTA,C'0'           CABLE REQUEST?                               
         JL    EXIT                NO                                           
*                                                                               
         L     R2,AIO2             A(MASTER RECORD)                             
         USING STARECD,R2          CAN HAVE UP TO 127 NETWORKS TO PASS          
         CLI   STAKTYPE,STAKTYPQ   HAVE A MASTER RECORD IN AIO2?                
         BNE   *+14                NO - READ IT INTO AIO2                       
         CLC   STAKCLT,=C'000'     HAVE STATION LEVEL MASTER RECORD?            
         BE    PROCST0             YES                                          
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R2,KEY              R2 = KEY                                     
         MVI   STAKTYPE,STAKTYPQ   C'S'                                         
         MVC   STAKMED,QMED        MEDIA                                        
         MVC   STAKCALL,QSTA       STATION                                      
         CLI   STAKCALL+4,C' '     MEDIA SET?                                   
         BH    *+10                YES                                          
         MVC   STAKCALL+4(1),QMED  NO - SET IT FROM QMED                        
         MVC   STAKAGY,QAGY        AGENCY                                       
         MVC   STAKCLT,=C'000'     WANT STATION LEVEL MASTER RECORD!            
         MVC   STAKFILL,=C'000'    FILL                                         
         L     R2,AIO2             USE AIO2                                     
         ST    R2,AIO              READSTA USES AIO                             
         GOTO1 READSTA             READ THE MATER RECORD                        
*                                                                               
PROCST0  OC    SCBL24(209),SCBL24  ANYTHING IN NETWORK LIST?                    
         JZ    EXIT                NO - NOTHING TO SEND                         
*                                                                               
         L     R5,AIO3             BUILD CABLE LIST HERE                        
         XCEF  (R5),382            CLEAR FOR 382 BYTES                          
         OC    SCBL24,SCBL24       ANYTHING IN TOP 24 LIST?                     
         JZ    PROCST02            NO                                           
         LA    R4,1                SET NETWORK POSITION FOR TOP24               
         MVC   FULL(3),SCBL24      FIRST LIST THE TOP 24                        
         LA    R6,FULL             TOP 24 BINARY LIST                           
*                                                                               
PROCST00 TM    0(R6),X'80'         BIT ON AT THIS POSITION?                     
         BZ    PROCST01            IF NO, GET TO NEXT ONE                       
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         MVI   WORK+2,X'F0'        GENERIC CABLE STATION                        
         STC   R4,WORK+4           TOP 24 NETWORK BIT                           
         LA    R1,WORK             PACKED MKT/STA                               
         BRAS  RE,STAUNPK          UNPACK IT                                    
         MVC   0(3,R5),WORK+5      NETWORK NAME TO PASS                         
         LA    R5,3(R5)            NEXT NETWORK NAME GOES HERE                  
*                                                                               
PROCST01 ZICM  RE,FULL,3           TOP 24 BINARY LIST                           
         SLL   RE,1                WE JUST PROCESSED THE LOST BIT               
         STCM  RE,7,FULL           NEXT TOP 24 IN HOB                           
         AHI   R4,1                NEXT NETWORK POSITION                        
         OC    FULL(3),FULL        ANY NETWORKS LEFT IN TOP24?                  
         BNZ   PROCST00            YES                                          
*                                                                               
PROCST02 LA    R6,SCBLSEQ          REST OF THE CABLE LIST                       
         LA    R7,206(R6)          END OF THE LIST                              
         DROP  R2                  DROP MASTER RECORD USING                     
*                                                                               
         LA    R2,25               NETWORK POSITION COUNTER                     
PROCST03 OC    0(2,R6),0(R6)       ANY MORE NETWORKS?                           
         BZ    PROCST05            NO                                           
         CR    R6,R7               END OF LIST?                                 
         BE    PROCST05            YES                                          
         CLC   =X'FFFF',0(R6)      PLACEHOLDER?                                 
         BE    PROCST04            YES - DO NOT PASS IT                         
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK+2(3),BSTA      STATION                                      
         STC   R2,BYTE             NETWORK NUMBER                               
         OC    WORK+4(1),BYTE      NETWORK                                      
         LA    R1,WORK             PACKED MKT/STA                               
         BRAS  RE,STAUNPK          UNPACK IT                                    
         MVC   0(3,R5),WORK+5      NETWORK NAME TO PASS                         
         LA    R5,3(R5)            NEXT NETWORK NAME GOES HERE                  
*                                                                               
PROCST04 LA    R6,2(R6)            ADVANCE TO NEXT NETWORK                      
         AHI   R2,1                BUMP TO NEXT NETWORK                         
         B     PROCST03            GET NEXT NETWORK NAME                        
*                                                                               
PROCST05 L     R2,AIO3             START OF CABLE LIST                          
         SR    R5,R2               GET LENGTH OF CABLE LIST IN R5               
         LR    R6,R5               SAVE LENGTH OF CABLE LIST                    
         XR    R4,R4               CLEAR R4                                     
         LA    R7,3                DIVIDE BY 3                                  
         DR    R4,R7               R5 = NUMBER OF NETWORKS IN THE LIST          
*                                                                               
         L     RF,ACOMFACS         A(COMFACS)                                   
         L     RF,CXSORT-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,(0,0(R2)),(R5),3,3,0                                   
*                                                                               
         LA    R1,H03Q             HEADER 3                                     
         BRAS  RE,SENDH            SEND IT                                      
*                                                                               
         LR    R5,R6               NETWORK LIST LENGTH                          
         LA    R1,24               MAPCODE 24                                   
         L     R4,AIO3             DATA                                         
         CHI   R5,252              LENGTH > 252                                 
         BNH   *+8                 NO                                           
         LA    R5,252              SET R5 TO MAX LENGTH                         
         BRAS  RE,SENDD            SEND IT                                      
*                                                                               
         SHI   R6,252              SUBTRACT 255 FROM ORIGINAL LENGTH            
         JNP   EXIT                IF <= 0 EXIT                                 
         LA    R1,24               MAPCODE 24                                   
         LA    R4,252(R4)          SEND THE REST OF THE NETWORKS                
         LR    R5,R6               LENGTH OF THE REST OF THE NETWORKS           
         BRAS  RE,SENDD            SEND IT                                      
         J     EXIT                DONE                                         
         LTORG                                                                  
***********************************************************************         
* MODIFY DECIMAL PRECISION FOR EACH DEMO PASSED TO MATCH REPORTING    *         
* PRECISION.                                                          *         
* DUB CONTAINS DEMO VALUE                                             *         
***********************************************************************         
*                                                                               
ADJPREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   BYTE,DUB            SAVE OVERRIDE/2DEMO FLAGS                    
         NI    DUB,X'3F'           DROP FLAGS FROM VALUE                        
*                                                                               
         TM    BYTE,X'40'          DEMO HAVE 2 DEC                              
         BO    ADJPREC2            YES                                          
*                                                                               
         CLI   PROF00+9,C'Y'       AGY USES 2 DEC                               
         BNE   ADJPREC4             NO - LEAVE IT ALONE                         
*                                                                               
         L     R0,DUB              ADJUST 1 DECIMAL PRECISION TO 2              
         MHI   R0,10                                                            
         ST    R0,DUB                                                           
         B     ADJPREC4                                                         
*                                                                               
* DEMO HAS 2 DEC                                                                
ADJPREC2 CLI   PROF00+9,C'Y'       AGY USED 2 DEC                               
         BE    ADJPREC4             YES - LEAVE IT ALONE                        
*                                                                               
         L     R0,DUB              ADJUST 2 DECIMAL PRECISION TO 1              
         SRDA  R0,31               R1 = 2*FULL                                  
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,DUB                                                           
*                                                                               
ADJPREC4 TM    BYTE,X'80'          TEST OVERRIDE FLAG SET                       
         BZ    *+8                                                              
         OI    DUB,X'80'           SET IT BACK ON                               
*                                                                               
ADJPX    J     EXIT                                                             
*                                                                               
         LTORG                                                                  
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
RTYPES   DS    0CL(L'BDCIND)       (SO A PANSCAN WILL FIND THIS TABLE!)         
         DC    AL1(BDCF)                                                        
         DC    AL1(BDCQ)                                                        
         DC    AL1(BDCGRS)                                                      
         DC    AL1(BDCV)                                                        
         DC    AL1(BDCN)                                                        
         DC    AL1(BDCS)                                                        
         DC    AL1(BDCX)                                                        
         DC    AL1(BDCC)                                                        
         DC    AL1(BDCNTP)                                                      
RTYPESLQ EQU   *-RTYPES                                                         
*                                                                               
SPTBLKD  DSECT                                                                  
       ++INCLUDE SPOTBLOCK                                                      
         EJECT                                                                  
       ++INCLUDE SPMAKWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
DEMELADR DS    A                                                                
STABLK   DS    XL24                                                             
BTKEYSV  DS    XL12                                                             
SPOTDATE DS    H                                                                
SPOTNUM  DS    H                                                                
OTONUM   DS    H                                                                
*                                                                               
FILTDAY  DS    X                                                                
FILTTIM  DS    XL4                                                              
FILTDPT  DS    CL1                                                              
FILTPROG DS    CL17                                                             
FILTPRGL DS    X                   PROGRAM FILTER LEN (-1)                      
FILTCOST DS    F                                                                
*                                                                               
INVSW    DS    C                   Y/N IF INVOICE HEADER SENT                   
INVHDPRD DS    X                   INVOICE HEADER PRD                           
INVHDPR2 DS    X                                                                
INVHDEST DS    X                                                                
INVSDATE DS    CL8                                                              
INVCOSTY DS    CL1                                                              
INVHDINV DS    CL10                                                             
INVHDEZS DS    CL4                                                              
INVHDCON DS    CL12                CONTRACT NUMBER                              
INVHDREP DS    CL3                 REP CODE                                     
INVHDSEQ DS    X                   INVOICE SEQUENCE NUMBER                      
*                                                                               
INVEOUID DS    XL2                 USER ID                                      
INVEOSTA DS    XL5                 STATION                                      
INVEODAT DS    XL2                 EASI BATCH DATE                              
INVEOQ   EQU   *-INVEOUID                                                       
*                                                                               
* PARMS TO FILMCK                                                               
FLMDATE  DS    XL2                                                              
FLMPRD   DS    X                                                                
FLMPIG   DS    X                   PIGGYBACK PRD                                
FLMTIME  DS    XL2                                                              
FLMSLN   DS    X                   FILM SPOT LENGTH                             
*                                                                               
BPERDEF  DS    XL2                 BPER START FOR DEFERRED SPOTS                
BUYPRD2  DS    X                                                                
CBLNET   DS    XL1                                                              
EDSAVE   DS    XL17                EDSAVE USED BY SETNUM                        
EDSAVEP  DS    XL17                MASK FOR POSITIVE NUMBERS ONLY               
EDSAVEM  DS    XL17                MASK FOR POS & NEG NUMBERS                   
*                                                                               
         SPACE 1                                                                
*===============================================================*               
* DSECTS FOR TSAR RECORDS                                       *               
*        S = SPOT RECORDS                                       *               
*        U = UNMATCHED INVOICES RECORDS                         *               
*===============================================================*               
         SPACE 1                                                                
         ORG   TSARREC                                                          
BTREC    DS    0D          *****   SPOT RECORD                                  
BTKEY    DS    0XL12                                                            
BTTYPE   DS    XL1                 S=SPOT                                       
BTEST    DS    XL1                 ESTIMATE                                     
BTLIN    DS    XL2                 LINE                                         
BTCBLNET DS    XL1                 CABLE NETWORK BYTE                           
BTDATE   DS    XL2                 DATE                                         
BTSPNUM  DS    XL1                 SEQNUM WITHIN DATE                           
BTSEQ    DS    XL2                 BUY SEQUENCE NUMBER                          
         DS    XL2                                                              
BTKEYX   EQU   *                                                                
*                                                                               
BTPRD    DS    XL1                                                              
BTPRD2   DS    XL1                                                              
BTEST2   DS    XL1                                                              
BTPAIDDT DS    XL2                                                              
*                                                                               
BTINDS   DS    XL1                                                              
BTIND_PAID   EQU X'80'                                                          
BTIND_OTO    EQU X'40'                                                          
BTIND_COSTOV EQU X'20'                                                          
BTIND_MISSED EQU X'10'                                                          
*                                                                               
BTMKGD   DS    CL2                                                              
BTGROSS  DS    XL4                                                              
BTNET    DS    XL4                                                              
BTTAX    DS    XL4                                                              
*                                                                               
BTAFFILM DS    XL1                 INTERNAL INV SEQ CODE                        
BTAFFLM2 DS    XL1                                                              
BTAFFSLN DS    XL1                                                              
BTAFDEMO DS    XL4                                                              
BTAFGRS  DS    XL4                                                              
BTAFEST  DS    X                                                                
*                                                                               
BTAFFDT  DS    XL2                                                              
BTAFFTM  DS    XL2                                                              
BTAFFSEQ DS    XL1                 SEQNUM FOR MULTIPLES                         
BTAFFORB DS    XL1                                                              
BTINVSEQ DS    X                   INVOICE SEQ NUM                              
*                                                                               
* MAKE SURE FLM CODES STAY RIGHT BEFORE AFFINDS                                 
BTTRFLM  DS    XL2                 TRAFFIC FILM CODE (FROM INVOICE)             
BTTRFLM2 DS    XL2                                                              
BTAFFIND DS    X                   AS DEFINED IN AFINDS                         
*AFIND_MG   EQU X'80'              AFFID INPUT AS MG                            
*AFIND_SIQ  EQU X'40'              SKIP INTERVAL CK (SEPERATION CK)             
*AFIND_EM   EQU X'20'              EMAIL SENT                                   
*AFIND_CH   EQU X'10'              MM CHANGED RNO COST                          
*AFIND_NEG  EQU X'08'              NEGATIVE AFFID COST                          
*AFIND_IGF  EQU X'04'              IGNORE FILM CODE FOR MATCHING                
*AFIND_IGC  EQU X'02'              IGNORE COST FOR MATCHING                     
*AFIND_IGT  EQU X'01'              IGNORE TIME FOR MATCHING                     
*                                                                               
BTAFFIN2 DS    X                   AS DEFINED IN AFIND2                         
*AFIN2_IGS  EQU X'80'              IGNORE SPOT LENGTH FOR MATCHING              
BTRECX   EQU   *                                                                
         SPACE 1                                                                
         ORG   TSARREC                                                          
AFREC    DS    0D          *****   UNMATCHED INVOICE RECORD                     
AFKEY    DS    0XL12                                                            
AFTYPE   DS    XL1                 U = UNMATCHED AFFID                          
AFDATE   DS    XL2                                                              
AFTIME   DS    XL2                                                              
AFSEQ    DS    XL1                 SEQNUM FOR DUPS                              
AFPRD    DS    XL1                                                              
AFEST    DS    XL1                                                              
AFCBLNET DS    XL1                                                              
AFAFFORB DS    XL1                                                              
AFINVSEQ DS    X                   INVOICE SEQ NUM                              
         DS    XL1                 SPARE                                        
AFKEYX   EQU   *                                                                
*                                                                               
AFGROSS  DS    XL4                                                              
AFNET    DS    XL4                                                              
AFFILM   DS    XL1                 INTERNAL INV SEQ CODE                        
AFFILM2  DS    XL1                                                              
AFDEMO   DS    CL4                                                              
AFSLN    DS    XL1                                                              
AFPRD2   DS    XL1                                                              
*                                                                               
* MAKE SURE FLM CODES STAY RIGHT BEFORE AFFINDS                                 
AFTRFLM  DS    XL2                 TRAFFIC FILM CODE (FROM INVOICE)             
AFTRFLM2 DS    XL2                                                              
AFINDS   DS    XL1                                                              
AFIND_MG   EQU X'80'               AFFID INPUT AS MG                            
AFIND_SIQ  EQU X'40'               SKIP INTERVAL CK (SEPERATION CK)             
AFIND_EM   EQU X'20'               EMAIL SENT                                   
AFIND_CH   EQU X'10'               MM CHANGED RNO COST                          
AFIND_NEG  EQU X'08'               NEGATIVE AFFID COST                          
AFIND_IGF  EQU X'04'               IGNORE FILM CODE FOR MATCHING                
AFIND_IGC  EQU X'02'               IGNORE COST FOR MATCHING                     
AFIND_IGT  EQU X'01'               IGNORE TIME FOR MATCHING                     
*                                                                               
AFIND2   DS    XL1                                                              
AFIN2_IGS  EQU X'80'               IGNORE SPOT LENGTH FOR MATCHING              
*                                                                               
AFRECX   EQU   *                                                                
         ORG                                                                    
*                                                                               
FLMTABD  DSECT                                                                  
FLMCOD   DS    CL12                ISCI/AD-ID CODE                              
FLMSEQ   DS    XL2                 SNV SEQ CODE                                 
FLMSTAT  DS    X                                                                
FLMSINV  EQU   X'80'               INVALID FILM                                 
FLMCMHD  EQU   SNVCMHDQ    X'01'   THIS AD-ID IS HI-DEF                         
FLMTABLQ EQU   *-FLMTABD                                                        
*                                                                               
* TABLE OF COMMERCIAL RECALL/RELEASE DATES                                      
CMLTABLD DSECT                                                                  
CMLTSEQ  DS    XL2                                                              
CMLTREL  DS    XL2                                                              
CMLTRCL  DS    XL2                                                              
CMLTFLAG DS    X                                                                
CMLTFDAY EQU   X'80'               CHECK TIMES DAILY                            
CMLTSTIM DS    XL2                 START TIME                                   
CMLTETIM DS    XL2                 END TIME                                     
CMLTSLN  DS    X                   SPOT LENGTH                                  
CMLTPRDS DS    12X                 ROOM FOR FIRST 12 CML PRDS                   
CMLTPRDQ EQU   (*-CMLTPRDS)/(L'CMLTPRDS)   NUMBER OF PRDS TO TEST               
*                                                                               
CMLTFLDT DS    0XL4                                                             
CMLTFLSD DS    XL2                 CML FLIGHT START/END DATES                   
CMLTFLED DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
CMLTFLDQ EQU   (*-CMLTFLDT)/(L'CMLTFLDT)   NUMBER OF CML FLT DATES              
*                                                                               
CMLTABLN EQU   *-CMLTABLD                                                       
*                                                                               
CMLSUBD  DSECT                     CMML BRAND SUBSTITUTION TABLE                
CMLSSEQ  DS    XL2                                                              
CMLSPRQ  EQU   15                  MAX SUBSTITUTION BRANDS                      
CMLSSUBS DS    (CMLSPRQ)XL(CMLSSUBQ)                                            
         ORG   CMLSSUBS                                                         
CMLSPRD  DS    CL3                 SUBSTITUTION BRAND                           
CMLSSDT  DS    XL3                 START DATE                                   
CMLSEDT  DS    XL3                 END DATE                                     
CMLSSUBQ EQU   L'CMLSPRD+L'CMLSSDT+L'CMLSEDT                                    
         ORG                                                                    
CMLSUBLN EQU   *-CMLSUBD                                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPGENRSN                                                       
       ++INCLUDE SPSTAPACKD                                                     
XCOMRECD DSECT                                                                  
       ++INCLUDE SPGENXCOM                                                      
         DSECT                                                                  
       ++INCLUDE SPGETBUBLD                                                     
       ++INCLUDE SPMGADN                                                        
       ++INCLUDE SPGENPURP                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'103SPMAK10   09/16/20'                                      
         END                                                                    
