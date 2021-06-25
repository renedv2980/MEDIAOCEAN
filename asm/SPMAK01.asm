*          DATA SET SPMAK01    AT LEVEL 047 AS OF 09/21/20                      
*PHASE T22801A                                                                  
T22801   TITLE 'SPMAK01 - SPOT MATCHMAKER - DOWNLOAD HEADERS'                   
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-46140  09/21/20 PASS IN REC LENGTH OVERRIDE TO RECUP      *         
* AKAT SPEC-20941  02/27/18 INCREASE TSPAGN FROM 8 TO 40 PAGES        *         
***********************************************************************         
*===============================================================*               
*                                                               *               
* HISTORY                                                       *               
* -------                                                       *               
*                                                               *               
* WHEN    LEV WHAT                                              *               
* ----    --- ----                                              *               
* 08AUG08 043 SUPPORT CLIENT GROUPS ON SUPERVISOR REC           *               
* 13MAY08 042 CHANGE INVOICE SEARCH TO USE PRD/EST FROM MM STAT *               
*             INSTEAD OF HEADER                                 *               
* 28OCT06 039 SUPPORT SEARCH FOR INVOICES                       *               
* 23OCT06 038 DON'T DIE IN RECUP WHEN TOO MANY MKTS IN GROUP    *               
* 11AUG06 037 SUPPORT SENDING ERROR FLAGS                       *               
* 30AUG05 036 UPGRADE MINIMUM VERSION NUMBERS                   *               
* 18APR05 035 CHECK FOR NO INVOICES PASSED TO PC                *               
* 09JUL04 034 EARNED DISCOUNT/COST2                             *               
*         --- 2 DECIMAL DEMOS                                   *               
* 07MAY04 033 UNFIX NEW TEST FOR CABLE!                         *               
* 23MAR04 032 NEW TESTS FOR CABLE                               *               
* 22OCT03 031 GIVE ERROR FOR MISSING SUPERVISOR                 *               
* 15JAN03 030 READ INVOICE PASSIVES BY BUYER CODE               *               
* 27JUN02 029 VERSION 3.0                                       *               
*         --- TEST SEND APPROVED                                *               
*         --- ONLY SEND INVOICES BETWEEN BPER & BPER+2          *               
*         --- SEND MKPROF+8 WITH INVOICES                       *               
*         --- WARN ON VERSIONS UNDER 2.5                        *               
* 13JUN02 028 CLIENT STRING SECURITY                            *               
* 19FEB02 027 UPGRADE MINIMUM VERSION NUMBERS                   *               
* 16SEP01 026 CHECK FOR TSAR OVERFLOW ERROR ON TSADD            *               
* 07SEP01 025 AUTHCLT - LET MKT LIMIT ACCESS PASS               *               
*                                                               *               
*===============================================================*               
         SPACE 2                                                                
T22801   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMK01**                                                       
*                                                                               
         LR    RC,R1     **NOTE NOTE NOTE YOU IDIOT **                          
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         BASR  R9,0                                                             
         AHI   R9,GLOBALS-*                                                     
         USING GLOBALS,R9                                                       
*                                                                               
*=================================================================*             
* VERSION CHECK                                                   *             
*    FIRST CHECK IS FOR MINIMUM REQUIRED,                         *             
*    SECOND IS NEED TO UPGRADE SOON                               *             
*=================================================================*             
         CLC   VERSION(2),=X'0205'  VERSION 2.5 AND HIGH ONLY                   
         BNL   S50                                                              
         LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         MVC   FAMSGXTR(2),=X'0205'                                             
         GOTO1 AADDDATA,DMCB,AFABLK,FALAYRF,FALAYRF                             
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
S50      TM    FLAGS,FLSKVRSN      SKIP WARNING CHECK?                          
         BNZ   S60                  YES                                         
         CLC   VERSION(2),=X'0300' VERSION 3.0 AND HIGH ONLY                    
         BNL   S60                                                              
         LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         MVC   FAMSGXTR(2),=X'0303'  UPGRADE TO 3.3                             
         GOTO1 AADDDATA,DMCB,AFABLK,FALAUSN,FALAUSN                             
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
         USING TSARD,TSARBLK                                                    
         SPACE 1                                                                
*=================================================================*             
* H1 DATA - SEND ONLY PROFMK+8                                    *             
*           REST GETS SENT LATER WHEN CLIENT IDENTIFIED           *             
*=================================================================*             
         SPACE 1                                                                
S60      CLI   SVRCVEL+1,H30Q      IS THIS A SEARCH?                            
         BE    S108                 YES SKIP GROUP/BUYER                        
         LA    R1,H01Q             LOCATE 01 HEADER                             
         BAS   RE,SENDH                                                         
         LA    R1,H01MK09          APPROVE INVOICES                             
         LA    R4,PROFMK+8                                                      
         BRAS  RE,SENDD                                                         
         SPACE 1                                                                
*=================================================================*             
* H2 DATA                                                         *             
*=================================================================*             
         SPACE 1                                                                
         LA    R1,H02Q             LOCATE 02 HEADER                             
         BAS   RE,SENDH                                                         
* READ THE OFFICE RECORD                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D60'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         NI    KEY+2,X'F0'         DROP MEDIA CODE                              
         MVC   KEY+3(2),QGRP       BUYING GROUP = OFFICE                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    S70                                                              
         MVC   ERROR,=Y(BADOFC)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
S70      L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         USING OFCRECD,R6                                                       
         LA    R1,OFCEL                                                         
         MVC   SVOFCNAM(24),2(R1)                                               
*                                                                               
         LA    R1,H02OFC                                                        
         LA    R4,SVOFCNAM                                                      
         BAS   RE,SENDD                                                         
         DROP  R6                                                               
         EJECT                                                                  
* READ THE BUYER RECORD                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D62'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         NI    KEY+2,X'F0'         DROP MEDIA CODE                              
         MVC   KEY+3(2),QGRP       BUYING GROUP = OFFICE                        
         MVC   KEY+5(4),QBYR                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    S80                                                              
         MVC   ERROR,=Y(BADBYR)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
S80      L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING BYRRECD,R6                                                       
*                                                                               
         LA    R6,BYREL                                                         
         USING BYRNAMED,R6                                                      
*                                                                               
         MVC   SVBYRNAM,SPACES                                                  
         MVC   SVBYRNAM(L'BYRFNAME),BYRFNAME                                    
         LA    R4,SVBYRNAM+L'BYRFNAME+1                                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         AHI   R4,2                                                             
         MVC   0(14,R4),BYRLNAME                                                
         MVC   SVBYRFLT,BYRFILT                                                 
         MVC   SVBYROP1,BYROPT1                                                 
         MVC   FULL,BYRSPV         SAVE SUPERVISOR CODE FOR GETSPV              
*                                                                               
         LA    R1,H02BYR                                                        
         LA    R4,SVBYRNAM                                                      
         BAS   RE,SENDD                                                         
         SPACE 1                                                                
*=================================================================*             
* READ SUPERVISOR RECORD                                          *             
*=================================================================*             
         SPACE 1                                                                
         BAS   RE,GETSPV                                                        
         SPACE 1                                                                
*=================================================================*             
* CHECK CONNECT PASSWORD MATCHES BUYER OR SUPERVISOR PASSWORD     *             
*=================================================================*             
         SPACE 1                                                                
         CLI   DDS,C'Y'            DDS TERM?                                    
         BE    S100                 YES - ALL ACCESS                            
         CLI   PROFMK+5,C'Y'       USE SECURITY?                                
         BNE   S100                 NO                                          
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',FULL),F#TPASS   EXTRACT CONNECT PWD         
         OC    FULL(2),FULL        ON SECURITY?                                 
         BZ    S100                 NO                                          
*                                                                               
         USING BPIDELD,R6                                                       
         L     R6,AIO2             A(BUYER REC)                                 
*                                                                               
         MVI   ELCODE,X'22'                                                     
         BRAS  RE,GETEL                                                         
         BNE   S90                 NONE - CHECK SUPV                            
         CLC   BPIDNO,FULL         THIS USER?                                   
         BE    S100                 NO                                          
*                                                                               
S90      L     R6,AIO3             A(SUPV REC)                                  
         BRAS  RE,GETEL                                                         
         BNE   *+14                                                             
         CLC   BPIDNO,FULL                                                      
         BE    S100                                                             
*                                                                               
         MVC   ERROR,=Y(NOTAUTH)                                                
         GOTO1 SENDMSG                                                          
         DROP  R6                                                               
         SPACE 1                                                                
*=================================================================*             
* BUILD TSAR BUFFER OF RELEVANT MATCHMAKER PASSIVES               *             
*=================================================================*             
         SPACE 1                                                                
         USING BYRBBLD,R6                                                       
S100     CLI   PROFMK+4,C'Y'       POPULATE MM BY BUYER?                        
         BNE   S108                                                             
         L     R6,AIO2             A(BUYER REC)                                 
         MVI   ELCODE,BYRBBLQ      X'30'                                        
         BRAS  RE,GETEL                                                         
         BNE   *+14                                                             
         MVC   BUBBLCD,BYRBBLCD                                                 
         B     S108                                                             
*                                                                               
         MVC   ERROR,=Y(BADBYR)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* BUILD TSAR BUFFER OF RELEVANT MATCHMAKER PASSIVES               *             
*=================================================================*             
         SPACE 1                                                                
S108     MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,HKEYX-HKEY                                                
***      MVI   TSPAGN,8            REQUEST 8 PAGES                              
         MVI   TSPAGN,40           REQUEST 40 PAGES                             
         OI    TSINDS,TSINODSK     TEMPEST IS IN USE BY FALINK !!!              
         OI    TSINDS,TSIXTTWA     AND IT HAS BIG PAGES !                       
         LHI   R0,HRECX-HREC                                                    
         STH   R0,TSRECL                                                        
         LA    R1,HREC                                                          
         ST    R1,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*                                                                               
         CLI   SVRCVEL+1,H30Q      SEARCH REQUEST?                              
         BE    S140                                                             
*                                                                               
         MVI   SVBYREL,BYRMKTT                                                  
         CLI   QMED,C'T'                                                        
         BE    S110                                                             
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   *+12                                                             
         CLI   QMED,C'N'           MEDIA N USES 'T' BUYERS                      
         BE    S110                                                             
*                                                                               
         MVI   SVBYREL,BYRMKTR                                                  
         CLI   QMED,C'R'                                                        
         BE    S110                                                             
*                                                                               
         MVI   SVBYREL,BYRMKTX                                                  
         CLI   QMED,C'X'                                                        
         BE    S110                                                             
         MVC   ERROR,=Y(BADMED)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
S110     L     R6,AIO2                                                          
         MVC   ELCODE,SVBYREL                                                   
         OI    ELCODE,X'10'        CHECK FOR MKT GROUPS                         
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
S120     BRAS  RE,NEXTEL                                                        
         BNE   S140                                                             
         BAS   RE,BLDMKGRP                                                      
         B     S120                                                             
*                                                                               
K        USING SNVKEYD,KEY                                                      
*                                                                               
S140     XC    KEY,KEY             SET TO READ SNV PASSIVE KEYS                 
         MVI   K.SNVPTYP,X'0E'                                                  
         MVI   K.SNVPSUB,X'83'                                                  
         MVC   K.SNVPAM,BAGYMD                                                  
*                                                                               
         MVI   ALLMKTS,C'N'                                                     
         CLI   SVRCVEL+1,H30Q      SEARCH REQUEST?                              
         BNE   *+14                 NO                                          
         MVC   K.SNVPMKT,BMKT       YES - SET MKT                               
         B     S180                                                             
*                                                                               
         CLI   PROFMK+4,C'Y'       POPULATE MM BY BUYER?                        
         BNE   *+14                                                             
         MVC   K.SNVPBYR,BUBBLCD                                                
         B     S180                                                             
*                                                                               
         L     R6,AIO2                                                          
         MVC   ELCODE,SVBYREL                                                   
         BRAS  RE,GETEL                                                         
         BE    S150                                                             
         MVC   ERROR,=Y(NOMKTS)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
S150     CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   *+12                                                             
         CLI   QMED,C'N'           MEDIA N IS ALL MARKETS (MKT 0)               
         BE    *+14                                                             
*                                                                               
         CLC   =X'FFFF',2(R6)      TEST ALL MARKETS ELEM                        
         BNE   S170                                                             
         MVI   ALLMKTS,C'Y'                                                     
         B     S180                                                             
*                                                                               
S160     CLI   PROFMK+4,C'Y'       POPULATE MM BY BUYER?                        
         BE    S280                 YES - ALL DONE!                             
         CLI   SVRCVEL+1,H30Q      OR SEARCH REQUEST?                           
         BE    S280                 YES - ALL DONE!                             
         MVC   ELCODE,SVBYREL                                                   
         BRAS  RE,NEXTEL                                                        
         BNE   S280                                                             
*                                                                               
S170     MVC   K.SNVPMKT,2(R6)          MOVE MARKET NUMBER                      
*                                                                               
S180     MVI   XSP,C'Y'                 SET TO READ XSPDIR                      
         XC    K.SNVPSTA(27),K.SNVPSTA  CLEAR THE REST                          
S182     GOTO1 HIGH                                                             
         B     S200                                                             
*                                                                               
S190     GOTO1 SEQ                                                              
*                                                                               
S200     CLC   KEY(3),KEYSAVE      SAME TYP/A-M?                                
         BNE   S280                 NO - DONE                                   
         CLI   ALLMKTS,C'Y'        SKIP MKT CHECK FOR ALL MKTS                  
         BE    S210                                                             
         CLC   KEY+3(2),KEYSAVE+3  SAME MKT/BUYER                               
         BNE   S160                NO - NEXT MARKET                             
*                                                                               
S210     CLI   PROFMK+4,C'Y'       POPULATE MM BY BUYER?                        
         BNE   S212                 NO, GO MAKE SURE IT'S NOT A BYR KEY         
         OC    K.SNVPMKT2,K.SNVPMKT2   TEST THIS IS A BUYER KEY                 
         BZ    S190                     NO - SKIP IT                            
         B     S214                                                             
*                                                                               
S212     OC    K.SNVPMKT2,K.SNVPMKT2   TEST THIS IS A BUYER KEY                 
         BNZ   S190                     YES - SKIP IT                           
*                                                                               
S214     CLI   SVRCVEL+1,H30Q      SEARCH FUNCTION?                             
         BNE   S216                 NO                                          
         OC    QSTA,QSTA           DO WE HAVE A STATION?                        
         BZ    S230                 NO                                          
         CLC   K.SNVPSTA,BSTA       YES - DOES IT MATCH?                        
         BE    S230                 YES                                         
         BH    S280                 IF HIGH, DONE                               
         MVC   K.SNVPSTA,BSTA       ELSE SKIP TO IT                             
         XC    K.SNVPMOS(22),K.SNVPMOS                                          
         B     S182                                                             
*                                                                               
S216     CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    S230                THERE IS NO CABLE                            
         CLI   K.SNVPSTA,X'E8'     TEST CABLE STATION                           
         BH    S220                YES                                          
* THIS IS NOT A CABLE STATION                                                   
         TM    SVBYROP1,BYROPT1_CBL THIS A CABLE BUYER                          
         BZ    S230                 NO - DO ALL NON-CABLE STATIONS              
         B     S190                 ELSE SKIP NON-CABLE STATIONS                
*                                                                               
S220     CLI   PROFMK+1,C'Y'       ALL BUYERS DO CABLE                          
         BE    S230                YES                                          
* ONLY DESIGNATED BUYERS DO CABLE                                               
         TM    SVBYROP1,BYROPT1_CBL                                             
         BZ    S190                                                             
*                                                                               
S230     OC    BPER,BPER           ONLY SET IN MM 3.0+                          
         BZ    S232                SEND ALL PERIODS                             
         MVC   HALF,K.SNVPMOS                                                   
         XC    HALF,=X'FFFF'                                                    
         CLC   HALF,BPER           THIS INVOICE BEFORE START PER?               
         BL    S190                 YES - SKIP IT                               
         CLC   HALF,BPER+2         OR AFTER END?                                
         BH    S190                 YES - SKIP IT                               
*                                                                               
S232     CLI   SVRCVEL+1,H30Q      IS THIS SEARCH?                              
         BNE   S236                                                             
         CLC   K.SNVPCLT,BCLT                                                   
         BE    S270                                                             
         BH    *+14                                                             
         MVC   K.SNVPCLT,BCLT      SKIP TO IT                                   
         B     *+10                                                             
         MVC   K.SNVPCLT,=X'FF'    FORCE NEXT MOS                               
         XC    K.SNVPPRD(20),K.SNVPPRD                                          
         B     S182                                                             
*                                                                               
S236     TM    FLAGS,FLSNDMAT      TEST SEND MATCHED/APPROVED                   
         BNZ   S240                 YES                                         
         TM    K.SNVDSTAT+1,X'80'  TEST MATCHED                                 
         BO    S190                YES - SKIP                                   
*                                                                               
         CLI   PROFMK+8,C'N'       TEST AGY USES APPROVAL                       
         BE    S240                 NO                                          
         TM    K.SNVDSTAT+1,X'02'  TEST APPROVED INVOICE                        
         BZ    S240                 NO - ALWAYS SEND                            
         CLI   PROFMK+8,C'Y'       TEST SEND APPROVED                           
         BNE   S190                 NO - SKIP IT                                
*                                                                               
S240     CLI   PROFMK+2,C'Y'       TEST EVERYONE DOES UNWIRED NTWK              
         BE    S260                THEY ALL DO IT                               
         TM    K.SNVDSTAT+1,X'20'  TEST UNWIRED NETWORK BUY                     
         BO    S250                YES                                          
*                                                                               
         TM    SVBYROP1,BYROPT1_UNW  NO - IS THIS THE UNWIRED DUDE              
         BO    S190                  YES -SKIP                                  
         B     S260                                                             
*                                                                               
S250     TM    SVBYROP1,BYROPT1_UNW  IS THIS THE UNWIRED DUDE                   
         BZ    S190                  NO - SKIP                                  
*                                                                               
* BUILD TSAR RECORD                                                             
*                                                                               
S260     LA    R1,K.SNVPCLT        POINT TO CLIENT CODE                         
         BAS   RE,CLTFILT                                                       
         BNE   S190                                                             
*                                                                               
S270     XC    TSARREC,TSARREC                                                  
         MVC   HCLT,K.SNVPCLT                                                   
         MVC   HPRD,K.SNVPPRD                                                   
         MVC   HEST,K.SNVPEST                                                   
         MVC   HMKT,K.SNVPMKT                                                   
         CLI   SVRCVEL+1,H30Q      SEARCH?                                      
         BE    *+18                 YES - NO BUYER PASSIVES                     
         CLI   PROFMK+4,C'Y'       POPULATE MM BY BUYER?                        
         BNE   *+10                 NO                                          
         MVC   HMKT,K.SNVPMKT2      YES - MKT IS HERE!                          
         MVC   HSTA,K.SNVPSTA                                                   
         MVC   HMOS,K.SNVPMOS                                                   
         MVC   HPRD2,K.SNVPPRD2                                                 
         MVC   HSTAT,K.SNVDSTAT                                                 
         MVC   HINV,K.SNVPINV                                                   
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    S190                                                             
         CLI   TSERRS,TSEDUP                                                    
         BE    S190                DUPLICATE KEY ERROR OK                       
         MVC   ERROR,=Y(MANYMKT)   TOO MANY MARKETS                             
         GOTO1 SENDMSG                                                          
         EJECT                                                                  
*                                                                               
S280     OC    TWASAGN,TWASAGN     ON NEW SECURITY                              
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS  OR HAVE LIMIT ACCESS                         
         BZ    S290                                                             
         L     R0,AIO2                                                          
         LHI   R1,SECLENQ                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     RF,ACOMFACS         INITIALIZE SECURITY BLOCK                    
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',AIO2),0                                    
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         CLI   TWAACCS,C'+'        IF THEY USE MKT LIMIT ACCESS                 
         BE    *+12                 GO VALIDATE MARKETS                         
         CLI   TWAACCS+2,C'+'                                                   
         BNE   S290                                                             
         BAS   RE,AUTHMKT          GO AUTH ALL MKTS                             
*                                                                               
S290     MVI   XSP,C'N'            RE-SET READ XSPDIR                           
         MVI   TSACTN,TSAGET                                                    
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BRAS  RE,CALLTSAR                                                      
         BE    S320                                                             
         MVC   ERROR,=Y(NOINVS)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
S300     MVI   TSACTN,TSANXT                                                    
*                                                                               
S310     BRAS  RE,CALLTSAR                                                      
         BNE   S440                                                             
*                                                                               
S320     BRAS  RE,AUTHCLT          MAKE SURE CLT IS AUTH FOR THIS ID            
         BE    S330                 YES                                         
         MVC   HALF,TSRNUM                                                      
         MVI   TSACTN,TSADEL        NO - DELETE IT                              
         BRAS  RE,CALLTSAR                                                      
         MVC   TSRNUM,HALF         AND GET SEQUENCE BACK                        
         MVI   TSACTN,TSAGET                                                    
         B     S310                                                             
*                                                                               
S330     LA    R1,H03Q                                                          
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R1,H03MKT                                                        
         LA    R4,HMKT                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         BAS   RE,STAUNPK                                                       
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BE    S340                 NO CABLE                                    
         CLI   WORK,C'0'           TEST CABLE STATION                           
         BL    S340                 NO                                          
         CLC   WORK+5(3),SPACES    IS THERE A NETWORK?                          
         BNH   S340                 NO                                          
         MVI   WORK+4,C'/'                                                      
*                                                                               
S340     LA    R1,H03STA                                                        
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VCLUNPK,DMCB,(SVCPROF+6,HCLT),WORK                               
         LA    R1,H03CLT                                                        
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R1,H03PRD                                                        
         LA    R4,HPRD                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R1,H03PR2                                                        
         LA    R4,HPRD2                                                         
         CLC   0(3,R4),SPACES                                                   
         BNH   *+8                                                              
         BAS   RE,SENDD                                                         
* DECODE MOS                                                                    
         MVC   HALF,HMOS                                                        
         XC    HALF,=X'FFFF'                                                    
         GOTO1 VDATCON,DMCB,(2,HALF),(X'20',WORK)  GET YYMMDD                   
         LA    R1,H03MOS                                                        
         LA    R4,WORK                                                          
         LA    R5,4                HABES WANTS YYMM                             
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R1,H03ESRNG                                                      
         XC    WORK,WORK                                                        
         SR    R0,R0                                                            
         ICM   R0,1,HEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         ICM   R0,1,HEST2                                                       
         BZ    S350                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   WORK+3,C'-'                                                      
         UNPK  WORK+4(3),DUB                                                    
*                                                                               
S350     LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
* READ FOR POL ESTHDR AND SET POL/NON-POL FLAG                                  
         XC    WORK,WORK                                                        
         USING H03FLAGD,R4                                                      
*                                                                               
         MVI   H03F_POL,C'Y'       SET DEFAULT TO POL                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),HCLT       MOVE PACKED CLIENT CODE                      
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),HEST       SET LOW EST NUMBER                           
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      TEST POL EST FOUND                           
         BE    *+8                 YES                                          
         MVI   H03F_POL,C'N'                                                    
*                                                                               
         MVI   H03F_STAT,C'M'      SET MATCHED                                  
         TM    HSTAT2,X'80'                                                     
         BO    S360                                                             
         MVI   H03F_STAT,C'W'      SET WIP                                      
         TM    HSTAT2,X'40'                                                     
         BO    S360                                                             
         MVI   H03F_STAT,C' '                                                   
*                                                                               
S360     MVI   H03F_APP,C'N'       SET NOT APPROVED                             
         CLI   PROFMK+8,C'N'       TEST AGY USES APPROVAL                       
         BE    S370                 NO - FLAG IS ALWAYS NO                      
         TM    HSTAT2,X'02'        IS IT APPROVED?                              
         BZ    S370                 NO                                          
         MVI   H03F_APP,C'Y'       SET APPROVED                                 
*                                                                               
S370     LA    R1,H03FLAGS                                                      
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R1,H03STAT1                                                      
         LA    R4,HSTAT1                                                        
         BAS   RE,SENDD                                                         
*                                                                               
         CLI   SVRCVEL+1,H30Q      IS THIS A SEARCH?                            
         BNE   S400                 NO - ALL DONE                               
         ZAP   HDCOST,=P'0'                                                     
         XC    HDSPOTS,HDSPOTS                                                  
         MVI   SVPAID,C'P'                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   K.SNVKTYPE,SNVKTYPQ                                              
         MVI   K.SNVKSUB,SNVKSUBQ                                               
         MVC   K.SNVKAM,BAGYMD                                                  
         MVC   K.SNVKCLT,HCLT                                                   
         MVC   K.SNVKSTA,HSTA                                                   
         MVC   K.SNVKMOS,HMOS                                                   
*                                                                               
S372     MVI   XSP,C'Y'                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   XSP,C'N'                                                         
         CLC   KEY(SNVKINV-SNVKMAST),KEYSAVE  STILL SAME AM/CLT/STA/MOS         
         BNE   S390                            NO                               
*                                                                               
         L     R6,AIO1              YES - READ INVOICE TO GET SPOTS/$           
         ST    R6,AIO                                                           
         MVI   XSP,C'Y'                                                         
         GOTO1 GETREC                                                           
         MVI   XSP,C'N'                                                         
         USING SNVKEYD,R6                                                       
         LA    R6,SNVELS                                                        
*                                                                               
         XR    R0,R0                                                            
S374     CLI   0(R6),0             MAKE SURE AT LEAST ONE DETAIL ELEM           
         BE    S388                IF NOT, SKIP INVOICE                         
         CLI   0(R6),SNVIDELQ      TEST DETAIL ELEM (X'40')                     
         BE    S376                                                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     S374                                                             
*                                                                               
S376     L     R6,AIO1                                                          
         LA    R6,SNVELS                                                        
*                                                                               
S378     CLI   0(R6),0                                                          
         BE    S388                                                             
         CLI   0(R6),SNVHDELQ      TEST HEADER ELEM (X'10')                     
         BE    S380                                                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     S378                                                             
*                                                                               
         USING SNVHDELD,R6                                                      
S380     DS    0H                                                               
         TM    SNVHDCTL,SNVHDMCQ   SKIP MIDFLIGHT CLEARANCES                    
         BNZ   S388                                                             
         MVC   DUB,SNVHDTCS               SAVE TOTAL COST                       
         MVC   HALF,SNVHDTSP              SAVE TOTAL SPOTS                      
         MVC   WORK(L'SNVHDCON),SNVHDCON  SAVE CONTRTACT NUMBER                 
         MVC   BYTE,SNVHDCTL              SAVE STATUS BYTE (FOR PAID)           
*                                                                               
         L     R6,AIO1                                                          
         CLC   K.SNVKMINK,=X'FFFFFFFFFFFF'  LAST RECORD???                      
         BE    S382                          YES - GO FIND X'E8'                
         MVC   K.SNVKMINK,=X'FFFFFFFFFFFF'                                      
         MVI   XSP,C'Y'                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'SNVKEY),KEYSAVE  BETTER BE THERE!                          
         BE    *+6                                                              
         DCHO                                                                   
         GOTO1 GETREC                                                           
         MVI   XSP,C'N'                                                         
         USING SNVKEYD,R6                                                       
*                                                                               
S382     LA    R6,SNVELS                                                        
         XR    R0,R0                                                            
*                                                                               
         USING SNVMMEL,R6                                                       
S384     CLI   0(R6),0             GO FIND MM STATUS ELEM                       
         BE    S388                 IF NOT, SKIP INVOICE                        
         CLI   0(R6),SNVMMELQ      TEST DETAIL ELEM (X'E8')                     
         BE    S385                                                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     S384                                                             
*                                                                               
S385     CLC   =C'POL',HPRD        REQUEST PRODUCT POL?                         
         BE    S386                 YES - GO CHECK EST                          
         CLC   SNVMMRP1,HPRD                                                    
         BNE   S388                                                             
         CLC   SNVMMRP2,HPRD2                                                   
         BNE   S388                                                             
*                                                                               
S386     CLI   HEST,0              EST ALL REQ?                                 
         BE    S387                 YES - PROCESS INVOICE                       
         XR    R0,R0                                                            
         CLC   HEST,SNVMMRE1       MATCH ON EST?                                
         BNE   S388                                                             
         DROP  R6                                                               
*                                                                               
S387     XR    RF,RF                                                            
         ICM   RF,3,HALF                                                        
         A     RF,HDSPOTS                                                       
         ST    RF,HDSPOTS                                                       
         AP    HDCOST,DUB                                                       
         TM    BYTE,SNVHDPDQ       TEST PAID STATUS                             
         BNZ   *+8                                                              
         MVI   SVPAID,C' '                                                      
*                                                                               
         CLC   WORK(L'SNVHDCON),SPACES  SEND EVERY CONTRACT NUMBER              
         BNH   S388                                                             
         LA    R1,H03CONT                                                       
         LA    R4,WORK                                                          
         BRAS  RE,SENDD                                                         
*                                                                               
S388     MVC   K.SNVKMINK(8),=X'FFFFFFFFFFFFFF'   MINKEY + SPARE!               
         B     S372                                                             
*                                                                               
S390     LA    R1,H03SPOTS                                                      
         LA    R4,HDSPOTS                                                       
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R1,H03DOLS                                                       
         LA    R4,HDCOST                                                        
         BAS   RE,SENDD                                                         
*                                                                               
         CLI   SVPAID,C' '         TEST PAID                                    
         BE    *+16                 NO                                          
         LA    R1,H03PAID                                                       
         LA    R4,SVPAID                                                        
         BAS   RE,SENDD                                                         
*                                                                               
S400     B     S300                                                             
         DROP  K                                                                
         EJECT                                                                  
*=================================================================*             
* H4 DATA - SORT BUFFER INTO MARKET NUMBER ORDER                  *             
*           AND THEN SEND MARKET NAMES                            *             
*=================================================================*             
         SPACE 1                                                                
S440     MVI   TSACTN,TSASRT                                                    
         XC    TSRTPARM,TSRTPARM                                                
         MVI   TSRTKDSP,HMKT-HKEY                                               
         MVI   TSRTKLEN,L'HMKT                                                  
         BAS   RE,CALLTSAR                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+6(2),QAGY                                                    
*                                                                               
         MVI   TSACTN,TSAGET                                                    
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BRAS  RE,CALLTSAR         IF NO INVOICES HERE, THEY WERE ALL           
         BE    S500                DELETED FOR NO CLT AUTH                      
         MVC   ERROR,=Y(NOINVS)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
S450     MVI   TSACTN,TSANXT                                                    
         BRAS  RE,CALLTSAR                                                      
         BNE   EXIT                                                             
*                                                                               
S500     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,HMKT                                                        
         CH    R0,HALF             TEST SAME AS PREVIOUS                        
         BE    S450                                                             
         STH   R0,HALF             SAVE CURRENT MARKET                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         GOTO1 HIGHSTA                                                          
         CLC   KEY(8),0(R6)        SHOULD FIND IT !                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,H04Q                                                          
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R1,H04MKT           H04/D01                                      
         LA    R4,HMKT                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R1,H04MKTNM         H04/D02                                      
         LA    R4,(MKTNAME-MKTRECD)(R6)                                         
         BAS   RE,SENDD                                                         
         B     S450                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*==============================================================*                
* DELETE ANY MKTS UNAUTHORIZED                                 *                
*==============================================================*                
         SPACE 1                                                                
AUTHMKT  NTR1                                                                   
         XC    FULL,FULL                                                        
         MVI   TSACTN,TSASRT       SORT TSAR BUFFER IN MKT SEQ                  
         XC    TSRTPARM,TSRTPARM                                                
         MVI   TSRTKDSP,HMKT-HKEY                                               
         MVI   TSRTKLEN,L'HMKT                                                  
         BAS   RE,CALLTSAR                                                      
         BNE   AMX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+6(2),QAGY                                                    
*                                                                               
         MVI   TSACTN,TSAGET                                                    
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BRAS  RE,CALLTSAR                                                      
         BNE   AMX                                                              
         B     AM20                                                             
*                                                                               
AM10     MVI   TSACTN,TSANXT                                                    
         BRAS  RE,CALLTSAR                                                      
         BNE   AMX                                                              
*                                                                               
AM20     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,HMKT                                                        
         CH    R0,FULL             TEST SAME AS PREVIOUS                        
         BNE   AM30                                                             
         CLI   FULL+2,X'FF'                                                     
         BE    AM40                                                             
         B     AM10                                                             
*                                                                               
AM30     XC    FULL,FULL                                                        
         STH   R0,FULL             SAVE CURRENT MARKET                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         GOTO1 HIGHSTA                                                          
         CLC   KEY(8),0(R6)        SHOULD FIND IT !                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MKTRECD,R6                                                       
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS     2 BYTES!                                     
         MVC   OFCLMT,TWAACCS      4 BYTES!                                     
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCACCSM(3),MKTLTACC                                             
         MVI   OFCACCSC,X'FF'      SUPPRESS CLIENT LMT ACCESS AUTH              
         MVC   OFCSECD,AIO2        SECRET BLOCK                                 
         DROP  R1,R6                                                            
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',WORK),ACOMFACS                               
         CLI   0(R1),0                                                          
         BE    AM10                                                             
*                                                                               
AM40     MVI   FULL+2,X'FF'        SET DELETE THIS MKT                          
         MVC   HALF,TSRNUM                                                      
         MVI   TSACTN,TSADEL       DELETE IT                                    
         BRAS  RE,CALLTSAR                                                      
         MVC   TSRNUM,HALF         AND GET SEQUENCE BACK                        
         MVI   TSACTN,TSAGET                                                    
         BRAS  RE,CALLTSAR                                                      
         BNE   AMX                                                              
         B     AM20                                                             
*                                                                               
AMX      MVI   TSACTN,TSASRT       SORT TSAR BUFFER BACK                        
         XC    TSRTPARM,TSRTPARM                                                
         MVI   TSRTKDSP,0                                                       
         MVI   TSRTKLEN,L'HKEY                                                  
         BAS   RE,CALLTSAR                                                      
         B     EXIT                                                             
         EJECT                                                                  
*==============================================================*                
* USE MKGRP PASSIVES TO BUILD ELEMENTS AND 'ADD' THEM BACK     *                
* INTO BUYER RECORD                                            *                
* R6 = MARKET GROUP ELEM ON BUYER REC                          *                
* *** NOTE NOTE NOTE *** UPDATE R6 BY L' ELEMS ADDED           *                
*==============================================================*                
         SPACE 1                                                                
         USING BYRMKGD,R6                                                       
BLDMKGRP NTR1                                                                   
         XC    DUB,DUB             BUILD BYRMKT ELEMENT                         
         MVC   DUB(1),SVBYREL                                                   
         MVI   DUB+1,4                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING MKGPTYP,RF                                                       
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,BAGYMD                                                  
         MVC   MKGPMID,BYRMKGID                                                 
         MVC   MKGPMGRP,BYRMKGRP                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     BM12                                                             
         DROP  R6,RF                                                            
*                                                                               
BM10     GOTO1 SEQ                                                              
*                                                                               
BM12     CLC   KEY(11),KEYSAVE                                                  
         BNE   BMX                                                              
         MVC   DUB+2(2),KEY+11     MARKET                                       
*                                                                               
         SR    R0,R0                                                            
         L     R4,AIO2                                                          
         AHI   R4,BYREL-BYRRECD    GET FIRST ELEM                               
*                                                                               
BM14     IC    R0,1(R4)                                                         
         AR    R4,R0               ADD AFTER X'01'                              
         CLC   DUB(4),0(R4)                                                     
         BH    BM14                                                             
         GOTO1 VRECUP,DMCB,(0,AIO2),DUB,(C'R',(R4))                             
         CLI   8(R1),C'R'                                                       
         BE    BM20                                                             
         MVC   ERROR,=Y(MANYMKT)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
BM20     AHI   R6,4                BUMP BY L'ELEM FOR CALLING RTN               
         B     BM10                                                             
*                                                                               
BMX      XIT1  REGS=(R6)                                                        
         EJECT                                                                  
*==============================================================*                
* BUYER ONLY GETS CLIENTS IN SUPERVISOR RECORD                 *                
* IF NO CLIENTS, SHOW ALL                                      *                
* IF BUYER HAS A FILTER, MATCH TO CLIENT FILTERS IN SUPV REC   *                
* IF MKPROF+6 = Y, BUYER GETS ALL CLT'S                        *                
*==============================================================*                
         SPACE 1                                                                
CLTFILT  NTR1                                                                   
         L     R6,AIO3                                                          
         USING SPVCLTD,R6                                                       
*                                                                               
         CLI   PROFMK+6,C'Y'       ALL BUYERS GET ALL CLTS?                     
         BE    CLTFYES              YES                                         
*                                                                               
         MVC   ELCODE,SVSPVEL                                                   
         BRAS  RE,GETEL                                                         
         BE    CLTF4                                                            
         B     CLTFYES             *** IF NO CLIENTS, ALLOW ALL ***             
*                                                                               
CLTF2    BRAS  RE,NEXTEL                                                        
         BNE   CLTFNO                                                           
*                                                                               
CLTF4    CLI   SVBYRFLT,C' '       BUYER FILTER ACTIVE                          
         BNH   CLTF6               NO - JUST MATCH CLIENT                       
*                                                                               
         CLI   SPVCLTLN,SPVFILT-SPVCLTEL  ELEM HAVE FILTER IN IT ?              
         BNH   CLTFYES                    NO - THEN PROCESS                     
         CLC   SVBYRFLT,SPVFILT           ELSE MATCH FILTER                     
         BNE   CLTF2                                                            
*                                                                               
CLTF6    CLC   0(2,R1),SPVCLT             MATCH CLIENT CODE                     
         BE    CLTFYES                                                          
         B     CLTF2                                                            
*                                                                               
CLTFYES  CR    RE,RE                                                            
         B     CLTFX                                                            
*                                                                               
CLTFNO   LTR   RE,RE                                                            
*                                                                               
CLTFX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*==============================================================*                
* INTERFACE TO STAPACK TO GET STATION CALL LETTERS             *                
*==============================================================*                
         SPACE 1                                                                
STAUNPK  NTR1                                                                   
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
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
         MVC   STAPMKST,HMKT       MOVE MARKET/STATION                          
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(5),STAPQSTA                                                 
         MVC   WORK+5(3),STAPQNET                                               
         B     EXIT                                                             
         SPACE 1                                                                
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
* READ SUPERVISOR RECORD                                          *             
*=================================================================*             
         SPACE 1                                                                
GETSPV   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D61'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         NI    KEY+2,X'F0'         DROP MEDIA CODE                              
         MVC   KEY+3(2),QGRP       BUYING GROUP = OFFICE                        
         MVC   KEY+5(4),FULL                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETSPV1                                                          
         MVC   ERROR,=Y(MISSSPV)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
GETSPV1  L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   SVSPVEL,SPVCLTT                                                  
         CLI   QMED,C'T'                                                        
         BE    GETSPV2                                                          
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   *+12                                                             
         CLI   QMED,C'N'           MEDIA 'N' USES MEDIA 'T'                     
         BE    GETSPV2                                                          
*                                                                               
         MVI   SVSPVEL,SPVCLTR                                                  
         CLI   QMED,C'R'                                                        
         BE    GETSPV2                                                          
*                                                                               
         MVI   SVSPVEL,SPVCLTX                                                  
         CLI   QMED,C'X'                                                        
         BE    *+6                                                              
         DCHO                                                                   
         SPACE 1                                                                
*===================================================================*           
* NEED TO PACK THE CLIENT CODES (IN PLACE)                                      
*===================================================================*           
         SPACE 1                                                                
         USING SPVCLTD,R6                                                       
GETSPV2  MVC   ELCODE,SVSPVEL                                                   
         BRAS  RE,GETEL                                                         
         BNE   GETSPV10            NO CLIENTS, TEST CLT GROUPS                  
         B     *+8                                                              
*                                                                               
GETSPV4  BRAS  RE,NEXTEL                                                        
         BNE   GETSPV10            NOW LOOK FOR CLIENT GROUPS                   
         GOTO1 VCLPACK,DMCB,SPVCLT,DUB                                          
         MVC   SPVCLT(2),DUB                                                    
         MVI   SPVCLT+2,0                                                       
         B     GETSPV4                                                          
         DROP  R6                                                               
         SPACE 1                                                                
*===================================================================*           
* HERE FOR CLIENT GROUPS                                                        
*    NOTE - NEED TO BE CAREFUL WITH ELEMS IN AIO3 - SINCE WE ARE                
*           ADDING NEW ELEMS, WE CAN'T RELY ON R6 TO STILL BE                   
*           POINTING TO THE LAST ELEM PROCESSED, SO START FROM                  
*           THE BEGINNING OF THE RECORD EACH TIME AND CHANGE THE                
*           ELCODE AFTER IT'S BEEN PROCESSED                                    
*===================================================================*           
         SPACE 1                                                                
         USING SPVCGRD,R6                                                       
GETSPV10 XC    DUB,DUB             BUILD CLT ELEM TO BE ADDED                   
         MVC   DUB(1),SVSPVEL                                                   
         MVI   DUB+1,6                                                          
         OI    ELCODE,X'10'        SET TO LOOK FOR CLT GRPS                     
*                                                                               
GETSPV20 L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BNE   GETSPVX                                                          
*                                                                               
* GET CLIENT GROUP RECORD(S) AND BUILD CLIENT ELEMS IN SPV REC                  
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING GRPPKEY,R3                                                       
         MVI   GRPKTYP,GRPKTYPQ    X'0D04'                                      
         MVI   GRPKSTYP,GRPKCTYQ                                                
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,SPVCGRID                                                  
         MVC   GRPKCODE,SPVCGRP                                                 
*                                                                               
* CHANGE ELEM CODE SO WE DON'T PICK IT UP AGAIN!                                
         MVI   SPVCGREL,X'FF'                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     GETSPV32                                                         
*                                                                               
GETSPV30 GOTO1 SEQ                                                              
*                                                                               
GETSPV32 CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BNE   GETSPV20                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO1                                                          
         AHI   R3,GRPEL-GRPKEY                                                  
*                                                                               
GETSPV40 CLI   0(R3),0             FIND CLT ELEM IN GRP REC                     
         BE    GETSPV30                                                         
         CLI   0(R3),GRPVALCQ                                                   
         BE    GETSPV50                                                         
*                                                                               
GETSPV42 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETSPV40                                                         
*                                                                               
* BUILD CLIENT ELEMS IN SPV REC FROM GROUP REC                                  
         USING GRPVALD,R3                                                       
GETSPV50 GOTO1 VCLPACK,DMCB,GRPVALUE,DUB+2                                      
         MVI   DUB+4,0                                                          
***      MVC   DUB+5,SPVCGRF       ADD FILTER FROM CGR TO CLT                   
         MVC   DUB+5(1),SPVCGRF    ADD FILTER FROM CGR TO CLT                   
         DROP  R3                                                               
*                                                                               
         L     R4,AIO3             A(SPV REC)                                   
         AHI   R4,SPVEL-SPVRECD                                                 
*                                                                               
GETSPV52 LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLC   DUB(4),0(R4)                                                     
         BH    GETSPV52                                                         
***      GOTO1 VRECUP,DMCB,(0,AIO3),DUB,(C'R',(R4))                             
         GOTO1 VRECUP,DMCB,(X'FE',AIO3),DUB,(C'R',(R4)),SPECLREC                
         CLI   8(R1),C'R'                                                       
         BE    GETSPV42                                                         
         MVC   ERROR,=Y(MANYMKT)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
GETSPVX  J     EXIT                                                             
*                                                                               
SPECLREC DC    AL2(24,13,5970)                                                  
         EJECT                                                                  
*                                                                               
*==============================================================*                
* GET CLIENT REC                                               *                
*==============================================================*                
         SPACE 1                                                                
GETCLT   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDR,R6                                                        
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,HCLT                                                     
*                                                                               
         L     R6,AIO                                                           
         CLC   KEY(13),0(R6)       HAVE THIS REC ALREADY?                       
         BE    GETCX                YES                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     MAKE SURE FOUND IT!                          
         BE    *+6                                                              
         DCHO                                                                   
         GOTO1 GETREC                                                           
*                                                                               
GETCX    J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*==============================================================*                
* MAKE SURE THIS USER ID IS AUTH TO THIS CLT                   *                
*    (NOTE - THE OLD AUTHCLT IS AT THE BOTTOM FOR REFERENCE)   *                
*==============================================================*                
         SPACE 1                                                                
AUTHCLT  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BRAS  RE,GETCLT                                                        
         MVC   SVCPROF,CPROF                                                    
*                                                                               
         CLI   TWAACCS,C'+'        IF THEY USE MKT LIMIT ACCESS ONLY            
         BE    ACYES                DON'T BOTHER WITH OFFICER CALL              
**         CLI   TWAACCS+2,C'+'                                                 
**         BE    ACYES                                                          
*                                                                               
         GOTO1 VCLUNPK,DMCB,(SVCPROF+6,CKEYCLT),DUB                             
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS     2 BYTES!                                     
         MVC   OFCLMT,TWAACCS      4 BYTES!                                     
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,DUB                                                       
         OI    OFCINDS,OFCI2CSC    PASSING 2 BYTE CLT CODE                      
         MVC   OFCCLT2,CKEYCLT                                                  
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCACCSC(3),CACCESS                                              
         MVI   OFCACCSM,X'FF'      SUPPRESS MKT LMT ACCESS AUTH                 
         MVC   OFCSECD,AIO2        SECRET BLOCK                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',WORK),ACOMFACS                               
         CLI   0(R1),0                                                          
         BE    ACYES                                                            
         B     ACNO                                                             
*                                                                               
ACYES    CR    RE,RE                                                            
         B     ACX                                                              
*                                                                               
ACNO     LTR   RE,RE                                                            
*                                                                               
ACX      J     EXIT                                                             
         DROP  R6                                                               
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
         L     RF,AIO                                                           
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
*=================================================================*             
* FIND PRODUCT CODE IN CLIENT RECORD                              *             
*=================================================================*             
         SPACE 1                                                                
*                                                                               
GETBPRD  BRAS  RF,*+8              PUT A(C'   'X'00') IN RF                     
         DC    CL3'   ',X'00'                                                   
         CLC   0(3,R1),SPACES                                                   
         BNHR  RE                                                               
*                                                                               
         L     RF,AIO                                                           
         AHI   RF,CLIST-CLTHDR                                                  
*                                                                               
GETBPRD2 CLC   0(3,R1),0(RF)                                                    
         BER   RE                                                               
         AHI   RF,4                                                             
         CLI   0(RF),C'A'                                                       
         JNL   GETBPRD2                                                         
*                                                                               
* IT ISN'T SAFE TO USE A LTORG HERE!                                            
         BRAS  RF,*+6              PUT A(BADPRD) IN RF                          
         DC    Y(BADPRD)                                                        
         MVC   ERROR,0(RF)                                                      
         GOTO1 SENDMSG                                                          
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         EJECT                                                                  
GLOBALS  DS    0D                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPMAKWRK                                                       
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   TSARREC                                                          
HREC     DS    0D                                                               
*                                                                               
HKEY     DS    0XL17                                                            
HCLT     DS    XL2                                                              
HPRD     DS    XL3                                                              
HEST     DS    XL1                                                              
HMKT     DS    XL2                                                              
HSTA     DS    XL3                 SPARE                                        
HMOS     DS    XL2                                                              
HPRD2    DS    XL3                                                              
HEST2    DS    XL1                                                              
HKEYX    EQU   *                                                                
*                                                                               
HSTAT    DS    0CL2                                                             
HSTAT1   DS    CL1                                                              
*        EQU   X'80'               ** DON'T EVER USE **                         
*        EQU   X'40'               ** DON'T EVER USE **                         
*SNVMMFLM EQU   X'20'              FILM ANALYSIS ERRORS                         
*SNVMMHVE EQU   X'10'              HORIZONTAL/VERTICAL ERRORS                   
*SNVMMSSE EQU   X'08'              SECONDARY SEPARATION ERRORS                  
*                                                                               
HSTAT2   DS    CL1                 X'80'= MATCHED                               
*                                  X'40'= WIP                                   
*                                  X'02'= APPROVED                              
HINV     DS    CL10                INVOICE NUMBER                               
*                                                                               
HRECX    EQU   *                                                                
         ORG                                                                    
         ORG   OVWORK                                                           
SVSPVEL  DS    X                                                                
SVBYREL  DS    X                                                                
ALLMKTS  DS    C                                                                
SVPAID   DS    C                                                                
HDCOST   DS    PL8                                                              
HDSPOTS  DS    F                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPGENSPV                                                       
       ++INCLUDE SPGENBYR                                                       
       ++INCLUDE SPGENOFC                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FASECRETD                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENMKG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPMAK01   09/21/20'                                      
         END                                                                    
