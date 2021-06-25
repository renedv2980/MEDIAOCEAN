*          DATA SET NEPAY01    AT LEVEL 135 AS OF 07/09/19                      
*PHASE T31301B,+0                                                               
         TITLE 'NETPAK PAY PROGRAM SUBSIDIARY BASE - T31301'                    
***********************************************************                     
********************   HISTORY  ***************************                     
*LEVEL 130  READS A0B PROFILE                             *                     
*LEVEL 130  NOAFFID OPTION IN TABLE ADDED BYTE TO DESCIPT *                     
************IT WAS SET FOR ONLY 48 BYTES INSTEAD OF 49    *                     
*LEVEL 131   CR/CK REVERSAL CODE   9/28/11                *                     
***********************************************************                     
***********************************************************                     
T31301   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PAY01,RA,RR=RE                                               
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING PAYWRKD,R9                                                       
         USING TWAD,R8                                                          
         L     R7,ASPOOLA                                                       
         USING SPOOLD,R7                                                        
         ST    RE,SBRELO           SAVE SUBSIDIARY ROOT RELO                    
         ST    RB,SBBASE1                                                       
         ST    RA,SBBASE2                                                       
         EJECT                                                                  
* START BY GETTING AGENCY VALUES                                                
*                                                                               
VALAGY   MVC   NBSELAGY,AGYALPH                                                 
         MVI   NBSELMED,C'N'                                                    
         MVI   NBSELMOD,NBVALAGY   READ AGENCY RECORD                           
         MVC   NBAIO,AIOAREA1                                                   
         MVC   NBACOM,ACOMFACS                                                  
         GOTO1 VNETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBGOOD      TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AGENCY,AGYALPH                                                   
         MVC   AGYMED,NBACTAM                                                   
         L     R4,NBAIO                                                         
         USING AGYHDRD,R4                                                       
         MVC   AGYPRO,AGYPROF      EXTRACT PROFILE                              
         MVC   USERNAME,AGYNAME                                                 
         MVC   USERADDR,AGYADDR                                                 
         MVC   AGYFLG1,AGYFLAG1                                                 
         MVC   AGYFLG2,AGYFLAG2                                                 
         MVC   AGYFLG3,AGYFLAG3                                                 
         B     VALACT                                                           
         DROP  R4                                                               
         EJECT                                                                  
* FIRST VALIDATE ACTION                                                         
*                                                                               
VALACT   LA    R2,PAYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FTERM,FTERM                                                      
         MVI   FTERM,EQUALS        LOOK FOR EQUALS SIGN                         
         GOTO1 AFVAL                                                            
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    ERROR                                                            
         MVI   FERN,INVERR                                                      
         LA    R3,ACTTAB           R3 POINTS TO ACTION TABLE                    
         USING ACTTABD,R3          R3                                           
         CLI   FLDH+5,L'ACTCODE                                                 
         BH    ERROR               LONGER THAN A VALID ACTION CODE              
         SPACE 1                                                                
VALACT2  CLI   ACTCODE,EOT         TEST FOR END-OF-TABLE                        
         BE    ERROR               YES-MUST BE INVALID                          
         CLC   ACTCODE,FLD         TEST FOR EXACT MATCH ON ACTION               
         BE    VALACT4             FOUND ONE                                    
         CLC   FLDH+5(1),ACTMINL   TEST ACTUAL LEN VS. MIN                      
         BL    VALACT3             TOO SMALL FOR VARIABLE COMPARE               
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),ACTCODE                                                   
         BE    VALACT4             OK                                           
         SPACE 1                                                                
VALACT3  LA    R3,ACTNTRL(R3)      NEXT TABLE ENTRY                             
         B     VALACT2                                                          
         SPACE 1                                                                
VALACT4  TM    ACTIND,DDSONLY      TEST FOR DDS ONLY ACTION                     
         BZ    *+12                NO                                           
         CLI   DDS,YES             ONLY PERMIT FROM DDS TERMINAL                
         BNE   ERROR                                                            
         ST    R3,AACTNTRY         SAVE A(ACTION ENTRY)                         
         MVC   ACTION,ACTN         EXTRACT ACTION VALUES                        
         MVC   OVERLAY,ACTOV                                                    
         MVC   SCREEN,ACTSCR                                                    
         MVC   ACTI,ACTIND         INDICATORS                                   
         CLI   ACTION,PRINT        TEST FOR PRINT                               
         BE    VALACT6             YES-SEARCH FOR REQUESTOR                     
         CLI   FSTOP,X'FF'         TEST FOR NO DATA LEFT                        
         BE    VALACTX             YES-ALL DONE WITH EDIT                       
         CLI   ACTION,LIST         TEST FOR LIST OR DIS                         
         BE    VALACT5                                                          
         CLI   ACTION,DIS                                                       
         BE    VALACT5                                                          
         B     ERROR               NO-MUST BE GARBAGE LEFT IN FIELD             
         SPACE                                                                  
VALACT5  CLI   FSTOP,EQUALS        TEST FOR EQUALS SIGN                         
         BNE   ERROR                                                            
         XC    FTERM,FTERM         GET REST OF FIELD                            
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,6                                                         
         BH    ERROR                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,TOTCOMP          TEST FOR T(OTALS)                            
         BNE   ERROR                                                            
         MVI   TOTOPT,YES          SET OPTION TO SHOW TOTALS ONLY               
         B     VALACTX                                                          
         SPACE 1                                                                
VALACT6  CLI   FSTOP,EQUALS        TEST IF EQUALS SIGN FOUND                    
         BNE   VALACT7             NO-MUST BE ERROR                             
         XC    FTERM,FTERM         GET REST OF FIELD                            
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0            TEST FOR MISSING INPUT                       
         BE    VALACT7                                                          
         CLI   FLDH+5,3            NO MORE THAN 3 CHARS                         
         BH    ERROR                                                            
         MVC   RCPROG,PLID         SET REPORT ID                                
         MVC   SPOOLID,FLD         SAVE REPORT ID                               
         CLI   SPOOLID+2,C' '                                                   
         BNE   *+8                                                              
         MVI   SPOOLID+2,STAR                                                   
         CLI   SPOOLID+1,C' '                                                   
         BNE   *+8                                                              
         MVI   SPOOLID+1,STAR                                                   
         B     VALACTX             ALL DONE                                     
         SPACE 1                                                                
VALACT7  MVC   XTRA(17),=C'MISSING REQUESTOR'                                   
         B     ERROR                                                            
         SPACE 1                                                                
VALACTX  B     VALCLI                                                           
         DROP  R3                                                               
         SPACE 2                                                                
TOTCOMP  CLC   FLD(0),=C'TOTALS'                                                
         EJECT                                                                  
* VALIDATE CLIENT                                                               
*                                                                               
VALCLI   MVI   FERN,MISERR         CLIENT IS ALWAYS COMPULSORY                  
         GOTO1 VGETFLD,PAYCLIH                                                  
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVI   FERN,INVERR                                                      
         GOTO1 VCLPACK,DMCB,FLD,CLIPK                                           
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    ERROR                                                            
         CLC   PRAGENCY,AGENCY                                                  
         BNE   VALCLI1                                                          
         CLC   PRCLIENT,FLD                                                     
         BNE   VALCLI1                                                          
         MVC   NBUSER,PRUSER                                                    
         MVC   NBUSER1,PRUSER1                                                  
         MVC   NBUSER2,PRUSER2                                                  
VALCLI1  MVC   NBSELCL2,CLIPK                                                   
         MVI   NBSELMOD,NBVALCLI                                                
         MVC   NBACLI,ACLIREC      NETIO SAVES RECORD IN GLOBAL WS              
         GOTO1 VNETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBGOOD      TEST FOR ERROR                               
         BE    VALCLI2                                                          
         MVC   FERN,NBERROR                                                     
         B     ERROR                                                            
         SPACE 1                                                                
VALCLI2  MVC   PRUSER,NBUSER                                                    
         MVC   PRUSER1,NBUSER1                                                  
         MVC   PRUSER2,NBUSER2                                                  
         MVC   PRAGENCY,AGENCY                                                  
         MVC   PRCLIENT,FLD                                                     
         MVC   CLI,FLD             ALPHA CLIENT                                 
         L     R4,NBACLI                                                        
         USING CLTHDRD,R4                                                       
         MVC   PAYCLIN,CNAME       MOVE CLIENT NAME TO SCREEN                   
         MVC   HALF(1),COFFICE     SAVE OFFICE CODE FOR PROFILES                
         OI    PAYCLINH+6,X'80'                                                 
         BAS   RE,BLDSECRT         CREATE SECRET BLOCK                          
         BAS   RE,CALLOFCR         CHECK CLIENT SECURITY                        
         B     VALCLIX                                                          
*                                                                               
***         OC    TWAACCS(2),TWAACCS  TEST FOR ANY SECURITY LIMITS              
***         BZ    VALCLIX                                                       
***         CLI   TWAACCS,C'+'        TEST FOR MARKET LOCKOUT                   
***         BE    VALCLIX             NO CHECK                                  
***         MVI   FERN,SCTYERR                                                  
***         CLI   TWAACCS,C'$'        TEST FOR OFFICE LIMIT                     
***         BE    VALCLI3                                                       
***         CLI   TWAACCS,C'*'        TEST FOR OFFICE LIMIT                     
***         BE    VALCLI4                                                       
***         CLC   TWAACCS(2),CLIPK    TEST FOR FILTERED CLIENT                  
***         BE    VALCLIX             OK                                        
***         B     ERROR                                                         
***         SPACE 1                                                             
***VALCLI3  DS    0H               * TEST OFFICE LIST SECURITY *                
***         XC    DMCB(8),DMCB                                                  
***         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                   
***         L     RF,VCALOV                                                     
***         GOTO1 (RF),DMCB                                                     
***         CLI   4(R1),X'FF'                                                   
***         BNE   *+6                                                           
***         DC    H'0'                                                          
***         XC    DUB,DUB                                                       
***         LA    R1,DUB                                                        
***         USING OFFICED,R1                                                    
***         MVI   OFCSYS,C'S'         SYSTEM ID                                 
***         MVC   OFCAUTH,TWAACCS     ID AUTH VALUE                             
***         MVC   OFCAGY,AGENCY                                                 
***         MVC   OFCOFC,COFFICE                                                
*                                                                               
***         L     RF,DMCB                                                       
***         GOTO1 (RF),DMCB,DUB,ACOMFACS,0                                      
***         CLI   0(R1),0                                                       
***         BNE   ERROR                                                         
***         B     VALCLIX                                                       
         SPACE 1                                                                
VALCLI4  CLC   TWAACCS+1(1),COFFICE TEST FOR FILTERED OFFICE                    
         BNE   ERROR                                                            
*                                                                               
VALCLIX  DS    0H                                                               
         BAS   RE,BLDETAB           BUILD TABLE OF ESTIMATES AND FLAGS          
         B     VALNETW                                                          
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
*    BUILD TABLE OF ESTIMATES #'S + FLAG (255*2)+1                              
*********************************************************************           
BLDETAB  NTR1                                                                   
         L     R2,AESTTAB                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),AGYMED                                                  
         MVC   KEY+2(2),CLIPK                                                   
         MVC   KEY+4(3),=CL3'POL'                                               
         MVC   KEY+7(1),EST                                                     
         GOTO1 AIO,DMCB,SPT+HIGH+DIR                                            
         CLC   KEY(7),KEYSAVE       1ST TIME THROUGH - SHOULD BE THERE          
         BE    BLDET10                                                          
         B     BLDETABX                                                         
*                                                                               
BLDESEQ  GOTO1 AIO,DMCB,SPT+SEQ+DIR                                             
*                                                                               
BLDET10  CLC   KEY(7),KEYSAVE       SAME AGY AND CLIENT AND POL?                
         BNE   BLDETABX                                                         
*                                                                               
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA4                                   
         L     R4,AIOAREA4                                                      
         USING ESTHDRD,R4                                                       
*                                                                               
         MVC   0(1,R2),EKEYEST      PUT ESTIMATE # IN TABLE                     
*                                                                               
         CLI   ETYPE,C'S'           IS ESTIMATE SET UP AS STEWARD?              
         BNE   *+8                                                              
         OI    1(R2),X'01'          TURN ON FLAG INDICATING EST IS STW          
*                                                                               
         LA    R2,2(R2)                                                         
         B     BLDESEQ                                                          
*                                                                               
BLDETABX DS    0H                                                               
         MVI   0(R2),X'FF'          DENOTE END OF EST TABLE                     
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* INITIALIZE SECRET                                                             
*                                                                               
BLDSECRT NTR1                                                                   
*                                                                               
         L     R2,ATWA                                                          
         A     R2,=A(COMMTWA)                                                   
         USING SVAREA2,R2                                                       
         LA    RE,SVSECRET                                                      
         ST    RE,ASECRET                                                       
*                                                                               
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS       OR HAVE LIMIT ACCESS                    
         BZ    BLDSECEX                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',SVSECRET),0                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDSECEX B     EXXMOD                                                           
         DROP  R2                                                               
         SPACE 3                                                                
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1                                                                   
         L     R4,NBACLI                                                        
         USING CLTHDRD,R4                                                       
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,PAYCLI                                                    
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,AGYMED                                                  
         MVC   OFCLMT(4),TWAACCS                                                
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECRET                                                  
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
         BE    CALLOFEX                                                         
*                                                                               
         MVI   FERN,SCTYERR                                                     
         B     ERROR                                                            
*                                                                               
CALLOFEX B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
VALNETW  GOTO1 VGETFLD,PAYNETH                                                  
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    ERROR               NO                                           
         MVI   FERN,INVERR                                                      
         MVC   NETWORK,FLD         EXTRACT INPUT                                
         TM    FLDH+4,X'04'        MSPACK REQUIRES ALPHA                        
         BZ    ERROR                                                            
         SPACE 1                                                                
VALNET2  MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NETWORK                                              
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGYALPH                                                  
         MVC   STAKCLT,CLI                                                      
         GOTO1 AIO,DMCB,STA+READ+FILE,AIOAREA1                                  
*                                                                               
         L     R4,AIOAREA1                                                      
         MVC   MARKET,SMKT         EXTRACT MARKET NUMBER                        
         MVC   REP,SPAYREP         EXTRACT STATION PAYING REP                   
         MVC   HALF+1(1),STYPE     STATION TYPE N,C,S OR X'00'                  
         MVC   SMEDTYPE,STYPE      SAVE AROUND SUB MEDIA                        
         MVC   NBSELNET,NETWORK    SET NETWORK IN NETBLOCK                      
         XC    PAYNETX,PAYNETX                                                  
         OI    PAYNETXH+6,X'80'    XMIT                                         
         XC    PAYEEX,PAYEEX                                                    
         OI    PAYEEXH+6,X'80'     XMIT                                         
         SPACE 1                                                                
VALNET4  CLI   TWAACCS,C'+'        TEST MKT LIMIT ACCESS                        
         BNE   VALNET6                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKTRECD,R4                                                       
         MVI   MKTKTYPE,C'M'       READ MARKET RECORD                           
         MVI   MKTKMED,C'N'                                                     
         MVC   MKTKMKT,MARKET                                                   
         MVC   MKTKAGY,AGYALPH                                                  
         GOTO1 AIO,DMCB,HIGH+FILE+STA,AIOAREA1                                  
         CLC   KEY(MKTKFILL-MKTKEY),KEYSAVE  TEST IF RECORD FOUND               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   FERN,SCTYERR                                                     
         L     R4,AIOAREA1                                                      
         LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
*                                                                               
         CLC   0(1,R1),TWAACCS+1                                                
         BE    VALNET6                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     ERROR                                                            
         SPACE 1                                                                
VALNET6  CLC   PRAGENCY,AGENCY                                                  
         BNE   VALNET7                                                          
         CLC   PRCLIENT,FLD                                                     
         BNE   VALNET7                                                          
         CLC   PRSTTYPE,HALF+1                                                  
         BE    VALNET10                                                         
*                                                                               
VALNET7  XC    KEY,KEY                                                          
         MVC   KEY(4),=C'S0A0'     GET PAY PROGRAM PROFILE                      
         MVC   KEY+4(2),AGYALPH                                                 
         MVI   KEY+6,C'N'                                                       
         CLI   HALF+1,X'40'                                                     
         BNH   *+10                                                             
*NMALIK  MVC   KEY+6,HALF+1                                                     
         MVC   KEY+6(1),HALF+1             NMALIK                               
         MVC   DUB,KEY             SAVE MEDIA PROFILE KEY                       
         MVC   KEY+7(3),CLI                                                     
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),HALF      CLIENT OFFICE CODE                           
         GOTO1 VGETPROF,DMCB,(X'90',KEY),PRPAY1,VDATMGR                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(7),DUB                                                       
         GOTO1 (RF),(R1),KEY,WORK                                               
         MVC   PRPAY1(1),WORK      TAKE GROSS/NET OPTION FROM MEDIA             
*                                                                               
         XC    KEY,KEY             READ SECOND PAY PROFILE                      
         MVC   KEY(7),DUB                                                       
         MVC   KEY+2(2),=C'A4'                                                  
         CLI   HALF+1,C'B'                                                      
         BNH   *+10                                                             
         MVC   KEY+6(1),HALF+1     STATION TYPE CODE N,C,S                      
         MVC   KEY+7(3),CLI                                                     
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),HALF      CLIENT OFIICE CODE                           
         GOTO1 (RF),(R1),KEY,PRPAY2                                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),=C'SA0A'     GET PAY PROGRAM PROFILE                      
         NI    KEY,X'BF'                                                        
         MVC   KEY+4(2),AGYALPH                                                 
         MVI   KEY+6,C'N'                                                       
         CLI   HALF+1,X'40'                                                     
         BNH   *+10                                                             
*NMALIK  MVC   KEY+6,HALF+1                                                     
         MVC   KEY+6(1),HALF+1         NMALIK                                   
         MVC   DUB,KEY             SAVE MEDIA PROFILE KEY                       
         MVC   KEY+7(3),CLI                                                     
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),HALF      CLIENT OFFICE CODE                           
         GOTO1 VGETPROF,DMCB,(X'90',KEY),PRPAYA,VDATMGR                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             READ A0B PAY PROFILE                         
         MVC   KEY(7),DUB                                                       
         MVC   KEY+2(2),=C'0B'                                                  
         MVC   KEY+7(3),CLI                                                     
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),HALF      CLIENT OFIICE CODE                           
         GOTO1 (RF),(R1),KEY,PAYPROFB                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALNET10 MVC   PAYPROF,PRPAY1                                                   
         MVC   PAYPROF2,PRPAY2                                                  
         MVC   PAYPROFA,PRPAYA                                                  
         MVC   PRAGENCY,AGENCY                                                  
         MVC   PRCLIENT,CLI                                                     
         MVC   PRSTTYPE,HALF+1                                                  
         SPACE 1                                                                
VALNETX  B     VALEE                                                            
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PAYEE                                                                
*                                                                               
VALEE    GOTO1 VGETFLD,PAYEEH                                                   
         XC    SRATREP,SRATREP                                                  
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   VALEE2              YES                                          
         CLC   REP,=C'000'         TEST FOR PAYING REP                          
         BE    VALEE1              YES                                          
         MVC   PAYNETX(23),=C'** PAYING REP = 999 **'                           
         MVC   PAYNETX+16(3),REP                                                
         BAS   RE,GETREP                                                        
         B     VALEEX                                                           
*                                                                               
VALEE1   BAS   RE,GETADDR                                                       
         B     VALEEX                                                           
         SPACE 1                                                                
VALEE2   MVI   FERN,INVERR                                                      
         TM    FLDH+4,X'08'        TEST NUMERIC                                 
         BZ    VALEE4              NO                                           
         B     VALEE8                                                           
*                                                                               
VALEE4   CLI   FLD,C'P'            TEST FOR PAYING REP PREFIX                   
         BE    VALEE5                                                           
         CLI   FLD,C'S'            TEST FOR SPECIAL REP PREFIX                  
         BNE   ERROR                                                            
         CLI   FLDH+5,1            TEST FOR ONLY 'S'                            
         BNE   VALEE5              NO                                           
         OI    SPECREP,X'80'       SET ALL SPECIAL REPS FLAG                    
         MVI   REPTYPE,C'S'                                                     
         BAS   RE,GETADDR          GET AND DISPLAY NETWORK ADDRESS              
         TM    ACTI,ALLSREP        TEST IF INPUT ALLOWED FOR ACTION             
         BO    VALEEX              YES-ALL DONE                                 
*                                                                               
         MVC   XTRA(13),=C'NOT VALID FOR'                                       
         L     R3,AACTNTRY         POINT TO ACTION ENTRY                        
         USING ACTTABD,R3                                                       
         MVC   XTRA+14(L'ACTCODE),ACTCODE EXTRACT ACTION CODE                   
         B     ERROR                                                            
         DROP  R3                                                               
*                                                                               
VALEE5   LA    R4,FLD+1                                                         
         ZIC   R5,FLDH+5                                                        
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BNP   ERROR                                                            
         STM   R4,R5,DUB           SAVE POINTER AND COUNT                       
*                                                                               
VALEE6   CLI   0(R4),C'0'                                                       
         BL    ERROR                                                            
         CLI   0(R4),C'9'                                                       
         BH    ERROR                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,VALEE6                                                        
*                                                                               
         LM    R4,R5,DUB                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)  *EXECUTED*                                          
         CVB   R0,DUB                                                           
*                                                                               
VALEE8   CH    R0,=H'999'                                                       
         BH    ERROR                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REP,DUB                                                          
         CLI   FLD,C'S'                                                         
         BNE   VALEE10                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         MVI   REPTYPE,C'S'                                                     
         UNPK  SRATREP,DUB         SAVE CHARACTER SPECIAL REP VALUE             
         STCM  R0,3,SPECREP        SAVE BINARY SPECIAL REP VALUE                
         B     VALEE12                                                          
*                                                                               
VALEE10  LTR   R0,R0                                                            
         BNZ   VALEE12                                                          
         BAS   RE,GETADDR                                                       
         B     VALEEX                                                           
         SPACE 2                                                                
VALEE12  MVC   PAYNETX,SPACES                                                   
         MVC   PAYNETX(23),=C'** PAYING REP = 999 **'                           
         MVC   PAYNETX+16(3),REP                                                
         CLI   REPTYPE,C'S'                                                     
         BNE   *+10                                                             
         MVC   PAYNETX+2(7),=C'SPECIAL'                                         
         BAS   RE,GETREP                                                        
*                                                                               
VALEEX   B     VALPER                                                           
         EJECT                                                                  
* VALIDATE PERIOD                                                               
*                                                                               
VALPER   LA    R2,PAYPERH                                                       
         ST    R2,FADDR                                                         
         XC    PAYPERX,PAYPERX                                                  
         MVI   DATETYP,0                                                        
         OI    PAYPERXH+6,X'80'                                                 
         XC    FTERM,FTERM         LOOK FOR DATE FOLLOWED BY DASH OR            
         MVC   FTERM(2),=C'-*'     STAR                                         
         XC    FLAST,FLAST                                                      
         GOTO1 AFVAL                                                            
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,3            DATE MUST BE AT LEAST 3 CHARS                
         BL    ERROR                                                            
*                                                                               
VALPER2  GOTO1 VDATVAL,DMCB,FLD,DUB                                             
         OC    0(R4,R1),0(R1)      TEST FOR VALID MMMDD/YY                      
         BZ    VALPER10            NO                                           
         CLC   FLDH+5(1),3(R1)     TEST DATE MAKES UP WHOLE FIELD               
         BNE   ERROR                                                            
         MVC   START,DUB                                                        
         MVC   END,START                                                        
         CLI   FSTOP,STAR          TEST IF STAR FOLLOWS DATE                    
         BE    VALPER6             YES                                          
         CLI   FSTOP,X'FF'         TEST FOR NOTHING AFTER DATE                  
         BNE   VALPER4                                                          
         CLI   PAYPROFB+8,C'B'     DEFAULT TO BROADCAST?                        
         BE    VALPER12                                                         
         B     VALPER16            YES-ALL DONE                                 
*                                                                               
VALPER4  XC    FTERM,FTERM         GET REST OF FIELD                            
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,3            TEST FOR 3 CHARS                             
         BL    ERROR                                                            
         GOTO1 VDATVAL,DMCB,FLD,DUB                                             
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         CLC   FLDH+5(1),3(R1)                                                  
         BNE   ERROR                                                            
         MVC   END,DUB             SET END DATE                                 
         MVI   FERN,SEQERR                                                      
         CLC   START,END           CHECK DATE SEQUENCE                          
         BH    ERROR                                                            
         CLI   PAYPROFB+8,C'B'     DEFAULT TO BROADCAST?                        
         BE    VALPER12                                                         
         B     VALPER16                                                         
         SPACE 1                                                                
VALPER6  XC    FTERM,FTERM         GET REST OF FIELD                            
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0            EDIT FOR SUB-LINE                            
         BE    ERROR                                                            
         TM    FLDH+4,X'08'        TEST FOR A NUMBER                            
         BZ    ERROR                                                            
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,SUB                                                           
         CLI   PAYPROFB+8,C'B'     DEFAULT TO BROADCAST?                        
         BE    VALPER12                                                         
         B     VALPER16                                                         
         SPACE                                                                  
VALPER10 GOTO1 VDATVAL,DMCB,(2,FLD),DUB                                         
         MVI   FERN,DATERR                                                      
         OC    0(4,R1),0(R1)       TEST FOR VALID MMMYY                         
         BZ    ERROR               NO                                           
         CLC   FLDH+5(1),3(R1)                                                  
         BNE   ERROR                                                            
         MVC   START,DUB                                                        
         MVC   START+4(2),=C'01'                                                
         CLI   FSTOP,DASH          TEST FOR NOTHING AFTER DATE                  
         BE    VALPER11                                                         
         CLI   PAYPROFB+8,C'B'     DEFAULT TO BROADCAST?                        
         BE    VALPER12                                                         
         B     VALPER14            YES-DEFAULT TO CALENDAR MONTH                
*                                                                               
VALPER11 XC    FTERM,FTERM                                                      
         GOTO1 AFVAL                                                            
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,1                                                         
         BNE   ERROR                                                            
         CLI   FLD,C'C'            TEST FOR CALENDAR MONTH                      
         BE    VALPER14            YES                                          
         CLI   FLD,C'B'            TEST FOR BROADCAST MONTH                     
         BNE   ERROR                                                            
         SPACE 1                                                                
VALPER12 GOTO1 VGTBROAD,DMCB,(1,START),WORK,VGETDAY,VADDAY                      
         MVC   START,WORK          GET START/END OF BROADCAST MONTH             
         MVC   END,WORK+6                                                       
         MVI   DATETYP,C'B'                                                     
         B     VALPER16                                                         
         SPACE 1                                                                
VALPER14 LA    R0,27               ADD 27 DAYS TO START OF MONTH                
         GOTO1 VADDAY,DMCB,START,END,(R0)                                       
         LA    R0,1                                                             
VALPER15 GOTO1 (RF),(R1),END,DUB,(R0)                                           
         CLC   END(4),DUB          TEST SAME MONTH/YEAR                         
         BNE   *+14                YES-HAVE FOUND END OF MONTH                  
         MVC   END,DUB             NO-KEEP TRYING                               
         B     VALPER15                                                         
         SPACE 1                                                                
VALPER16 GOTO1 VDATCON,DMCB,START,(5,PAYPERX)                                   
         MVI   PAYPERX+8,DASH                                                   
         GOTO1 (RF),(R1),END,(5,PAYPERX+9)                                      
         B     VALPER20                                                         
         SPACE                                                                  
VALPER20 MVC   USERQSTR,START                                                   
         MVC   USERQEND,END                                                     
         SPACE                                                                  
         CLI   OVERLAY,2           CHECK FOR CLEAR OR TEST ACTION               
         BNE   VALSOFF                                                          
*                                                                               
*--PERIOD MUST BE WITHIN 1 YEAR OF TODAYS DATE FOR CLEARENCES                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,DUB),(0,DUB)                                     
         CLC   END,DUB                                                          
         BL    VALSOFF                                                          
         GOTO1 VPERVERT,DMCB,DUB,END                                            
         CLC   DMCB+14(2),=H'12'                                                
         BNH   VALSOFF                                                          
         MVI   FERN,DATERR                                                      
         B     ERROR                                                            
         EJECT                                                                  
* TEST FOR SPECIAL OFFICE OVERRIDE - PAY PERIOD CANNOT OVERRLAP                 
* SPECIAL OFFICE START (I.E. EITHER BEFORE OR AFTER)                            
*                                                                               
VALSOFF  CLI   PAYPROF+4,X'C1'     TEST SPECIAL OFFICE PRESENT                  
         BL    VALSOFFX            NO                                           
*                                                                               
         OC    PAYPROF+5(2),PAYPROF+5                                           
         BZ    VALSOF05                                                         
         CLI   PAYPROF+5,0         SEE IF EFFECTIVE DATES                       
         BE    *+12                                                             
         CLI   PAYPROF+6,0         MUST HAVE MONTH AS WELL                      
         BNE   VALSOF10                                                         
VALSOF05 MVI   FERN,SPODTERR       DATE ERROR                                   
         B     ERROR                                                            
*&&DO                                                                           
         OC    PAYPROF+5(2),PAYPROF+5  SEE IF EFFECITVE DATES                   
         BNZ   *+12                                                             
         MVI   FERN,SPODTERR       DATE ERROR                                   
         B     ERROR                                                            
*&&                                                                             
VALSOF10 CLI   DATETYP,C'B'                                                     
         BNE   VALSOF20                                                         
         MVC   THREE(2),PAYPROF+5  START YEAR/MONTH                             
         MVI   THREE+2,15          USE CALENDAR MONTH START                     
         GOTO1 VDATCON,DMCB,(3,THREE),(0,DUB)                                   
         GOTO1 VGTBROAD,DMCB,(1,DUB),WORK,VGETDAY,VADDAY                        
         MVC   DUB(6),WORK         GET BROADCAST START INTO DUB                 
         GOTO1 VDATCON,DMCB,(0,DUB),(0,DUB)  SET YEAR 2K                        
         B     VALSOF50                                                         
VALSOF20 MVC   THREE(2),PAYPROF+5  START YEAR/MONTH                             
         MVI   THREE+2,1           USE CALENDAR MONTH START                     
         GOTO1 VDATCON,DMCB,(3,THREE),DUB                                       
         GOTO1 VDATCON,DMCB,(0,DUB),(0,DUB)  SET YEAR 2K                        
VALSOF50 MVI   FERN,SPOFFERR                                                    
         CLC   END,DUB             TEST PERIOD ENDS BEFORE OFFICE START         
         BL    VALSOFFX            YES-OPTION NOT IN EFFECT                     
         CLC   START,DUB           TEST PERIOD START AFTER OFFICE START         
         BL    ERROR               NO-OVERLAP AND AN ERROR                      
         MVI   SPOFFSW,YES         SET OPTION IN EFFECT                         
         SPACE 1                                                                
VALSOFFX B     VALOPT                                                           
*                                                                               
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
VALOPT   LA    R1,PAYOPTH                                                       
         ST    R1,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0            TEST FOR NO DATA FOUND                       
         BNE   VALOPT1             NO-EDIT FIELD                                
         CLI   FSTOP,X'FF'         TEST FOR EMPTY FIELD                         
         BE    VALOPTX             YES-ALL DONE                                 
         MVI   FERN,INVERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
VALOPT1  CLI   FLD,QUESTION        TEST FOR QUESTION MARK                       
         BE    OPTHELP             RETURN HELP SCREEN                           
         CLI   FLDH+5,2            TEST FOR HE(LP)                              
         BL    VALOPT1A                                                         
         CLI   FLDH+5,4                                                         
         BH    VALOPT1A                                                         
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,HELPCOMP                                                      
         BE    OPTHELP                                                          
*                                                                               
VALOPT1A MVI   FLEN,0              FORCE RE-EDIT OF FIELD                       
         SPACE                                                                  
VALOPT2  ZIC   R1,FNDX                                                          
         LA    R1,1(R1)            BUMP SUB-FIELD INDEX                         
         STC   R1,FNDX                                                          
         XC    FTERM,FTERM                                                      
         MVC   FTERM(2),=C'=,'     LOOK FOR EQUALS OR COMMA                     
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BNE   VALOPT4                                                          
         CLI   FSTOP,X'FF'         TEST FOR NO MORE DATA                        
         BE    VALOPT20                                                         
         MVI   FERN,MISERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
VALOPT4  LA    R3,OPTTAB           POINT R3 AT OPTIONS TABLE                    
         USING OPTTABD,R3                                                       
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,L'OPTNAME    TEST IF LONGER THAN OPTION NAME              
         BH    VALOPTR             YES                                          
*                                                                               
VALOPT5  CLI   OPTNAME,EOT         TEST FOR END OF TABLE                        
         BE    VALOPTR             MUST BE INVALID                              
         CLC   OPTNAME,FLD         TEST FOR EXACT MATCH ON NAME                 
         BE    VALOPT8                                                          
         CLC   FLDH+5(1),OPTMINL   TEST FOR MIN LENGTH FOR THIS OPTION          
         BL    VALOPT6                                                          
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),OPTNAME                                                   
         BE    VALOPT8                                                          
*                                                                               
VALOPT6  LA    R3,OPTNTRL(R3)                                                   
         B     VALOPT5                                                          
         SPACE                                                                  
VALOPT8  TM    OPTIND,DDSONLY      TEST FOR DDS ONLY OPTION                     
         BZ    *+12                NO                                           
         CLI   DDS,YES             CHECK FOR DDS TERMINAL                       
         BNE   VALOPTR                                                          
         ST    R3,AOPTNTRY         SAVE OPTION ENTRY POINTER                    
         CLI   FSTOP,EQUALS        TEST IF EQUALS FOUND                         
         BNE   VALOPT9             NO                                           
         TM    OPTIND,KEYBOTH      ALLOW KEY ONLY OPTION                        
         BO    VALOPT10                                                         
         TM    OPTIND,KEYONLY      CANNOT ALLOW KEY ONLY OPTION                 
         BO    ERROR                                                            
         B     VALOPT10                                                         
         SPACE                                                                  
VALOPT9  TM    OPTIND,KEYONLY      NO EQUALS-PROBLEM WITH                       
         BO    VALOPT10            KEYWORD-PARAM OPTIONS                        
         TM    OPTIND,KEYBOTH      ALLOW KEY + OPTION                           
         BO    VALOPT10                                                         
         MVC   XTRA(12),=C'OPTION VALUE'                                        
         B     ERROR                                                            
         SPACE                                                                  
VALOPT10 SR    R2,R2               DUPLICATE OPTION CHECK                       
         ICM   R2,3,OPTOUT                                                      
         LA    R2,PAYWRKD(R2)                                                   
         ZIC   R1,OPTOUTL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),0(R2)                                                    
         BZ    *+14                                                             
         MVC   XTRA(16),=C'DUPLICATE OPTION'                                    
         B     ERROR                                                            
*                                                                               
         SR    RF,RF               CALL OPTION VALUE ROUTINE                    
         ICM   RF,3,OPTORTN        DISPLACEMENT TO ROUTINE                      
         A     RF,SBBASE1                                                       
         BASR  RE,RF                                                            
         B     VALOPT2                                                          
         SPACE                                                                  
VALOPTR  MVC   XTRA(6),=C'OPTION'                                               
         B     ERROR                                                            
VALOPT20 CLI   SCREEN,X'FB'                                                     
         BNE   VALOPTX             NO COMMERCIAL OPTION                         
         CLI   PAYPROF2+1,C'Y'                                                  
         BNE   VALOPTR                                                          
         CLI   PAYDLRS,0           GR OR NET                                    
         BNE   VALOPTX                                                          
         MVI   FERN,CMLRGORN                                                    
         B     ERROR                                                            
         SPACE                                                                  
VALOPTX  MVI   FNDX,0                                                           
         B     VLOC                                                             
         SPACE 2                                                                
HELPCOMP CLC   FLD(0),=C'HELP'                                                  
         EJECT                                                                  
* COMMON ROUTINE TO LOCK RECS BY TYPE, OR CHECK TO SEE IF LOCKED *              
         SPACE                                                                  
VLOC     L     R3,AACTNTRY         SAVE A(ACTION ENTRY)                         
         USING ACTTABD,R3                                                       
*                                                                               
*--CHECK FOR LOCKABLE ACTIONS                                                   
         CLC   ACTCODE,=CL6'CHANGE'                                             
         BE    VLOC20                                                           
         CLC   ACTCODE,=CL6'CLEAR '                                             
         BE    VLOC20                                                           
         CLC   ACTCODE,=CL6'CLAPY '                                             
         BE    VLOC20                                                           
         B     VLOCEX                                                           
*                                                                               
VLOC20   LA    R4,WORK                                                          
         USING LKKEYD,R4                                                        
         XC    WORK,WORK                                                        
         L     R7,ACOMFACS                                                      
         SPACE                                                                  
         L     RF,CGETFACT-COMFACSD(,R7)                                        
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         MVC   LOCKSE,FASYS-FACTSD(RE)                                          
         SPACE                                                                  
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=CL2'UN'                                                 
         MVC   LOCKKEY(3),CLI                                                   
         OI    LOCKKEY+2,X'40'                                                  
         XC    LOCKKEY+3(7),LOCKKEY+3                                           
         SPACE                                                                  
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         GOTO1 VCALOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         SPACE                                                                  
         PRINT GEN                                                              
         GOTO1 (R2),(R1),(C'W',WORK),(R7)                                       
         PRINT NOGEN                                                            
         SPACE                                                                  
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   VLOCERR                                                          
*                                                                               
*  CHECK STATION LEVEL LOCKS                                                    
*                                                                               
         MVC   LOCKKEY+3(4),NETWORK                                             
         L     R7,ACOMFACS                                                      
         GOTO1 (R2),(R1),(C'W',WORK),(R7)                                       
         PRINT NOGEN                                                            
         SPACE                                                                  
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   VLOCERR                                                          
*                                                                               
VLOCEX   B     EXXMOD                                                           
*--LOCK  ERROR                                                                  
VLOCERR  MVI   FERN,CLLOKERR                                                    
         B     ERROR                                                            
         SPACE                                                                  
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET OPTION PARAMETER VALUE                                     
*                                                                               
GETVAL   LR    R0,RE                                                            
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BNE   GETVALX                                                          
GETVALE  MVC   XTRA(15),=C'NO OPTION VALUE'                                     
         B     ERROR                                                            
         SPACE                                                                  
GETVALX  LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK FOR NUMERIC DATA 1-255                                   
*                                                                               
CHKNUM   TM    FLDH+4,X'08'                                                     
         BZ    ERROR                                                            
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO VALIDATE PRODUCT (FIRST)                                           
*                                                                               
VALPRD   ST    RE,SAVEBRE                                                       
         XC    FTERM,FTERM                                                      
         MVC   FTERM(3),=C'/*,'                                                 
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BE    GETVALE             ERROR NO INFO FOUND                          
*                                                                               
         USING CLTHDRD,RE                                                       
*                                                                               
         BAS   RE,PRDCHECK         CHECK FORMAT OF PRODUCT                      
*                                                                               
         BAS   RE,PRDKEY           GET PASSIVE KEY                              
         BNE   PRD1ERR                                                          
*                                                                               
VALPRD4  MVC   PROD,THREE                                                       
         MVC   PRDN,BYTE           RETURNED FROM PDKEY                          
         MVC   NBSELPRD,PROD                                                    
         MVC   NBSELPNM,PRDN                                                    
         MVI   NBPIGOPT,C'N'       ONLY SINGLE PRODUCT                          
         L     RE,SAVEBRE                                                       
*                                                                               
         CLI   FSTOP,C'/'                                                       
         BNE   VALPRD15                                                         
*-VALIDATE SECOND PRODUCT                                                       
*******  MVC   NBSELPRD(3),=C'POL'                                              
         MVI   NBSELPNM,0                                                       
*                                                                               
         XC    FTERM,FTERM                                                      
         MVC   FTERM(2),=C'*,'                                                  
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BE    GETVALE             ERROR NO INFO FOUND                          
         CLI   FLDH+5,3                                                         
         BNE   *+14                                                             
         CLC   FLD(3),=CL3'ALL'    CHECK FOR ALL SECOND PRODUCT                 
         BE    VALPRD9                                                          
         CLC   FLD(3),=CL3'+++'    CHECK FOR *** SECOND PRODUCT                 
         BE    VALPRD10                                                         
         BAS   RE,PRDCHECK         CHECK FORMAT OF PRODUCT                      
         BAS   RE,PRDKEY                                                        
         BNE   PRD2ERR                                                          
*                                                                               
VALPRD8  MVC   NBSELPG3(3),NBSELPRD                                             
         MVC   PROD2,THREE                                                      
         MVC   PRDN2,BYTE          RETURNED BY PRDKEY                           
         MVC   NBSELPG3+3(3),THREE                                              
         MVI   NBPIGOPT,C'Y'       PIGGYS                                       
         B     VALPRD15                                                         
*--"ALL" OPTION  GIVES PIGS AND SINGLES                                         
VALPRD9  MVI   NBPIGOPT,0          BOTH SINGLES AND PIGGYS                      
         MVI   PRDLKSW,X'FF'       CHECK PRODUCT IN FILTERS                     
         B     VALPRD15                                                         
*                                                                               
*--"***" OPTION  GIVES JUST PIGS                                                
VALPRD10 MVI   NBPIGOPT,C'Y'       BOTH SINGLES AND PIGGYS                      
         MVI   PRDLKSW,X'FF'       CHECK PRODUCT IN FILTERS                     
         B     VALPRD15                                                         
*                                                                               
VALPRD15 CLI   FSTOP,C'*'                                                       
         BNE   VALPRDX                                                          
*-VALIDATE FIRST PRODUCT PCT                                                    
         XC    FTERM,FTERM                                                      
         MVI   FTERM,C','                                                       
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BE    GETVALE             ERROR NO INFO FOUND                          
         CLI   FLDH+5,3            MAX NUMBER =100                              
         BH    PRDERPT                                                          
*                                                                               
         BAS   RE,CHKNUM                                                        
         MH    R0,=H'100'          GET RIGHT PRECISSION                         
         STCM  R0,3,P1PCT          1ST PRODUCT PERCENT                          
*                                                                               
VALPRDX  L     RE,SAVEBRE                                                       
         BR    RE                                                               
*                                                                               
PRD1ERR  MVC   XTRA(21),=C'INVALID FIRST PRODUCT'                               
         B     ERROR                                                            
PRD2ERR  MVC   XTRA(22),=C'INVALID SECOND PRODUCT'                              
         B     ERROR                                                            
PRDERPT  MVC   XTRA(23),=C'INVALID PRODUCT PERCENT'                             
         B     ERROR                                                            
         DROP  RE                                                               
         SPACE 2                                                                
* PRDCHECK CHECKS THE LENGTH AND FORMAT OF THE PRODUCT                          
* IN FLD MOVES PRODUCT IN THREE                                                 
PRDCHECK NTR1                                                                   
         CLI   FLDH+5,2            PRODUCT IS 2-3 CHARS AND ALPHA               
         BE    *+12                                                             
         CLI   FLDH+5,3                                                         
         BNE   ERROR                                                            
         ZIC   RF,FLDH+5                                                        
         LA    R1,FLD                                                           
         BAS   RE,PRDFORM          CHECK FORMAT OF PRODUCT                      
         BNZ   ERROR                                                            
         MVC   THREE,FLD                                                        
         B     EXXMOD                                                           
* PRDFORM-CHECKS FOR ALPHA IN FIRST PRODUCT FIELD, AND ALPHA-NUMERIC            
* IN THE REST.                                                                  
*        RF=LENGTH OF FIELD                                                     
*        R1=FIELD                                                               
*                                                                               
PRDFORM  NTR1                                                                   
         BCTR  RF,0                                                             
*--CHECK FIRST CHARACTER FOR ALPHA                                              
         CLI   0(R1),X'C1'                                                      
         BL    PRDBFRM                                                          
         CLI   0(R1),X'E9'                                                      
         BH    PRDBFRM                                                          
         LA    R1,1(R1)                                                         
*                                                                               
PRDFM20  CLI   0(R1),X'F0'                                                      
         BL    PRDFM30                                                          
         CLI   0(R1),X'F9'                                                      
         BNH   PRDFM40                                                          
         B     PRDBFRM                                                          
PRDFM30  CLI   0(R1),X'C1'                                                      
         BL    PRDBFRM                                                          
         CLI   0(R1),X'E9'                                                      
         BH    PRDBFRM                                                          
PRDFM40  LA    R1,1(R1)                                                         
         BCT   RF,PRDFM20                                                       
*                                                                               
         SR    R1,R1               SET GOOD RETURN                              
PRDBFRM  LTR   R1,R1                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO VALIDATE ESTIMATE VALUES                                           
*                                                                               
VALEST   ST    RE,SAVEBRE                                                       
         XC    FTERM,FTERM                                                      
         MVC   FTERM(4),=C'-=/,'                                                
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BNE   VALEST2                                                          
         MVC   XTRA(15),=C'NO OPTION VALUE'                                     
         B     ERROR                                                            
         SPACE                                                                  
VALEST2  CLI   FSTOP,DASH          TEST FOR RANGE                               
         BE    VALEST4             YES                                          
         CLI   FSTOP,SLASH         TEST FOR POSSIBLE FILTER                     
         BE    VALEST6             YES                                          
         BAS   RE,CHKNUM                                                        
         MVI   ESTTYP,ISINGLE                                                   
         STC   R0,EST                                                           
         MVC   NBSELEST,EST        SEED NETBLOCK VALUE                          
         B     VALESTX                                                          
         SPACE                                                                  
VALEST4  BAS   RE,CHKNUM                                                        
         STC   R0,EST                                                           
         XC    FTERM,FTERM         GET END OF RANGE                             
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         BAS   RE,CHKNUM                                                        
         CLM   R0,1,EST                                                         
         BL    ERROR                                                            
         STC   R0,EST+1                                                         
         MVI   ESTTYP,IRANGE                                                    
         MVC   NBSELEST(2),EST     SEED RANGE IN NETBLOCK                       
         B     VALESTX                                                          
         SPACE                                                                  
VALEST6  CLI   FLDH+5,1                                                         
         BNE   ERROR                                                            
         CLI   FLD,C'F'            TEST FOR 'F' PREFIX                          
         BNE   ERROR                                                            
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,3            FILTER IS UP TO 3 CHARS                      
         BH    ERROR                                                            
         MVI   ESTTYP,IFILTER                                                   
         MVC   NBSELEFL,FLD                                                     
         MVC   EST,FLD                                                          
         B     VALESTX                                                          
         SPACE                                                                  
VALESTX  L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO VALIDATE INVOICE NUMBER                                            
*                                                                               
VALINV   ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         MVC   INVNUM,FLD                                                       
         OC    INVNUM,SPACES                                                    
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO VALIDATE PRODUCT GROUP                                             
*                                                                               
VALPGRP  ST    RE,SAVEBRE                                                       
         XC    FTERM,FTERM                                                      
         MVC   FTERM(4),=C'-=/,'                                                
         GOTO1 AFVAL                                                            
         BAS   RE,GETPGRP           GET PRODUCT GROUP                           
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO VALIDATE PACKAGE NUMBER                                            
*                                                                               
VALPAK   ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         BAS   RE,CHKNUM                                                        
         STC   R0,PACK                                                          
         MVC   NBSELPAK,PACK                                                    
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO VALIDATE PROGRAM NAME                                              
*                                                                               
VALPROG  ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         CLI   FLDH+5,6                                                         
         BH    ERROR                                                            
         MVC   PROGRAM,FLD                                                      
         MVC   NBSELPRG,PROGRAM                                                 
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
* ROUTINE TO VALIDATE DAYPART                                                   
*                                                                               
VALDP    ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         CLI   FLDH+5,2                                                         
         BH    ERROR                                                            
         OI    FLD+1,X'40'                                                      
*                                                                               
         LA    R4,KEY                                                           
         USING DPTRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,AGYMED                                                   
*                                                                               
VALDP010 GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         B     VALDP040                                                         
VALDP020 GOTO1 AIO,DMCB,UNT+DIR+SEQ                                             
                                                                                
VALDP040 CLC   KEY(5),KEYSAVE      CHECK UP TO CLIENT                           
         BNE   VALDP080            YES                                          
         CLC   NDPTDPTA,FLD        CHECK FOR MATCH ON CODE                      
         BNE   VALDP020            GET NEXT RECORD                              
*  MOVE DATA OUT AND EXIT                                                       
         B     VALDP090                                                         
*                                                                               
* CHECK KEY FOR AGENCY OR CLIENT LEVEL CHECK                                    
* IF AGENCY LEVEL RESET KEY MOVE CLIENT CODE IN RESTART SEARCH                  
* IF CLIENT LEVEL EXIT ROUTINE DEMO WAS INVALID                                 
*                                                                               
VALDP080 OC    KEYSAVE+3(2),KEYSAVE+3                                           
         BNZ   VALDP100                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),KEYSAVE                                                   
         MVC   KEY+3(2),CLIPK                                                   
         B     VALDP010                                                         
*                                                                               
VALDP090 MVC   DAYPART,NDPTDPTE                                                 
         MVC   NBSELDP,DAYPART                                                  
         B     VALDPEX                                                          
*                                                                               
*  OLD TABLE VALIDATION METHOD                                                  
*                                                                               
VALDP100 BAS   RE,GETVAL                                                        
         CLI   FLDH+5,1                                                         
         BNE   ERROR                                                            
         LA    R0,DAYPARTS                                                      
         LA    RE,DPTTAB                                                        
         CLC   FLD(1),0(RE)                                                     
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,*-14                                                          
         B     ERROR                                                            
*                                                                               
         MVC   DAYPART,FLD                                                      
         MVC   NBSELDP,DAYPART                                                  
*                                                                               
VALDPEX  L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO VALIDATE FILE SEQUENCE                                             
*                                                                               
VALSEQ   ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         CLI   FLDH+5,1                                                         
         BNE   ERROR                                                            
         CLI   FLD,C'D'                                                         
         BE    *+16                                                             
         CLI   FLD,C'P'                                                         
         BNE   ERROR                                                            
         MVI   FLD,C'Q'            INTERNAL VALUE FOR PROGRAM SEQUNCE           
         MVC   SEQUENCE,FLD                                                     
         MVC   NBSEQ,SEQUENCE                                                   
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO VALIDATE FILTER VALUE                                              
*                                                                               
VALFIL   ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         CLI   FLDH+5,1                                                         
         BNE   VALFIL2                                                          
         TM    FLDH+4,X'0C'        EITHER ALPHA OR NUMBER                       
         BZ    ERROR                                                            
         MVC   FILTER,FLD                                                       
         B     VALFILX                                                          
         SPACE                                                                  
VALFIL2  CLI   FLDH+5,2                                                         
         BNE   ERROR                                                            
         CLI   FLD,STAR            TEST FOR NEGATIVE FILTER                     
         BNE   ERROR                                                            
         LA    R0,1                                                             
         GOTO1 CHKAN,FLD+1                                                      
         MVC   FILTER,FLD+1                                                     
         NI    FILTER,X'FF'-X'40'  SET TO LOWER CASE                            
         SPACE                                                                  
VALFILX  L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE                                                                  
* SUB-ROUTINE TO CHECK FOR ALPHA-NUMERIC CHARACTERS (R1 POINTS TO CHA-          
* RACTERS AND R0 HAS COUNT OF CHARS TO CHECK                                    
*                                                                               
CHKAN    CLI   0(R1),X'C1'                                                      
         BL    ERROR                                                            
         CLI   0(R1),X'C9'                                                      
         BNH   CHKAN2                                                           
         CLI   0(R1),X'D1'                                                      
         BL    ERROR                                                            
         CLI   0(R1),X'D9'                                                      
         BNH   CHKAN2                                                           
         CLI   0(R1),X'E2'                                                      
         BL    ERROR                                                            
         CLI   0(R1),X'E9'                                                      
         BNH   CHKAN2                                                           
         CLI   0(R1),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(R1),X'F9'                                                      
         BH    ERROR                                                            
*                                                                               
CHKAN2   LA    R1,1(R1)                                                         
         BCT   R0,CHKAN                                                         
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE BILLED STATUS                                                        
*                                                                               
VALBI    ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         CLI   FLDH+5,4                                                         
         BH    ERROR                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,BILLTAB                                                       
*                                                                               
VALBI2   CLI   0(RE),EOT                                                        
         BE    ERROR                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RE)                                                     
         BE    VALBI4                                                           
         LA    RE,L'BILLTAB(RE)                                                 
         B     VALBI2                                                           
*                                                                               
VALBI4   MVC   BILLST,4(RE)                                                     
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE CLEARED STATUS                                                       
*                                                                               
VALCL    ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         CLI   FLDH+5,4                                                         
         BH    ERROR                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,CLEARTAB                                                      
*                                                                               
VALCL2   CLI   0(RE),EOT                                                        
         BE    ERROR                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RE)                                                     
         BE    VALCL4                                                           
         LA    RE,L'CLEARTAB(RE)                                                
         B     VALCL2                                                           
*                                                                               
VALCL4   MVC   CLEARST,4(RE)                                                    
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE AMOUNT OPTION                                                        
*                                                                               
VALAM    ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         CLI   FLDH+5,1                                                         
         BNE   ERROR                                                            
         CLI   FLD,C'G'                                                         
         BE    *+12                                                             
         CLI   FLD,C'N'                                                         
         BNE   ERROR                                                            
         MVC   AMTOPT,FLD                                                       
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* TIME KEYWORD CODE                                                             
*                                                                               
VALTIM   MVI   PAYTYPE,C'T'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* INTEGRATION KEYWORD CODE                                                      
*                                                                               
VALINT   MVI   PAYTYPE,C'I'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* TIME AND INTEGRATION KEYWORD CODE                                             
*                                                                               
VALTINT  MVI   PAYTYPE,C'Z'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* CUT-IN KEYWORD CODE                                                           
*                                                                               
VALCUT   MVI   PAYTYPE,C'U'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* BLACKOUT KEYWORD CODE                                                         
*                                                                               
VALBLK   MVI   PAYTYPE,C'B'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* COPY-SPLIT KEYWORD CODE                                                       
*                                                                               
VALCOP   MVI   PAYTYPE,C'S'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* TAX KEYWORD CODE                                                              
*                                                                               
VALTAX   MVI   PAYTYPE,C'X'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* ADMINISTRATION KEYWORD CODE                                                   
*                                                                               
VALADM   MVI   PAYTYPE,C'A'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* LATE CHARGE KEYWORD CODE                                                      
*                                                                               
VALLCH   MVI   PAYTYPE,C'L'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* SECTIONAL KEYWORD CODE                                                        
*                                                                               
VALSEC   MVI   PAYTYPE,C'E'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* OTHER KEYWORD CODE                                                            
*                                                                               
VALOTH   MVI   PAYTYPE,C'O'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* DELIVERY ADJUSTMENT                                                           
*                                                                               
VALDAD   MVI   PAYTYPE,C'D'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* NEW EC TYPE                                                                   
*                                                                               
VALEC    MVI   PAYTYPE,C'Q'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* LIST WITH COMMERCIAL INPUT ABILITY                                            
*                                                                               
VALCOML  CLI   ACTION,LIST                                                      
         BE    VALC100                                                          
         CLI   ACTION,CHA                                                       
         BE    VALC100                                                          
         CLI   ACTION,DIS                                                       
         BNE   VALC200                                                          
VALC100  MVI   SCREEN,X'FB'        CALL IN SCREEN WITH COMML FIELD              
*                                                                               
VALC200  ST    RE,SAVEBRE                                                       
         CLI   FSTOP,C'='                                                       
         BNE   VALCEXT                                                          
         BAS   RE,GETVAL                                                        
         LA    R0,8                                                             
         GOTO1 CHKAN,FLD                                                        
         MVC   SVCOML,FLD                                                       
*                                                                               
VALCEXT  L     RE,SAVEBRE                                                       
         BR    RE                                                               
         SPACE 2                                                                
* LIST WITH GROSS /PRODUCT CODE DISPLAYED                                       
*                                                                               
VALNAFF  MVI   SVNAFF,C'Y'                                                      
         BR    RE                                                               
         SPACE 2                                                                
* LIST WITH GROSS /PRODUCT CODE DISPLAYED                                       
*                                                                               
VALGRS   MVI   PAYDLRS,C'G'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* LIST WITH NET /PRODUCT CODE DISPLAYED                                         
*                                                                               
VALNET   MVI   PAYDLRS,C'N'                                                     
         BR    RE                                                               
         SPACE 2                                                                
* NO AFFIDAVIT CHECK REQUIRED FOR CLEARING                                      
*                                                                               
VALMTCH  MVI   NOMTCH,C'Y'                                                      
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE EDIT ON ACTUAL COST                                                  
*                                                                               
VALAC    ST    RE,SAVEBRE                                                       
         BAS   RE,GETVAL                                                        
         ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,(2,FLD),(R0)                                       
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         MVC   ACTOPT,4(R1)                                                     
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO PRODUCE OPTION HELP SCREEN                                         
*                                                                               
OPTHELP  BAS   RE,HELPSCR          SET UP HELP SCREEN                           
         MVC   HELHED1(L'OPTHED1),OPTHED1 SET UP HEADLINES                      
         MVC   HELHED1+12(L'OPTHED2),OPTHED2                                    
         MVC   HELHED1+24(L'OPTHED3),OPTHED3                                    
*                                                                               
         MVI   HELHED2,DASH        UNDERSCORE THE HEADLINES                     
         MVC   HELHED2+1(L'OPTHED1-1),HELHED2                                   
         MVI   HELHED2+12,DASH                                                  
         MVC   HELHED2+13(L'OPTHED2-1),HELHED2+12                               
         MVI   HELHED2+24,DASH                                                  
         MVC   HELHED2+25(L'OPTHED3-1),HELHED2+24                               
*                                                                               
         LA    R0,LINES            SET MAXIMUM SCREEN LINE COUNTER              
         LA    R2,HELLIN1          POINT TO FIRST DATA FIELD                    
         LA    R3,OPTTAB           R3 POINTS TO OPTION TABLE                    
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0            TEST FOR NO DATA FOUND                       
         BE    OPTHELP2            NO-EDIT FIELD                                
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,NEXTCOMP                                                      
         BNE   OPTHELP2                                                         
*******  LA    R3,NEXTOPTA                                                      
         L     R3,=A(NEXTOPTA)                                                  
         A     R3,SBRELO                                                        
         USING OPTTABD,R3                                                       
         SPACE 1                                                                
OPTHELP2 CLI   OPTNAME,EOT         TEST FOR END-OF-TABLE                        
         BE    OPTHELPX                                                         
         MVC   0(L'OPTNAME,R2),OPTNAME                                          
         ZIC   R1,OPTMINL          MINIMUM LENGTH OF OPTION                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R2),OPTNAME                                                 
         LA    RE,13(R1,R2)        POINT TO END OF ABBREV.                      
         TM    OPTIND,KEYONLY      TEST FOR KEYWORD ONLY                        
         BO    *+8                                                              
         MVI   0(RE),EQUALS                                                     
         MVC   24(L'OPTDESC,R2),OPTDESC  OPTION DESCRIPTION                     
         SPACE 1                                                                
OPTHELP4 LA    R2,LINELEN(R2)      NEXT SCREEN LINE                             
         LA    R3,OPTNTRL(R3)      NEXT OPTION ENTRY                            
         CLI   0(R3),X'FF'         LAST OPTION                                  
         BE    *+8                                                              
         BCT   R0,OPTHELP2                                                      
         SPACE 1                                                                
OPTHELPX NI    MODE,X'FF'-FIRST-DISPLAY                                         
         MVC   PAYMSG(L'HELPMSG),HELPMSG                                        
         LA    R2,PAYOPTH          CURSOR AT OPTIONS FIELD                      
         ST    R2,FADDR                                                         
         GOTO1 VEXIT                                                            
         SPACE 2                                                                
NEXTCOMP CLC   FLD(0),=C'NEXT'                                                  
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE HELP SCREEN                                         
*                                                                               
HELPSCR  ST    RE,SAVEBRE                                                       
         CLI   LSCREEN,X'FC'       TEST IF HELP SCREEN THERE                    
         BE    HELPSCR2            YES                                          
         MVC   DMCB+4(4),=X'D90313FC'                                           
         GOTO1 VCALOV,DMCB,PAYLAST                                              
         CLI   DMCB+4,X'FF'        TEST FOR ERROR                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   LSCREEN,X'FC'                                                    
         SPACE 1                                                                
HELPSCR2 MVC   SCREEN,LSCREEN                                                   
         GOTO1 VCLEARF,DMCB,(1,HELHED1H),HELLAST                                
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
*                                                                               
VALCRCK  ST    RE,SAVEBRE                                                       
         XC    FTERM,FTERM                                                      
         MVI   FTERM,EQUALS                                                     
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BNE   *+14                                                             
         MVC   XTRA(15),=C'NO OPTION VALUE'                                     
         B     ERROR                                                            
         GOTO1 VDATVAL,DMCB,(0,FLD),DUB                                         
         MVI   FERN,DATERR                                                      
         OC    0(4,R1),0(R1)       TEST FOR VALID DATE                          
         BZ    ERROR               NO                                           
         GOTO1 VDATCON,DMCB,(0,DUB),(2,NBPAYSTR)                                
         MVC   NBPAYEND,NBPAYSTR                                                
         MVC   CRCKDAT,NBPAYSTR                                                 
         L     RE,SAVEBRE                                                       
         BR    RE                                                               
                                                                                
         EJECT                                                                  
*******************************************************************             
* GET PRODUCT GROUP                                                             
*******************************************************************             
GETPGRP  NTR1                                                                   
         LA    R4,KEY                                                           
         USING PRGRECD,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   PRGKAGMD,AGYMED                                                  
         MVC   PRGKCLT,CLIPK                                                    
         MVC   PRGKID,FLD           GROUP ID                                    
*                                                                               
         GOTO1 AIO,DMCB,SPT+HIGH+DIR                                            
         CLC   KEY(L'PRGKEY),KEYSAVE                                            
         BNE   GPGRPERR                                                         
*                                                                               
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA4                                   
         GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE'),(X'01',AIOAREA4),0                
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BE    *+6                 YES                                          
         DC    H'00'                                                            
*                                                                               
         L     R5,12(R1)           VALIDATE GROUP # FOR GROUP ID                
         USING PRGEL01,R5                                                       
*                                                                               
         ZIC   R1,PRGBK1LN                                                      
         ZIC   R4,PRGBK2LN                                                      
         AR    R1,R4                                                            
         LA    R1,1(R1)            COMPENSATE FOR LETTER CODE                   
         STC   R1,WORK                                                          
         MVC   BYTE,FLDH+5                                                      
         CLC   BYTE,WORK                                                        
         BNE   GPGRPERR                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),FLD+1       LET ALIGNED PWOS                             
         PACK  WORK+10(3),WORK(5)                                               
*                                                                               
         MVC   PGRP,FLD            GROUP ID                                     
         MVC   PGRP+1(2),WORK+10   GROUP NUMBER (LEFT ALIGNED)                  
*                                                                               
GPGRPX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4,R5                                                            
*                                                                               
GPGRPERR DS    0H                                                               
         XC    FTERM,FTERM                                                      
         GOTO1 AFVAL                                                            
         LA    R1,PAYOPTH                                                       
         ST    R1,FADDR                                                         
         MVC   XTRA(21),=C'INVALID PRODUCT GROUP'                               
         B     ERROR                                                            
*                                                                               
* SUB-ROUTINE TO READ A STATION ADDRESS RECORD AND FORMAT TO SCREEN             
*                                                                               
GETADDR  NTR1                      READ STATION ADDRESS REC                     
         LA    R4,KEY                                                           
         USING ADDRECD,R4                                                       
         MVI   ADDRKEY,C'0'                                                     
         MVC   ADDRKEY+1(L'ADDRKEY-1),ADDRKEY                                   
         MVI   ADDKTYPE,C'A'                                                    
         MVI   ADDKMED,C'N'                                                     
         MVC   ADDKCALL(4),NETWORK                                              
         MVI   ADDKCALL+4,C'N'                                                  
         MVC   ADDKAGY,AGYALPH                                                  
         GOTO1 AIO,DMCB,HIGH+FILE+STA,AIOAREA2                                  
         MVI   FERN,ADDRERR                                                     
         CLC   KEY(L'ADDRKEY),KEYSAVE                                           
         BE    GETADDR2                                                         
         LA    R1,PAYNETH                                                       
         ST    R1,FADDR                                                         
         B     ERROR                                                            
*                                                                               
GETADDR2 L     R2,AIOAREA3                                                      
         L     R4,AIOAREA2         POINT TO RECORD NOW                          
         MVC   0(81,R2),SPACES                                                  
         MVC   0(20,R2),ANAME                                                   
         MVC   21(24,R2),A1LINE                                                 
         MVC   46(24,R2),A2LINE                                                 
         MVC   71(2,R2),A3LINE                                                  
         MVC   74(5,R2),AZIP                                                    
         OC    1(80,R2),SPACES                                                  
         GOTO1 VSQUASH,DMCB,(R2),80                                             
*                                                                               
         MVC   PAYNETX,0(R2)                                                    
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET REP RECORD AND TO FORMAT ON SCREEN                         
*                                                                               
GETREP   NTR1                      READ REP RECORD                              
         LA    R4,KEY                                                           
         USING REPRECD,R4                                                       
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(L'REPKEY-1),REPKEY                                      
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,REP                                                      
         MVC   REPKAGY,AGYALPH                                                  
         GOTO1 AIO,DMCB,HIGH+FILE+STA,AIOAREA2                                  
         MVI   FERN,REPERR                                                      
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         L     R2,AIOAREA3                                                      
         L     R4,AIOAREA2         POINT R4 AT RECORD                           
         MVC   0(85,R2),SPACES                                                  
         MVC   0(22,R2),RNAME                                                   
         MVC   REPNAME,RNAME       SAVE REP NAME FOR REPORT                     
         MVC   23(24,R2),R1LINE                                                 
         MVC   48(24,R2),R2LINE                                                 
         MVC   73(2,R2),R3LINE                                                  
         MVC   76(5,R2),RZIP                                                    
         OC    1(84,R2),SPACES                                                  
         GOTO1 VSQUASH,DMCB,(R2),85                                             
         MVC   PAYEEX,0(R2)                                                     
*                                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* RETURN FROM MODULE AND ROUTINES                                               
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
********************************************************************            
* PRDKEY       INPUT: THREE HAS 3 BYTE ALPHA PRODUCT                            
*              OUTPUT: BYTE (1 BYTE BINARY OR 0 FOR OVERFLOW)                   
********************************************************************            
PRDKEY   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PLSTPSSV,R2                                                      
         MVC   PLSTTYPE(2),=X'0DF1'                                             
         MVC   PLSTAM,AGYMED                                                    
         MVC   PLSTCLT,CLIPK                                                    
         MVC   PLSTPRD,THREE                                                    
         CLC   PLSTPRD,=C'POL'                                                  
         BNE   *+8                                                              
         MVI   PLSTXFF,X'FF'                                                    
         GOTO1 AIO,DMCB,SPT+HIGH+DIR                                            
         CLC   KEY(10),KEYSAVE                                                  
         BNE   PRDKX                                                            
         MVC   BYTE,PLSTBPRD                                                    
         SR    R0,R0                                                            
         LTR   R0,R0                                                            
PRDKX    B     EXXMOD                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
PLID     DC    C'NEPL'             PAYING LIST REPORT ID                        
HELPMSG  DC    C'** AVAILABLE OPTIONS DISPLAYED **'                             
OPTHED1  DC    C'OPTION'                                                        
OPTHED2  DC    C'ABBREV.'                                                       
OPTHED3  DC    C'OPTION DESCRIPTION'                                            
         SPACE 2                                                                
* TABLE OF BILLING STATUS VALUES                                                
*                                                                               
BILLTAB  DS    0CL6                                                             
         DC    CL4'YES',AL2(BILLED)                                             
         DC    CL4'NO',AL2(UNBILLED)                                            
         DC    CL4'TIME',AL2(TBILL)                                             
         DC    CL4'INT',AL2(INTBILL)                                            
         DC    CL4'CS',AL2(COPYBILL)                                            
         DC    CL4'BO',AL2(BLAKBILL)                                            
         DC    CL4'CI',AL2(CUTIBILL)                                            
         DC    CL4'OT',AL2(OTHBILL)                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF CLEARED STATUS VALUES                                                
*                                                                               
CLEARTAB DS    0CL6                                                             
         DC    CL4'YES',AL2(PAID)                                               
         DC    CL4'NO',AL2(UNPAID)                                              
         DC    CL4'PART',AL2(PARTPAID)                                          
         DC    CL4'TIME',AL2(TPAID)                                             
         DC    CL4'INT',AL2(INTPAID)                                            
         DC    CL4'CS',AL2(COPYPAID)                                            
         DC    CL4'BO',AL2(BLAKPAID)                                            
         DC    CL4'CI',AL2(CUTIPAID)                                            
         DC    CL4'OT',AL2(OTHPAID)                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
* DAYPART TABLE                                                                 
*                                                                               
DPTTAB   DC    C'DFPKTYSENLCXAO'                                                
DAYPARTS EQU   *-DPTTAB                                                         
         SPACE 2                                                                
* TABLE OF ACTIONS COVERED BY ACTTABD DSECT                                     
*                                                                               
ACTTAB   DS    0CL(ACTNTRL)                                                     
         DC    CL6'CLEAR',AL1(CLEAR),X'02'                                      
         DC    X'00',X'02',X'FE'                                                
*                                                                               
         DC    CL6'CLAPY',AL1(CLEAR),X'02'                                      
         DC    X'00',X'02',X'FE'                                                
*                                                                               
         DC    CL6'TEST',AL1(TEST),X'01'                                        
         DC    X'00',X'02',X'FE'                                                
*                                                                               
         DC    CL6'PRINT',AL1(PRINT),X'01'                                      
         DC    AL1(ALLSREP),X'04',X'00'                                         
*                                                                               
         DC    CL6'DIS',AL1(DIS),X'01'                                          
         DC    AL1(ALLSREP),X'03',X'FD'                                         
*                                                                               
         DC    CL6'LIST',AL1(LIST),X'01'                                        
         DC    AL1(ALLSREP),X'03',X'FD'                                         
*                                                                               
         DC    CL6'CHANGE',AL1(CHA),X'02'                                       
         DC    AL1(ALLSREP),X'03',X'FD'                                         
*                                                                               
         DC    CL6'AUDIT',AL1(AUD),X'02'                                        
         DC    X'00',X'05',X'FA'                                                
*                                                                               
         DC    CL6'NAUDIT',AL1(AUD),X'02'                                       
         DC    X'00',X'06',X'FA'                                                
*                                                                               
         DC    X'FF'               END OF TABLE                                 
         SPACE 2                                                                
* TABLE OF OPTIONS COVERED BY OPTTABD DSECT                                     
*                                                                               
OPTTAB   DS    0CL(OPTNTRL)                                                     
         DC    CL11'PRODUCT'                                                    
         DC    X'01',X'000000'                                                  
         DC    AL2(PROD-PAYWRKD),AL1(L'PROD),AL2(VALPRD-T31301)                 
         DC    C'PRODUCT FILTERING - P=AA                             '         
*                                                                               
         DC    CL11'ESTIMATE'                                                   
         DC    X'01',X'000000'                                                  
         DC    AL2(EST-PAYWRKD),AL1(L'EST),AL2(VALEST-T31301)                   
         DC    C'ESTIMATE SELECTION - E=1,E=2-5,E=F/XY (EST FILTER XY)'         
*                                                                               
         DC    CL11'INVOICE'                                                    
         DC    X'03',X'000000'                                                  
         DC    AL2(INVNUM-PAYWRKD),AL1(L'INVNUM),AL2(VALINV-T31301)             
         DC    C'INVOICE FILTER - INV=XXXXXXXXXX                      '         
*                                                                               
         DC    CL11'PGROUP'                                                     
         DC    X'03',X'000000'                                                  
         DC    AL2(PGRP-PAYWRKD),AL1(L'PGRP),AL2(VALPGRP-T31301)                
         DC    C'PRODUCT GROUP FILTER - PGR=XXXX                      '         
*                                                                               
         DC    CL11'PACKAGE'                                                    
         DC    X'02',X'000000'                                                  
         DC    AL2(PACK-PAYWRKD),AL1(L'PACK),AL2(VALPAK-T31301)                 
         DC    C'PACKAGE FILTER - PA=1                                '         
*                                                                               
         DC    CL11'PROGRAM'                                                    
         DC    X'04',X'000000'                                                  
         DC    AL2(PROGRAM-PAYWRKD),AL1(L'PROGRAM),AL2(VALPROG-T31301)          
         DC    C'PROGRAM FILTER - PROG=DALLAS                         '         
*                                                                               
         DC    CL11'DP'                                                         
         DC    X'02',X'000000'                                                  
         DC    AL2(DAYPART-PAYWRKD),AL1(L'DAYPART),AL2(VALDP-T31301)            
         DC    C'DAYPART SELECTION - DP=P (PRIME TIME ONLY)           '         
*                                                                               
         DC    CL11'SEQUENCE'                                                   
         DC    X'03',X'000000'                                                  
         DC    AL2(SEQUENCE-PAYWRKD),AL1(L'SEQUENCE),AL2(VALSEQ-T31301)         
         DC    C'UNIT DISPLAY SEQUENCE - S=D (DATE) OR S=P (PROGRAM)  '         
*                                                                               
         DC    CL11'FILTER'                                                     
         DC    X'01',X'000000'                                                  
         DC    AL2(FILTER-PAYWRKD),AL1(L'FILTER),AL2(VALFIL-T31301)             
         DC    C'F=A OR F=*A (INCLUDE/EXCLUDE UNITS WITH FILTER=A)    '         
*                                                                               
         DC    CL11'BILLED'                                                     
         DC    X'01',X'000000'                                                  
         DC    AL2(BILLST-PAYWRKD),AL1(L'BILLST),AL2(VALBI-T31301)              
         DC    C'BILLED STATUS FILTER - YES,NO,TIME,INT (OR Y,N,T,I)  '         
*                                                                               
         DC    CL11'CLEARED'                                                    
         DC    X'01',X'000000'                                                  
         DC    AL2(CLEARST-PAYWRKD),AL1(L'CLEARST),AL2(VALCL-T31301)            
         DC    C'CLEARED STATUS FILTER - YES,NO,PART,TIME,INT (YNPTI) '         
*                                                                               
         DC    CL11'AMOUNT'                                                     
         DC    X'01',X'000000'                                                  
         DC    AL2(AMTOPT-PAYWRKD),AL1(L'AMTOPT),AL2(VALAM-T31301)              
         DC    C'A=G OR N TO OVERRIDE GROSS/NET OPTION FOR ACTION TEST'         
*                                                                               
         DC    CL11'ACTUAL'                                                     
         DC    X'03',X'000000'                                                  
         DC    AL2(ACTOPT-PAYWRKD),AL1(L'ACTOPT),AL2(VALAC-T31301)              
         DC    C'FILTER ON ACTUAL COST OF BUY, INPUT MUST BE NUMERIC  '         
*                                                                               
         DC    CL11'TIME'                                                       
         DC    X'01',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALTIM-T31301)           
         DC    C'CLEAR/DISPLAY TIME ONLY                              '         
*                                                                               
         DC    CL11'INTEGRATION'                                                
         DC    X'01',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALINT-T31301)           
         DC    C'CLEAR/DISPLAY INTEGRATION ONLY                       '         
*                                                                               
         DC    CL11'TINT'                                                       
         DC    X'04',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALTINT-T31301)          
         DC    C'CLEAR/DISPLAY TIME AND INTEGRATION                   '         
*                                                                               
         DC    CL11'CI'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALCUT-T31301)           
         DC    C'CLEAR/DISPLAY CUT-IN ONLY                            '         
*                                                                               
         DC    CL11'BO'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALBLK-T31301)           
         DC    C'CLEAR/DISPLAY BLACKOUT ONLY                          '         
*                                                                               
         DC    CL11'CS'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALCOP-T31301)           
         DC    C'CLEAR/DISPLAY COPY-SPLIT ONLY                        '         
*                                                                               
         DC    CL11'TX'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALTAX-T31301)           
         DC    C'CLEAR/DISPLAY TAX ONLY                               '         
*                                                                               
         DC    CL11'SE'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALSEC-T31301)           
         DC    C'CLEAR/DISPLAY SECTIONAL ONLY                         '         
*                                                                               
         DC    CL11'AD'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALADM-T31301)           
         DC    C'CLEAR/DISPLAY ADMINISTRATION ONLY                    '         
*                                                                               
         DC    CL11'LC'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALLCH-T31301)           
         DC    C'CLEAR/DISPLAY LATE CHARGES ONLY                      '         
*                                                                               
         DC    CL11'OT'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALOTH-T31301)           
         DC    C'CLEAR/DISPLAY OTHER ONLY                             '         
*                                                                               
         DC    CL11'DA'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALDAD-T31301)           
         DC    C'CLEAR/DISPLAY DELIVERY ADJUSTMENT                    '         
*                                                                               
         DC    CL11'EC'                                                         
         DC    X'02',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYTYPE-PAYWRKD),AL1(L'PAYTYPE),AL2(VALEC-T31301)            
         DC    C'CLEAR/DISPLAY NEW EC TYPE                            '         
*                                                                               
         DC    CL11'NOMATCH'                                                    
         DC    X'03',X'0000',AL1(KEYONLY)                                       
         DC    AL2(NOMTCH-PAYWRKD),AL1(L'NOMTCH),AL2(VALMTCH-T31301)            
         DC    C'NO AFFIDAVIT INFORMATION NEEDED TO CLEAR             '         
*                                                                               
         DC    CL11'GROSS'                                                      
         DC    X'01',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYDLRS-PAYWRKD),AL1(L'PAYDLRS),AL2(VALGRS-T31301)           
         DC    C'LIST/DISPLAY GROSS DOLLARS AND PRODUCT CODE          '         
*                                                                               
NEXTOPTA DC    CL11'NET'                                                        
         DC    X'01',X'0000',AL1(KEYONLY)                                       
         DC    AL2(PAYDLRS-PAYWRKD),AL1(L'PAYDLRS),AL2(VALNET-T31301)           
         DC    C'LIST/DISPLAY NET DOLLARS AND PRODUCT CODE            '         
*&&DO                                                                           
         DC    CL11'COMMERCIAL'                                                 
         DC    X'02',X'0000',AL1(KEYBOTH)                                       
         DC    AL2(SVCOML-PAYWRKD),AL1(L'SVCOML),AL2(VALCOML-T31301)            
         DC    C'LIST/DISPLAY COMMERCIALS-WITH N/G OPTION (= OPTIONAL)'         
*&&                                                                             
         DC    CL11'NOAFFID'                                                    
         DC    X'03',X'0000',AL1(KEYBOTH)                                       
         DC    AL2(SVNAFF-PAYWRKD),AL1(L'SVNAFF),AL2(VALNAFF-T31301)            
         DC    C'NO COMMERCIAL MATCH NEEDED WHEN CLEARING             '         
*                                                                               
         DC    CL11'REVERSE'                                                    
         DC    X'07',X'000000'                                                  
         DC    AL2(CRCKDAT-PAYWRKD),AL1(L'CRCKDAT),AL2(VALCRCK-T31301)          
         DC    C'REVERSE CR/CK PAID ON THIS DATE                      '         
*                                                                               
         DC    X'FF'               END-OF-TABLE                                 
         EJECT                                                                  
       ++INCLUDE NEPAYWRK                                                       
         EJECT                                                                  
* HELP SCREEN                                                                   
*                                                                               
         ORG   PAYLAST                                                          
       ++INCLUDE NEPAYFCD                                                       
         EJECT                                                                  
* DSECT TO COVER ACTION TABLE                                                   
*                                                                               
ACTTABD  DSECT                                                                  
ACTCODE  DS    CL6                 ACTION NAME                                  
ACTN     DS    X                   ACTION NUMBER (EQUATED VALUE)                
ACTMINL  DS    X                   MININUM LENGTH OF CODE                       
ACTIND   DS    X                   ACTION INDICATORS (E.G. DDSONLY)             
ACTOV    DS    X                   OVERLAY FOR ACTION                           
ACTSCR   DS    X                   SCREEN NUMBER FOR ACTION                     
ACTNTRL  EQU   *-ACTTABD                                                        
         SPACE 2                                                                
* DSECT TO COVER OPTION TABLE ENTRY                                             
*                                                                               
OPTTABD  DSECT                                                                  
OPTNAME  DS    CL11                OPTION CODE                                  
OPTMINL  DS    X                   MINIMUM LENGTH FOR CODE                      
OPTOPTN  DS    X                   OPTION NUMBER                                
OPTOPTB  DS    B                   OPTION BIT SETTING                           
OPTIND   DS    X                   OPTION INDICATORS                            
OPTOUT   DS    AL2                 DISPLACEMENT TO OUTPUT IN WORK STOR.         
OPTOUTL  DS    X                   LENGTH OF OUTPUT                             
OPTORTN  DS    AL2                 DISPLACEMENT TO VALIDATION ROUTINE           
OPTDESC  DS    CL53                OPTION DESCRIPTION                           
OPTNTRL  EQU   *-OPTTABD           LENGTH OF OPTION ENTRY                       
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
QUESTION EQU   C'?'                                                             
LINELEN  EQU   HELLIN2H-HELLIN1H   LENGTH OF HELP SCREEN LINE                   
LINES    EQU   (HELLAST-HELLIN1H)/LINELEN                                       
         EJECT                                                                  
* SPGENAGY (AGYHDRD)                                                            
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENSTA (STARECD)                                                            
         PRINT OFF                                                              
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENMKT (MKTRECD)                                                            
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENADD (ADDRECD)                                                            
         PRINT OFF                                                              
ADDRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENREP (REPRECD)                                                            
         PRINT OFF                                                              
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENPRD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* NEGENDPT (REPRECD)                                                            
         PRINT OFF                                                              
DPTRECD  DSECT                                                                  
       ++INCLUDE NEGENDPT                                                       
         PRINT ON                                                               
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135NEPAY01   07/09/19'                                      
         END                                                                    
