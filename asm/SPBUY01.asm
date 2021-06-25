*          DATA SET SPBUY01    AT LEVEL 131 AS OF 11/03/20                      
*PHASE T21101C                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE DPTRD                                                                  
*================================================================*              
* COPIED FROM HWON'S  SPBUY01C  LEVEL 128 AS OF NOV19/2018       *              
* 04NOV20 HWON REMOVING BUYER/BILLER FEATURE                     *              
*                                                                *              
* 20NOV17 HWON FIX PROJ VALIDATION IN OPTIONS                    *              
*                                                                *              
* 02APR13 MHER BUILD GOAL TABLE IN UTL XA STORAGE                *              
*                                                                *              
* 15AUG11 MHER BUILD TABLE OF PG GOALS                           *              
*                                                                *              
* 22MAY06 EJOR NEW NODEMO OPTION TO MIMIC EF1NODEM (RTG BOOK NOT *              
*               ON FILE ERROR)                                   *              
*                                                                *              
* 30SEP03 MHER SVNDDEM SUPPORT FOR CANADIAN NETWORK DEMOS        *              
*               INCLUDING CABLE                                  *              
*                                                                *              
* 22JAN03 MHER READ 'ALL' CLT MGREQ IF CLT SPECIFIC NOT FOUND    *              
*                                                                *              
* 05APR01 MHER SUPPORT FOR PURPOSE CODES (LIKE IDR OPTION)       *              
*              AND MOVE OPTION VALIDATION TO NTR1 BASE=*...      *              
* 02MAR00 MHER READ STATION MASTER RECORDS FOR CANADIAN NETWORK  *              
*              AND SAVE GST/PST OVERRIDES                        *              
*================================================================*              
         TITLE 'T21101 - SPOTPAK BUY - HEADLINE EDITS'                          
T21101   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21101,RR=R8                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21101+4096,R9                                                   
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO MVI   TWA1FLAG,C'Y'       FORCE TWA1 WRITE                             
         GOTO1 VDATCON,DMCB,(5,0),(3,SVTODAYB)                                  
* CLEAR DISPLAY AREA                                                            
         LA    R2,BUYOUTH                                                       
         CLI   SVSCR,X'FE'         TEST BASE SCREEN                             
         BNL   CLR4                YES                                          
* FOR OTHER SCREENS, SET AT END OF SCREEN                                       
         LA    R2,BUYMSGH                                                       
         SR    R0,R0                                                            
CLR2     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   CLR2                                                             
         B     CLR6                                                             
*                                                                               
CLR4     XC    8(80,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
CLR6     MVC   0(3,R2),=X'000101'  FORCE ERASE WRITE TO CLEAR SCREEN            
*                                                                               
         MVI   SVRCLOPT,0            CLEAR LAST RECALL OPTION                   
*                                                                               
         TM    UPSW,UPON           IF UPLOADING                                 
         BO    AG                  ALWAYS GO READ THE AGENCY RECORD             
*                                  ($MAD USES SAME AREA AS SVKEY)               
*                                                                               
         XC    SVKEY+14(4),SVKEY+14  CANNOT HAVE A BUY RECALLED                 
         OC    SVKEY,SVKEY         TEST FIRST TIME                              
         BNZ   MD                  NO                                           
         B     AG                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*======================================================*                        
* READ AGENCY HEADER                                   *                        
*======================================================*                        
         SPACE 1                                                                
AG       DS    0H                                                               
         BRAS  RE,READAGY                                                       
*                                                                               
MD       CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   MD1                                                              
         CLC   =C'KEY=',BUYINP1    TEST USER GIVING A HEXKEY                    
         BNE   MD1                                                              
         BRAS  RE,CLRMD            CLEAR ALL THE HEADLINE FIELDS                
         BRAS  RE,GETKEY                                                        
*                                                                               
MD1      LA    R2,BUYMDH                                                        
*                                                                               
         MVI   UPNDX,SHDRMEDQ                                                   
         TM    4(R2),X'20'                                                      
         BO    BUYER                                                            
*                                                                               
         BRAS  RE,CLRMD                                                         
         CLC   =C'EFJ',8(R2)                                                    
         BNE   MD2                                                              
         BRAS  RE,SETTMS                                                        
         MVC   BUYOP(3),=C'NFG'                                                 
         MVI   BUYOPH+5,3                                                       
MD2      CLC   =C'LISA',8(R2)                                                   
         BNE   MD4                                                              
         BRAS  RE,SETTMS                                                        
*                                                                               
MD4      GOTO1 ANY                                                              
*                                                                               
         MVI   SVORIGNL,C'N'                                                    
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),2             TWO CHARS ALLOWED                            
         BNE   MD6                 IF 2ND CHAR IS *                             
         CLI   9(R2),C'*'                                                       
         BE    *+12                                                             
         CLI   9(R2),C'X'          SFSTEREO                                     
         BNE   MD6                                                              
         MVI   SVORIGNL,C'Y'       SET FLAG                                     
         B     MD8                                                              
*                                                                               
MD6      CLI   5(R2),1                                                          
         BNE   BUYERR                                                           
*                                                                               
MD8      CLI   BUYMD,C'C'          DO NOT ALLOW CANAD 'C'                       
         BNE   *+12                                                             
         CLI   SVAPROF+7,C'C'                                                   
         BE    BUYERR                                                           
*                                                                               
         GOTO1 =V(MEDGET),DMCB,(BUYMD,AGYALPHA),VDATAMGR,WORK,RR=RELO           
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BE    BUYERR                                                           
*                                                                               
         MVC   SVAGYMD,WORK                                                     
         MVC   BUYMDXP(10),WORK+1                                               
         CLI   SVORIGNL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   BUYMDXP(10),=C'*ORIGINAL*'                                       
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
BUYER    DS    0H                                                               
         LA    R2,BUYBUH                                                        
         MVI   UPNDX,SHDRBIDQ                                                   
         TM    4(R2),X'20'                                                      
         BO    CLT                                                              
         BRAS  RE,CLRCL            FORCE REVALIDATION OF CLT/PRD                
         SPACE 1                                                                
*=======================================================*                       
* SPECIAL CODE TO PREVENT SUPPRESSING TURN-AROUNDS      *                       
*=======================================================*                       
         SPACE 1                                                                
         CLC   =C'SPOT',8(R2)                                                   
         BNE   BUYERX                                                           
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLC   T211FFD+10(2),=AL2(IDDNNY)                                       
         BE    BUYERR                                                           
         CLC   T211FFD+10(2),=AL2(IDDNNYA)                                      
         BE    BUYERR                                                           
*                                                                               
BUYERX   CLI   SVAPROF+14,C'Y'     TEST BUYER/BILLER SAVED                      
         BE    CLT                                                              
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         OI    4(R2),X'20'         SET VALIDATED                                
         B     CLT                                                              
*                                                                               
IDDNNY   EQU   1312                                                             
IDDNNYA  EQU   1314                                                             
         EJECT                                                                  
CLT      LA    R2,BUYCLH                                                        
         MVI   UPNDX,SHDRCLTQ                                                   
         TM    4(R2),X'20'                                                      
         BO    PRD                                                              
*                                                                               
         BRAS  RE,CLRCL                                                         
*                                                                               
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
*                                                                               
         MVC   QCLT,WORK                                                        
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),2                                                          
         BL    BUYERR                                                           
         CLI   5(R2),3                                                          
         BH    BUYERR                                                           
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         ST    RF,VCLPACK                                                       
         GOTO1 (RF),(R1),QCLT,SVCLT                                             
*                                                                               
         CLI   0(R1),0                                                          
         BNE   BUYERR                                                           
*                                                                               
         MVC   SV1OR2,GBY1OR2                                                   
         CLC   AGYALPHA,=C'SJ'                                                  
         BNE   CLT1                                                             
         CLC   QCLT,=C'TBL'                                                     
         BE    *+10                                                             
         CLC   QCLT,=C'PG0'                                                     
         BE    *+10                                                             
         CLC   QCLT,=C'PG1'                                                     
         BNE   CLT1                                                             
         MVI   SV1OR2,2                                                         
* READ CLIENT                                                                   
CLT1     MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOCLTREC)                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
         LA    RE,CLTHDR                                                        
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
* SAVE CLIST                                                                    
         L     R1,ASVCLIST                                                      
         MVC   0(220,R1),CLIST+000                                              
         MVC   220(220,R1),CLIST+220                                            
         MVC   440(220,R1),CLIST+440                                            
         MVC   660(220,R1),CLIST+660                                            
         MVC   BUYCLXP(20),CNAME                                                
         MVC   SVCPROF,CPROF                                                    
         MVC   SVOFFC,COFFICE                                                   
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   SVCOPT2,COPT2                                                    
         MVC   SVCOPT3,COPT3                                                    
         MVC   SVCOPT4,COPT4                                                    
         MVC   SVCCOST2,CCOST2                                                  
         MVC   SVC2CONV,CC2CONV                                                 
         MVC   SVCLOCK,CLOCKYM                                                  
         MVC   SVRFPGRP,CRFPGRP                                                 
         CLI   SVCXTRA+2,C'N'                                                   
         BNE   *+8                                                              
         MVI   SVCXTRA+2,0         'N' MEANS 'NO'                               
* SET SPECIAL-CODE-FOR-PAID-SPOT OTO FLAG                                       
         NI    SVOPT1,X'FF'-SVOPT1_PAIDOTO                                      
         CLI   SVAPROF+15,C'N'     N MEANS NO                                   
         BE    CLT2                                                             
         OI    SVOPT1,SVOPT1_PAIDOTO                                            
         MVC   SVOTOCHR,SVAPROF+15   ELSE SAVE THE CHARACTER                    
****                                                                            
**** CODE REMOVED 7/20/09                                                       
*&&DO                                                                           
         CLC   =C'WI',AGYALPHA                                                  
         BNE   CLT2                                                             
         CLI   SVOFFC,C'Y'         OFFICE Y DOESN'T USE SPCL CODE               
         BNE   CLT2                                                             
         NI    SVOPT1,X'FF'-SVOPT1_PAIDOTO                                      
*&&                                                                             
**** CODE REMOVED 7/20/09                                                       
****                                                                            
*                                                                               
* SPECIAL CODE FOR VELMA LEWIS TO SUPPRESS MGRPS                                
CLT2     CLC   =C'NOMGR',BUYBU                                                  
         BNE   *+8                                                              
         MVI   SVCXTRA+2,0                                                      
         MVC   SVCOPT1,COPT1                                                    
         TM    SVCOPT1,COP1CTAQ    TEST CTA=Y IN CLTHDR                         
         BZ    *+8                                                              
         OI    SVAFLAG1,AGYFCTAQ   SET CTA IN AGY                               
*                                                                               
         MVC   SVCACCS,CACCESS                                                  
         MVI   SVMACCS,X'FF'       SET IGNORE MARKET CODES                      
         MVC   SVCLTIFC,CCLTIFC                                                 
         MVC   SVPOLONL,CPOLONLY                                                
         XC    SVMCLCOD,SVMCLCOD                                                
         XC    SVMCLPRD,SVMCLPRD                                                
         MVC   SVMCLUNQ,CMCLTUNQ   SAVE MASTER TRAFFIC SEQNUM                   
         CLI   SVMCLUNQ,0                                                       
         BE    *+16                                                             
         MVC   SVMCLCOD,CMCLTCOD   SAVE MASTER TRAFFIC CLIENT                   
         MVC   SVMCLPRD,CMCLTPRD   SAVE MASTER TRAFFIC PRODUCT                  
         CLI   SVCXTRA+9,C'C'      TEST CLIENT COUNTRY SET TO C/U               
         BE    CLT6                                                             
         CLI   SVCXTRA+9,C'U'                                                   
         BE    CLT6                                                             
         MVI   SVCXTRA+9,C'U'      NO-CLIENT=USA                                
         CLI   SVAPROF+7,C'C'         UNLESS AGANCY IS CANADIAN                 
         BNE   CLT6                                                             
         MVI   SVCXTRA+9,C'C'                                                   
*                                                                               
CLT6     XC    SVFNPROF,SVFNPROF   CLEAR FILM NUMBER PROFILE                    
*                                                                               
CLT10    CLC   =C'MGEACC',BUYINP1  TEST INPUT FROM DARE SCRIPT                  
         BE    CLTX                AVOID  LIMIT ACCESS-'TO BE OK'  PROB         
         BRAS  RE,CALLOFCR                                                      
         BNE   BUYERR                                                           
         EJECT                                                                  
CLTX     DS    0H                                                               
         CLI   SVCXTRA+2,C'*'      TEST CCUSA INTERFACE                         
         BNE   CLTX10                                                           
* READ A0 PROFILE FOR HIGH XFR EST NUMBER                                       
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0A0'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),BUYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,ELEM,VDATAMGR                                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVXFREST,ELEM+11    SAVE HIGH XFR EST NUM                        
*                                                                               
CLTX10   OI    4(R2),X'20'                                                      
*                                                                               
         XC    SV1WPROF,SV1WPROF      READ 1W PROFILE                           
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S01W'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),BUYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
*                                                                               
         MVC   WORK+16(16),WORK    SAVE VALUES JUST BUILT                       
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SV1WPROF,VDATAMGR                                 
*                                                                               
         LA    R4,SV00PROF         READ 00 PROFILE                              
         XC    0(16,R4),0(R4)                                                   
         XC    WORK(16),WORK                                                    
         MVC   WORK(6),WORK+16     RESTORE KEY THRU AGY                         
         MVC   WORK(4),=C'S000'                                                 
         GOTO1 (RF),(R1),(X'90',WORK),(R4)  DO NOT READ UID PROFILES            
*                                                                               
         LHI   R4,SVF0PROF-BUYSAVE READ F0 PROFILE                              
         AR    R4,RA                                                            
         XC    0(16,R4),0(R4)                                                   
         MVC   WORK(16),WORK+16    RESTORE KEY VALUES                           
         MVC   WORK(4),=C'S0F0'                                                 
         GOTO1 (RF),(R1),WORK,(R4)                                              
*                                                                               
         LHI   R4,SVB0PROF-BUYSAVE READ B0 PROFILE                              
         AR    R4,RA                                                            
         XC    0(16,R4),0(R4)                                                   
         MVC   WORK(16),WORK+16    RESTORE KEY VALUES                           
         MVC   WORK(4),=C'S0B0'                                                 
         GOTO1 (RF),(R1),WORK,(R4)                                              
*                                                                               
         LHI   R4,SVMKPROF-BUYSAVE READ MK PROFILE                              
         AR    R4,RA                                                            
         XC    0(16,R4),0(R4)                                                   
         MVC   WORK(16),WORK+16    RESTORE KEY VALUES                           
         MVC   WORK(4),=C'S0MK'                                                 
         GOTO1 (RF),(R1),WORK,(R4)                                              
*                                                                               
         LHI   R4,SVD0PROF-BUYSAVE READ D0 PROFILE                              
         AR    R4,RA                                                            
         XC    0(16,R4),0(R4)                                                   
         MVC   WORK(16),WORK+16    RESTORE KEY VALUES                           
         MVC   WORK(4),=C'S0D0'                                                 
         GOTO1 (RF),(R1),WORK,(R4)                                              
*                                                                               
         LHI   R4,SV00APRF-BUYSAVE READ 00A PROFILE                             
         AR    R4,RA                                                            
         XC    0(16,R4),0(R4)                                                   
         XC    WORK(16),WORK                                                    
         MVC   WORK(6),WORK+16     RESTORE KEY THRU AGY                         
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'FF'-X'40'    3 CHAR PROFILE NEEDS LOWERCASE 'S'           
         GOTO1 (RF),(R1),(X'90',WORK),(R4)  DO NOT READ UID PROFILES            
*                                                                               
         LA    R4,SVDARPRF                                                      
         MVC   WORK(16),WORK+16    RESTORE KEY VALUES                           
         MVC   WORK(4),=C'SDAR'                                                 
         NI    WORK,X'FF'-X'40' ** MAKE THE S LOWERCASE                         
         MVC   WORK2(16),WORK      SAVE THE SDAR VALUES                         
         GOTO1 (RF),(R1),WORK,(R4)                                              
*                                                                               
* NEED TO GET LAST CHAR OF SDAR FROM AGENCY OR USERID PROFILE                   
*                                                                               
         MVC   WORK(16),WORK2      RESTORE SDAR KEY                             
         MVC   WORK+6(4),SPACES    SUPPRESS MEDIA AND CLIENT                    
         MVC   WORK(4),=C'S0OM'    OM PROFILE TELLS US IF DAR OR OM             
         LA    R4,SVOMPROF                                                      
         GOTO1 (RF),DMCB,WORK,(R4)                                              
*                                                                               
         TM    COPT2,COP2BP     BUY PROG PROFILE FLAG ON?                       
         BZ    CLTX30                                                           
         LHI   R4,SVBPPROF-BUYSAVE READ BP PROFILE                              
         AR    R4,RA                                                            
         XC    0(16,R4),0(R4)                                                   
         MVC   WORK(16),WORK+16    RESTORE KEY VALUES                           
         MVC   WORK(4),=C'S0BP'                                                 
         GOTO1 (RF),(R1),WORK,(R4)                                              
*                                                                               
CLTX30   DS    0H                                                               
         EJECT                                                                  
PRD      LA    R2,BUYPRH                                                        
         MVI   UPNDX,SHDRPRDQ                                                   
         TM    4(R2),X'20'                                                      
         BZ    PRD1                                                             
*                                                                               
         CLI   SVPOLPRD,0          TEST BRD POL BY BRD                          
         BE    EST                 NO                                           
         TM    BUYESH+4,X'20'      TEST ESTIMATE CHANGED                        
         BO    EST                 NO                                           
         NI    4(R2),X'DF'         ELSE MUST RE-VALIDATE PRD                    
*                                                                               
PRD1     BRAS  RE,CLRPR                                                         
*                                                                               
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
*                                                                               
         MVC   QPRD,WORK                                                        
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(USECASH)                                               
         CLI   QPRD+2,C'#'         TEST TRADE PRODUCT                           
         BE    BUYERR              DON'T ALLOW IN PRODUCT FIELD                 
         MVI   ERRCD,INVERR                                                     
         CLC   =C'AAA',QPRD                                                     
         BE    BUYERR                                                           
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         B     PRD2                *** NOP  MHER 10MAR00 ***                    
         BNE   PRD2                                                             
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BNE   PRD2                                                             
         CLC   =C'POL',QPRD        CANAD/NTWK MUST BE 'POL'                     
         BNE   BUYERR                                                           
*                                                                               
PRD2     CLI   5(R2),2                                                          
         BL    BUYERR                                                           
         CLI   5(R2),3                                                          
         BH    BUYERR                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY      A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOPRDREC)                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
*                                                                               
         LA    RE,PRDHDR                                                        
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
PRDX     MVC   BUYPRXP(20),PNAME                                                
         MVC   SVPRD,PCODE+1                                                    
         MVI   SVPOLPRD,0                                                       
         MVI   SVPOLPR2,0                                                       
         MVC   SVPRATE,PRATE       SAVE RATE OVERRIDE                           
         XC    SVBRDEMS,SVBRDEMS                                                
*                                                                               
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
EST      LA    R2,BUYESH                                                        
         MVI   UPNDX,SHDRESTQ                                                   
         TM    4(R2),X'20'         TEST PREV VALID                              
         BZ    EST1                NO - EDIT NOW                                
* FOR A COS2 CLIENT, IF STATION CHANGES, NEED TO REREAD ESTIMATE                
* TO GET THE COS2 FACTOR BACK !                                                 
         TM    SVPWFLG,X'01'       TEST PW EST                                  
         BZ    STAT                NO - GO DO STAT NOW                          
         TM    BUYSTH+4,X'20'      TEST STATION CHANGED                         
         BO    STAT                NO -                                         
*                                                                               
EST1     BRAS  RE,CLRES                                                         
         XC    SVESTREP,SVESTREP                                                
         XC    SVELOCK,SVELOCK                                                  
         MVI   SVEST1,0                                                         
         MVI   SVEST2,0                                                         
         MVI   SVPOLNPW,0                                                       
         MVI   SVPWFLG,0                                                        
         MVI   SVESLN,0                                                         
         MVI   SVECNTRL,0          RESET LOCK STATUS                            
         MVI   SVERATE,0           CLEAR RATE OVERRIDE                          
         MVI   SVEBKTYP,0                                                       
         MVI   SVESTFL1,0          RESET EFLAG1                                 
         XC    SVECOST2,SVECOST2                                                
*                                                                               
         GOTO1 ANY                                                              
         GOTO1 PACK                                                             
*                                                                               
         MVI   ERRCD,INVERR                                                     
         LTR   R0,R0                                                            
         BNP   BUYERR                                                           
         CHI   R0,255                                                           
         BH    BUYERR                                                           
         STC   R0,SVEST                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY                                                   
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),SVEST                                                   
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOESTREC)                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
*                                                                               
EST2     LA    RE,ESTHDR                                                        
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOBUYMAS)                                              
         CLI   EMSTRIND,C'M'       TEST MASTER ESTIMATE                         
         BE    BUYERR              YES - NO BUYS ALLOWED !                      
*                                                                               
         CLI   SVPOLPRD,0          TEST SECOND PASS FOR POL                     
         BNE   EST2A               YES - USE BRAND DESCRIPTION                  
         MVC   BUYESXP(20),EDESC                                                
         MVC   SVTRDPR,ETRDPRD     SAVE TRADE                                   
         MVC   SVCSHPR,ECASHPRD    AND CASH PRODUCT CODES                       
*                                                                               
EST2A    GOTO1 VDATCON,DMCB,ESTART,SVSTART  GET IN Y2K FORM                     
         GOTO1 (RF),(R1),,(5,BUYESXP+23)    GET IN DISPLAY FORM                 
         MVI   BUYESXP+31,C'-'                                                  
*                                                                               
         GOTO1 (RF),(R1),EEND,SVSTART+6                                         
         GOTO1 (RF),(R1),,(5,BUYESXP+32)                                        
* EXPAND RATING BOOK                                                            
         MVC   BUYESXP+41(13),SPACES                                            
         MVC   BUYESXP+41(8),=C'(LATEST)'                                       
         OC    EBOOK,EBOOK                                                      
         BZ    EST3                                                             
         GOTO1 VDATCON,DMCB,(3,EBOOK),(9,BUYESXP+42)                            
         ZIC   R0,EHUTADJ                                                       
         SRL   R0,4                                                             
         LTR   R0,R0                                                            
         BE    EST3                                                             
         MVC   DUB(3),=X'4F0101'                                                
         STC   R0,DUB+1                                                         
         GOTO1 (RF),(R1),(3,DUB),(9,WORK)                                       
         MVI   BUYESXP+48,C'-'                                                  
         MVC   BUYESXP+49(3),WORK                                               
         MVI   BUYESXP+52,C')'                                                  
         EJECT                                                                  
EST3     DS    0H                                                               
* GET BINARY DATES                                                              
         GOTO1 (RF),(R1),ESTART,(3,SVSTARTB)                                    
         GOTO1 (RF),(R1),EEND,(3,SVENDB)                                        
* GET COMPRESSED DATES                                                          
         GOTO1 (RF),(R1),ESTART,(2,SVSTARTP)                                    
         GOTO1 (RF),(R1),EEND,(2,SVENDP)                                        
*                                                                               
EST4     MVC   SVMGDATP,SVENDP     ** FORCE MG END DT = EST END DT **           
         MVC   SVMGDATB,SVENDB                                                  
         MVC   SVBOOK,EBOOK                                                     
         MVC   SVHUTADJ,EHUTADJ                                                 
         MVC   SVCOPY,ECOPY        SAVE COPY CODE FOR TRAFFIC                   
         CLI   SVERATE,0                                                        
         BNE   *+10                                                             
         MVC   SVERATE,ERATE       SAVE MANDATORY RATE TYPE                     
         MVC   SVEOWSDY,EOWSDAY    SAVE OUT-OF-WEEK ROT START DAY               
         XC    SVPRDEST,SVPRDEST                                                
         XC    SVFLTDTS,SVFLTDTS                                                
         OC    SVESTFL1,EFLAG1                                                  
         CLC   EEND,=C'960701'     DOES EST END AFTER JUN30/96                  
         BL    *+8                                                              
         OI    SVESTFL1,EF1NMG     YES - FORCE NEW MAKEGOODS !                  
         OC    SVECOST2,SVECOST2   USE BRAND, NOT POL COS2 FACTOR               
         BNZ   *+10                                                             
         MVC   SVECOST2,ECOST2                                                  
         MVC   SVEST1(2),EREQLO    SAVE ESTIMATE SERIES NUMBERS                 
         OC    SVESTREP,SVESTREP   THESE INSTRUCTIONS SAVE BRD REP              
         BNZ   *+10                (IF PRESENT) FOR BRD POL BY BRD              
         MVC   SVESTREP,EREP                                                    
         OC    ELOCKYM,ELOCKYM     USE PRD OR POL LOCK DATE                     
         BZ    *+10                                                             
         MVC   SVELOCK,ELOCKYM                                                  
         MVI   SVNEWDEM,C'Y'                                                    
         LA    R0,3                                                             
         STH   R0,SVEDEMLN                                                      
         LA    R0,1                                                             
         STH   R0,SVDEMDSP                                                      
         XC    SVDEMOS,SVDEMOS                                                  
         MVC   SVDEMLST(60),EDEMLST  LEAVE 3X'00' AS E-O-L FLAG                 
         MVC   SVWGTLST,EWGTLST                                                 
         MVC   SVUSRNMS(35),EUSRNMS                                             
         CLI   SVPOLPRD,0          TEST PASS 2 FOR BRD POL BY BRD               
         BNE   *+10                YES - SAVE =BRAND= NOT POL VALUE             
         MVC   SVDEMMIN,ETDMIN     SAVE MINIMUM TARGET                          
         OC    EPWPCT,EPWPCT                                                    
         BZ    *+8                                                              
         OI    SVPWFLG,X'01'       SET PW EST FLAG                              
         CLI   SVESLN,0                                                         
         BNE   *+10                                                             
         MVC   SVESLN,ESLN         SAVE SINGLE SLN RESTRICTION                  
         CLI   SVEBKTYP,C' '       TEST HAVE PREVIOUS                           
         BH    *+10                                                             
         MVC   SVEBKTYP,EBKTYPE                                                 
         LHI   RF,SVNTDMS-BUYSAVE  NON-TRAD INDEX DEMO LIST                     
         AR    RF,RA                                                            
         XC    0(L'SVNTDMS,RF),0(RF)  RESET NON-TRAD LIST                       
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JNH   EST6                                                             
         MVC   0(L'SVNTDMS,RF),ENONTDMS                                         
         EJECT                                                                  
* CHECK FOR POL ESTIMATE *                                                      
         SPACE 1                                                                
EST6     DS    0H                                                               
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
*                                                                               
         CLC   =C'NON-POL',BUYBU                                                
         BE    EST15                                                            
*                                                                               
         CLI   SVCPROF+0,C'0'      TEST BRAND POOL                              
         BNE   EST10               YES                                          
         CLI   SVPRD,X'FF'         TEST BUYING POL                              
         BE    EST15               YES - ESTHDR WAS FOUND                       
         CLI   SVTRDPR,0           IS THIS A NON-TBS TRADE EST                  
         BNE   EST10               YES                                          
         MVI   ERRCD,POLESERR                                                   
         CLC   KEY(13),KEYSAVE     POL EST SHOULD NOT BE OPEN                   
         BE    BUYERR                                                           
         B     EST15                                                            
* BRAND POOL                                                                    
EST10    MVI   ERRCD,BRPOLERR                                                   
*================================================================               
* REMOVE TRANSITIONAL CLIENT REQUIREMENT 27NOV01 MHER                           
* SFM WAS CHANGED TO REQUIRE POL ESTIMATE, SO IF NO POL ESTIMATE                
* IT IS OLD BRAND BUYING                                                        
*                                                                               
**NOP**  CLI   BUYBU,C'+'          TEST TRANSITIONAL CLIENT                     
**NOP**  BNE   EST12               NO                                           
*================================================================               
* CODE MEANT TO BE USED IF SWITCHING TO BRAND POL                               
         CLC   KEY(13),KEYSAVE     TEST POL EST OPEN                            
         BNE   EST15               NO - TREAT AS BRAND                          
*                                                                               
EST12    DS    0H                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
*                                                                               
         CLI   SVCPROF,C'0'        TEST BRD POL CLT                             
         BE    EST12B              NO                                           
         CLI   SVAPROF+11,C'Y'     TEST AGYHDR ALLOWS POL RADIO NPW             
         BNE   EST12B                                                           
         CLI   BUYMD,C'R'                                                       
         BE    EST12A                                                           
         B     EST12B                                                           
EST12A   MVI   SVPOLNPW,X'80'                                                   
*                                                                               
EST12B   CLI   SVPRD,X'FF'         ARE THEY ENTERING UNDER POL                  
         BE    EST15               YES                                          
* DOING BRAND POL UNDER BRAND                                                   
         MVC   SVPOLPRD,SVPRD      SAVE BRAND CODE                              
         OC    SVECNTRL,ECNTRL     SAVE BRD LOCK STATUS                         
         MVI   SVPRD,X'FF'          AND SWITCH TO POL                           
         MVC   SVBRDEMS,SVDEMOS    SAVE BRAND DEMOS                             
         B     EST2                GO PROCESS POL ESTHDR                        
         EJECT                                                                  
EST15    DS    0H                                                               
         XC    SVNETYM,SVNETYM                                                  
         SPACE 1                                                                
*======================================================*                        
* FINAL ESTIMATE VALIDATION                            *                        
*======================================================*                        
         SPACE 1                                                                
EST17    DS    0H                                                               
         OC    SVECNTRL,ECNTRL     USE LOCK STATUS FROM POL OR BRD              
         SPACE 1                                                                
* US SPILL CLIENT MUST BE POL AND SPOT TV *                                     
         SPACE 1                                                                
         MVI   ERRCD,BADUSSPL                                                   
         CLI   SVCXTRA+5,C'D'      TEST US DPT SUMMARY SPILL                    
         BE    EST17A                                                           
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BNE   EST18                                                            
         B     EST20               OKAY NOW FOR RADIO                           
*                                                                               
EST17A   CLI   BUYMD,C'T'                                                       
         BNE   BUYERR                                                           
         B     EST20                                                            
         SPACE 1                                                                
* CANADIAN INPUT TESTS *                                                        
         SPACE 1                                                                
EST18    CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   EST20                                                            
         CLI   BUYMD,C'R'                                                       
         BE    EST20                                                            
         CLI   BUYMD,C'X'                                                       
         BE    EST20                                                            
EST18X   MVI   ERRCD,NOTPOL        ALL NON-RADIO MUST BE POL                    
         CLI   BUYINP1,C'@'        LET ANN DO IT THOUGH                         
         BE    *+12                                                             
         CLI   SVPRD,X'FF'                                                      
         BNE   BUYERR                                                           
         EJECT                                                                  
*======================================================*                        
* READ DAYPART MENU                                    *                        
*======================================================*                        
         SPACE 1                                                                
EST20    LA    R1,DMCB                                                          
         MVC   0(2,R1),AGYALPHA                                                 
         MVC   2(1,R1),BUYMD                                                    
         MVC   3(1,R1),EDAYMENU                                                 
         GOTO1 =V(DPTRD),(R1),,REC,VDATAMGR,RR=RELO                             
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BE    EST20A                                                           
         TM    8(R1),X'08'                                                      
         BZ    EST20B                                                           
EST20A   MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NODPTMNU)                                              
         B     BUYERR                                                           
* SAVE VALID CODES                                                              
EST20B   XC    SVMENU,SVMENU                                                    
         LA    R0,L'SVMENU-1       R0=COUNTER                                   
         LA    R1,REC                                                           
         LA    R4,SVMENU                                                        
*                                                                               
EST20C   CLI   0(R1),0             TEST FOR EOT                                 
         BE    EST21                                                            
         MVC   0(1,R4),0(R1)                                                    
         LA    R4,1(R4)                                                         
         LA    R1,5(R1)                                                         
         BCT   R0,EST20C                                                        
         EJECT                                                                  
* BUILD DAYPART EQUIV TABLE FOR GOAL CHECKING *                                 
* ENTRIES ARE 2 BYTES -- DAYPART CODE/NUMBER  *                                 
         SPACE 1                                                                
EST21    LA    R4,SVGLDPEQ                                                      
         XC    SVGLDPEQ,SVGLDPEQ                                                
*                                                                               
         LA    R0,L'SVGLDPEQ/2     R0=MAX N'ENTRIES                             
         LA    R1,REC                                                           
*                                                                               
EST21A   CLI   0(R1),0                                                          
         BE    EST21X                                                           
         CLI   0(R1),C'Z'                                                       
         BE    EST21X                                                           
         MVC   0(2,R4),0(R1)       SAVE CODE AND NUMBER                         
         LA    R4,2(R4)                                                         
*                                                                               
EST21X   LA    R1,5(R1)            NEXT DAYPART                                 
         BCT   R0,EST21A                                                        
         EJECT                                                                  
         CLI   SVCXTRA+4,C'E'      TEST FLIGHTS BY EST                          
         BNE   EST24                                                            
* PREVENT TRUE POL BUYING SINCE NO FLIGHTS FOR POL *                            
         MVI   ERRCD,NOFLTREC                                                   
         CLI   SVPRD,X'FF'         IF NOT POL MUST BE OLD BRD EST               
         BNE   EST24               -- SO IGNORE IT                              
         CLI   SVPOLPRD,0                                                       
         BE    BUYERR                                                           
* READ FLIGHT RECORD                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D0D'                                                  
         MVC   KEY+2(3),SVKEY      A-M/CLT                                      
         MVC   KEY+5(3),QPRD       NOTE USES INPUT PRD                          
         MVC   KEY+9(1),SVEST                                                   
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOFLTREC                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
* EXTRACT FLIGHTS AND SAVE DATES                                                
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,REC+24                                                        
         LA    R7,SVFLTDTS                                                      
         BAS   RE,NEXTEL                                                        
         BNE   BUYERR                                                           
EST22    MVI   ERRCD,MAXFLT16                                                   
         LA    R0,SVFLTDTS+63                                                   
         CR    R7,R0                                                            
         BH    BUYERR                                                           
         MVC   0(4,R7),3(R6)                                                    
         LA    R7,4(R7)                                                         
         BAS   RE,NEXTEL                                                        
         BE    EST22                                                            
*                                                                               
EST24    CLI   SVPWFLG,0           TEST PW BUY                                  
         BE    EST30               NO                                           
         CLI   SVPOLPRD,0          TEST BRAND POL BY BRAND                      
         BNE   EST30               YES - NO PROBLEM                             
         CLC   =C'TEST',BUYBU                                                   
         BE    EST30                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOPWPOL)                                             
         B     BUYERR                                                           
         EJECT                                                                  
*=================================================================*             
* IF ANY PART OF ESTIMATE PERIOD IS LOCKED, CALC START/END DATES  *             
*=================================================================*             
         SPACE 1                                                                
EST30    LA    R4,SVCLOCK                                                       
         OC    SVCLOCK,SVCLOCK                                                  
         BZ    *+8                                                              
         BAS   RE,SETLKDT                                                       
*                                                                               
         LA    R4,SVELOCK                                                       
         OC    SVELOCK,SVELOCK                                                  
         BZ    *+8                                                              
         BAS   RE,SETLKDT                                                       
*                                                                               
         CLI   SVCXTRA+8,C'P'      TEST PG GOALS                                
         BNE   EST40                                                            
*                                                                               
         XC    SVPGPROF,SVPGPROF   READ PG PROFILE FOR THIS YEAR                
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0PX'                                                 
*                                                                               
         GOTO1 VDATCON,DMCB,SVEND,(X'20',WORK+16)  GET EST END YYMMDD           
         MVC   WORK+3(1),WORK+17    MOVE BDCST YEAR TO PROFILE                  
*                                                                               
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),BUYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVPGPROF,VDATAMGR                                 
*                                                                               
         OC    SVPGPROF,SVPGPROF                                                
         BNZ   EST40                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPGPROF)                                              
         J     BUYERR                                                           
                                                                                
*============================================================                   
* IF UPLOADING, NEED TO VALIDATE LIST OF DEMOS SAVED IN                         
* SVCOMNMS AND BUILD LIST OF DEMO CODES IN SVUPDEM                              
*============================================================                   
                                                                                
EST40    TM    UPSW,UPON           IF UPLOADING                                 
         BZ    ESTX                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LHI   R4,SVCOMNMS-BUYSAVE  SAVED 7 CHAR + C' ' DEMO NAMES              
         AR    R4,RA                                                            
         LA    RE,ELEM+8                                                        
         LR    RF,R4                                                            
         LA    R0,20               COUNT NUMBER OF ENTRIES                      
*                                                                               
EST42    CLC   0(7,RF),SPACES                                                   
         BNH   EST44                                                            
         MVC   0(7,RE),0(RF)                                                    
         LA    RE,6(RE)            POINT TO LAST CHAR                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C','                                                       
         LA    RE,2(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,EST42                                                         
*                                                                               
EST44    BCTR  RE,0                                                             
         MVI   0(RE),C' '                                                       
*                                                                               
         LA    RF,ELEM+8                                                        
         SR    RE,RF                                                            
         STC   RE,ELEM                                                          
         AHI   RE,-8                                                            
         STC   RE,ELEM+5                                                        
*                                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCK,R5             SET UP CALL TO DEMOVAL                     
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = TP                            
         MVI   DBSELMED,C'T'                                                    
         CLI   BUYMD,C'R'            SET DBSELMED = R FOR RADIO                 
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   EST46                 AGENCY USING US DEMOS                      
         CLI   SV00PROF+10,C'Y'      VALIDATE WITH US DEMOS                     
         BNE   *+8                                                              
         OI    DBVOPT,X'80'                                                     
         CLI   SVCXTRA,C'U'                                                     
         BE    EST46                                                            
         MVI   DBSELMED,C'C'                                                    
*                                                                               
EST46    LHI   R0,SVNTDMS-BUYSAVE                                               
         AR    R0,RA                                                            
         ST    R0,DMCB+16          SET EXISTING LIST IN DMCB5                   
*                                                                               
         L     R0,AREC5            DUMMY AREA FOR NONT DEMO OUTPUT              
         ST    R0,DMCB+20                                                       
*                                                                               
         L     RF,VDEMOVAL           AND CALL DEMOVAL                           
         GOTO1 (RF),DMCB,(1,ELEM),(20,(R0)),(C'S',ADBLOCK),SVUSRNMS             
         CLI   0(R1),0             TEST FOR ERRORS                              
         JE    EST50                                                            
*                                                                               
         MVI   ERRCD,DEMERR                                                     
         L     RE,4(R1)            GET BAD DEMO NAME                            
         MVC   ERRTEXT(7),0(RE)                                                 
         J     BUYERR                                                           
*                                                                               
EST50    TM    UPSW,UPON           ARE WE UPLOADING?                            
         JZ    ESTX                NO                                           
*                                                                               
         LHI   RE,UPDEMS-BUYSAVE                                                
         AR    RE,RA                                                            
         OC    0(L'UPDEMS,RE),0(RE)  ANY UPDEMS?                                
         JNZ   ESTX                  YES, DIDN'T HAVE NON-TRAD DEMOS            
*                                                                               
         L     R4,VTIA             THIS SHOULD BE THE HDR* OBJ                  
         USING SHDRD,R4                                                         
         CLC   SHDRTYPE,=C'HDR*'   WE NEED IT TO BE THE HEADER                  
         JNE   *+2                                                              
         CLC   SHDRUTYP,=C'BY2'    UPLOAD WITH MAX OF 20 DEMOS?                 
         JE    EST55                                                            
         LA    R0,14               CODE FROM SPBUYUPL                           
         LA    R1,SHDRDEMO                                                      
         J     EST60                                                            
*                                                                               
EST55    LA    R0,20                                                            
         LA    R1,SHD2DEMO                                                      
*                                                                               
EST60    LA    RE,ELEM+8           SIMULATE FIELD HEADER FOR DEMOVAL            
         XC    ELEM,ELEM                                                        
EST65    CLC   0(7,R1),SPACES                                                   
         JNH   EST70                                                            
         MVC   0(7,RE),0(R1)                                                    
         LA    RE,6(RE)                                                         
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         MVI   1(RE),C','                                                       
         LA    RE,2(RE)                                                         
         LA    R1,7(R1)                                                         
         JCT   R0,EST65                                                         
*                                                                               
EST70    BCTR  RE,0                                                             
         MVI   0(RE),C' '                                                       
         LA    RF,ELEM+8                                                        
         SR    RE,RF                                                            
         STC   RE,ELEM+5           SET L(FIELD)                                 
         LA    RE,8(RE)            FLDHDR+0 IS L'FLDHDR+L'FIELD                 
         STC   RE,ELEM                                                          
*                                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCK,R5             SET UP CALL TO DEMOVAL                     
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = TP                            
         MVI   DBSELMED,C'T'                                                    
         CLI   BUYMD,C'R'            SET DBSELMED = R FOR RADIO                 
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   EST75                 AGENCY USING US DEMOS                      
         CLI   SV00PROF+10,C'Y'      VALIDATE WITH US DEMOS                     
         BNE   *+8                                                              
         OI    DBVOPT,X'80'                                                     
         CLI   SVCXTRA,C'U'                                                     
         BE    EST46                                                            
         MVI   DBSELMED,C'C'                                                    
*                                                                               
EST75    LHI   R0,SVNTDMS-BUYSAVE                                               
         AR    R0,RA                                                            
         ST    R0,DMCB+16          SET EXISTING LIST IN DMCB5                   
*                                                                               
         L     R1,AREC5            DUMMY AREA FOR NONT DEMO OUTPUT              
         ST    R1,DMCB+20                                                       
*                                                                               
         LHI   R5,UPDEMS-BUYSAVE                                                
         AR    R5,RA                                                            
         XC    0(L'UPDEMS,R5),0(R5)                                             
*                                                                               
         L     RF,VDEMOVAL           AND CALL DEMOVAL                           
         GOTO1 (RF),DMCB,(1,ELEM),(20,0(R5)),(C'S',ADBLOCK),SVUSRNMS            
         CLI   0(R1),0             TEST FOR ERRORS                              
         JE    EST80               NONE                                         
*                                                                               
         MVI   ERRCD,DEMERR                                                     
         L     RE,4(R1)            GET BAD DEMO NAME                            
         MVC   ERRTEXT(7),0(RE)                                                 
         J     BUYERR                                                           
         DROP  R4                                                               
*                                                                               
EST80    LHI   R5,UPDEMS-BUYSAVE                                                
         AR    R5,RA                                                            
         LA    RE,L'UPDEMS(R5)     RE = BYTE AFTER EOL                          
EST85    OC    0(3,R5),0(R5)       NULLS IS TRADITIONAL EOL HERE                
         JZ    ESTX                                                             
         CLI   0(R5),X'FF'         NEW EOL                                      
         JNE   *+14                                                             
         XC    0(3,R5),0(R5)       MAKE IT NULLS                                
         J     ESTX                                                             
*                                                                               
         LA    R5,3(R5)            NEXT DEMO CATEGORY                           
         CR    R5,RE               THERE IS ROOM FOR 21                         
         JNH   EST85                                                            
*                                                                               
ESTX     OI    4(R2),X'20'                                                      
         B     STAT                                                             
         SPACE 1                                                                
*===============================================================*               
* R4 POINTS TO LOCK DATE Y/M  (2)                               *               
* OUTPUT LOCK START DATE AT 2(2,R4)                             *               
*    AND LOCK   END DATE AT 4(2,R4)                             *               
*===============================================================*               
         SPACE 1                                                                
SETLKDT  NTR1                                                                   
         MVC   DUB(2),0(R4)                                                     
         NI    DUB+1,X'3F'         DROP PRIOR/SUBSEQ FLAGS                      
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),WORK                                        
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QGETBRD                                                   
         GOTO1 VCALLOV,DMCB                                                     
*                                                                               
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 (RF),(R1),(1,WORK),WORK+6,VGETDAY,VADDAY                         
         GOTO1 VDATCON,DMCB,WORK+6,(2,2(R4))                                    
         GOTO1 (RF),(R1),WORK+12,(2,4(R4))                                      
*                                                                               
         TM    1(R4),X'80'         TEST MONTH AND PRIOR                         
         BZ    *+10                                                             
         XC    2(2,R4),2(R4)       CLEAR START DATE                             
*                                                                               
         TM    1(R4),X'40'         TEST MONTH AND SUBSEQUENT                    
         BZ    *+12                                                             
         LHI   R0,-1                                                            
         STCM  R0,3,4(R4)          SET HIGH END DATE                            
         XIT1                                                                   
         EJECT                                                                  
STAT     MVI   UPNDX,SBUYSTAQ      UPLOAD INDEX                                 
         TM    UPSW,UPON+UPHDR     EXCEPT IF VALIDATING HDR* OBJECT             
         BNO   *+8                                                              
         MVI   UPNDX,0                                                          
         LA    R2,BUYSTH                                                        
         TM    4(R2),X'20'                                                      
         BO    OPT                                                              
*                                                                               
         MVI   SVNETBTS,0                                                       
         MVI   SVNOVSET,0          CLEAR FLAG                                   
         BRAS  RE,CLRNDEF                                                       
*                                                                               
         MVI   SVNRGN,C' '                                                      
         XC    SVNOVRD,SVNOVRD                                                  
         MVI   SVARBF94,0                                                       
         XC    SVSPLKEY,SVSPLKEY   KILL SAVED SPILL DATA                        
*                                                                               
         BRAS  RE,CLRST                                                         
         GOTO1 ANY                                                              
*                                                                               
         CLI   BUYST,C'0'          PREVENT SPACE FOR SYS/NET                    
         BL    STA02                                                            
         MVI   BYTE,0                                                           
         LA    RF,BUYST            RF=A(START OF STATION HEADER)                
         LA    RE,BUYST+L'BUYST-1  RE=A(END OF STATION HEADER)                  
STA01    CR    RE,RF               VALIDATED ENTIRE STATION?                    
         BNH   STA02               YES                                          
         CLI   0(RE),C' '          SPACE?                                       
         BH    STA01A              NO                                           
         CLI   BYTE,C' '           YES, WAS PREVIOUS SPACE?                     
         BNH   STA01A               NO, WE'RE FINE                              
         MVI   ERRCD,STAERR         YES, CAN'T HAVE SPACE IN                    
         B     BUYERR                MIDDLE OF INPUT, SEND ERROR                
STA01A   MVC   BYTE,0(RE)          SAVE AS PREV CHAR                            
         JCT   RE,STA01                                                         
*                                                                               
STA02    L     R4,AREC2                                                         
         USING STABLKD,R4                                                       
         XC    QCBLNET,QCBLNET                                                  
         XC    0(STBLNQ,R4),0(R4)  CLEAR INTERFACE BLOCK                        
         MVC   STBMED,BUYMD        SET MEDIA                                    
         ST    R2,STBADDR          SET A(STATION FIELD)                         
         MVI   STBCTRY,C'U'                                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'                                                     
         MVC   STBACOM,VCOMFACS                                                 
* GET STAVAL ADDRESS                                                            
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A68'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NETNOTON)                                            
         CLI   STBERR,STBNOTON                                                  
         BE    BUYERR                                                           
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   STBERR,0                                                         
         BNE   BUYERR                                                           
*                                                                               
         MVC   QSTA,STBSTA         SET OUTPUT STATION                           
         CLI   QSTA+4,C' '                                                      
         BH    *+10                                                             
         MVC   QSTA+4(1),BUYMD                                                  
         MVC   QCBLNET,STBNET      SAVE CBLNET                                  
*                                                                               
         MVC   DUB(5),QSTA         SET STATION                                  
         BRAS  RE,GETSTMAS                                                      
         BNE   BUYERR                                                           
*                                                                               
         NI    SVOPT1,X'FF'-SVOPT1_VNDRLCK                                      
         TM    SFLAG1,SLOCK        X'04'-STA LOCKED (VENDOR LOCK)??             
         BZ    *+8                                                              
         OI    SVOPT1,SVOPT1_VNDRLCK  X'10' - YES, VENDOR LOCKED                
         MVC   SVMIDAS,STMIDAS        SAVE MIDAS FLAG                           
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   STA10                                                            
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BNE   STA10                                                            
         CLI   STBNET+1,C' '       IF 2 CHARS IT'S LOCAL CBLNET                 
         BH    STA05               AND NOT REGION                               
         XC    QCBLNET,QCBLNET                                                  
         MVC   SVNRGN,STBNET       SAVE NTWK REGION CODE                        
         B     STA20               SO IT'S A CBLDEF OR NETDEF                   
*                                                                               
STA05    MVI   CBLFLAG,C'Y'        SET CANAD LOCAL CABLE FLAG                   
         MVC   QCBLNET,STBNET      INPUT IS CBLNET                              
         BRAS  RE,GETCBNET         GET MARKET NUMBER INTO WORK                  
*                                                                               
STA10    CLI   CBLFLAG,C'Y'        MKTNUM ALREADY IN WORK FOR CABLE             
         BE    *+10                                                             
         MVC   WORK(4),SMKT        MOVE MARKET NUMBER                           
         BRAS  RE,GETMKT           READ MARKET RECORD NOW                       
*                                                                               
         L     RE,AREC                                                          
         USING MKTRECD,RE                                                       
*                                                                               
         MVC   SVRTGSVC,MKTRSVC    SAVE RATING SERVICE                          
         MVC   SVMKTMKT,MKTALST    SAVE ALPHA MARKET CODE                       
*                                                                               
STA10A   MVC   SVDMLST0,SRS1CALL   MOVE NSI LOOKUP CALL LETTERS                 
         MVC   SVDMLST1,SRS2CALL   AND BBM CALL LETTERS                         
         MVC   SVDFLTBT,MKTBKTYP                                                
*                                                                               
         LHI   RF,SQNORS1I         SET FOR STATION 1 IMPS FLAG                  
         MVI   SVDMLFLG,2          SET NSI LOOKUP FLAG                          
         CLI   SVRTGSVC,C'0'       TEST NSI MARKET                              
         BE    *+12                                                             
         MVI   SVDMLFLG,1          SET BBM LOOKUP FLAG                          
         LHI   RF,SQNORS2I                                                      
*                                                                               
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    SFLAG1,0 ** EXECUTED **                                          
         BZ    *+8                                                              
         OI    SVDMLFLG,X'80'      SET TO SUPPRESS IMPS                         
         DROP  RE                                                               
*                                                                               
         CLI   CBLFLAG,C'Y'        TEST CANADIAN CABLE                          
         BE    STA11A                                                           
*                                                                               
         MVC   SVPST,SPST          PROVINCE SERVICE TAX                         
* * MEANS ALLOW NETWORK OVERRIDE, SO GET RID OF IT FOR MEDIA T/R                
         LA    R0,L'SVPST                                                       
         LA    R1,SVPST                                                         
         CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         MVC   SVNTAX,SNEWTAX                                                   
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   STA10B                                                           
         CLI   SCOUNTRY,C'U'       TEST US STATION                              
         BE    STA10B                                                           
         XC    SVNTAX,SVNTAX                                                    
*                                                                               
STA10B   MVC   SVSTAMKT,SMKTALPH   SAVE ALPHA MARKET                            
         MVC   SVOLDMKT,STOLDMK1                                                
         MVC   SVOLDMKT+2(2),STOLDMK2                                           
*                                                                               
STA10C   TM    SFLAG1,SQARBF94     TEST ARBF94 OPTION                           
         BZ    *+8                                                              
         OI    SVARBF94,QARBF94                                                 
*                                                                               
         MVI   SVDSTFLG,C'Y'       ASSUME DST                                   
         TM    SFLAG1,SQNODST                                                   
         BZ    *+8                                                              
         MVI   SVDSTFLG,C'N'       SET NO DST                                   
*                                                                               
         CLI   SBKTYPE,0                                                        
         BE    *+10                                                             
         MVC   SVDFLTBT,SBKTYPE                                                 
         MVI   SVSTACNT,0          STATION'S COUNTRY                            
         XC    SVSTACTX,SVSTACTX   CLEAR CANADIAN C58 TAX                       
         XC    SVSTAMSF,SVSTAMSF   CLEAR CANADIAN MEDIA SERVICE FEE             
         CLI   BUYST,C'0'          TEST CABLE                                   
         BL    *+10                                                             
         MVC   BUYCBL(20),SSYSNAME CABLE NAME                                   
         FOUT  BUYCBLH                                                          
*                                                                               
         MVC   SVNTI,SPACES                                                     
         CLI   STYPE,C'C'          TEST CABLE                                   
         BL    STA10D                                                           
         CLI   SNTISTA,C' '        TEST NTI STATION PRESENT                     
         BNH   STA10D                                                           
         MVC   SVNTI,SNTISTA       THEN SAVE IT                                 
*                                                                               
STA10D   OC    SEFFDATE,SEFFDATE   TEST FOR EFFECTIVE DATE                      
         BZ    STA10E                                                           
         GOTO1 VDATCON,DMCB,(5,0),(3,DUB)                                       
         CLC   DUB,SEFFDATE        ARE WE PAST THE DATE                         
         BH    STA10E              YES                                          
         MVC   BUYMSG(27),=C'** ERROR ** CAN''T BUY UNTIL'                      
         GOTO1 VDATCON,DMCB,(3,SEFFDATE),(8,DUB)                                
         MVC   BUYMSG+28(8),DUB                                                 
         MVI   ERRAREA,X'FF'                                                    
         B     BUYERR                                                           
         EJECT                                                                  
STA10E   CLI   SVAPROF+7,C'C'      ONLY USEFUL FOR CANADIAN AGENCIES            
         BNE   STA10F                                                           
         SPACE 1                                                                
*=====================================================*                         
* FOR CANADIAN AGENCIES, SET DOLLAR AND COUNTRY TYPES *                         
* POSSIBLE VALUES:                                    *                         
*                  STATION   ACTUAL     DOLLAR        *                         
*                  COUNTRY   COUNTRY     TYPE         *                         
*                  -------   -------    -------       *                         
*                     C        C           C          *                         
*                     U        U           U          *                         
*                     V        U           C          *                         
*                     ?        C           C          *                         
*=====================================================*                         
         SPACE 1                                                                
         MVC   SVSTACNT,SCOUNTRY   SAVE COUNTRY TYPE (C/U/V)                    
         CLI   SVSTACNT,C' '       TEST COUNTRY SPECIFIED                       
         BH    *+8                 YES                                          
         MVI   SVSTACNT,C'C'       IF NOT, ASSUME CANADIAN                      
*                                                                               
         MVC   SVSTADOL,SVSTACNT   NORMALLY $ TYPE = COUNTRY                    
         CLI   SVSTACNT,C'V'       TEST US STA IN CAN $                         
         BNE   *+8                                                              
         MVI   SVSTACNT,C'U'       SET COUNTRY = USA                            
         CLI   SVSTADOL,C'U'       TEST US STA/US $                             
         BE    *+8                                                              
         MVI   SVSTADOL,C'C'       ELSE $ MUST BE CAN                           
         MVC   SVSTACTX,SCANTAX                                                 
         MVC   SVSTAMSF,SSVCFEE    MEDIA SERVICE FEE                            
         MVC   SVGSTCOD,SGSTCODE                                                
         CLI   SVGSTCOD,C' '                                                    
         BH    *+8                                                              
         MVI   SVGSTCOD,C'S'                                                    
*                                                                               
STA10F   CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   STA11                                                            
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BNE   STA11                                                            
         LA    R4,SCANNTWK                                                      
         FOUT  BUYCBLH                                                          
         MVC   BUYCBL(4),0(R4)     DISPLAY DEFAULT AFFL                         
         CLI   0(R4),C' '                                                       
         BH    *+10                                                             
         MVC   BUYCBL(7),=C'NO AFFL'                                            
* 24AUG01 - DO NOT REQUIRE DEFAULT AFFILIATE - MHER                             
         MVI   ERRCD,NOAFFL                                                     
         CLI   SCANNTWK,C' '                                                    
         BNH   STA11                                                            
         MVC   WORK(4),0(R4)                                                    
         MVI   ERRCD,NONETFND                                                   
         BRAS  RE,GETBITS          GET MSPACK BITS                              
         BNE   BUYERR                                                           
         EJECT                                                                  
*=======================================================*                       
* READ MARKET REC                                       *                       
*=======================================================*                       
         SPACE 1                                                                
STA11    MVC   WORK(4),SMKT                                                     
         TM    SVPWFLG,X'01'       TEST WI PW ESTIMATE                          
         BZ    *+8                                                              
         BRAS  RE,CHKMKT                                                        
                                                                                
         BRAS  RE,GETMKT                                                        
*                                                                               
STA11A   L     RE,AREC                                                          
         USING MKTRECD,RE                                                       
*                                                                               
         MVC   BUYSTXP(4),WORK     MARKET NUMBER                                
         MVC   BUYSTXP+5(17),MKTNAME                                            
         FOUT  BUYSTXPH                                                         
*                                                                               
         CLI   BUYMD,C'N'            IF NOT NETWORK MEDIA                       
         BE    STA11B                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(ZEROMKT)  CANNOT BUY FOR MARKET ZERO                 
         CLC   =C'0000',WORK                                                    
         BE    BUYERR                                                           
*                                                                               
STA11B   MVC   SVSWPCLS,MKTCLASS                                                
         MVC   SVMZONE,MKTZONE     SAVE DAYLIGHT TIME DSPL                      
         MVC   SVRTGSVC,MKTRSVC    SAVE RATING SERVICE                          
         MVC   SVMKTMKT,MKTALST    SAVE ALPHA MARKET CODE                       
         MVC   SVMACCS,MKTLTACC                                                 
         MVC   SVLPMDAT,MKTLPMDT   SAVE LPM START DATE                          
*                                                                               
         MVC   SVRSMKT,MKTRSM1                                                  
         CLC   SVRTGSVC,MKTRS1     CHOOSE APPROPRIATE RTGSVC DATA               
         BE    STA11C                                                           
         MVC   SVRSMKT,MKTRSM2                                                  
         CLC   SVRTGSVC,MKTRS2                                                  
         BE    STA11C                                                           
         XC    SVRSMKT,SVRSMKT     DEFER ERROR TILL ADDS SEND                   
         DROP  RE                                                               
*                                                                               
STA11C   CLC   =C'MGEACC',BUYINP1  TEST INPUT FROM DARE SCRIPT                  
         BE    STA12               AVOID  LIMIT ACCESS-'TO BE OK'  PROB         
         BRAS  RE,CALLOFCR         TEST FOR MARKET LIMIT ACCESS                 
         EJECT                                                                  
STA12    DS    0H                                                               
         MVC   DUB(5),QSTA                                                      
         MVC   DUB+5(3),QCBLNET                                                 
         GOTO1 STAPACK,DMCB,(C'P',KEY+2),DUB,SVKEY+4                            
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   STA13                                                            
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BNE   STA13                                                            
         CLI   SVNETBTS,0                                                       
         BE    STA13                                                            
         MVC   SVKEY+8(1),SVNETBTS                                              
*                                                                               
STA13    CLI   SVCXTRA+2,C'A'      TEST MKTGRP ID'S ACTIVE                      
         BL    STAX                                                             
         CLI   SVCXTRA+2,C'Z'                                                   
         BH    STAX                                                             
         CLI   SVCXTRA+2,C'Y'                                                   
         BE    STAX                                                             
* READ DEFAULT MKTGRP FOR THIS MARKET                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D03'                                                  
         MVC   KEY+8(1),SVKEY                                                   
         CLI   SVCXTRA+2,C'F'                                                   
         BH    *+10                                                             
         MVC   KEY+9(2),SVCLT                                                   
         MVC   KEY+11(2),SVMKT                                                  
         MVI   ERRCD,BADMGRP                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
* NOW FIND ELEM FOR THIS SCHEME                                                 
         LA    R8,REC                                                           
         USING MKARECD,R8                                                       
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R6,MKAEL                                                         
         B     STA14B                                                           
STA14A   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
STA14B   CLI   0(R6),5                                                          
         BE    STA14C                                                           
         CLI   0(R6),0                                                          
         BNE   STA14A                                                           
         B     BUYERR                                                           
STA14C   CLC   5(1,R6),SVCXTRA+2   MATCH MKTGRP SCHEME                          
         BNE   STA14A                                                           
* DISPLAY MKTGRP ON SCREEN AND STORE IN SVID                                    
         MVC   WORK(21),BUYSTXP    SAVE                                         
         MVC   BUYSTXP,SPACES                                                   
         MVC   BUYSTXP(1),5(R6)   SCHEME ID                                     
         UNPK  DUB,6(3,R6)                                                      
         MVC   BUYSTXP+1(4),DUB+3 SCHEME NUM                                    
         MVC   SVID(5),BUYSTXP     ** SET DEFAULT ID **                         
         MVI   BUYSTXP+5,C'-'                                                   
         MVC   BUYSTXP+6(15),WORK                                               
         B     STAX                                                             
         EJECT                                                                  
STA20    BRAS  RE,GETNDEF                                                       
         BNE   STA10               IF NET NOT FOUND, MUST BE STATION            
*                                                                               
         BRAS  RE,GETBITS          GET NETWORK BITS (AGAIN)                     
         BRAS  RE,SETNDEF          REDISTRIBUTE PERCENTAGES                     
         BRAS  RE,SAVNDEF          SAVE NETDEF                                  
*                                                                               
         LHI   RE,SVD0PROF-BUYSAVE READ D0 PROFILE                              
         AR    RE,RA                                                            
         CLI   13(RE),C'Y'         TEST NATIONAL RATINGS                        
         JE    *+12                                                             
         CLI   13(RE),C'B'                                                      
         JNE   STAX                NO                                           
         MVC   WORK(4),=C'0000'    SET NETWORK MARKET                           
         BRAS  RE,GETMKT                                                        
*                                                                               
         L     RE,AREC                                                          
         USING MKTRECD,RE                                                       
         MVC   SVRTGSVC,MKTRSVC    SAVE RATING SERVICE                          
         MVC   SVMKTMKT,MKTALST    SAVE ALPHA MARKET CODE                       
         MVC   SVDFLTBT,MKTBKTYP                                                
         DROP  RE                                                               
*                                                                               
         MVC   DUB(5),QSTA         REREAD STATION MASTER RECORD                 
         BRAS  RE,GETSTMAS                                                      
         MVC   SVDMLST0,SRS1CALL   MOVE NSI LOOKUP CALL LETTERS                 
         MVC   SVDMLST1,SRS2CALL   AND BBM CALL LETTERS                         
*                                                                               
         LHI   RF,SQNORS1I         SET FOR STATION 1 IMPS FLAG                  
         MVI   SVDMLFLG,2          SET NSI LOOKUP FLAG                          
         CLI   SVRTGSVC,C'0'       TEST NSI MARKET                              
         BE    *+12                                                             
         MVI   SVDMLFLG,1          SET BBM LOOKUP FLAG                          
         LHI   RF,SQNORS2I                                                      
*                                                                               
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    SFLAG1,0 ** EXECUTED **                                          
         BZ    *+8                                                              
         OI    SVDMLFLG,X'80'      SET TO SUPPRESS IMPS                         
*                                                                               
STAX     OI    4(R2),X'20'                                                      
         MVC   SVDFLTXP,BUYSTXP    SAVE DEFAULT STA EXP                         
         MVC   SVDFLTMK,SVKEY+4    SAVE DEFAULT MARKET                          
         MVC   SVDFLTRT,SVRTGSVC   SAVE DEFAULT RATING SERVICE                  
         MVC   SVDFLTSW,SVSWPCLS   SAVE DEFAULT SWEEP CLASS                     
         MVC   SVDFLTID,SVID       SAVE DEFAULT ID                              
*                                                                               
         TM    SVPWFLG,X'01'       TEST WI PW ESTIMATE                          
         BO    *+14                                                             
         OC    SVECOST2,SVECOST2   OR WI COS2 ESTIMATE                          
         BZ    OPT                                                              
         BRAS  RE,GETPW                                                         
         B     OPT                                                              
         EJECT                                                                  
OPT      OC    SVNDEF,SVNDEF       TEST CANAD NTWK                              
         BZ    OPT0                                                             
*                                                                               
         BRAS  RE,RSTRNDEF                                                      
         MVI   SVNOVSET,C'Y'       SET AREA INITIALIZED                         
* FIX SVKEY                                                                     
         MVC   DUB(5),QSTA                                                      
         MVC   DUB+5(3),QCBLNET                                                 
         LA    R4,=C'0000'         NTWK BUYS IN MKT 0                           
         GOTO1 STAPACK,DMCB,(C'P',(R4)),DUB,SVKEY+4                             
                                                                                
OPT0     BRAS  RE,VALOPT                                                        
*                                                                               
         OC    SVNDEF,SVNDEF       TEST CANAD NTWK                              
         BZ    EQXIT                                                            
         CLI   SVNOVSET,C'Y'       TEST NEED TO RECALC                          
         BE    EQXIT               NO                                           
         BRAS  RE,SETNDEF                                                       
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUYERR   GOTO1 ERROR                                                            
         SPACE 2                                                                
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JZ    NEXTELX                                                          
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   0(1,R6),ELCDLO                                                   
         JL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         JH    NEXTEL                                                           
         CR    RE,RE               EXIT WITH CC EQ                              
         BR    RE                                                               
NEXTELX  LTR   RE,RE               EXIT WITH CC NOT EQ                          
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================*               
* ROUTINES TO CLEAR TWA FLDHDRS AND RELATED STORAGE             *               
*===============================================================*               
         SPACE 1                                                                
CLRMD    NI    BUYMDH+4,X'DF'                                                   
         XC    BUYMDXP,BUYMDXP                                                  
         XC    SVKEY+1(17),SVKEY+1                                              
         MVI   SVOPT1,0                                                         
         NI    BUYSTH+4,X'DF'      SO STATION WILL GET VALIDATED                
         FOUT  BUYMDXPH                                                         
*                                                                               
CLRBU    NI    BUYBUH+4,X'DF'                                                   
         XC    BUYBUXP,BUYBUXP                                                  
         FOUT  BUYBUXPH                                                         
*                                                                               
CLRCL    NI    BUYCLH+4,X'DF'                                                   
         XC    BUYCLXP,BUYCLXP                                                  
         XC    SVKEY+3(15),SVKEY+3                                              
         XC    SVADJLST,SVADJLST                                                
         XC    SVOLDNET,SVOLDNET                                                
         FOUT  BUYCLXPH                                                         
*                                                                               
CLRPR    NI    BUYPRH+4,X'DF'                                                   
         XC    BUYPRXP,BUYPRXP                                                  
         XC    SVKEY+4(14),SVKEY+4                                              
         FOUT  BUYPRXPH                                                         
*                                                                               
CLRES    NI    BUYESH+4,X'DF'                                                   
         XC    BUYESXP,BUYESXP                                                  
         XC    SVKEY+4(14),SVKEY+4                                              
         FOUT  BUYESXPH                                                         
*                                                                               
CLRST    NI    BUYSTH+4,X'DF'                                                   
         XC    BUYSTXP,BUYSTXP                                                  
         IC    R0,SVKEY+9          SAVE EST                                     
         XC    SVKEY+4(14),SVKEY+4                                              
         STC   R0,SVKEY+9          RESTORE EST                                  
         FOUT  BUYSTXPH                                                         
         XC    BUYCBL,BUYCBL                                                    
         FOUT  BUYCBLH                                                          
*                                                                               
         XC    SVQPRDS,SVQPRDS     FORCE REQUEST ON ANY HL CHANGE               
         MVI   SVUPDATE,0          CLEAR REASON FOR CHANGE FLAG                 
         XC    SVQLIST,SVQLIST                                                  
         XC    SVID,SVID           CLEAR ACTUAL ID                              
         XC    SVDFLTID,SVDFLTID   CLEAR DEFAULT ID                             
         XC    SVDFLTXP,SVDFLTXP   CLEAR DEFAULT STA EXP                        
         NI    SVPWFLG,X'FF'-X'C0' CLEAR PW BUY LOCKED FLAGS                    
*                                                                               
CLROP    NI    BUYOPH+4,X'DF'                                                   
         CLI   SVSCR,X'F3'         IF IN MG DTLS, ALLOW OPT CHANGES             
         JL    CLROP2                                                           
         CLI   SVSCR,X'F5'                                                      
         JH    CLROP2                                                           
         J     CLROPX                                                           
CLROP2   MVI   SVMGINIT,C'N'                                                    
         SPACE 1                                                                
CLROPX   BR    RE                                                               
         EJECT                                                                  
*=========================================================*                     
* READ CANAD NTWK DEF REC TO GET MSPACK BITS              *                     
*=========================================================*                     
         SPACE 1                                                                
GETBITS  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),WORK       WORK HAS NTWK                                
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   NEQXIT                                                           
         L     R0,AREC                                                          
         L     R8,AREC3                                                         
         USING NDEFRECD,R8                                                      
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC             RESTORE                                      
         LA    R6,NDEFEL                                                        
         DROP  R8                                                               
GETBITS2 CLI   0(R6),2                                                          
         BE    GETBITS4                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETBITS2                                                         
         B     *+12      ***** FOR COMPATABILITY ONLY *****                     
         DC    H'0'                                                             
GETBITS4 MVC   SVNETBTS,2(R6)                                                   
         J     EXIT                                                             
         EJECT                                                                  
*=============================================================*                 
* ROUTINE TO READ A STATION EQUIVALENCE RECORD                                  
*=============================================================*                 
         SPACE 1                                                                
GETSTEQ  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D44'                                                  
         MVC   KEY+2(1),SVKEY      A/M                                          
         MVC   KEY+3(2),SVKEY+1    CLT                                          
         MVC   KEY+5(5),QSTA       STA                                          
         CLI   QSTA,C'0'           TEST CABLE STATION                           
         BL    *+8                                                              
         MVI   KEY+9,C' '          THEN THERE IS NO 'T'                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JE    GETSTEQ2                                                         
         MVC   KEY,KEYSAVE         RESTORE                                      
         MVC   KEY+3(2),=X'FFFF'                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   NEQXIT                                                           
*                                                                               
GETSTEQ2 LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         J     EQXIT                                                            
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T211FFD+6                                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,SVKEY                                                   
         MVC   OFCLMT(4),T211FFD+6                                              
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK2),VCOMFACS                                  
         CLI   0(R1),0                                                          
         JE    EXIT                                                             
*                                                                               
         MVI   ERRCD,SCRTYERR                                                   
         CLI   SVMACCS,X'FF'                                                    
         BE    *+8                                                              
         MVI   ERRCD,NOMKTACC                                                   
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* READ MARKET RECORD TO AREC2                                                   
*     INPUT : WORK(4) = MARKET                                                  
*===================================================================            
         SPACE 1                                                                
GETMKT   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(4),WORK                                                    
         MVC   KEY+6(2),AGYALPHA                                                
         MVC   AREC,AREC2       <===== NOTE AREC2                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOMKTREC)                                            
         MVC   COMMAND,=C'DMRDHI'                                               
*                                                                               
         L     RE,AREC                                                          
         CLC   0(8,RE),KEY                                                      
         BE    GETMKTX                                                          
*                                                                               
         GOTO1 STA                                                              
*                                                                               
         L     RE,AREC                                                          
         CLC   KEY(8),0(RE)                                                     
         BE    GETMKTX                                                          
         GOTO1 ERROR                                                            
GETMKTX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* READ CANADIAN CBLNET RECORD AND EXTRACT MARKET NUMBER INTO WORK               
*===================================================================            
         SPACE 1                                                                
GETCBNET NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(CBLNETNF)                                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),QSTA                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETCBERR                                                         
* TRY FOR CLIENT EXCEPTION REC                                                  
         MVC   WORK(20),KEY        SAVE DEFAULT KEY                             
         MVC   KEY+8(2),SVCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETCB2                                                           
         MVC   KEY,WORK            RESTORE KEY                                  
*                                                                               
GETCB2   LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING NDEFRECD,R8                                                      
         GOTO1 GETREC                                                           
         SPACE 1                                                                
* RETRIEVE NTWKID ELEM                                                          
         SPACE 1                                                                
         LA    R6,NDEFEL                                                        
         SR    R0,R0                                                            
GETCB10  CLI   0(R6),X'02'                                                      
         BE    GETCB12                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETCB10                                                          
         DC    H'0'                                                             
*                                                                               
GETCB12  MVC   NERRCD,=Y(NOTCABLE)                                              
         CLI   2(R6),1             IS IT A CBLDEF                               
         BNE   GETCBERR                                                         
*                                                                               
         LA    R6,NDEFEL                                                        
*                                                                               
GETCB20  CLI   0(R6),1                                                          
         BNE   GETCB22                                                          
         USING NDEFEL01,R6                                                      
*                                                                               
         CLC   QCBLNET(2),NDEFMSUF   MATCH MARKET                               
         BE    GETCB24                                                          
*                                                                               
GETCB22  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETCB20                                                          
         MVC   NERRCD,=Y(NOCBLMKT)                                              
         B     GETCBERR                                                         
*                                                                               
GETCB24  SR    R0,R0                                                            
         ICM   R0,3,NDEFMNUM       GET MARKET NUMBER                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         XIT1                                                                   
*                                                                               
GETCBERR GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* VALIDATE AND DISPLAY CCUSA ACN NUMBERS *                                      
*===================================================================            
         SPACE 1                                                                
GETACN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVACNDT,SVACNDT                                                  
         XC    SVOLDACN,SVOLDACN                                                
*                                                                               
         CLI   BUYMD,C'T'         INTERFACE SPOT TV/RADIO ONLY                  
         BE    *+12                                                             
         CLI   BUYMD,C'R'                                                       
         BNE   GETACNX                                                          
*                                                                               
         CLC   SVEST,SVXFREST      TEST TO XFR THIS EST                         
         BH    GETACNX             NO - EXIT                                    
*                                                                               
         LA    R4,ELEM             SET UP SPACNVAL INTERFACE BLOCK              
         USING SPAVBLKD,R4                                                      
         LA    RF,SPAVBLKL                                                      
         XCEF  SPAVBLK,(RF)                                                     
         LA    R1,T211FFD                                                       
         ST    R1,SPAVATWA                                                      
         MVC   SPAVACMF,VCOMFACS                                                
         LA    R1,SVID                                                          
         ST    R1,SPAVAACN                                                      
         MVC   SPAVAGY,AGYALPHA                                                 
         MVC   SPAVMED,BUYMD                                                    
         MVC   SPAVSTA,QSTA                                                     
         MVC   SPAVCIFC,SVCLTIFC                                                
         MVC   SPAVSDT,SVSTARTB    SET EST START/END DATES                      
         MVC   SPAVEDT,SVENDB                                                   
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A44'   GET A(SPACNVAL)                     
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,SPAVBLK   CALL IT                                      
* SHOW US THE ACN NUMBER WE GOT BACK - ALWAYS                                   
         MVC   WORK(21),BUYSTXP    SAVE PRESENT STATION EXP                     
         MVC   BUYSTXP,SPACES                                                   
         MVC   BUYSTXP(5),SPAVDACN SET DEFAULT ACN                              
*                                                                               
         CLI   SPAVERR,0                                                        
         BE    GETACN2                                                          
*                                                                               
         LA    R2,BUYSTH           POSITION CURSOR TO STATION FIELD             
         NI    4(R2),X'DF'         SET STATION INVALID                          
         NI    SPAVERR,X'7F'       DROP HOB                                     
         ZIC   RE,SPAVERR                                                       
         LA    RE,700(RE)          ERROR MESSAGE NUM IS +700                    
         STCM  RE,3,NERRCD                                                      
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
*                                                                               
GETACN2  MVC   SVXFRSYS,SPAVASYS   SAVE ACCPAK SYSTEM                           
         MVC   SVXFRCOM,SPAVACOM               COMPANY                          
         MVC   SVXFRAGY,SPAVAAGY               AGENCY                           
         MVC   SVXFRNAM,SPAVNAME   SAVE BOTTLER NAME                            
         MVC   SVID,SPAVDACN       USE DEFAULT ACN ID                           
         MVC   BUYSTXP+6(15),WORK  AND THEN MKT DATA                            
*                                                                               
         OC    SPAVDTCH,SPAVDTCH                                                
         BZ    GETACNX                                                          
         MVC   SVOLDACN,SPAVOACN   SAVE OLD ACN NUMBER TOO !                    
         MVC   WORK(2),SPAVDTCH                                                 
         MVI   WORK+2,1                                                         
         GOTO1 VDATCON,DMCB,(3,WORK),WORK+3                                     
         GOTO1 VCALLOV,DMCB,0,X'D9000A1D'   GET GETBROAD ADDRESS                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(1,WORK+3),WORK+9,VGETDAY,VADDAY                       
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
         GOTO1 VDATCON,DMCB,WORK+9,(3,SVACNDT)                                  
         B     GETACNX                                                          
         DROP  R4                                                               
*                                                                               
GETACNX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*===================================================================            
* SUBROUTINE TO BUILD TABLE OF WEEKLY GOALS BY DAYPART/SLN                      
*            CLIENT OPTION SPECIFIED IN CEXTRA+8                                
* === NOTE === THIS AREA IS CLEARED AT BEGINNING OF STATION                     
*  EDIT LOGIC - IF CLEARED HERE SVNDEF IS DESTROYED FOR                         
*  CANADIAN NETWORK BUYING -- THE 800 BYTE AREA IS SHARED                       
* !!!                                                                           
* MHER 2APR13                                                                   
* SINCE THERE WASN'T ROOM FOR THE WHOLE TABLE IN NBUYSAVE,                      
* SVGLWKS/DPTS IS NOW BUILT IN AREC2 AND SAVED IN UTL XA STORAGE                
* FIELDS ARE NOW PREFIXED WITH WSGL INSTEAD OF SVGL                             
*===================================================================            
         SPACE 1                                                                
BLDGLS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVCXTRA+8,C'Y'      TEST OPTION REQUESTED                        
         BE    BLDGL2                                                           
         CLI   SVCXTRA+8,C'P'      TEST PG GOALS                                
         BE    BLDGL4                                                           
         B     BLDGLXIT                                                         
*                                                                               
BLDGL2   LHI   RE,WSGLTAB-SPBUYWKD    GET DSPL TO TABLE                         
         AR    RE,RC                  POINT TO TABLE                            
         LHI   RF,WSGLTABX-WSGLTAB    LENGTH OF TABLE                           
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         TM    SVOPT2,X'20'        TEST OVRD NOT TO CHECK                       
         BO    BLDGLXIT                                                         
         CLI   SVKEY+3,X'FF'       TEST POL BUY                                 
         BNE   BLDGL4                                                           
         CLI   SVPOLPRD,0          MUST NOT BE TRUE POL                         
         BNE   BLDGL4                                                           
         CLC   =C'MGEACC',BUYINP1  AVOID "CHECK GOALS FOR POL" MSG              
         BE    BLDGLXIT            BEHAVE LIKE NFG                              
         MVI   ERRCD,NOPOLWKS                                                   
         B     BLDGLBER                                                         
                                                                                
* STEP 1 - BUILD A LIST OF SUNDAY DATES IN EST PERIOD *                         
                                                                                
BLDGL4   GOTO1 VGETDAY,DMCB,SVSTART,WORK GET ESTIMATE START DAY                 
         LA    R0,7                                                             
         ZIC   RE,0(R1)                                                         
         SR    R0,RE                                                            
         CLI   SVEOWSDY,0          TEST OUT-OF-WEEK DATA                        
         BE    *+8                 NO                                           
         LA    R0,6                SET END DAY = START DAY +6                   
         GOTO1 VADDAY,DMCB,SVSTART,WORK,(R0)   ADVANCE TO 'SUNDAY'              
*                                                                               
         LHI   R4,WSGLTAB-SPBUYWKD                                              
         AR    R4,RC                                                            
         B     BLDGL8                                                           
*                                                                               
BLDGL6   LA    R0,7                                                             
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         MVC   WORK(6),WORK+6                                                   
*                                                                               
BLDGL8   GOTO1 VDATCON,DMCB,WORK,(2,(R4))                                       
         LA    R4,2(R4)                                                         
         CLC   WORK(6),SVEND                                                    
         BNH   BLDGL6                                                           
         MVC   0(2,R4),=X'FFFF'    FORCE HIGH DATE AT END OF TABLE              
                                                                                
* STEP 2 - READ THROUGH GOAL RECORDS                                            
* TABLE FORMAT IS DPT(1)/SLN(1)/WEEK BITS(8)                                    
                                                                                
BLDGL10  XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(6),SVKEY      A-M/CLT/PRD/MKT                              
         MVC   KEY+7(1),SVKEY+9    EST                                          
         OC    SVEST1(2),SVEST1    TEST SERIES SPECIFIED                        
         BZ    *+10                                                             
         MVC   KEY+7(1),SVEST1     YES - USE SERIES ESTIMATES                   
         CLI   KEY+4,X'FF'         IF KEY HAS POL                               
         BNE   *+10                                                             
         MVC   KEY+4(1),SVPOLPRD   MOVE ACTUAL PRD                              
*                                                                               
         CLI   SVCXTRA+8,C'P'      TEST PG GOALS                                
         BNE   BLDGL12                                                          
*                                                                               
         MVI   KEY+4,1             SET LOW BRAND NUMBER                         
         PACK  DUB,SVPGPROF+2(4)   GET MARKET NUMBER                            
         CVB   R0,DUB                                                           
         STCM  R0,3,KEY+5          SET MARKET NUMBER                            
         MVC   KEY+7(1),SVPGPROF+0 SET ESTIMATE NUMBER                          
*                                                                               
BLDGL12  GOTO1 HIGH                                                             
*                                                                               
BLDGL16  CLC   KEY(8),KEYSAVE      SAME A-M/CLT/PRD/MKT/EST                     
         BE    BLDGL18                                                          
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD/MKT                         
         BNE   BLDGL50                                                          
         OC    SVEST1(2),SVEST1    TEST SERIES SPECIFIED                        
         BZ    BLDGL50                                                          
         CLC   KEY+7(1),SVEST2     TEST PAST SERIES END                         
         BH    BLDGL50                                                          
         MVC   KEYSAVE,KEY         MOVE THRU EST TO KEYSAVE                     
*                                                                               
BLDGL18  MVI   ERRCD,DPTSLNMX                                                   
         LHI   R4,WSGLTAB-SPBUYWKD   POINT TO TABLE                             
         AR    R4,RC                                                            
         AHI   R4,WSGLDPTS-WSGLTABD  POINT TO DPT ENTRIES                       
*                                                                               
         MVC   HALF,KEY+8          MOVE DPT/SLN TO HALF                         
         CLI   SVCXTRA+8,C'P'      TEST PG GOALS                                
         BNE   *+10                                                             
         MVC   HALF(1),KEY+4       MOVE PRODUCT OVER DAYPART                    
*                                                                               
BLDGL20  CLI   0(R4),0             TEST E-O-T                                   
         BE    BLDGL22                                                          
         CLC   0(2,R4),HALF        MATCH DPT/SLN  (OR PRD/SLN)  ?               
         BE    BLDGL22             STILL MAKE SURE NOT PAST E-O-T               
         LA    R4,10(R4)           NEXT ENTRY                                   
         B     BLDGL20                                                          
*                                                                               
BLDGL22  LHI   R0,WSGLTAB-SPBUYWKD   DSPL TO TABLE IN SPBUYWK                   
         AR    R0,RC                 POINT TO TABLE                             
         AHI   R0,WSGLTABX-WSGLTAB   ADD TABLE LENGTH                           
         CR    R4,R0                                                            
         BNL   BLDGLBER                                                         
         MVC   0(2,R4),HALF        MOVE DPT/SLN  (OR PRD/SLN)                   
*                                                                               
         L     R8,AREC1                                                         
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,24(R8)                                                        
         MVI   ELCDLO,X'21'                                                     
         MVI   ELCDHI,X'21'                                                     
*                                                                               
BLDGL24  BAS   RE,NEXTEL                                                        
         BNE   BLDGL40                                                          
                                                                                
* DETERMINE WEEK BIT FOR THIS DATE *                                            
                                                                                
         LHI   RF,WSGLTAB-SPBUYWKD                                              
         AR    RF,RC                                                            
         SR    RE,RE                                                            
*                                                                               
BLDGL30  CLC   2(2,R6),0(RF)       ELEM DATE TO TABLE ENTRY                     
         BNH   BLDGL32                                                          
         LA    RF,2(RF)                                                         
         BCT   RE,BLDGL30                                                       
*                                                                               
BLDGL32  LPR   RE,RE                                                            
         SRDL  RE,3                DIVIDE BY 8                                  
         LA    RE,2(R4,RE)         POINT TO BYTE IN TABLE ENTRY                 
         SRL   RF,29               SHIFT WEEK COUNT                             
         IC    R0,BITTAB(RF)       GET APPROPRIATE BIT                          
         STC   R0,BYTE                                                          
         OC    0(1,RE),BYTE        OR BIT INTO TABLE ENTRY                      
         B     BLDGL24                                                          
*                                                                               
BLDGL40  GOTO1 SEQ                                                              
         B     BLDGL16                                                          
*                                                                               
BLDGL50  CLI   SVCXTRA+8,C'P'      TEST PG GOALS                                
         BNE   BLDGLX                                                           
         CLC   KEY(4),KEYSAVE      TEST SAME A-M/CLT                            
         BNE   BLDGLX                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE PREVIOUS KEY                         
         LLC   RE,KEY+4            GET PRODUCT                                  
         LA    RE,1(RE)                                                         
         STC   RE,KEY+4                                                         
         XC    KEY+8(5),KEY+8                                                   
         B     BLDGL12                                                          
*                                                                               
BLDGLX   DS    0H                  SAVE THE AREA JUST BUILT                     
         LA    R1,DMCB                                                          
         USING FAWSSVRD,R1                                                      
*                                                                               
         MVC   FAWSTOKN,=CL4'GLDP'                                              
         MVI   FAWSACTN,C'F'       SAVE INTO UTL XA BUFFER                      
         MVC   FAWSLEN,=AL2(WSGLTABX-WSGLTAB)                                   
         LHI   R0,WSGLTAB-SPBUYWKD                                              
         AR    R0,RC                                                            
         ST    R0,FAWSADR                                                       
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
BLDGLXIT XIT1                                                                   
*                                                                               
BLDGLBER GOTO1 ERROR                                                            
*                                                                               
BITTAB   DC    X'8040201008040201'                                              
         EJECT                                                                  
         LTORG                                                                  
*=================================================================*             
* SUBROUTINE READS AGENCY HEADER AND INITIALIZES SECRET           *             
*=================================================================*             
         SPACE 1                                                                
READAGY  NTR1  BASE=*,LABEL=*                                                   
         XC    SVRFPID,SVRFPID                                                  
         XC    SVAGYLK,SVAGYLK                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),AGYALPHA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,AGYHDR                                                        
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         MVC   SVAPROF,AGYPROF     SAVE AGY PROFILE                             
         MVC   SVAFLAG1,AGYFLAG1   SAVE FLAG BITS                               
         MVC   SVAFLAG2,AGYFLAG2                                                
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGY                            
         BE    *+8                 YES                                          
         MVI   SVAPROF+7,C'U'      ELSE FORCE US                                
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   *+8                                                              
         BAS   RE,GETXRATE         YES-GET EXCHANGE RATE                        
         SPACE 1                                                                
*=====================================================*                         
* SAVE AGENCY LOCK DATE                               *                         
*=====================================================*                         
         SPACE 1                                                                
         MVI   ELCDLO,X'71'        AGENCY EXTENSION ELEMENT                     
         MVI   ELCDHI,X'71'                                                     
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         USING AGYEXTEL,R6                                                      
         BAS   RE,NEXTEL2                                                       
         BNE   READAG2                                                          
         MVC   SVAGYLK,AGYLOCK                                                  
         MVC   SVRFPID,AGYPRNID    SAVE PRINCIPAL ID                            
         DROP  R6                                                               
         SPACE 1                                                                
*=====================================================*                         
* CHECK PASSWD PROTECT ACTIVE                         *                         
*=====================================================*                         
         SPACE 1                                                                
READAG2  XC    SVPASSWD,SVPASSWD                                                
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         DROP  RF                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVPASSWD,FAPASSWD                                                
         DROP  R1                                                               
         SPACE 1                                                                
*=====================================================*                         
* INITIALIZE SECRET                                   *                         
*=====================================================*                         
         SPACE 1                                                                
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    T211FFD+6(2),T211FFD+6   OR HAVE LIMIT ACCESS                    
         BZ    READAGYX                                                         
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
READAGYX DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*====================================================================*          
* ROUTINE TO GET A CANADIAN AGENCY'S EXCHANGE RATE                   *          
*====================================================================*          
         SPACE 1                                                                
GETXRATE NTR1                                                                   
         XC    SVXRATE,SVXRATE                                                  
         XC    KEY,KEY             READ EXCHANGE RATE RECORD                    
         LA    R5,KEY                                                           
         USING XRTRECD,R5                                                       
         MVI   XRTKTYP,XRTKTYPQ                                                 
         MVI   XRTKSUB,XRTKSUBQ                                                 
         MVC   XRTKAGY,AGYALPHA                                                 
         MVC   XRTKYR,SVTODAYB     TODAY'S YEAR                                 
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   GETXX                                                            
         L     R5,AREC4                                                         
         ST    R5,AREC                                                          
         GOTO1 GETREC                                                           
         MVI   ELCDLO,MXRELCDQ     GET EXCHANGE RATE ELEMENT                    
         MVI   ELCDHI,MXRELCDQ                                                  
         LA    R6,XRTFSTEL                                                      
*                                                                               
GETXR2   CLI   0(R6),0                                                          
         BE    GETXX                                                            
         CLI   0(R6),MXRELCDQ                                                   
         BE    GETXR4                                                           
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETXR2                                                           
*                                                                               
         USING MXREL,R6                                                         
GETXR4   SR    RE,RE                                                            
         IC    RE,SVTODAYB+1       TODAY'S MONTH                                
         BCTR  RE,0                                                             
         SLL   RE,1                                                             
         LA    RE,MXRMNTHS(RE)                                                  
         MVC   SVXRATE,0(RE)       CURRENT EXCHANGE RATE                        
*                                                                               
GETXX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* CANADIAN NETWORK - READ NTWK DEF REC                         *                
* EXIT WITH CC EQ IF NETWORK FOUND, ELSE NEQ                   *                
*==============================================================*                
         SPACE 2                                                                
GETNDEF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SVNOVSET,C'N'       FORCE THROUGH SETNDEF                        
         XC    WORK,WORK                                                        
         MVC   WORK(4),QSTA                                                     
         MVC   WORK+4(1),SVNRGN                                                 
         MVC   WORK+5(2),SVKEY+1   CLT                                          
         MVC   WORK+7(1),SVKEY+9   EST                                          
                                                                                
* READ NETWORK STATION MASTER RECORD FOR TAX CODES                              
                                                                                
GETND2   MVI   SVGSTCOD,C'S'       DEFAULT GST IS 'S'                           
         XC    SVPST,SVPST                                                      
*                                                                               
         MVC   DUB(5),QSTA                                                      
         BRAS  RE,GETSTMAS                                                      
         BNE   GETND10             IGNORE RECORD NOT FOUND                      
*                                                                               
         MVC   SVOLDNET,WORK       SAVE KEY OF SVOLDNET                         
         MVC   SVPST,SPST          PROVINCE SERVICE TAX                         
         MVC   SVGSTCOD,SGSTCODE                                                
         CLI   SVGSTCOD,C' '                                                    
         BH    *+8                                                              
         MVI   SVGSTCOD,C'S'                                                    
                                                                                
* FOLLOWING CODE FOR CABLE AND IRRELEVANT FOR NETWORK                           
                                                                                
         MVC   SVDMLST0,SRS1CALL   MOVE NSI LOOKUP CALL LETTERS                 
         MVC   SVDMLST1,SRS2CALL   AND BBM CALL LETTERS                         
*                                                                               
GETND10  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),QSTA                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IF NOT FOUND INPUT MUST BE STATION           
         JNE   NEQXIT                                                           
* TRY FOR CLIENT EXCEPTION REC                                                  
         MVC   WORK(20),KEY        SAVE DEFAULT KEY                             
         MVC   KEY+8(2),SVCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   KEY,WORK            RESTORE KEY                                  
         B     GETND12                                                          
* TRY FOR ESTIMATE EXCEPTION REC                                                
         MVC   WORK,KEY            SAVE CLIENT EXCEPTION KEY                    
         MVC   KEY+10(1),SVEST                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+10                                                             
         MVC   KEY,WORK                                                         
*                                                                               
GETND12  MVC   SVNDA,KEY+14        SAVE NETDEF DISK ADDRESS                     
*                                                                               
         LA    R8,REC                                                           
         USING NDEFRECD,R8                                                      
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         SPACE 1                                                                
* RETRIEVE NTWKID ELEM                                                          
         SPACE 1                                                                
         LA    R6,NDEFEL                                                        
GETND14  CLI   0(R6),X'02'                                                      
         BE    GETND16                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETND14                                                          
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(27),=C'BAD NETDEF RECORD. CALL DDS'                       
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 ERROR                                                            
*                                                                               
GETND16  MVC   SVNETBTS,2(R6)                                                   
         LA    R6,NDEFEL                                                        
         DROP  R8                                                               
*                                                                               
         LA    R4,BUYSTXP                                                       
         CLI   SVNETBTS,X'01'      TEST CBLDEF                                  
         BNE   GETND18                                                          
         MVC   0(4,R4),=C'CBL='                                                 
         B     GETND19                                                          
*                                                                               
GETND18  MVC   0(4,R4),=C'RGN='                                                 
         MVC   4(1,R4),SVNRGN                                                   
         MVI   5(R4),C','                                                       
         LA    R4,6(R4)                                                         
         CLI   SVNRGN,C'*'                                                      
         BE    *+12                                                             
         CLI   SVNRGN,C' '                                                      
         BNE   GETND20                                                          
*                                                                               
         LA    R4,BUYSTXP                                                       
         MVC   0(4,R4),=C'NET='                                                 
*                                                                               
GETND19  MVC   4(4,R4),QSTA                                                     
         CLI   7(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   8(R4),C','                                                       
         LA    R4,9(R4)                                                         
*                                                                               
GETND20  OC    KEY+8(2),KEY+8      TEST CLT EXCEPTION                           
         BZ    GETND30                                                          
         CLI   KEY+10,0            TEST EST EXCEPTION TOO                       
         BNE   GETND22             YES                                          
         MVC   0(3,R4),=C'CL='                                                  
         MVC   3(3,R4),QCLT                                                     
         CLI   5(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   6(R4),C','                                                       
         LA    R4,7(R4)            LEAVE R4 SET FOR NUM STATNS                  
         B     GETND30                                                          
                                                                                
* NOT ENOUGH ROOM FOR EXPLANATIONS - DISPLAY CLT AND EST                        
                                                                                
GETND22  MVC   0(3,R4),QCLT                                                     
         MVI   3(R4),C'/'                                                       
         ZIC   R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
         MVI   7(R4),C','                                                       
         LA    R4,8(R4)                                                         
                                                                                
* SAVE STATION LIST *                                                           
                                                                                
GETND30  LA    R7,SVNDEF                                                        
         USING SVNDEFD,R7                                                       
         LA    R8,SVNDDEM                                                       
         USING SVNDDEMD,R8                                                      
         XC    FULL,FULL                                                        
*                                                                               
GETND32  CLI   0(R6),1                                                          
         BNE   GETND48                                                          
         CLC   =F'-1',6(R6)        TEST NOT BOUGHT                              
         BE    GETND48                                                          
         ICM   R0,15,6(R6)         GET PERCENTAGE                               
         A     R0,FULL                                                          
         ST    R0,FULL                                                          
         CLI   SVNRGN,C'*'                                                      
         BE    GETND46                                                          
         CLI   SVNRGN,C' '         TEST RGN FILTER                              
         BNH   GETND46                                                          
* FILTER ON RGN                                                                 
         CLI   SVNRGN,C'Z'         TEST NUMERIC                                 
         BNH   GETND40             NO                                           
* NUMERIC RGN - MATCH RGN CODE                                                  
         LA    R0,4                                                             
         LA    R1,12(R6)                                                        
         CLC   0(1,R1),SVNRGN                                                   
         BE    GETND46                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     GETND48                                                          
                                                                                
* ALPHA RGN - USE STA IF LOW OR EQ                                              
                                                                                
GETND40  LA    R0,4                                                             
         LA    R1,12(R6)                                                        
*                                                                               
GETND42  CLI   0(R1),C'A'                                                       
         BL    GETND44                                                          
         CLI   0(R1),C'Z'                                                       
         BH    GETND44                                                          
         CLC   0(1,R1),SVNRGN                                                   
         BNH   GETND46                                                          
*                                                                               
GETND44  LA    R1,1(R1)                                                         
         BCT   R0,GETND42                                                       
         B     GETND48                                                          
*                                                                               
GETND46  DS    0H                                                               
         CLC   =C'ZZZZ',2(R6)      TEST NULL STATION                            
         BE    GETND48                                                          
*                                                                               
         USING NDEFEL01,R6                                                      
         MVC   1(10,R7),NDEFSTA    SAVE MKT/STA/PCTG/OFFSET                     
         AHI   R7,L'SVNDEF                                                      
         MVC   SVNDDMKT,NDEFAMKT   SAVE DEMO LOOKUP MKT                         
         AHI   R8,L'SVNDDEM                                                     
         DROP  R6                                                               
*                                                                               
GETND48  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETND32                                                          
*                                                                               
GETND50  MVI   ERRCD,NOSTATNS                                                   
         OC    SVNDEF,SVNDEF                                                    
         BZ    GETNDERR                                                         
*                                                                               
         LA    R7,SVNDEF           COUNT NUMBER OF STATIONS                     
         ZAP   HALF,=P'0'                                                       
GETND52  OC    SVNDMKST,SVNDMKST                                                
         BZ    GETND54                                                          
         AP    HALF,=P'1'                                                       
         AHI   R7,L'SVNDEF                                                      
         B     GETND52                                                          
*                                                                               
GETND54  MVC   0(2,R4),=C'N='                                                   
         OI    HALF+1,X'0F'                                                     
         UNPK  2(2,R4),HALF                                                     
*                                                                               
         DROP  R7,R8                                                            
*                                                                               
         MVI   ERRCD,NTWKPCTS                                                   
         CLC   FULL,=F'100000'     PCTS MUST SUM TO 100                         
         BNE   GETNDERR                                                         
*                                                                               
         MVC   DUB(5),QSTA                                                      
         MVC   DUB+5(3),QCBLNET                                                 
         LA    R4,=C'0000'         NTWK BUYS IN MKT 0                           
         GOTO1 STAPACK,DMCB,(C'P',(R4)),DUB,SVKEY+4                             
*                                                                               
GETNDX   J     EQXIT                                                            
*                                                                               
GETNDERR XC    SVOLDNET,SVOLDNET                                                
         NI    BUYMDH+4,X'FF'-X'20'   FORCE HEADLINE VALIDATION                 
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* ROUTINE TO FIGURE OUT WHICH MARKET TO USE                    *                
*==============================================================*                
                                                                                
CHKMKT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SVOLDMKT,SVOLDMKT   TEST ANY OLD MARKET NUMBER                   
         BZ    CHKMKTX             NO - EXIT                                    
* GET PACKED STATION                                                            
         XC    DUB,DUB                                                          
         MVC   DUB(5),QSTA                                                      
         GOTO1 STAPACK,DMCB,(C'P',=C'0000'),DUB,KEYSAVE                         
         MVC   DUB(3),KEYSAVE+2    SAVE PACKED STATION                          
*                                                                               
         XC    KEY,KEY             CHECK OLD MARKET FIRST                       
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,SVKEY       A-M                                          
         MVC   PWKCLT,SVKEY+1                                                   
         MVC   PWKPRD,SVPOLPRD     USE BRAND NOT POL                            
         MVC   PWKEST,SVKEY+9                                                   
         MVC   PWKMKT,SVOLDMKT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND                                   
         BNE   CHKMKT20            NO - TRY SECOND OLD MARKET                   
*                                                                               
         GOTO1 GETREC                                                           
         BAS   RE,CHKDOLS                                                       
         BZ    CHKMKT20            NO LOCKED DOLLARS, TRY AGAIN                 
         MVC   PWKSTA,DUB          MOVE STATION TO KEY                          
*                                                                               
         GOTO1 HIGH                TEST STATION IN THIS MARKET TOO              
         CLC   KEY(12),KEYSAVE                                                  
         BE    CHKMKT30            YES- USE THIS MARKET                         
*                                                                               
CHKMKT20 OC    SVOLDMKT+2(2),SVOLDMKT+2   TEST SECOND OLD MKT                   
         BZ    CHKMKTX                    NO - DONE                             
         XC    KEY,KEY                    ELSE TRY AGAIN                        
         MVC   KEY(7),KEYSAVE             TYPE/A-M/CLT/PRD/EST                  
         MVC   PWKMKT,SVOLDMKT+2                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND                                   
         BNE   CHKMKTX             IF NOT, USE ORIGINAL MARKET                  
         GOTO1 GETREC                                                           
         BAS   RE,CHKDOLS                                                       
         BZ    CHKMKTX             NO LOCKED DOLLARS, USE CURRENT MKT           
*                                                                               
         MVC   PWKSTA,DUB          MOVE STATION TO KEY                          
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     TEST THIS STATION WAS LOCKED                 
         BNE   CHKMKTX             NO - USE CURRENT MKT                         
*                                                                               
CHKMKT30 EDIT  PWKMKT,(4,WORK),FILL=0                                           
         DROP  R6                                                               
*                                                                               
CHKMKTX  XIT1                                                                   
         EJECT                                                                  
*==============================================================*                
* CHECK FOR LOCKED PW DOLLARS AND RETURN WITH CC NEQ IF FOUND                   
*==============================================================*                
         SPACE 1                                                                
CHKDOLS  NTR1                                                                   
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
*                                                                               
CHKDOL2  CLI   0(R6),0                                                          
         BE    CHKDOLX                                                          
         CLI   0(R6),6                                                          
         BNE   CHKDOL4                                                          
         USING PWDOLEL,R6                                                       
*                                                                               
         OC    PWDOLWG,PWDOLWG                                                  
         BNZ   CHKDOLNX                                                         
         OC    PWDOLWN,PWDOLWN                                                  
         BNZ   CHKDOLNX                                                         
         OC    PWDOLCG,PWDOLCG                                                  
         BNZ   CHKDOLNX                                                         
*                                                                               
CHKDOL4  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CHKDOL2                                                          
*                                                                               
CHKDOLX  CR    RE,RE                                                            
         B     *+6                                                              
CHKDOLNX LTR   RE,RE                                                            
         XIT1                                                                   
*==============================================================*                
* ROUTINE TO READ PW STATUS RECORD                             *                
*==============================================================*                
         SPACE 1                                                                
GETPW    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,SVKEY       A-M                                          
         MVC   PWKCLT,SVKEY+1                                                   
         MVC   PWKPRD,SVPOLPRD     USE BRAND NOT POL                            
         MVC   PWKEST,SVKEY+9                                                   
         MVC   PWKMKT,SVKEY+4                                                   
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND                                   
         BNE   GETPWX              IF NOT, CAN'T BE LOCKED                      
         LA    R6,REC                                                           
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
         USING PWRECD,R6                                                        
*                                                                               
         MVC   BYTE,PWGNFLG        MOVE STATUS FLAG                             
         NI    BYTE,X'C0'          DROP ALL BUT BUY LOCKED BITS                 
         OC    SVPWFLG,BYTE                                                     
* NOW SEE IF DOLLARS ARE LOCKED IN YET                                          
         LA    R6,PWEL             POINT TO FIRST ELEMENT                       
GETPW6   CLI   0(R6),0                                                          
         BE    GETPWX                                                           
         CLI   0(R6),4             COST2 INPUT ELEMENT                          
         BE    GETPW10                                                          
         CLI   0(R6),6                                                          
         BE    GETPW8                                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETPW6                                                           
GETPW8   OI    SVPWFLG,X'20'       SET DOLLARS LOCKED FLAG                      
         B     GETPWX                                                           
         DROP  R6                                                               
*                                                                               
         USING C2STEL,R6                                                        
GETPW10  MVC   SVECOST2,C2STFCTR   SAVE INPUT PERCENT FROM FIRST STA            
         OI    SVPWFLG,X'01'       SET FLAG THAT THIS WAS A PW CLIENT           
*                                                                               
GETPWX   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
* SUBROUTINE TO READ ALL DARE RECORDS FOR PRD/EST/STA       *                   
* AND SAVE PRD1/PRD2/FLIGHT FOR LOCKED DATA                 *                   
* TABLE DSECT IS SVDARED, BUT TO SAVE YOU HAVING TO LOOK -- *                   
* DRFLAG1  DS  CL1   C'Y' IF THERE IS DATA BY FLIGHT                            
* DRFLAG2  DS  XL1   NOT RELEVANT HERE (SEE TESTDARE)                           
* DRFLT0   DS  XL2   FLIGHT 0 END DATE                                          
* DRELEM2  DS  XL4   NOT RELEVANT HERE (SEE TESTDARE)                           
* DRSTRT1  DS  XL2   FLIGHT NUMBER 1 START DATE                                 
* DREND1   DS  XL2   FLIGHT NUMBER 1 END DATE                                   
* 15 MORE SETS OF FLIGHT DATES                                                  
* PRODUCT TABLE ENTRIES BEGIN AT +72                                            
*          DS  XL1   PRODUCT 1                                                  
*          DS  XL1   PRODUCT 2                                                  
*          DS  XL1   FLIGHT NUMBER                                              
*************************************************************                   
         SPACE 1                                                                
BLDDARE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,ASVDARE                                                       
         L     R1,=A(SVDAREX-SVDARE)  GET LENGTH OF AREA                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SAVE AREA                              
*                                                                               
         BAS   RE,FLTBL               BUILD 72 BYTES FLIGHT TABLE               
*                                                                               
         L     R4,ASVDARE                                                       
         LA    R4,72(R4)           SKIP PAST FLIGHT TABLE                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         MVC   DCKTYPE(2),=X'0DB5'                                              
***                                                                             
         MVC   DCKAGMD,SVKEY                                                    
         NI    DCKAGMD,X'0F'       ISOLATE THE MEDIA                            
         CLI   DCKAGMD,X'02'       IF WE'RE RADIO, THEN DON'T LOCK              
         BNE   BLDDR1                                                           
         MVC   BYTE,SVKEY+8        ISOLATE BAND                                 
         NI    BYTE,X'0F'          UNLESS YOU ARE                               
         CLI   BYTE,X'06'          -SM RADIO STREAMING?                         
         BE    BLDDR1                                                           
         CLI   BYTE,X'07'          -CM IHEART?                                  
         BNE   BLDDRX                                                           
***                                                                             
BLDDR1   MVC   DCKAGMD,SVKEY                                                    
         MVC   DCKCLT,SVCLT                                                     
         MVC   DCKPRD,SVPRD                                                     
         CLI   SVPRD,X'FF'         TEST POL ESTIMATE                            
         BNE   *+10                NO                                           
         MVC   DCKPRD,SVPOLPRD     YES - TRY TO USE BRAND POL BRAND             
         CLI   DCKPRD,0            BUT IF IT'S ZERO                             
         BNE   *+8                                                              
         MVI   DCKPRD,1            MAY AS WELL START WITH PRD 1                 
         MVC   DCKEST,SVKEY+9                                                   
         MVC   DCKSTA,SVKEY+6                                                   
*                                **FIX BUY DARE LOCKING FOR CABLE**             
         TM    SVXFRCTL,SVXFR_SDT  TEST SPOT DESKTOP MODE                       
         JZ    BLDDR1A             -NO                                          
         OC    SVPCVRSN,SVPCVRSN   -YES, HAVE PC VERSION?                       
         JZ    BLDDR1A              -NO, APPLY FIX                              
         CLC   SVPCVRSN,=AL1(4,6,0,050) -YES, IS VERSION LESS V4.6.0.5          
         JL    BLDDR2                -YES, DON'T APPLY FIX                      
                                                                                
BLDDR1A  CLI   DCKSTA,X'E8'        IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    DCKSTA+2,X'80'      YES, WE ONLY WANT SYSCODE LEVEL              
         DROP  R6                                                               
*                                                                               
BLDDR2   GOTO1 HIGH                DMLOCKER SETS AS IF RD4UPDATE                
*                                                                               
BLDDR4   GOTO1 VDATAMGR,DMCB,=C'DMUNLK',=C'SPTDIR'                              
         CLC   KEY(5),KEYSAVE      TYPE/AM/CLT                                  
         BNE   BLDDRX                                                           
*                                                                               
         CLC   KEY(10),KEYSAVE     TYPE/AM/CLT/PRD/EST/STA                      
         BNE   BLDDR20             TRY NEXT PRODUCT                             
*                                                                               
         MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
         GOTO1 VDATAMGR,DMCB,=C'DMUNLK',=C'SPTFIL'                              
*                                                                               
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
*                                                                               
BLDDR6   CLI   0(R6),0                                                          
         BE    BLDDR18             NEXT RECORD                                  
         CLI   0(R6),DOSPELQ       SUPPLEMENT ELEM?                             
         BNE   BLDDR7                                                           
         USING DOSPELD,R6                                                       
         CLI   DOSPLEN,DOSPTLNQ                                                 
         BL    BLDDR7                                                           
         CLI   DOSPIMTH,C'E'       LAST SENT VIA EXPRESS/EXPORT                 
         BE    BLDDR18             YES, UNLOCKED                                
         CLI   DOSPIMTH,C'F'       LAST SENT VIA FAX                            
         BE    BLDDR18             YES, UNLOCKED                                
         DROP  R6                                                               
*                                                                               
BLDDR7   CLI   0(R6),DOSTELQ       FIND STATUS ELEMENT                          
         BE    BLDDR10                                                          
BLDDR9   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     BLDDR6                                                           
***********************************                                             
LST_STAT DC    AL1(QRJCT)          REJECTED                                     
         DC    AL1(QCFMD)          CONFIRMED                                    
         DC    AL1(QUNDARE)        UNDARED                                      
         DC    AL1(QNODARE)        NOT DARE ANY MORE                            
         DC    AL1(QERRORED)       SF ERROR                                     
         DC    AL1(QRECALL)        RECALLING IS OKAY FOR AUTO-RECALL            
         DC    AL1(QRCLAPPR)       RECALL OF APPROVED ORDER                     
         DC    AL1(QRCLDELN)       RECALL OF DELIVERED ORDERE                   
         DC    AL1(QRCLTRNS)       RECALL AND TRANSMITTED TO STA                
         DC    AL1(QRCLWIP)        RECALL AND WORK IN PROGRESS                  
         DC    AL1(QEMPTY)         NO BUYS WHEN THIS ORDER WAS SENT             
         DC    AL1(QBYRCNFM)       BUYER CONFIRMED                              
         DC    AL1(QSNTXCNF)       SENT CANCELLED, RECALL CONF W/COM            
         DC    AL1(QSNTXREJ)       SENT CANCELLED, RECALLED REJECTED            
         DC    X'FF'               END OF TABLE                                 
***********************************                                             
         USING DOSTELD,R6                                                       
*                                                                               
BLDDR10  CLI   DOSTSTAT,DDLVRD     NEXT STATUS IF CURRENT IS DELIVERED          
         BE    BLDDR9                                                           
*                                                                               
         LA    RF,LST_STAT                                                      
BLDDR10A CLC   DOSTSTAT,0(RF)                                                   
         BE    BLDDR18             YES - ALLOW CHANGES                          
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'         HIT END OF TABLE?                            
         BNE   BLDDR10A                                                         
* STATUS IS PENDING - ADD PRODUCT TO BUFFER                                     
         L     RF,ASVDARE                                                       
         LA    RF,72(RF)                                                        
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
*                                                                               
BLDDR12  CR    RF,R4               AT WHERE WE'RE GOING TO ADD?                 
         BE    BLDDR14             YES                                          
*                                                                               
         MVC   BYTE,2(RF)          CHECK FLIGHT NUMBER IN TABLE                 
         NI    BYTE,X'0F'          IGNORE THE HIGH ORDER NIBBLE                 
         CLC   BYTE,DCKFLTNM                                                    
         BNE   BLDDR13                                                          
*                                                                               
         CLC   DCKPRD,DCKPRD2      CHECK ENTRY LOW/HIGH PRD CODE                
         BL    BLDDR12A                                                         
         CLC   1(1,RF),DCKPRD                                                   
         BNE   BLDDR13                                                          
         CLC   0(1,RF),DCKPRD2                                                  
         BNE   BLDDR13                                                          
         B     BLDDR13A            WE HAVE A MATCH                              
*                                                                               
BLDDR12A CLC   0(1,RF),DCKPRD                                                   
         BNE   BLDDR13                                                          
         CLC   1(1,RF),DCKPRD2                                                  
         BE    BLDDR13A                                                         
*                                                                               
BLDDR13  LA    RF,3(RF)            UNTIL WE FIND MATCH OR WHERE TO ADD          
         B     BLDDR12                                                          
*                                                                               
BLDDR13A TM    DCKFLAG,DCKFTRDE    TRADE?                                       
         BZ    *+12                                                             
         OI    2(RF),X'40'         YES, TRADE ORDER LOCKED                      
         B     *+8                                                              
         OI    2(RF),X'80'         NO, CASH ORDER LOCKED                        
         B     BLDDR18                                                          
*                                                                               
BLDDR14  L     R0,=A(SVDAREX-SVDARE-4)                                          
         A     R0,ASVDARE                                                       
         CR    R4,R0               WILL THE ENTRY FIT IN BUFFER                 
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         CLC   DCKPRD,DCKPRD2      INSERT ENTRY LOW/HIGH                        
         BL    BLDDR15                                                          
         MVC   1(1,R4),DCKPRD      ENTRY IS PRD1/PRD2                           
         MVC   0(1,R4),DCKPRD2                                                  
         B     BLDDR17                                                          
*                                                                               
BLDDR15  MVC   0(1,R4),DCKPRD      ENTRY IS PRD1/PRD2                           
         MVC   1(1,R4),DCKPRD2                                                  
*                                                                               
BLDDR17  MVC   2(1,R4),DCKFLTNM    FLIGHT NUMBER                                
         TM    DCKFLAG,DCKFTRDE    TRADE?                                       
         BZ    *+12                                                             
         OI    2(R4),X'40'         YES, TRADE ORDER LOCKED                      
         B     *+8                                                              
         OI    2(R4),X'80'         NO, CASH ORDER LOCKED                        
         LA    R4,3(R4)            POINT TO NEXT ENTRY                          
         DROP  R6                                                               
*                                                                               
BLDDR18  GOTO1 SEQ                 NEXT RECORD                                  
         B     BLDDR4                                                           
*                                                                               
* IF GET HERE, GOT CHANGE OF SOMETHING IN KEY                                   
*                                                                               
BLDDR20  CLC   KEY(6),KEYSAVE      TYPE/AM/CLT/PRD                              
         BE    BLDDR25             YES - GO ADD 1 TO PRD CODE                   
* PRODUCT CHANGED                                                               
         CLC   KEY+6(7),KEYSAVE+6  DID WE PASS EST/STA WE WANTED ?              
         BH    BLDDR25             YES - WE PASSED IT                           
         MVC   KEY+6(7),KEYSAVE+6  NO - TRY FOR OUR EST/STA                     
         B     BLDDR2                                                           
* PRODUCT DIDN'T CHANGE - WE NEED TO CHANGE IT !                                
BLDDR25  MVC   KEY,KEYSAVE                                                      
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         CLI   DCKPRD,X'FF'                                                     
         BE    BLDDRX                                                           
         IC    RE,DCKPRD           ELSE TRY NEXT PRODUCT NUMBER                 
         LA    RE,1(RE)                                                         
         STC   RE,DCKPRD                                                        
         B     BLDDR2                                                           
         DROP  R6                                                               
*                                                                               
BLDDRX   GOTO1 VDATAMGR,DMCB,=C'DMUNLK',=C'SPTDIR'                              
         GOTO1 (RF),(R1),=C'DMUNLK',=C'SPTFIL'                                  
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*                BUILD FLIGHT TABLE                                  *          
**********************************************************************          
                                                                                
FLTBL    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DFLRECD,R6          DARE FLIGHT RECORD                           
*                                                                               
         MVC   KEY(2),=X'0D38'     DARE FLIGHT RECORD                           
         MVC   DFLKAGMD,SVKEY      AG/MED,CLIENT                                
         MVC   DFLKCLT,SVCLT       CLIENT                                       
*                                  FIND PRODUCT MNEMONIC                        
         L     R1,ASVCLIST                                                      
FTBL40   CLC   3(1,R1),SVPRD                                                    
         BE    FTBL60                                                           
*                                                                               
FTBL50   LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   FTBL40                                                           
         DC    H'0'                                                             
*                                                                               
FTBL60   MVC   DFLKPRD,0(R1)       MOVE PRODUCT INTO THE KEY                    
*                                                                               
         MVC   DFLKEST,SVKEY+9     EST                                          
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     FLIGHT RECORD?                               
         BNE   FTBL110             NO                                           
*                                                                               
         MVC   AREC,AREC2                                                       
         GOTO1 GETREC                                                           
*                                                                               
         L     R5,ASVDARE          START OF THE TABLE                           
         LA    R7,72(R5)           END OF TABLE                                 
         LA    R5,8(R5)            FIRST FLIGHT DATE                            
         LR    R2,R5                                                            
*                                                                               
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
*                                                                               
FTBL70   CLI   0(R6),0             FIRST ELEMENT                                
         BE    FTBL110                                                          
         CLI   0(R6),DFINFELQ      X'01'-FLT0 ELEM?                             
         BE    FTBL80                                                           
         CLI   0(R6),DFFLTELQ      X'05'-FLT1-FLT16 ELEM?                       
         BE    FTBL90                                                           
*                                                                               
FTBL72   SR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     FTBL70                                                           
*                                                                               
         USING DFINFEL,R6          FLIGHT ELEMENT                               
FTBL80   L     RF,ASVDARE          START OF THE TABLE                           
         USING SVDARED,RF                                                       
         OC    SVDRFLT0,SVDRFLT0   ALREADY HAVE FLT0?                           
         JNZ   *+2                  THAT'S NOT GOOD                             
         GOTO1 VDATCON,DMCB,(3,DFINFSDT),(2,SVDRFLT0)                           
         B     FTBL72                                                           
         DROP  RF,R6                                                            
*                                                                               
         USING DFFLTEL,R6          FLIGHT ELEMENT                               
FTBL90   CR    R5,R7               MAXIMUM REACHED?                             
         BNL   FTBL110                                                          
                                                                                
*                                                                               
* HOLES IN THE TABLE ARE IDENTIFIED BY X'FF' IN THE FIRST BYTE                  
* END OF THE TABLE IS IDENTIFIED BY NULLS IN THE FIRST TWO BYTES                
* FLIGHT1 GOES TO FIRST SLOT IN TABLE, FOURTH TO FOURTH, ETC                    
*                                                                               
                                                                                
         LR    R5,R2               START OF TABLE                               
         SR    R4,R4                                                            
         ICM   R4,1,DFFLTNUM                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         SLL   R4,2                MULTIPLY IT BY 4                             
         AR    R5,R4                                                            
         GOTO1 VDATCON,DMCB,(3,DFFLTSTR),(2,0(R5))   START DATE                 
         GOTO1 VDATCON,DMCB,(3,DFFLTEND),(2,2(R5))   END DATE                   
         B     FTBL72                                                           
         DROP  R6                                                               
*                                                                               
FTBL110  L     R4,ASVDARE                                                       
         LA    R4,8(R4)            LIMIT                                        
*                                                                               
FTBL115  CR    R5,R4                                                            
         BL    FTBL120                                                          
         CLI   0(R5),0                                                          
         BNE   *+8                                                              
         MVI   0(R5),X'FF'                                                      
         AHI   R5,-4                                                            
         B     FTBL115                                                          
*                                                                               
FTBL120  L     R5,ASVDARE                                                       
         MVI   0(R5),C'N'          INDICATE NO FLIGHTS                          
         CLI   8(R5),0             TEST ANY DATES IN TABLE                      
         BE    FTBLEX                                                           
         MVI   0(R5),C'Y'                                                       
*                                                                               
FTBLEX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
* MAKE IT EASIER FOR ABS BY FILLING IN HEADLINES *                              
         SPACE 1                                                                
SETTMS   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,TMSDATA                                                       
         CLC   =C'LIS',BUYMD                                                    
         BNE   *+8                                                              
         LA    R4,LISADATA                                                      
*                                                                               
         XC    BUYMD,BUYMD                                                      
         LA    R2,BUYMDH                                                        
         MVI   5(R2),1                                                          
         MVI   4(R2),X'04'                                                      
         MVC   8(1,R2),0(R4)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BUYBUH                                                        
         MVI   4(R2),0                                                          
         MVI   5(R2),2                                                          
         MVC   8(12,R2),1(R4)                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BUYCLH                                                        
         MVI   4(R2),0                                                          
         MVI   5(R2),3                                                          
         MVC   8(3,R2),13(R4)                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BUYPRH                                                        
         MVI   4(R2),0                                                          
         MVI   5(R2),3                                                          
         MVC   8(3,R2),16(R4)                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BUYESH                                                        
         MVI   4(R2),X'08'                                                      
         MVI   5(R2),3                                                          
         MVC   8(3,R2),19(R4)                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BUYSTH                                                        
         MVI   4(R2),X'04'                                                      
         MVI   5(R2),4                                                          
         MVC   8(4,R2),22(R4)                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
*        GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
TMSDATA  DC    C'TTEST        MH POL197KYW '                                    
LISADATA DC    C'TLC          DTBDTB010WNRK'                                    
MAXLEN   EQU   72                  MAX LEN OF FLIGHT TABLE                      
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* PUT DATA VALUES TO GLOBBER                                      *             
*=================================================================*             
PUTGLOB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BUYMD,1,GLVSPMD                           
*                                                                               
         GOTO1 (RF),(R1),,BUYBU,12,GLVSPREQ                                     
         BAS   RE,GLBTEST                                                       
*                                                                               
         GOTO1 (RF),(R1),,QCLT,3,GLVSPCLT                                       
         BAS   RE,GLBTEST                                                       
*                                                                               
         GOTO1 (RF),(R1),,QPRD,3,GLVSPPRD                                       
         BAS   RE,GLBTEST                                                       
*                                                                               
         ZIC   R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         GOTO1 (RF),(R1),,WORK,3,GLVSPEST                                       
         BAS   RE,GLBTEST                                                       
*                                                                               
         MVC   ELEM(8),BUYST                                                    
         OC    ELEM(8),SPACES                                                   
         GOTO1 (RF),(R1),,ELEM,8,GLVSPSTA                                       
         BAS   RE,GLBTEST                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SVKEY+4        GET MARKET NUMBER                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM(4),DUB                                                      
         GOTO1 (RF),(R1),,ELEM,4,GLVSPMKT                                       
         XIT1                                                                   
*                                                                               
GLBTEST  CLI   DMCB+8,0                                                         
         BER   RE                                                               
         DC    H'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================*               
* READ STATION MASTER RECORD FOR STATION IN DUB                 *               
*===============================================================*               
         SPACE 1                                                                
GETSTMAS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,STAREC                                                        
         LHI   R1,SCBLSQNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(5),DUB                                                     
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   KEY+9(3),QCLT                                                    
         LA    RE,STAREC                                                        
         ST    RE,AREC                                                          
         XC    STAREC(256),STAREC                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOSTAREC)                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         CLC   KEY(12),STAREC      SAME TYP/MED/STA/AGY/CLT                     
         JE    EQXIT                                                            
         MVC   KEY+9(3),=C'000'                                                 
         GOTO1 STA                                                              
         CLC   KEY(12),STAREC      SAME TYP/MED/STA/AGY/CLT                     
         JE    EQXIT                                                            
         J     NEQXIT                                                           
         LTORG                                                                  
*==============================================================                 
* VALIDATE OPTIONS                                                              
*==============================================================                 
         SPACE 1                                                                
VALOPT   NTR1  BASE=*,LABEL=*                                                   
*&&DO                                                                           
************************************************************************        
* MAINFRAME BUY NO LONGER DIRECTLY USED, REMOVING BUYER/BILLER FEATURE          
*                                                        -HWON 8/7/2020         
         CLI   SVAPROF+14,C'Y'     TEST BUYER/BILLER SAVED                      
         BNE   OPT3                                                             
         LA    R2,BUYBUH                                                        
         TM    SVXFRCTL,SVXFR_DARE TEST XFR FROM DARE SCRIPT?                   
         BO    OPT2X               YES - SKIP GETBUBL                           
         CLC   =C'NON-POL',BUYBU                                                
         BE    OPT2X                                                            
*                                                                               
         CLC   =C'MGEACC',BUYINP1  TEST INPUT FROM DARE SCRIPT                  
         BE    OPT1A               YES - SET SVXFR_DAR & SKIP GETBUBL           
*                                                                               
         TM    SVXFRCTL,SVXFR_SDT  TEST SPOT DESKTOP MODE                       
         BZ    OPT1B                                                            
         CLC   =C'MGE',BUYINP1     YES, TEST DOING MAKEGOODS                    
         BNE   OPT1B                                                            
*                                  YES, MAKE APPEAR XFR FROM DARE               
OPT1A    OI    SVXFRCTL,SVXFR_DARE SET XFR FROM DARE SCRIPT FLAG                
         B     OPT2X                                                            
*                                                                               
OPT1B    CLI   BUYBU,C'/'          SO DARE CAN WORK                             
         BNE   OPT1X                                                            
         MVC   BUYBU(L'BUYBU-1),BUYBU+1                                         
         B     OPT2X                                                            
*                                                                               
OPT1X    CLI   5(R2),4             LET 'TEST' GO THROUGH                        
         BNE   OPT2                                                             
         CLC   =C'TEST',BUYBU                                                   
         BE    OPT2X                                                            
*                                                                               
OPT2     BRAS  RE,GOGETBU          CALL BUYER NAME INTF                         
*                                                                               
* MAINFRAME BUY NO LONGER DIRECTLY USED, REMOVING BUYER/BILLER FEATURE          
************************************************************************        
*&&                                                                             
OPT2X    LA    R2,BUYBUH                                                        
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET BUYER VALIDATED                          
*                                                                               
OPT3     NI    DRMGFLG,X'FF'-OTOOK                                              
         CLC   =C'TEST',BUYBU                                                   
         BNE   OPT3A                                                            
         CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   OPT3A                                                            
         OI    DRMGFLG,OTOOK       OTO OK EVEN IF MKGD PENDING                  
*                                                                               
OPT3A    LA    R2,BUYOPH                                                        
         TM    4(R2),X'20'                                                      
         BO    OPTX                                                             
         XC    BUTRCODE,BUTRCODE                                                
         CLI   SVAPROF+7,C'C'                                                   
         BNE   OPT4                                                             
         OC    SVNDEF,SVNDEF       TEST NTWK LEVEL                              
         BNZ   OPT4                YES - LEAVE BITS ALONE                       
         CLI   SVNETBTS,0          TEST SAVED CANAD NTWK BITS                   
         BE    OPT4                                                             
         MVC   SVKEY+8(1),SVNETBTS RESTORE ORIGINAL BITS                        
*                                                                               
OPT4     BRAS  RE,CLROP                                                         
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   SVOPTS,0                                                         
         MVI   SVOPT2,0                                                         
         LHI   RE,SVDAROPT-BUYSAVE                                              
         AR    RE,RA                                                            
         MVI   0(RE),0                                                          
         MVI   SVCTAFLG,0          RESET CTA OPTIONS                            
         MVI   SVDPTOPT,0          RESET DAYPART FROM OPTIONS                   
         MVI   SVOPTBT,0           RESET BOOKTYPE FROM OPTIONS                  
         NI    SVARBF94,X'FF'-QF94NSI-QF94ARB                                   
         XC    SVOPTBK(3),SVOPTBK  RESET BOOK/HUT FROM OPTIONS                  
         XC    SVUPDTA,SVUPDTA                                                  
*   CLEAR ALL BUT PAIDOTO AND VNDRLCK FLAGS                                     
         NI    SVOPT1,SVOPT1_PAIDOTO+SVOPT1_VNDRLCK                             
         LHI   RE,SVUPTXT-BUYSAVE                                               
         AR    RE,RA                                                            
         XC    0(L'SVUPTXT,RE),0(RE)                                            
         XC    SVNOVEST(2),SVNOVEST  CLEAR NETWK OVRD EST/LIN                   
*                                                                               
         CLC   SVKEY+4(2),SVDFLTMK   TEST DEFAULT MKT = ACTUAL                  
         BE    *+10                                                             
         XC    SVKEY+10(8),SVKEY+10  NO-CLEAR LAST LINE RECALL                  
*                                                                               
         MVC   SVKEY+4(2),SVDFLTMK   RESTORE DEFAULT MARKET                     
         MVC   SVRTGSVC,SVDFLTRT     RESTORE DEFAULT RATING SERVICE             
         MVC   SVSWPCLS,SVDFLTSW     RESTORE DEFAULT SWEEP CLASS                
         MVC   SVID,SVDFLTID         RESTORE ACTUAL ID = DEFAULT                
         MVC   BUYSTXP,SVDFLTXP      RESTORE ACTUAL STA EXP = DEFAULT           
         MVC   SVBKTYPE,SVDFLTBT     RESTORE DEFAULT STA/MKT BOOKTYPE           
         XC    SVOPTREP,SVOPTREP                                                
         OI    BUYSTXPH+6,X'80'      AND XMT                                    
*                                                                               
OPT6     MVI   BYTE,0                                                           
         MVI   BYTE2,0                                                          
         XC    FSTOPS,FSTOPS       RESET STOP CHARS                             
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    OPTX                                                             
         CLC   =C'BK=',0(R4)                                                    
         BNE   *+12                                                             
         LA    R4,3(R4)                                                         
         B     OPT10                                                            
         CLC   =C'BOOK=',0(R4)                                                  
         BNE   *+12                                                             
         LA    R4,5(R4)                                                         
         B     OPT10                                                            
         CLC   =C'UPT=X',0(R4)                                                  
         BE    OPT41                                                            
         CLC   =C'UPT=',0(R4)                                                   
         BE    OPT40                                                            
         CLC   =C'UPP=',0(R4)                                                   
         BE    OPT40                                                            
         CLC   =C'ID=',0(R4)                                                    
         BE    OPT20                                                            
         CLC   =C'MKT=',0(R4)                                                   
         BE    OPT15                                                            
         CLC   =C'REP=',0(R4)                                                   
         BE    OPT16                                                            
         CLC   =C'PB',0(R4)        PIGGYBACKS                                   
         BNE   *+8                                                              
         MVI   BYTE,X'80'                                                       
         CLC   =C'COS',0(R4)                                                    
         BNE   *+8                                                              
         OI    BYTE,X'80'                                                       
         CLC   =C'UPB',0(R4)       UNEQUAL PIGGYBACKS                           
         BNE   *+8                                                              
         MVI   BYTE,X'C0'                                                       
         CLC   =C'DEM',0(R4)       DISPLAY DEMOS                                
         BNE   *+8                                                              
         MVI   BYTE,X'08'                                                       
         CLI   SVPRD,X'FF'                                                      
         BNE   OPT6A                                                            
         CLI   FLEN+1,1                                                         
         BNE   OPT6A                                                            
         CLI   0(R4),C'R'          ROTATION                                     
         BNE   *+8                                                              
         MVI   BYTE,X'04'                                                       
OPT6A    CLC   =C'NR',0(R4)        SUPPRESS ROTATION                            
         BNE   *+8                                                              
         MVI   BYTE,X'02'                                                       
         CLC   =C'MAS',0(R4)                                                    
         BNE   *+8                                                              
         MVI   BYTE,X'01'                                                       
         CLC   =C'NET=',0(R4)                                                   
         BE    OPT30                                                            
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    OPT8                                                             
         CLI   SVCXTRA+5,C'D'      TEST US DPT SUMMARY SPILL                    
         BE    OPT8                                                             
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   OPT8X                                                            
OPT8     CLC   0(2,R4),=C'NSP'     NO SPILL                                     
         BNE   *+8                                                              
         MVI   BYTE2,X'80'                                                      
*                                                                               
OPT8X    CLC   =C'NOTAX',0(R4)                                                  
         BNE   *+8                                                              
         MVI   BYTE2,X'40'                                                      
         CLC   =C'NTX',0(R4)                                                    
         BNE   *+8                                                              
         MVI   BYTE2,X'40'                                                      
*                                                                               
         CLC   =C'NC=N',0(R4)      SUPPRESS NOCHARGE SPOTS IN MGA               
         BNE   *+12                                                             
         OI    SVOPT1,SVOPT1_NONOCHG                                            
         B     OPT6                                                             
*                                                                               
         CLC   =C'NC=Y',0(R4)      INCLUDLE NOCHARGE SPOTS                      
         BNE   *+12                                                             
         OI    SVOPT1,SVOPT1_NONOCHG                                            
         B     OPT6                                                             
*                                                                               
         CLC   =C'PCT=',0(R4)                                                   
         BE    OPT50                                                            
*                                                                               
         CLC   =C'NOGOAL',0(R4)                                                 
         BNE   *+8                                                              
         MVI   BYTE2,X'20'                                                      
         CLC   =C'NFG',0(R4)                                                    
         BNE   *+8                                                              
         MVI   BYTE2,X'20'                                                      
*                                                                               
         CLC   =C'NODEMO',0(R4)                                                 
         BNE   *+12                                                             
         OI    SVESTFL1,EF1NODEM   SET NO DEMOS EXPECTED                        
         B     OPT6                                                             
*                                                                               
         CLC   =C'NOMIN',0(R4)                                                  
         BNE   *+8                                                              
         MVI   BYTE2,X'10'                                                      
*                                                                               
         CLC   =C'OVERW',0(R4)     TEST OVERWRITING BUY LINES OK                
         BNE   *+8                                                              
         MVI   BYTE2,X'08'                                                      
*                                                                               
         CLC   =C'BT=',0(R4)       BOOKTYPE OVERRIDE                            
         BE    OPT80                                                            
*                                                                               
         CLC   =C'MSSD',0(R4)                                                   
         BNE   OPT9A                                                            
         LHI   RE,SVDAROPT-BUYSAVE                                              
         AR    RE,RA                                                            
         OI    0(RE),SVDAROPT_MSSD                                              
         B     OPT6                                                             
*                                                                               
OPT9A    CLC   =C'NODAR',0(R4)                                                  
         BE    *+14                                                             
         CLC   =C'NDE',0(R4)                                                    
         BNE   OPT9X                                                            
         LHI   RE,SVDAROPT-BUYSAVE                                              
         AR    RE,RA                                                            
         OI    0(RE),SVDAROPT_NOERRS                                            
         B     OPT6                                                             
*                                                                               
OPT9X    CLC   =C'NOUNLK',0(R4)                                                 
         BNE   *+8                                                              
         MVI   BYTE2,SVOPT2_NOUNLK    PW OVERRIDE FOR MISSING LOCKS             
*                                                                               
         CLC   =C'CTA=',0(R4)                                                   
         BE    OPT83                                                            
         CLC   =C'TRD=',0(R4)                                                   
         BE    OPT83A                                                           
         CLC   =C'BB=',0(R4)       CTA BACK/BACK OPTION                         
         BE    OPT84                                                            
*                                                                               
         CLC   =C'IDR=',0(R4)                                                   
         BE    OPT85                                                            
         CLC   =C'PUR=',0(R4)                                                   
         BE    OPT85                                                            
*                                                                               
         CLC   =C'OLDMG',0(R4)     TEST FORCE TO OLD MAKEGOODS                  
         BNE   *+12                                                             
         NI    SVESTFL1,X'FF'-EF1NMG  UNSET NEW MAKEGOOD FLAG                   
         B     OPT6                                                             
*                                                                               
         CLC   =C'CNET',0(R4)      TO FORCE A CABLE NETWORK CODE                
         BE    OPT86                                                            
*                                                                               
         CLC   =C'DPT=',0(R4)                                                   
         BNE   OPT9X2                                                           
         MVC   SVDPTOPT,4(R4)                                                   
         CLI   SVDPTOPT,C'A'                                                    
         BL    OPT9X2                                                           
         CLI   SVDPTOPT,C'Y'                                                    
         BH    OPT9X2                                                           
         B     OPT6                                                             
*                                                                               
OPT9X2   CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   OPT9X4               NO - OPTION ONLY VALID FOR DDS              
         CLC   =C'NFB',0(R4)       JUST FOR MEL                                 
         BE    OPT9X3                                                           
         CLC   =C'NOBUYVAL',0(R4)                                               
         BE    OPT9X3                                                           
         CLC   =C'NOBV',0(R4)                                                   
         BNE   OPT9X4                                                           
OPT9X3   OI    SVOPT1,SVOPT1_NOBYVAL                                            
         B     OPT6                                                             
*                                                                               
OPT9X4   MVI   ERRCD,INVERR                                                     
         CLI   BYTE,0                                                           
         BNE   *+12                                                             
         CLI   BYTE2,0                                                          
         BE    OPTERR                                                           
         OC    SVOPTS,BYTE                                                      
         OC    SVOPT2,BYTE2                                                     
         B     OPT6                                                             
         EJECT                                                                  
*                                                                               
* EDIT OVERRIDE BOOK/HUT , BK= OR BOOK= OPTION                                  
*                                                                               
OPT10    MVI   ERRCD,BOOKERR                                                    
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         ST    R4,FADDR                                                         
         MVI   FSTOPS+1,C'-'       SET TO STOP ON OPTIONAL HUT MONTH            
         GOTO1 FLDVAL                                                           
         MVI   FSTOPS+1,0                                                       
         SPACE 1                                                                
* CREATE FLDHDR FOR BOOKVAL *                                                   
         SPACE 1                                                                
         XC    ELEM,ELEM                                                        
         STC   R5,ELEM+5                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),0(R4) *EXECUTED*                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A00'   GET BOOKVAL ADDRESS                 
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(C'N',ELEM),(1,DUB),VSCANNER                           
         CLI   4(R1),0                                                          
         BE    OPTERR                                                           
         TM    DUB,X'BF'           TEST ANY GARBAGE OPTIONS INPUT               
         BNZ   OPTERR                                                           
         MVC   SVOPTBK,DUB+1       ELSE SAVE BOOK                               
*                                                                               
         CLI   FSTOP,C'-'          TEST STOP CHAR                               
         BNE   OPT6                                                             
         SPACE 1                                                                
* EDIT HUT MONTH *                                                              
         SPACE 1                                                                
         GOTO1 FLDVAL              SCAN MONTH DATA                              
         MVI   ERRCD,HUTERR                                                     
         CLI   FLEN+1,3                                                         
         BNE   OPTERR                                                           
         MVC   DUB(3),0(R4)                                                     
         MVC   DUB+3(2),=C'15'                                                  
         GOTO1 VDATVAL,DMCB,(1,DUB),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    OPTERR                                                           
         PACK  DUB,WORK+2(2)                                                    
         CVB   R0,DUB                                                           
         SLL   R0,4                                                             
         STC   R0,SVOPTHUT                                                      
         B     OPT6                                                             
         EJECT                                                                  
*                                                                               
* EDIT OVERRIDE MARKET NAME, MKT= OPTION                                        
*                                                                               
OPT15    LA    R4,4(R4)                                                         
         AHI   R5,-5               SET FOR EX                                   
         MVI   ERRCD,MKTERR                                                     
         BM    OPTERR                                                           
         CHI   R5,3                MAX LEN IS (4)                               
         BH    OPTERR                                                           
*                                                                               
         LA    R1,1(R5)            TEST VALID NUMERIC                           
         LR    RE,R4                                                            
OPT15B   CLI   0(RE),C'0'                                                       
         BL    OPTERR                                                           
         CLI   0(RE),C'9'                                                       
         BH    OPTERR                                                           
         LA    RE,1(RE)                                                         
         BCT   R1,OPT15B                                                        
*                                                                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   R5,DUB                                                           
         CLC   =C'TEST',BUYBU                                                   
         BE    OPT15F                                                           
* CHECK IF MARKET IS IN STATION RECORD                                          
         MVC   NERRCD,=AL2(STANTMKT)                                            
         MVI   ERRCD,NEWERRS                                                    
         SR    R1,R1                                                            
         ICM   R1,3,SVOLDMKT                                                    
         BZ    OPT15D                                                           
         CR    R5,R1                                                            
         BE    OPT15F                                                           
*                                                                               
OPT15D   SR    R1,R1                                                            
         ICM   R1,3,SVOLDMKT+2                                                  
         BZ    OPTERR                                                           
         CR    R5,R1                                                            
         BNE   OPTERR                                                           
OPT15F   EDIT  (R5),(4,WORK),FILL=0                                             
*                                                                               
         STCM  R5,3,SVKEY+4                CHANGE BUY KEY                       
         XC    SVKEY+10(8),SVKEY+10        FORCE NEW RECALL                     
*                                                                               
         BRAS  RE,GETMKT                                                        
*                                                                               
         L     RE,AREC                                                          
         USING MKTRECD,RE                                                       
         MVC   BUYSTXP(4),WORK           FORMAT MARKET TO SCREEN                
         MVC   BUYSTXP+5(17),MKTNAME                                            
         MVC   SVSWPCLS,MKTCLASS                                                
         MVC   SVRTGSVC,MKTRSVC    SAVE RATING SERVICE                          
         MVC   SVMKTMKT,MKTALST    SAVE ALPHA MARKET CODE                       
         OI    SVOPT2,SVOPT2_MKTOV       OVERRIDE MARKET ENTERED                
         MVC   SVBKTYPE,MKTBKTYP     SET OVERRIDE MARKET BOOKTYPE               
         B     OPT6                                                             
         DROP  RE                                                               
*                                                                               
* EDIT OVERRIDE REP, REP= OPTION                                                
*                                                                               
OPT16    MVI   ERRCD,REPERR                                                     
*                                                                               
         XC    FLEN,FLEN                                                        
         LA    R4,4(R4)            POINT BEYOND REP=                            
         ST    R4,FADDR                                                         
         GOTO1 FLDVAL              EDIT FOR REP NUMBER                          
         LTR   R5,R5                                                            
         BNP   OPTERR                                                           
         CHI   R5,3                                                             
         BH    OPTERR                                                           
         MVC   FULL(3),0(R4)       MOVE FIELD                                   
         OC    FULL,SPACES                                                      
*                                                                               
         LHI   RE,VRCPACK-BUYSAVE                                               
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'P',FULL),SVOPTREP                                   
         BNZ   OPTERR                                                           
         OC    SVOPTREP,SVOPTREP                                                
         BZ    OPTERR                                                           
* NEED IT IN 3-BYTE FORMAT TOO                                                  
         GOTO1 (RF),(R1),(C'U',SVOPTREP),FULL                                   
*                                                                               
         OC    SVESTREP,SVESTREP   IS THERE AN ESTREP?                          
         BZ    *+14                 NO                                          
         CLC   SVESTREP,SVOPTREP    YES - MUST BE SAME                          
         BNE   OPTERR                                                           
* CHECK REP ON FILE                                                             
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(3),FULL                                                    
         MVC   KEY+5(2),AGYALPHA                                                
         L     R0,AREC                                                          
         L     RE,AREC2                                                         
         ST    RE,AREC                                                          
         MVC   DUB(1),DMOUTBTS     SAVE DMOUTBTS                                
         NI    DMOUTBTS,X'EF'      NF IS OK (SO I CAN RETURN ERROR)             
         GOTO1 RDSTA                                                            
         MVC   DMOUTBTS,DUB        RESTORE DMOUTBTS                             
         ST    R0,AREC                                                          
         L     RF,AREC2                                                         
         CLC   KEY(7),0(RF)        DID WE FIND IT?                              
         BNE   OPTERR               NO                                          
         B     OPT6                                                             
         EJECT                                                                  
*                                                                               
* EDIT OVERRIDE ID, ID= OPTION                                                  
*                                                                               
OPT20    LHI   RF,SVB0PROF-BUYSAVE                                              
         AR    RF,RA                                                            
         CLI   9(RF),C'O'          TEST OPTIONAL PURPOSE CODES                  
         JE    OPT20A              NOT NECESSARILY REQUIRED                     
         CLI   9(RF),C'Y'          TEST USING PURPOSE CODES                     
         BNE   OPT20A                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(CANTDOID)                                              
         B     OPTERR                                                           
*                                                                               
OPT20A   LA    R4,3(R4)                                                         
         AHI   R5,-4               SET FOR EX                                   
         MVI   ERRCD,INVERR                                                     
         BM    OPTERR                                                           
         CHI   R5,11               TEST TO MAX ID LEN(12)                       
         BH    OPTERR                                                           
         MVC   SVID,SPACES                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SVID(0),0(R4) *EXECUTED*                                         
* TEST ID=MKTGRP                                                                
         CLI   SVCXTRA+2,C'Y'                                                   
         BE    OPT6                                                             
         CLI   SVCXTRA+2,C'A'                                                   
         BL    OPT6                                                             
         CLI   SVCXTRA+2,C'Z'                                                   
         BH    OPT6                                                             
         MVI   ERRCD,BADMGRP                                                    
         CLC   SVID(1),SVCXTRA+2   SCHEME SHOULD AGREE                          
         BNE   OPTERR                                                           
         CHI   R5,4                MKTGRP SHOULD BE J9999                       
         BNE   OPTERR                                                           
         SPACE 1                                                                
* VALIDATE MKTGRP *                                                             
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),SVKEY      A-M                                          
         CLI   SVCXTRA+2,C'F'                                                   
         BH    *+10                                                             
         MVC   KEY+3(2),SVKEY+1    CLT SCHEMES NEED CLT                         
         MVC   KEY+8(1),SVCXTRA+2                                               
         PACK  DUB,SVID+1(5)                                                    
         MVC   KEY+9(2),DUB+5                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   OPTERR                                                           
*                                                                               
         BRAS  RE,GETSTEQ          GET THE STATION EQUIVALENCE RECORD           
         BNE   OPT28               DOESN'T EXIST                                
*                                                                               
         MVI   ELCDLO,3            LOOK FOR MKTGRP EQUIVALENCE ELEMENTS         
         MVI   ELCDHI,3                                                         
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL2                                                       
         B     *+8                                                              
*                                                                               
OPT22    BRAS  RE,NEXTEL           LOOK AT NEXT ELEMENT                         
         BNE   OPT28                                                            
*                                                                               
         USING STEEL03,R6                                                       
         CLC   STEMGID,SVCXTRA+2   TEST MKTGRP ID                               
         BNE   OPT22                                                            
         PACK  WORK(3),SVID+1(5)                                                
         CLC   STEMGRP,WORK        TEST MKTGRP NO                               
         BNE   OPT22                                                            
         MVC   SVKEY+4(2),STEMGMKT   MATCH - CHANGE BUY KEY                     
         XC    SVKEY+10(8),SVKEY+10   AND FORCE NEW RECALL                      
*                                                                               
         SR    R1,R1                 GET THE MARKET RECORD                      
         ICM   R1,3,STEMGMKT                                                    
         CVD   R1,DUB                                                           
         UNPK  WORK(4),DUB+5(3)                                                 
         OI    WORK+3,X'F0'                                                     
         BRAS  RE,GETMKT                                                        
*                                                                               
         L     RE,AREC                                                          
         USING MKTRECD,RE                                                       
*                                                                               
         MVC   BUYSTXP+6(4),WORK           FORMAT MKT TO SCREEN                 
         MVC   BUYSTXP+11(10),MKTNAME                                           
         MVC   SVSWPCLS,MKTCLASS                                                
         MVC   SVRTGSVC,MKTRSVC    SAVE RATING SERVICE                          
         MVC   SVMKTMKT,MKTALST    SAVE ALPHA MARKET CODE                       
         MVC   SVBKTYPE,MKTBKTYP   SET MKTGRP EQUIV MARKET BOOKTYPE             
         B     OPT29                                                            
         DROP  RE                                                               
* MGREQ NOT FOUND                                                               
OPT28    TM    SVCOPT1,COP1MGRQ    TEST MGREQ RECORD REQUIRED                   
         BZ    OPT29                                                            
         MVI   ERRCD,NOMGRREC                                                   
         B     OPTERR                                                           
*                                                                               
OPT29    B     OPT6                                                             
         EJECT                                                                  
*===========================================================                    
* EDIT OVERRIDE CANADIAN NETWORK, NET= OPTION                                   
*===========================================================                    
         SPACE 1                                                                
OPT30    LA    R4,4(R4)                                                         
         AHI   R5,-4                                                            
         MVI   ERRCD,INVERR                                                     
         CLI   BUYMD,C'N'          TEST MEDIA=N                                 
         BNE   OPTERR                                                           
*===============================================================                
* 4/5/01 THIS IS JUST TOO ANNOYING TO KEEP ERASING EVERY  TIME                  
*        SO JUST IGNORE IT WHEN WE'RE AT THE NETWORK LEVEL                      
*===============================================================                
         OC    SVNDEF,SVNDEF       TEST NTWK LEVEL                              
         BNZ   OPT6  <<<<          JUST IGNORE                                  
         BNZ   OPTERR                                                           
         CHI   R5,3                                                             
         BL    OPTERR                                                           
         CHI   R5,4                                                             
         BH    OPTERR                                                           
         MVC   WORK,SPACES                                                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4) *EXECUTED*                                         
         MVI   ERRCD,NONETFND                                                   
         ZIC   R0,SVNETBTS         SAVE                                         
         BRAS  RE,GETBITS                                                       
         BNE   OPTERR                                                           
         CLI   SVNETBTS,1          NOT VALID FOR SPECIALTY CABLE SUFX           
         BE    OPT30A              (E.G. DSCY/MO)                               
         MVC   SVKEY+8(1),SVNETBTS                                              
         STC   R0,SVNETBTS                                                      
         B     OPT6                                                             
OPT30A   STC   R0,SVNETBTS         PRESERVE ORIG SVNETBTS                       
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NONETCBL)                                              
         B     OPTERR                                                           
         EJECT                                                                  
*                                                                               
* EDIT OVERRIDE UPGRADE PARAMETERS, UPT= OR UPP= OPTION                         
*                                                                               
OPT40    MVI   FSTOPS+1,C'='                                                    
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL              RESCAN FOR UPEDT COMPATABILITY               
*                                                                               
* UPEDT INCORRECTLY SETS FADDR CAUSING ISSUES VALIDATING OPTIONS                
* SUCCEEDING THE UPGRADE                                                        
*                                                                               
         MVC   FULL,FADDR          SO LETS SAVE FADDR IN FULL                   
*                                    AND RESTORE IT BACK LATER                  
         MVI   EDTVAL,UPEDT                                                     
         GOTO1 CALLEDT                                                          
         L     RE,ADBLOCK                                                       
         USING SPDEMUPD,RE                                                      
         LAY   RF,BUUPGD                                                        
         MVC   SVUPIN,0(RF)        SAVE EBCDIC INPUT                            
         MVC   SVUPOPTS,SPUPTYPE   SAVE UPGRADE DEFAULT VALUES                  
         MVC   SVOPTBK,SPUPFBK                                                  
         MVC   SVUPFIL,SPUPFIL                                                  
         CLI   SPUPBTYP,0          HAVE BOOKTYPE IN UPGRADE?                    
         BE    OPT40A              NO                                           
         MVI   ERRCD,BADKEYWD                                                   
         CLI   SVOPTBT,0           HAVE BT= IN OPTIONS?                         
         BNE   OPTERR               YES, CAN'T HAVE BOTH                        
         MVC   SVOPTBT,SPUPBTYP    SAVE IT SVOPTBT, SAVED BTWN XACTIONS         
OPT40A   MVI   ERRCD,BADKEYWD                                                   
         OC    SPUPUDAY,SPUPUDAY   TEST DAY-TIME SPECIFIED                      
         BNZ   OPTERR                                                           
         MVI   ADBLOCK,0           RESET FLAG                                   
*                                                                               
         MVC   FADDR,FULL          RESTORE FADDR BACK TO START OF FIELD         
*                                  AND CACULATE NEW FLEN USING THE              
         LAY   RF,SVUPTXT          PROJECTION SAVED IN SVUPTXT                  
         LA    RE,L'SVUPTXT-1(RF)                                               
OPT40B   CR    RE,RF               BEYOND START OF SVUPTXT?                     
         JNH   *+2                  YES DIE, WENT PAST START OF STRING          
         CLI   0(RE),C','          SOMETIMES PROJECTION STILL HAS COMMA         
         BE    *+12                 ON THE END, SO SKIP COMMA                   
         CLI   0(RE),C' '          SKIP ANYTHING <= SPACES                      
         BH    OPT40C                                                           
         BCT   RE,OPT40B                                                        
OPT40C   SR    RE,RF               LESS START OF FIRST FIELD                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,FLEN           SET NEW FLEN                                 
         B     OPT6                                                             
         DROP  RE                                                               
*                                                                               
OPT41    OI    SVOPT2,SVOPT2_NOUPGD   FOR MGE= TO SUPPRESS UPGRADE              
         B     OPT6                                                             
         EJECT                                                                  
*                                                                               
* EDIT OVERRIDE LINE OR EST-LIN FOR CANAD NTWK PCTGS *                          
*                                                                               
OPT50    MVI   ERRCD,INVERR                                                     
         OC    SVNDEF,SVNDEF                                                    
         BZ    OPTERR                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       MOVE THRU EST                                
*                                                                               
         LA    R4,4(R4)            POINT TO DATA                                
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN           SET FOR EDIT                                 
         MVI   FSTOPS+1,C'-'                                                    
         GOTO1 FLDVAL                                                           
         MVI   FSTOPS+1,0          RESET STOP CHAR                              
*                                                                               
         CLI   FSTOP,C'-'          TEST EST ENTERED                             
         BNE   OPT52               NO                                           
* EDIT ESTIMATE *                                                               
         MVI   ERRCD,ESTERR                                                     
         LTR   R5,R5                                                            
         BZ    OPTERR                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    OPTERR                                                           
         CVB   R0,DUB                                                           
         CHI   R0,255                                                           
         BH    OPTERR                                                           
         STC   R0,KEY+9                                                         
*                                                                               
         GOTO1 FLDVAL              READ FOR LINE NUMBER                         
*                                                                               
OPT52    MVI   ERRCD,BADLINE                                                    
         LTR   R5,R5                                                            
         BZ    OPTERR                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    OPTERR                                                           
         CVB   R0,DUB                                                           
         LHI   RE,MAXBUYS                                                       
         CLI   SV1OR2,2                                                         
         BE    *+8                                                              
         LHI   RE,255                                                           
         CR    R0,RE                                                            
         BH    OPTERR                                                           
         STCM  R0,3,KEY+11                                                      
*                                                                               
         MVI   ERRCD,NOTFOUND                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OPTERR                                                           
*                                                                               
         MVC   AREC,AREC2          SET IO ADDRESS                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ERRCD,RGNNOTEQ                                                   
         L     RE,AREC2                                                         
         CLC   SVNRGN,BDNRGN-BUYREC(RE)  TEST SAME REGION                       
         BNE   OPTERR                                                           
*                                                                               
         MVC   SVNOVEST,KEY+9      SAVE OVERRIDE EST/LIN                        
         MVC   SVNOVLIN,KEY+11                                                  
         EJECT                                                                  
*================================================================               
* NEW OVERRIDE IS PRESENT - RE-READ NETDEF REC AND BUILD AREA                   
*        SPACE 1                                                                
*           0  1  2  3  4   5   6   7   8   9   10  11 12 13-15                 
*                                                                               
* SVNDEF   00  W  A  B  C  PCT PCT PCT PCT OFS OFS TAX TAX 000                  
*                                                                               
* NETDEF   01 10  W  A  B   C  PCT PCT PCT PCT OFS OFS                          
*================================================================               
         SPACE 1                                                                
OPT64    XC    KEY,KEY                                                          
         MVC   KEY+14(4),SVNDA                                                  
         L     R8,AREC1                                                         
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   SVNOVSET,0          SET SWITCH TO REBUILD SVNDEF                 
         BRAS  RE,CLRNDEF          CLEAR SVNDEF AREA                            
         SPACE 1                                                                
* MOVE X'68' ELEM DATA TO SVNDEF *                                              
         SPACE 1                                                                
         LA    R7,SVNDEF                                                        
         USING SVNDEFD,R7                                                       
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         L     R6,AREC2                                                         
         LA    R6,24(R6)                                                        
*                                                                               
OPT66    BRAS  RE,NEXTEL                                                        
         BNE   OPT68                                                            
         MVC   SVNDMKST(9),2(R6)      MOVE MKT-STA/PCTG                         
         LA    R7,L'SVNDEF(R7)                                                  
         B     OPT66                                                            
         SPACE 1                                                                
* UNPACK STATIONS IN SVNDEF LIST SO CAN MATCH TO NETDEF REC DATA *              
         SPACE 1                                                                
OPT68    LA    R7,SVNDEF                                                        
         LA    R8,SVNDDEM                                                       
         USING SVNDDEMD,R8                                                      
*                                                                               
OPT70    GOTO1 STAPACK,DMCB,(C'U',SVNDMKT),WORK,WORK+4                          
         MVI   0(R7),0             CLEAR SO LOOKS LIKE FROM NETDEF              
         MVC   1(4,R7),WORK+4      STATION CALL LETTERS                         
         LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   OPT70                                                            
                                                                                
         LA    R7,SVNDEF           EXTRACT OFFSETS FROM NETDEF                  
*                                                                               
OPT72    L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'01'                                                     
         MVI   ELCDHI,X'01'                                                     
         BRAS  RE,NEXTEL2                                                       
         BNE   OPT74ERR                                                         
*                                                                               
OPT74    CLC   1(4,R7),2(R6)       MATCH EBCDIC CALL LETTERS                    
         BE    OPT76                                                            
         BRAS  RE,NEXTEL                                                        
         BE    OPT74                                                            
*                                                                               
OPT74ERR MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(MSSNGSTA)                                              
         MVC   ERRTEXT(4),1(R7)                                                 
         GOTO1 ERROR                                                            
*                                                                               
         USING NDEFEL01,R6                                                      
OPT76    MVC   SVNDOFF,NDEFOSET    MOVE OFFSET                                  
         AHI   R7,L'SVNDEF                                                      
         MVC   SVNDDMKT,NDEFAMKT   SAVE DEMO LOOKUP MKT                         
         AHI   R8,L'SVNDDEM                                                     
         DROP  R6                                                               
*                                                                               
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   OPT72                                                            
*                                                                               
         MVI   SVNOVSET,1          ASK SETNDEF FOR DEMO STUFF                   
         BRAS  RE,SETNDEF                                                       
         SPACE 1                                                                
* RESET NUMBER OF STATIONS IN DISPLAY AREA *                                    
         SPACE 1                                                                
         LA    R7,SVNDEF                                                        
         ZAP   HALF,=P'0'                                                       
*                                                                               
OPT78    OC    SVNDMKST,SVNDMKST                                                
         BZ    OPT78X                                                           
         AP    HALF,=P'1'                                                       
         AHI   R7,L'SVNDEF                                                      
         B     OPT78                                                            
         DROP  R7                                                               
*                                                                               
OPT78X   LA    R4,BUYSTXP+L'BUYSTXP                                             
         LA    R0,L'BUYSTXP                                                     
*                                                                               
OPT79    CLC   0(2,R4),=C'N='                                                   
         BE    OPT79X                                                           
         BCTR  R4,0                                                             
         BCT   R0,OPT79                                                         
         DC    H'0'                                                             
*                                                                               
OPT79X   OI    HALF+1,X'0F'                                                     
         UNPK  2(2,R4),HALF                                                     
         B     OPT6                                                             
*                                                                               
* EDIT OVERRIDE BOOKTYPE, BT= OPTION                                            
*                                                                               
OPT80    MVI   ERRCD,BADKEYWD                                                   
         CLI   SVOPTBT,0                                                        
         BNE   OPTERR                                                           
*                                                                               
         LA    R1,3(R4)              POINT TO BOOKTYPE VALUE                    
         LHI   RF,VEDITBTY-BUYSAVE   GET DSPL TO EDIT ROUTINE                   
         AR    RF,RA                 POINT TO ROUTINE ADDRESS                   
         L     RF,0(RF)                                                         
         GOTO1 (RF)                                                             
*                                                                               
         MVC   SVOPTBT,BUBKTYPE    MOVE RESULT                                  
         MVI   BUBKTYPE,0          AND CLEAR INPUT FIELD!                       
         B     OPT6                                                             
*                                                                               
OPT82    MVI   ERRCD,INVF94                                                     
         CLI   4(R4),C'A'                                                       
         BNE   *+12                                                             
         OI    SVARBF94,QF94ARB                                                 
         B     OPT6                                                             
         CLI   4(R4),C'N'                                                       
         BNE   OPTERR                                                           
         OI    SVARBF94,QF94NSI                                                 
         B     OPT6                                                             
*                                                                               
* EDIT OVERRIDE CTA, CTA= OPTION                                                
*                                                                               
OPT83    MVI   BYTE,X'20'          SET CTA FLAG                                 
         B     *+8                                                              
*                                                                               
* EDIT OVERRIDE TRADE, TRD= OPTION                                              
*                                                                               
OPT83A   MVI   BYTE,X'02'          SET TRD FLAG                                 
         MVI   ERRCD,INVERR                                                     
         CLI   4(R4),C'Y'                                                       
         BNE   *+14                                                             
         OC    SVAFLAG1,BYTE       SET CTA ON                                   
         B     OPT6                                                             
         CLI   4(R4),C'N'                                                       
         BNE   OPTERR                                                           
         XI    BYTE,X'FF'                                                       
         NC    SVAFLAG1,BYTE       UNSET FLAG                                   
         B     OPT6                                                             
*                                                                               
* EDIT OVERRIDE CTA BACK/BACK. BB= OPTION                                       
*                                                                               
OPT84    MVI   ERRCD,INVERR                                                     
         TM    SVAFLAG1,X'20'      TEST CTA ON                                  
         BZ    OPTERR              NO - CAN'T SPECIFY BB                        
         CLI   3(R4),C'Y'                                                       
         BNE   *+12                                                             
         OI    SVCTAFLG,SVCTA_BBYES                                             
         B     OPT6                                                             
         CLI   3(R4),C'N'                                                       
         BNE   OPTERR                                                           
         OI    SVCTAFLG,SVCTA_BBNO                                              
         B     OPT6                                                             
         SPACE 1                                                                
*=========================================================                      
* CHECK FOR IDR=<ID> OR PUR=<PURPOSE CODE>                                      
*=========================================================                      
         SPACE 1                                                                
OPT85    LHI   RF,SVB0PROF-BUYSAVE                                              
         AR    RF,RA                                                            
         CLI   9(RF),C'O'          TEST OPTIONAL PURPOSE CODES                  
         JE    *+12                                                             
         CLI   9(RF),C'Y'          TEST PURPOSE CODE OPTION                     
         BNE   OPT85E5                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,4(R4)            POINT TO PURPOSE CODE                        
         AHI   R5,-4               ADJUST INPUT LENGTH                          
         BZ    OPT85E3                                                          
         CHI   R5,6                NO MORE THAN 6 CHARS                         
         BH    OPT85E3                                                          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
         OC    WORK,SPACES                                                      
         MVC   SVID,WORK                                                        
*                                                                               
K        USING PRPRECD,KEY         READ PURPOSE CODE RECORD                     
*                                                                               
         XC    KEY,KEY                                                          
         MVI   K.PRPKTYP,PRPKTYPQ                                               
         MVI   K.PRPKSUB,PRPKSUBQ                                               
         MVC   K.PRPKAGY,AGYALPHA                                               
         MVC   K.PRPKMED,BUYMD                                                  
         MVC   K.PRPCODE,SVID                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OPT85E4                                                          
         B     OPT6                                                             
         DROP  K                                                                
         SPACE 1                                                                
*=========================================================                      
* IDR= <4 TO 6 CHARS> OLD MILLER FEATURE                                        
*=========================================================                      
         SPACE 1                                                                
OPT85A   CLI   SVCXTRA+2,C'A'      MUST USE MKTGRP SCHEME                       
         BL    OPT85E1                                                          
         CLI   SVCXTRA+2,C'Z'                                                   
         BH    OPT85E1                                                          
         CLI   SVCXTRA+2,C'Y'                                                   
         BE    OPT85E1                                                          
         CLI   FLEN+1,4+4                                                       
         BL    OPT85E2                                                          
         CLI   FLEN+1,4+6                                                       
         BH    OPT85E2                                                          
         MVC   WORK(6),SPACES                                                   
         ZIC   RE,FLEN+1                                                        
         AHI   RE,-5                                                            
         EX    RE,OPT85MVC                                                      
         MVI   SVID+5,C'-'                                                      
         MVC   SVID+6(6),WORK                                                   
         B     OPT6                                                             
*                                                                               
OPT85E1  LA    R0,NOMGRID                                                       
         B     *+8                                                              
OPT85E2  LA    R0,BADIDR                                                        
         B     *+8                                                              
OPT85E3  LA    R0,BADPURP                                                       
         B     *+8                                                              
OPT85E4  LA    R0,PURCODNF                                                      
         B     *+8                                                              
OPT85E5  LA    R0,NOPUROPT                                                      
         STCM  R0,3,NERRCD                                                      
         MVI   ERRCD,NEWERRS                                                    
OPT85EX  GOTO1 ERROR                                                            
*                                                                               
OPT85MVC MVC   WORK(0),4(R4)                                                    
*                                                                               
* EDIT OVERRIDE CABLE NETWORK, CNET= OPTION                                     
*                                                                               
OPT86    L     RF,VCOMFACS         CNET=XX FOR CABLE NETWORK                    
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,5(R4),BYTE,2                                           
         MVI   ERRCD,INVERR                                                     
         OC    12(4,R1),12(R1)                                                  
         BZ    OPT85EX                                                          
         NI    SVKEY+8,X'80'       DROP OLD NETWORK                             
         OC    SVKEY+8(1),BYTE                                                  
         B     OPT6                                                             
*                                                                               
*                                                                               
OPTX     DS    0H                                                               
         CLI   BUYMD,C'N'          TEST NETWORK                                 
         BNE   OPTX2                                                            
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   OPTX2                                                            
         OC    SVNDEF,SVNDEF       TEST NTWK LEVEL                              
         BNZ   OPTX2               YES - EXPL PROBLEM ONLY                      
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(WHATNET)                                               
         CLI   SVKEY+8,3           MAKE SURE THERE'S A VALID NETWORK            
         BNH   OPTERR                                                           
*                                                                               
OPTX2    TM    SVOPT2,SVOPT2_MKTOV   OVERRIDE MARKET ENTERED                    
         BO    OPTX18                                                           
         CLI   SVCXTRA+2,C'*'        TEST CCUSA INTERFACE ACTIVE                
         BNE   OPTX18                                                           
*                                                                               
         BRAS  RE,GETACN                                                        
*                                                                               
         BRAS  RE,GETSTEQ          GET THE STATION EQUIVALENCE RECORD           
         BNE   OPTX18              DOESN'T EXIST                                
*                                                                               
         MVI   ELCDLO,2            LOOK FOR ACN NO EQUIVALENCE ELEMENTS         
         MVI   ELCDHI,2                                                         
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL2                                                       
         B     *+8                                                              
OPTX10   BRAS  RE,NEXTEL           LOOK AT NEXT ELEMENT                         
         BNE   OPTX18                                                           
         USING STEEL02,R6                                                       
         CLC   STEACN,SVID         TEST ACN NUMBER                              
         BNE   OPTX10                                                           
         SR    R1,R1               MATCH - GET THE MARKET RECORD                
         ICM   R1,3,STEACNMK                                                    
         CVD   R1,DUB                                                           
         UNPK  WORK(4),DUB+5(3)                                                 
         OI    WORK+3,X'F0'                                                     
         MVC   SVKEY+4(2),STEACNMK         CHANGE BUY KEY                       
         XC    SVKEY+10(8),SVKEY+10        FORCE NEW RECALL                     
*                                                                               
         BRAS  RE,GETMKT                                                        
*                                                                               
         L     RE,AREC                                                          
         USING MKTRECD,RE                                                       
*                                                                               
         MVC   BUYSTXP+6(4),WORK           FORMAT MARKET TO SCREEN              
         MVC   BUYSTXP+11(10),MKTNAME                                           
         MVC   SVSWPCLS,MKTCLASS                                                
         MVC   SVRTGSVC,MKTRSVC    SAVE RATING SERVICE                          
         MVC   SVMKTMKT,MKTALST    SAVE ALPHA MARKET CODE                       
         MVC   SVBKTYPE,MKTBKTYP                                                
         DROP  RE                                                               
*                                                                               
OPTX18   DS    0H                  GO BUILD WEEKLY GOALS BY DPT                 
         BRAS  RE,BLDGLS                                                        
* BUILD DARE LOCKOUT LIST                                                       
         BRAS  RE,BLDDARE                                                       
*                                                                               
         MVI   SVANYLOK,C'N'                                                    
         LHI   R4,SVB0PROF-BUYSAVE POINT TO B0 PROFILE                          
         AR    R4,RA                                                            
         CLI   7(R4),C'1'          TEST REASON CODE SCHEME ACTIVE               
         BNE   OPTX20                                                           
* CHECK TO SEE IF ANY LOCKED BUYS EXIST                                         
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         CLI   SVPOLPRD,0          USE BRAND IF ENTERED                         
         BE    *+10                                                             
         MVC   KEY+3(1),SVPOLPRD                                                
         OI    KEY,X'08'           SET FOR 'ORIGINAL' DATA                      
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   *+8                                                              
         MVI   SVANYLOK,C'Y'                                                    
*                                                                               
OPTX20   OI    4(R2),X'20'                                                      
*                                                                               
         BRAS  RE,PUTGLOB                                                       
         J     EXIT                                                             
*                                                                               
OPTERR   GOTO1 ERROR                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* INTERFACE TO SPGETBUBL                                                        
*============================================================                   
         SPACE 1                                                                
GOGETBU  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   R4,SVMKPROF-BUYSAVE                                              
         AR    R4,RA                                                            
         CLI   4(R4),C'Y'          TEST TO VALIDATE BUYER NAME                  
         BNE   GETBU2                                                           
*                                                                               
         CLI   BUYBUH+5,0          UNLESS NOTHING IS INPUT                      
         BE    GETBU2                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DE4'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         NI    KEY+2,X'F0'         DROP MEDIA                                   
*                                                                               
         LA    RE,BUYBU                                                         
         LHI   RF,9                SET FOR EX MOVE OF 10 CHAR                   
         CLI   0(RE),C'='                                                       
         BNE   *+10                                                             
         LA    RE,1(RE)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(RE)                                                   
*                                                                               
         OC    KEY+3(10),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETBUER3                                                         
*                                                                               
GETBU2   XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING GETBUBLD,R4                                                      
*                                                                               
         MVC   GBCOMFAC,VCOMFACS                                                
         MVC   GBIOA,AREC1                                                      
         LA    R2,BUYBUH                                                        
         ST    R2,GBNAMFLD                                                      
         MVC   GBAGY,AGYALPHA                                                   
         MVC   GBMEDEBC,BUYMD                                                   
         MVC   GBCLTEBC,QCLT                                                    
         MVC   GBOFFICE,SVOFFC                                                  
         MVC   GBAGYMD,SVKEY                                                    
         MVC   GBCLT,SVKEY+1                                                    
         MVC   GBPRD,SVKEY+3                                                    
         MVC   GBEST,SVKEY+9                                                    
         MVC   GBMKT(5),SVKEY+4                                                 
         MVI   GBTYPE,C'B'         SET FOR BUYER NAME                           
* GET ADDRESS OF CORE RESIDENT GETBUBL *                                        
         GOTO1 VCALLOV,DMCB,0,X'D9000A77'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R4)                                                   
         CLI   GBERR,0                                                          
         BNE   GETBUERR                                                         
*                                                                               
GETBUX   XIT1                                                                   
*                                                                               
GETBUERR MVI   ERRCD,1                                                          
         CLI   GBERR,X'81'                                                      
         BNE   GETBUER2                                                         
         GOTO1 ERROR                                                            
*                                                                               
GETBUER2 DC    H'0'                DIE FOR NOW ON DMGR ERROR                    
*                                                                               
GETBUER3 MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOBUYER)                                               
         MVI   ERRAREA,X'FE'       SET TO UNWIND NAME ADD/CHG                   
         GOTO1 ERROR                                                            
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* DISTRIBUTE PERCENTAGES OVER STATIONS IN CANAD NTWK RGN                        
*====================================================================           
         SPACE 1                                                                
SETNDEF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVNOVSET,1          TEST CALLED BY COPYNET FUNCTION              
         BE    SETND8                                                           
*                                                                               
         CLI   SVNOVSET,C'Y'       TEST PREVIOUSLY INITIALIZED                  
         BNE   SETND0              NO                                           
*                                                                               
         OC    SVNOVEST(2),SVNOVEST  TEST OVERRIDE THIS TIME                    
         BZ    SETNDX                NO - USE SVOLDNET                          
*                                                                               
SETND0   LA    R7,SVNDEF                                                        
         USING SVNDEFD,R7                                                       
         SR    R8,R8                                                            
*                                                                               
SETND2   MVC   FULL,SVNDPCT                                                     
         A     R8,FULL                                                          
         LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   SETND2                                                           
*                                                                               
         ST    R8,SVNPCTG          SAVE RGN PCTG                                
         C     R8,=F'100000'                                                    
         BE    SETND8                                                           
         MVI   ERRCD,NOSTATNS                                                   
         LTR   R8,R8                                                            
         BZ    SETNDERR                                                         
*                                                                               
         LA    R7,SVNDEF                                                        
*                                                                               
SETND4   MVC   FULL,SVNDPCT                                                     
         L     R1,FULL                                                          
         M     R0,=F'200000'                                                    
         DR    R0,R8                                                            
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,FULL                                                          
         MVC   SVNDPCT,FULL                                                     
         LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   SETND4                                                           
         SPACE 1                                                                
* CHECK REDIST SUMS TO 100 *                                                    
         SPACE 1                                                                
         LA    R7,SVNDEF                                                        
         SR    R8,R8                                                            
         LR    RE,R7               SAVE A(LARGEST)                              
         SPACE 1                                                                
SETND6   MVC   FULL,SVNDPCT                                                     
         A     R8,FULL                                                          
         CLC   SVNDPCT-SVNDEFD(4,RE),FULL                                       
         BH    *+6                                                              
         LR    RE,R7                                                            
         LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   SETND6                                                           
         S     R8,=F'100000'                                                    
         ICM   R0,15,SVNDPCT-SVNDEFD(RE)  ADJUST LARGEST PCTG                   
         SR    R0,R8                                                            
         STCM  R0,15,SVNDPCT-SVNDEFD(RE)                                        
         SPACE 1                                                                
*================================================================               
* NOW READ STARECS FOR MARKET NUMBERS AND TAX RATES                             
*================================================================               
         SPACE 1                                                                
SETND8   LA    R7,SVNDEF                                                        
         LA    R8,SVNDDEM                                                       
         USING SVNDDEMD,R8                                                      
         MVI   SVNDINDX,1          RESET INDEX VALUE                            
*                                                                               
SETND10  CLI   SVNETBTS,X'01'      TEST CBLNET                                  
         BNE   SETND12                                                          
         MVC   DUB(4),QSTA                                                      
         MVC   DUB+4(1),QSTA+4                                                  
         MVC   DUB+5(2),1(R7)      MOVE MKT SUFFIX                              
         ICM   R0,3,3(R7)          SAVE MARKET NUMBER                           
         GOTO1 STAPACK,DMCB,(C'P',=C'0000'),DUB,SVNDMKST                        
         STCM  R0,3,SVNDMKST       MOVE MARKET NUMBER                           
         MVI   SVNDGST,C'S'        SET FOR STANDARD GST                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         B     SETND13B            GO READ MARKET                               
*                                                                               
SETND12  MVC   DUB(4),1(R7)                                                     
         MVC   DUB+4(1),QSTA+4     SET SUB-MEDIA FROM QSTA                      
         BRAS  RE,GETSTMAS                                                      
         BE    SETND13                                                          
         MVC   ERRTEXT(4),1(R7)                                                 
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOMASREC)                                            
         B     SETNDERR                                                         
*                                                                               
SETND13  MVC   DUB(4),1(R7)        STATION                                      
         MVC   DUB+4(1),QSTA+4                                                  
         GOTO1 STAPACK,DMCB,(C'P',SMKT),DUB,SVNDMKST                            
         MVC   4(1,R7),SVNETBTS                                                 
* NEWFOUNDLAND HAS NO NETWORK TAX                                               
         LA    RE,NFLST                                                         
         LA    R0,NFLSTX-NFLST                                                  
         SRL   R0,2                                                             
         CLC   0(4,RE),DUB                                                      
         BE    SETND13A                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         MVC   SVNDTAX,SNEWTAX     SAVE TAX RATE                                
         MVC   SVNDGST,SVGSTCOD    USE NETWORK GST CODE                         
*                                                                               
SETND13A MVC   WORK(4),SMKT        MOVE MARKET NUMBER                           
*                                                                               
SETND13B BRAS  RE,GETMKT           AND READ MARKET RECORD                       
*                                                                               
         L     RE,AREC                                                          
         USING MKTRECD,RE                                                       
*                                                                               
         CLI   SVNETBTS,X'01'      TEST CBLDEF                                  
         BE    *+10                YES - ALPHA MKT FROM CBLDEF ONLY!            
         MVC   SVNDDMKT,MKTALST    SAVE ALPHA MARKET FOR DEMO LKUPS             
*                                                                               
         MVC   WORK(4),SRS1CALL                                                 
         MVC   WORK+4(4),SRS2CALL                                               
         MVC   WORK+8(1),SFLAG1                                                 
         MVC   WORK+9(1),SFLAG1                                                 
         CLI   SVNETBTS,1          TEST CBLNET                                  
         BNE   SETND13C                                                         
         MVC   WORK(4),SVDMLST0                                                 
         MVC   WORK+4(4),SVDMLST1                                               
         MVC   WORK+8(1),SVDMLFLG                                               
         MVC   WORK+9(1),SVDMLFLG                                               
*                                                                               
SETND13C MVC   SVNDDST0,WORK       MOVE NSI LOOKUP CALL LETTERS                 
         MVC   SVNDDST1,WORK+4     AND BBM CALL LETTERS                         
*                                                                               
         MVI   SVNDDFLG,2          SET FOR NSI LOOKUP                           
         SR    RF,RF                                                            
         IC    RF,WORK+8           SET FOR STATION 1 IMPS FLAG                  
         CLI   MKTRSVC,C'0'        TEST NSI MARKET                              
         BE    SETND13D                                                         
         MVI   SVNDDFLG,1          SET FOR BBM LOOKUP                           
         IC    RF,WORK+9                                                        
         DROP  RE                                                               
*                                                                               
SETND13D EX    RF,*+8                                                           
         B     *+8                                                              
         TM    SFLAG1,0 ** EXECUTED **                                          
         BZ    *+8                                                              
         OI    SVNDDFLG,X'80'      SET TO SUPPRESS IMPS                         
*                                                                               
SETND14  MVC   SVNDPST,SPST        SAVE STATION PST CODES                       
         LA    R1,SVNDPST                                                       
         LA    R0,10                                                            
*                                                                               
SETND15  CLI   0(R1),C'H'          HST DEFAULTS TO X                            
         BNE   *+8                                                              
         MVI   0(R1),C'X'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,SETND15                                                       
*                                                                               
         LA    R1,SVPST            POINT TO NETWORK PST VALUES                  
         LA    R0,10                                                            
         LA    RE,SVNDPST                                                       
*                                                                               
SETND16  CLI   0(R1),C' '          TEST NETWORK HAS PST CODE                    
         BNH   SETND17             NO - IGNORE                                  
         CLI   0(RE),C' '          TEST STATION HAS A PST CODE                  
         BNH   SETND17             NO - IGNORE                                  
         MVC   0(1,RE),0(R1)       ELSE SET NTWK VALUE FOR STATION              
*                                                                               
SETND17  LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,SETND16                                                       
*                                                                               
SETND18  MVC   SVNDSEQ,SVNDINDX                                                 
         AHI   R7,L'SVNDEF                                                      
         AHI   R8,L'SVNDDEM                                                     
         SR    R0,R0                                                            
         IC    R0,SVNDINDX                                                      
         AHI   R0,1                                                             
         STC   R0,SVNDINDX                                                      
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   SETND10                                                          
*                                                                               
SETNDX   XIT1                                                                   
*                                                                               
SETNDERR DS    0H                                                               
         XC    SVOLDNET,SVOLDNET                                                
         NI    BUYMDH+4,X'FF'-X'20'   FORCE HEADLINE VALIDATION                 
         GOTO1 ERROR                                                            
*                                                                               
NFLST    DC    C'CJON'                                                          
         DC    C'CBNT'                                                          
         DC    C'CBYT'                                                          
NFLSTX   EQU   *                                                                
*                                                                               
         DROP  R7,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* SUBROUTINES TO CLEAR/SAVE NETDEF AREAS                                        
*=============================================================                  
                                                                                
CLRNDEF  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,SVNDEF                                                        
         LHI   R1,SVNDEFX-SVNDEF                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,SVNDDEM          CLEAR DEMO EXT AREA                          
         LHI   R1,SVNDDEMX-SVNDDEM                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
SAVNDEF  NTR1  BASE=*,LABEL=*                                                   
         L     R0,ASVNOLD                                                       
         LHI   R1,SVNDEFX-SVNDEF                                                
         LA    RE,SVNDEF                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY SVNDEF TO SVOLDNET                      
*                                                                               
         LHI   R0,SVNDDOLD-BUYSAVE                                              
         AR    R0,RA                                                            
         LHI   R1,SVNDDEMX-SVNDDEM                                              
         LA    RE,SVNDDEM                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY SVNDDEM TO SVNDDOLD                     
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
RSTRNDEF NTR1  BASE=*,LABEL=*                                                   
         LA    R0,SVNDEF                                                        
         LHI   R1,SVNDEFX-SVNDEF                                                
         L     RE,ASVNOLD                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY SVOLDNET IN CASE NO OVRD                
*                                                                               
         LA    R0,SVNDDEM                                                       
         LHI   R1,SVNDDEMX-SVNDDEM                                              
         LHI   RE,SVNDDOLD-BUYSAVE                                              
         AR    RE,RA                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* A XMAS PRESENT FOR HWON FROM MHER  DECEMBER, 2009                             
* DECODE HEX KEY IN BUYINP1 TO HEADLINES AND RECALL BUY                         
*================================================================               
                                                                                
GETKEY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,READAGY          READ AGYHDR TO REC                           
*                                                                               
         MVC   DUB(1),AGYPAHEX                                                  
         MVI   DUB+1,C'0'                                                       
         L     RF,VCOMFACS                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB,BYTE,2,0   1 BYTE=2 CHARS                          
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R0,BUYINP1H+5       GET INPUT LENGTH                             
         AHI   R0,-4               ADJUST FOR KEY=                              
         CHI   R0,6                MUST HAVE AT LEAST MD/CL                     
         BL    GETKEYE1                                                         
         GOTO1 (RF),DMCB,BUYINP1+4,KEY,(R0),0                                   
         OC    12(4,R1),12(R1)     TEST INPUT VALID HEX                         
         BZ    GETKEYE1                                                         
*                                                                               
         LLC   RE,KEY                                                           
         SRL   RE,4                DROP MEDIA                                   
         LLC   RF,BYTE                                                          
         SRL   RF,4                                                             
         CR    RE,RF                                                            
         BNE   GETKEYE2                                                         
*                                                                               
         LLC   RE,KEY                                                           
         N     RE,=X'0000000F'     DROP AGY                                     
*                                                                               
         CHI   RE,8                TEST CANADIAN COMBINED                       
         BNE   GETKEY2                                                          
         LLC   RE,KEY+10           THEN THE REAL MEDIA IS HERE                  
         N     RE,=X'0000000F'                                                  
*                                                                               
GETKEY2  LA    RE,MDTAB(RE)                                                     
         MVC   BUYMD(1),0(RE)                                                   
         MVI   BUYMDH+5,1          SET INPUT LENGTH                             
         OI    BUYMDH+6,X'80'                                                   
*                                                                               
         MVC   BUYBU(4),=C'TEST'                                                
         MVI   BUYBUH+5,4                                                       
         OI    BUYBUH+6,X'80'                                                   
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A15'  GET CLUNPK ADDRESS                   
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),KEY+1,BUYCL                                            
         MVI   BUYCLH+5,3          SET INPUT LENGTH                             
         CLI   BUYCL+2,C' '                                                     
         BH    *+8                                                              
         MVI   BUYCLH+5,2                                                       
         OI    BUYCLH+6,X'80'                                                   
*                                                                               
         MVC   BUYPR(3),=C'POL'                                                 
         MVI   BUYPRH+5,3                                                       
         OI    BUYPRH+6,X'80'                                                   
*                                                                               
         GOTO1 STAPACK,DMCB,(C'U',KEY+4),WORK,(X'80',BUYST)                     
         LA    RE,BUYST+L'BUYST-1  POINT TO LAST CHAR OF STATION                
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'/'          GET RID OF TRAILING /                        
         BNE   *+10                                                             
         MVI   0(RE),C' '                                                       
         BCTR  RE,0                                                             
*                                                                               
         LA    RF,BUYST            CALCULATE FIELD LEN                          
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BUYSTH+5         SET INPUT LENGTH                             
         OI    BUYSTH+6,X'80'                                                   
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   GETK15                                                           
         CLI   BUYMD,C'N'                                                       
         BNE   GETK15                                                           
         OC    KEY+4(2),KEY+4      TEST MARKET 0                                
         BZ    GETK15                                                           
*                                                                               
* READ THE RECORD TO GET NETWORK CODE FROM 68 ELEM                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETK10              IF CAN'T FIND IT, FORGET IT                  
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,REC+24                                                        
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   GETK10                                                           
*                                                                               
         MVC   BUYOP(4),=C'NET='                                                
         MVC   BUYOP+4(4),2(R6)                                                 
         MVI   BUYOPH+5,8                                                       
         CLI   BUYOP+7,C' '                                                     
         BH    *+8                                                              
         MVI   BUYOPH+5,7                                                       
         OI    BUYOPH+6,X'80'                                                   
*                                                                               
GETK10   MVC   KEY,KEYSAVE                                                      
GETK15   LLC   R0,KEY+9                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUYES(3),DUB                                                     
         MVI   BUYESH+4,X'08'                                                   
         MVI   BUYESH+5,3                                                       
         OI    BUYESH+6,X'80'                                                   
*                                                                               
         MVC   ELEM(70),BUYINP1                                                 
         XC    BUYINP1,BUYINP1                                                  
         MVC   BUYINP1+6(70),ELEM                                               
         MVI   BUYINP1,C'*'                                                     
*                                                                               
         LLC   R0,KEY+11                                                        
         CLI   KEY+3,X'FF'         TEST POL BUY                                 
         BE    *+10                                                             
         LLC   R0,KEY+12                                                        
         CLI   SV1OR2,2                                                         
         BNH   *+8                                                              
         ICM   R0,3,KEY+11                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUYINP1(3),DUB                                                   
         MVI   BUYINP1H+5,3                                                     
         J     EXIT                                                             
*                                                                               
GETKEYE1 MVC   BUYMSG(7),=C'BAD KEY'                                            
         B     *+10                                                             
GETKEYE2 MVC   BUYMSG(9),=C'WRONG AGY'                                          
*                                                                               
         MVI   ERRAREA,X'FF'                                                    
         J     BUYERR                                                           
MDTAB    DC    C' TRNX'                                                         
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
SPBUYWKD DSECT                                                                  
         ORG   REC                                                              
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE SPGENADV                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE SPGENFLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPURP                                                      
         EJECT                                                                  
       ++INCLUDE SPGENDRFLT                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSTEQ                                                      
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENXRT                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         SPACE 2                                                                
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDGLOBEQUS                                                     
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
BUBLD    DSECT                                                                  
       ++INCLUDE SPGETBUBLD                                                     
         EJECT                                                                  
SPAVBLKD DSECT                                                                  
       ++INCLUDE SPACNVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'131SPBUY01   11/03/20'                                      
         END                                                                    
