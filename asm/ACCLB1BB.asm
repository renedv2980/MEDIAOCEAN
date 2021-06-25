*          DATA SET ACCLB1BB   AT LEVEL 197 AS OF 12/23/99                      
*PHASE T6211BB                                                                  
CLB1B    TITLE '- COLUMN LIST'                                                  
CLB1B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL CWRKL,**CLB1B*,R8,RR=RE                                          
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R0,RC                                                            
         L     RC,AOVERWRK                                                      
         USING CWRKD,RC            RC=COLUMN LOCAL W/S                          
         ST    R0,ASAVE            A(LOCAL W/S SAVE AREA)                       
         ST    RE,BORELO                                                        
         L     R5,ALSVALS                                                       
         LH    R6,=Y(BSDICT-TWAD)                                               
         LA    R6,TWAD(R6)                                                      
         USING BSDICT,R6                                                        
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     EXITY               LAST FOR THIS SCREEN                         
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     EXITN               VALIDATE COLUMN                              
         B     EXITY               LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     EXIT                SET UP MY OWN HEADING                        
         B     EXIT                VALIDATE SELECT TABLE                        
         B     EXIT                DISPLAY COLUMN TOTAL                         
         EJECT                                                                  
***********************************************************************         
* FIRST FOR THIS SCREEN                                               *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  DS    0H                                                               
*                                                                               
         TM    BCINDS2,BCINTRS     TEST JUST NTRSES'D                           
         BZ    SFRST10                                                          
         XC    BASOPT,BASOPT                                                    
         OI    BASOPTH+FHOID,FHOITR                                             
         XR    R1,R1               YES - SEE IF LAST ACTION WAS A LIST          
         IC    R1,TWASESNL                                                      
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,TWASESRA(R1)                                                  
         GOTO1 ATSTMIX,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AMIXNTRY                                                      
         USING MIXTABD,R3                                                       
         TM    MIXINDS1,MIXILST                                                 
         BO    *+12                                                             
         CLI   MIXACTB,ACTSUM      SUMMARY IS A SPECIAL CASE                    
         BNE   SFRST10                                                          
         GOTO1 ATSTACT,L'CSREC(R1) TSTACT EXTRACTS ACTION NAME                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   COLACT,BCWORK+ACTNAMLQ LIST COLS FOR PREVIOUS ACTION             
*                                                                               
SFRST10  L     RE,=A(ALOVAL)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS    SET OUTPUT BASE ADDRESS                      
         MVC   SFLTOPS,LSOPS       SAVE FILTERING OPTIONS FOR COMPARE           
         XC    LSOPS,LSOPS         CLEAR ALL OPTIONS                            
         XC    LSCLM,LSCLM         CLEAR DISPLAY COLUMNS                        
*                                                                               
         LA    RF,BOWORK2                                                       
         XR    R0,R0                                                            
         LA    RF,DEFCLMR                                                       
         GOTO1 AVALOPT,BOPARM,ALOTAB,(RF),(R0)                                  
         BNE   SCRFRSTN                                                         
         GOTO1 VALACT                                                           
         BNE   SCRFRSTN                                                         
*                                                                               
         CLC   SFLTOPS,LSOPS                                                    
         BE    EXITY                                                            
         B     EXITH               FILT OPTIONS CHANGED - RESTART               
*                                                                               
SCRFRSTN MVC   LSOPS(L'SFLTOPS),LSOPS   RESTORE OLD FILTERS IF ERROR            
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALACT   NTR1  ,                                                                
         MVI   FVMINL,1            SET FIELD IS REQUIRED                        
         GOTO1 AFVAL,COLACTH                                                    
         BNE   EXITN                                                            
         L     R2,AACTTAB                                                       
         USING ACTTABD,R2          R2=A(ACTION TABLE)                           
*                                                                               
VALACT02 CLI   ACTTABD,EOT         TEST EOT                                     
         BNE   VALACT04                                                         
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         LA    R0,ACTNAMLQ+1                                                    
         STC   R0,FVPARMS                                                       
         MVC   FVPARMS+1(ACTNAMLQ),FVIFLD                                       
         B     EXITN                                                            
*                                                                               
VALACT04 XR    R1,R1                                                            
         ICM   R1,3,ACTNAMEU                                                    
         LA    R1,TWAD(R1)                                                      
         MVC   BOWORK1(ACTNAMLQ),0(R1)                                          
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         BE    VALACT10                                                         
         CLC   BOWORK1(0),FVIFLD   MATCH ON INPUT NAME                          
*                                                                               
VALACT08 LA    R2,ACTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     VALACT02                                                         
*                                  EXTRACT ACTION VALUES INTO W/S               
VALACT10 MVC   COLACT,BOWORK1                                                   
         OI    COLACTH+FHOID,FHOITR                                             
         OI    COLACTH+FHIID,FHIIVA                                             
*                                                                               
         CLI   ACTNUMB,ACTFOP      FORMOPTS IS A SPECIAL CASE                   
         BNE   VALACT11                                                         
         GOTO1 ATSTACT,=AL1(ACTLFT) USE FORMLIST CODES                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     EXITN                                                            
         L     R2,AACTNTRY                                                      
         OI    LSCINDS,LSCIFOP                                                  
*                                                                               
VALACT11 L     R3,AMIXTAB                                                       
         USING MIXTABD,R3                                                       
         XR    R0,R0                                                            
         IC    R0,BCMIXLEN                                                      
VALACT12 CLI   MIXTABD,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MIXACTB,ACTNUMB                                                  
         BE    *+10                                                             
         AR    R3,R0                                                            
         B     VALACT12                                                         
         TM    MIXINDS1,MIXILST                                                 
         BO    VALACT14                                                         
         CLI   MIXACTB,ACTSUM      SUMMARY IS A SPECIAL CASE                    
         BE    VALACT14                                                         
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     EXITN                                                            
VALACT14 MVC   LSACTION,ACTNUMB                                                 
         MVC   LSCLM#,MIXLCLM#                                                  
         MVC   LSOVR#,MIXOVER                                                   
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH   USING                                                           
         USING  CLMTABD,TLCDATA                                                 
DISCLM   DS     0H                                                              
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISCODE     00      CHARACTER CODE                               
         B     DISHED1     01      HEADING                                      
         B     DISHED2     02      HEADING                                      
         B     DISFIX      03      FIXED OR NOT                                 
         B     DISOPEN     04      OPEN OR NOT                                  
         B     DISCTRY     05      COUNTRY                                      
         B     DISSEC      06      SECURITY                                     
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CHARACTER CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCODE  MVC   FVIFLD(L'TLCCODE),TLCCODE                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY HEADING 1                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISHED1  MVC   FVIFLD(L'TLCH1),TLCH1                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY HEADING 2                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISHED2  MVC   FVIFLD(L'TLCH2),TLCH2                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FIXED                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISFIX   MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    CLMINDS1,CLMIKEY                                                 
         BZ    EXIT                                                             
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OPEN                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISOPEN  MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    CLMINDS1,CLMIPRO                                                 
         BO    EXIT                                                             
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COUNTRY                                                     *         
***********************************************************************         
         SPACE 1                                                                
DISCTRY  MVC   FVIFLD(L'LC@ALL),LC@ALL                                          
         CLI   CLMCTRY,0                                                        
         BE    EXIT                                                             
         MVC   FVIFLD,BCSPACES                                                  
         MVC   BOBYTE1,CLMCTRY                                                  
         NI    BOBYTE1,FF-CTRYNOT                                               
         GOTO1 GETCTRY,BOBYTE1                                                  
         MVC   FVIFLD(L'CTRYSHR),CTRYSHR-CTRYTABD(R2)                           
         TM    CLMCTRY,CTRYNOT                                                  
         BZ    EXIT                                                             
         MVI   FVIFLD,C'*'                                                      
         MVC   FVIFLD+1(L'CTRYSHR),CTRYSHR-CTRYTABD(R2)                         
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY SECURITY                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISSEC   TM    CLMINDS1,CLMIDDS    TEST DDS ONLY                                
         BZ    DSEC02                                                           
         MVC   FVIFLD(3),=C'DDS'                                                
         MVC   FVIFLD+4(L'LC@ONLY),LC@ONLY                                      
         B     EXIT                                                             
*                                                                               
DSEC02   MVC   FVIFLD(L'LC@NO),LC@NO                                            
         CLI   CLMSEC,0                                                         
         BE    EXIT                                                             
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         XR    RF,RF                                                            
         ICM   RF,1,CLMSEC                                                      
         MVI   FVIFLD+3,C'('                                                    
         CVD   RF,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  FVIFLD+4(3),BODUB1                                               
         MVI   FVIFLD+7,C')'                                                    
         B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  DS    0H                                                               
         MVC   SBILCUR,CSBILCUR                                                 
         TM    BCCPYST6,CPYSFBIL                                                
         BZ    LFRST10                                                          
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    LFRST02                                                          
         CLC   CSBILCUR,BCSPACES                                                
         BH    LFRST10                                                          
LFRST02  MVC   CSBILCUR,BCEFFS     SET DUMMY BILLING CURRENCY                   
*                                                                               
LFRST10  GOTO1 VCOLY,BOPARM,('O#GENCLM',0),0                                    
         MVC   ASETHEAD,0(R1)      SET A(SET HEAD) FOR GENERAL ROUTINES         
         MVI   ASETHEAD,1                                                       
*                                                                               
         GOTO1 VCOLY,BOPARM,(LSOVR#,0),0                                        
         MVC   OSETHEAD,0(R1)      SET A(SET HEAD) FOR OVERLAY                  
         MVI   OSETHEAD,9                                                       
*                                                                               
         MVC   SLSGENM,LSGENM      SAVE COLUMN VALUES                           
         MVC   SCLMHEAD,ACLMHEAD                                                
         MVC   SCLMDATA,ACLMDATA                                                
         GOTO1 ASETCLM,BOPARM,(LSCLM#,0)                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT LIST RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  XC    LST#,LST#                                                        
*                                                                               
GETNEXT  DS    0H                                                               
         GOTO1 ASETCLM,FF          GET NEXT COLUMN                              
         BNL   GNEXT02                                                          
         MVC   LSGENM,SLSGENM      RESTORE COLUMN VALUES                        
         MVC   ACLMHEAD,SCLMHEAD                                                
         MVC   ACLMDATA,SCLMDATA                                                
         MVC   CSBILCUR,SBILCUR                                                 
         B     EXITN                                                            
*                                                                               
GNEXT02  L     R2,ACLMDATA                                                      
         USING CLMTABD,R2                                                       
         GOTO1 TSTCLM,BOPARM,CLMTABD,LSCLMCOD                                   
         BNE   GETNEXT                                                          
*                                                                               
         TM    LSCINDS,LSCIFOP     TEST FORMOPTS                                
         BZ    GNEXT03                                                          
         CLI   CLMRTN,64           YES - ONLY WANT OPTION COLUMNS               
         BNL   GETNEXT                                                          
*                                                                               
GNEXT03  MVC   TLRLEN,=AL2(TLCOLLNQ)                                            
         MVI   TLKSRT,C'V'         ENSURE FIXED COLS GO FIRST                   
         TM    CLMINDS1,CLMIKEY                                                 
         BZ    *+8                                                              
         MVI   TLKSRT,C'F'                                                      
         CLI   LSALPHA,C'Y'        TEST ALPHANUMERICAL ORDER                    
         BNE   *+10                                                             
         MVC   TLKSRT(L'LSCLMCOD),LSCLMCOD                                      
*                                                                               
         MVC   TLCCODE,LSCLMCOD                                                 
         MVC   TLCDATA,CLMTABD                                                  
*                                                                               
         MVC   BOWORK1,BCSPACES                                                 
         MVC   BOWORK2,BCSPACES                                                 
         MVC   BOWORK1(L'CLMHEAD1),CLMHEAD1                                     
         MVC   BOWORK2(L'CLMHEAD2),CLMHEAD2                                     
         CLI   BOWORK1,C' '        TEST DIRECT TEXT                             
         BH    GNEXT10                                                          
*                                                                               
         TM    CLMHEAD1+L'CLMHEAD1-1,X'80'                                      
         BZ    GNEXT06             TEST SPLIT HEADLINE                          
         MVC   BOELEM(L'CLMHEAD1),CLMHEAD1                                      
         GOTO1 VDICTAT,BOPARM,C'SL  ',BOELEM                                    
         XR    RF,RF                                                            
         IC    RF,CLMHWDTH                                                      
         LA    RE,BOELEM(RF)       RE=A(START OF HEADLINE 2)                    
         BCTR  RF,0                                                             
         CLC   0(0,RE),BCSPACES    TEST ANYTHING ON LINE 2                      
         BE    GNEXT04                                                          
         EX    RF,*+4                                                           
         MVC   BOWORK1(0),BOELEM   COPY SPLIT FOR 1                             
         EX    RF,*+4                                                           
         MVC   BOWORK2,0(RE)       AND 2                                        
         B     GNEXT10                                                          
GNEXT04  MVC   BOWORK1(L'CLMHEAD1),CLMHEAD1 SET NORMAL HEADLINE 1               
         MVC   BOWORK1+L'CLMHEAD1-1(L'CLMHWDTH),CLMHWDTH                        
         MVC   BOWORK2(L'CLMHEAD1),BOWORK1                                      
         OI    BOWORK2,X'08'       SET UNDERLINING FOR 2                        
*                                                                               
GNEXT06  GOTO1 ,BOPARM,C'SL  ',BOWORK1     CALL DICTATE FOR FIRST               
         CLI   BOWORK1,0                                                        
         BE    GNEXT08                                                          
         CLI   BOWORK1,C' '                                                     
         BNL   GNEXT08                                                          
         GOTO1 VDICTAT                                                          
GNEXT08  GOTO1 ,(R1),,BOWORK2              CALL DICTATE FOR SECOND              
         CLI   BOWORK2,0                                                        
         BE    GNEXT10                                                          
         CLI   BOWORK2,C' '                                                     
         BNL   GNEXT10                                                          
         GOTO1 VDICTAT                                                          
*                                                                               
GNEXT10  TM    CLMINDS1,CLMIHEAD   TEST OVERLAY CAN OVERWRITE HEADLINE          
         BZ    GNEXT20                                                          
         L     R3,OSETHEAD                                                      
         TM    CLMRTN,CLMRGEN      TEST GENERAL ROUTINE                         
         BO    GNEXT12                                                          
         TM    CLMINDS2,CLMIPROR   TEST PRORATA COLUM                           
         BZ    GNEXT14                                                          
         TM    CLMINDS2,CLMIHOUR   DON'T BOTHER IF HOURS                        
         BO    GNEXT20                                                          
         TM    CLMINDS2,CLMIAGYC   TEST AGENCY CURRENCY                         
         BO    GNEXT12                                                          
         CLC   CSBILCUR,BCEFFS                                                  
         BNE   GNEXT12                                                          
         BAS   RE,SETBILC          INTERNAL 'BILLING CURRENCY' ROUTINE          
         B     GNEXT20                                                          
GNEXT12  L     R3,ASETHEAD                                                      
*                                                                               
GNEXT14  LA    RE,CWRKD            SAVE LOCAL W/S                               
         LA    RF,CWRKL                                                         
         L     R0,ASAVE                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         L     R0,ASAVE                                                         
         GOTO1 (R3),BOPARM,CLMTABD,BOWORK1,BOWORK2                              
         LA    RE,CWRKD            RESTORE LOCAL W/S                            
         LA    RF,CWRKL                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
GNEXT20  DS    0H                                                               
         LA    R0,L'TLCH1          LEFT JUSTIFY HEADLINE 1                      
         CLI   BOWORK1,C' '                                                     
         BH    *+14                                                             
         MVC   BOWORK1(L'BOWORK1-1),BOWORK1+1                                   
         BCT   R0,*-14                                                          
*                                                                               
         LA    R0,L'TLCH2          CHANGE UNDERLINES TO SPACES                  
         LA    RF,BOWORK2                                                       
GNEXT22  CLI   0(RF),C'-'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,GNEXT22                                                       
*                                                                               
         LA    R0,L'TLCH2          LEFT JUSTIFY HEADLINE 2                      
         CLI   BOWORK2,C' '                                                     
         BH    *+14                                                             
         MVC   BOWORK2(L'BOWORK2-1),BOWORK2+1                                   
         BCT   R0,*-14                                                          
*                                                                               
         MVC   TLCH1,BOWORK1                                                    
         MVC   TLCH2,BOWORK2                                                    
*                                                                               
GETNEXTX B     EXITY                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         SPACE 1                                                                
SETBILC  NTR1  ,                   SET HEAD2 2 TO "(BILLING CURRENCY)"          
         MVC   BOWORK2,BCSPACES                                                 
         LH    RF,=Y(LC@BLGCU-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         MVI   BOWORK2,C'('                                                     
         MVC   BOWORK2+1(L'LC@BLGCU),0(RF)                                      
         LA    RF,BOWORK2+L'LC@BLGCU+1                                          
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST FOR DUPLICATE ENTRY                                 *         
*                                                                     *         
* NTRY: P1 = A(COLUMN TABLE ENTRY)                                    *         
*       P2 = A(CHARACTER CODE)                                        *         
***********************************************************************         
         SPACE 1                                                                
TSTCLM   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING CLMTABD,R2                                                       
         XR    RF,RF                                                            
         ICM   RF,3,LST#                                                        
         BZ    TCLM04                                                           
         LR    R0,RF                                                            
         LA    R1,LST                                                           
         USING LSTD,R1                                                          
TCLM02   CLC   LSTCODE,0(R3)                                                    
         BNE   *+14                                                             
         CLC   LSTCTRY,CLMCTRY                                                  
         BE    EXITN                                                            
         LA    R1,LSTL(R1)                                                      
         BCT   R0,TCLM02                                                        
         DROP  R1                                                               
*                                                                               
TCLM04   MH    RF,=Y(LSTL)                                                      
         LA    RF,LST(RF)                                                       
         USING LSTD,RF                                                          
         MVC   LSTCODE,0(R3)                                                    
         MVC   LSTCTRY,CLMCTRY                                                  
         DROP  RF                                                               
         LH    RF,LST#                                                          
         LA    RF,1(RF)                                                         
         STH   RF,LST#                                                          
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOOK UP COUNTRY TABLE                                    *         
*                                                                     *         
* NTRY: R1 = A(COUNTRY CODE)                                          *         
* EXIT: R2 = A(TABLE ENTRY)                                           *         
***********************************************************************         
         SPACE 1                                                                
GETCTRY  LA    R2,CTRYTAB0                                                      
         USING CTRYTABD,R2                                                      
GCTRY02  CLI   CTRYTABD,FF                                                      
         BE    GETCTRYN                                                         
         CLC   CTRYCODE,0(R1)                                                   
         BE    GETCTRYY                                                         
         LA    R2,CTRYTABL(R2)                                                  
         B    GCTRY02                                                           
*                                                                               
GETCTRYY CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
GETCTRYN LTR   RE,RE                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT POINTS                                                 *         
***********************************************************************         
         SPACE 1                                                                
EXITY    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,FF                                                             
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
       ++INCLUDE FACTRYTAB                                                      
*                                                                               
DEFCLMR  EQU   *                                                                
         DC    C'123456'                                                        
         DC    AL1(EOT)                                                         
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         SPACE 1                                                                
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
ALOTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,LSCLMMAX,LSCLMMAX)                               
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTDISQ,0)                                                   
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  ALPHA=YES/NO                                 
         DC    AL2(UC@ALPHA-TWAD,UC@ALPHA-TWAD)                                 
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,0,0,1,4,L'LSALPHA)                                     
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTYNQ,LSALPHA-LSVALSD)                                      
         DC    CL4'N'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
ALOTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DROP                                                                   
ALOVAL   NMOD1 250,**ALOV**,CLEAR=YES                                           
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         SPACE 1                                                                
VALX     XMOD1                                                                  
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* FACTRY                                                                        
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBEAD                                                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
*                                                                               
*                                                                               
SFLTOPS  DS    CL(LSFLTOPL)        SAVED FILTERING OPTIONS                      
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
LSVALSD  DSECT                                                                  
         ORG   LSOPS                                                            
LSACTION DS    XL1                 ACTION CODE                                  
LSCLM#   DS    XL1                 COLUMN HEADER CODE                           
LSOVR#   DS    XL1                 ACTION OVERLAY                               
LSALPHA  DS    CL1                 ALPHANUMNERICAL = YES/NO                     
LSCINDS  DS    XL1                 INDICATOR BYTE                               
LSCIFOP  EQU   X'80'               ACTION IS REALLY FORMOPTS                    
         SPACE 1                                                                
LSTD     DSECT                                                                  
LSTCODE  DS    CL2                 COLUMN CODE                                  
LSTCTRY  DS    XL1                 COUNTRY FILTER                               
LSTL     EQU   *-LSTD                                                           
*                                                                               
CWRKD    DSECT                     ** COLUMNS LOCAL W/S **                      
ASAVE    DS    A                                                                
ASETHEAD DS    A                                                                
OSETHEAD DS    A                                                                
SLSGENM  DS    XL(L'LSGENM)                                                     
SCLMHEAD DS    XL(L'ACLMHEAD)                                                   
SCLMDATA DS    XL(L'ACLMDATA)                                                   
SBILCUR  DS    XL(L'CSBILCUR)                                                   
LST#     DS    H                                                                
LST      DS    256XL(LSTL)                                                      
         DS    0D                                                               
CWRKL    EQU   *-CWRKD                                                          
         SPACE 1                                                                
TLSTD    DSECT                     ** TSAR RECORD **                            
         ORG   TLOVR                                                            
TLCDATA  DS    XL(CLMDATAL)        COLUMN TABLE ENTRY                           
TLCCODE  DS    CL2                 CHARACTER CODE                               
TLCH1    DS    CL20                HEADING 1                                    
TLCH2    DS    CL20                HEADING 2                                    
TLCOLLNQ EQU   *-TLREC                                                          
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'197ACCLB1BB  12/23/99'                                      
         END                                                                    
