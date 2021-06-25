*          DATA SET ACCLB1B    AT LEVEL 155 AS OF 08/16/00                      
*PHASE T6211BA                                                                  
CLB1B    TITLE '- COLUMN LIST'                                                  
CLB1B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB1B*,R8,R7,CLEAR=YES,RR=RE                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,RC                                                      
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
         L     RE,=A(ALOVAL)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS    SET OUTPUT BASE ADDRESS                      
         MVC   SFLTOPS,LSOPS       SAVE FILTERING OPTIONS FOR COMPARE           
         XC    LSOPS,LSOPS         CLEAR ALL OPTIONS                            
         XC    LSDIS,LSDIS         CLEAR DISPLAY COLUMNS                        
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
         L     R3,AMIXTAB                                                       
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
DISCODE  MVC   FVIFLD(L'CLMCHAR),CLMCHAR                                        
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
         XC    IOKEY,IOKEY         DUMMY KEY                                    
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
         GOTO1 AIO,IOACCDIR+IOHI+IO1 DUMMY IO READ                              
GETNEXT  DS    0H                                                               
         GOTO1 ASETCLM,FF          GET NEXT COLUMN                              
         BNL   GNEXT02                                                          
         MVC   LSGENM,SLSGENM      RESTORE COLUMN VALUES                        
         MVC   ACLMHEAD,SCLMHEAD                                                
         MVC   ACLMDATA,SCLMDATA                                                
         B     EXITN                                                            
*                                                                               
GNEXT02  L     R2,ACLMDATA                                                      
         USING CLMTABD,R2                                                       
         GOTO1 TSTCLM,CLMTABD                                                   
         BNE   GETNEXT                                                          
*                                                                               
         MVI   TLKSRT,C'V'         ENSURE FIXED COLS GO FIRST                   
         TM    CLMINDS1,CLMIKEY                                                 
         BZ    *+8                                                              
         MVI   TLKSRT,C'F'                                                      
         CLI   LSALPHA,C'Y'        TEST ALPHANUMERICAL ORDER                    
         BNE   *+10                                                             
         MVC   TLKSRT(L'CLMCHAR),CLMCHAR                                        
*                                                                               
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
GNEXT10  MVC   TLCH1,BOWORK1                                                    
         MVC   TLCH2,BOWORK2                                                    
*                                                                               
GETNEXTX B     EXITY                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST FOR DUPLICATE ENTRY                                 *         
*                                                                     *         
* NTRY: R1 = A(COLUMN TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
TSTCLM   NTR1  ,                                                                
         USING CLMTABD,R1                                                       
         XR    RF,RF                                                            
         ICM   RF,3,LST#                                                        
         BZ    TCLM04                                                           
         LR    R0,RF                                                            
         LA    R2,LST                                                           
         USING LSTD,R2                                                          
TCLM02   CLC   LSTCHAR,CLMCHAR                                                  
         BNE   *+14                                                             
         CLC   LSTCTRY,CLMCTRY                                                  
         BE    EXITN                                                            
         LA    R2,LSTL(R2)                                                      
         BCT   R0,TCLM02                                                        
         DROP  R2                                                               
*                                                                               
TCLM04   MH    RF,=Y(LSTL)                                                      
         LA    RF,LST(RF)                                                       
         USING LSTD,RF                                                          
         MVC   LSTCHAR,CLMCHAR                                                  
         MVC   LSTCTRY,CLMCTRY                                                  
         DROP  RF                                                               
         LH    RF,LST#                                                          
         LA    RF,1(RF)                                                         
         STH   RF,LST#                                                          
         B     EXITY                                                            
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
         DC    AL1(0,0,0,0,0,1,L'LSDISLST,L'LSDISLST)                           
         DC    AL1((*-ALOTAB)/OPTTABL)                                          
         DC    AL2(OPTDISQ,LSDIS-LSVALSD)                                       
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
         DROP  R6,R7,R8,RB                                                      
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
       ++INCLUDE ACCLBWORKC                                                     
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
LSALPHA  DS    CL1                 ALPHANUMNERICAL = YES/NO                     
         SPACE 1                                                                
LSTD     DSECT                                                                  
LSTCHAR  DS    CL1                 COLUMN CODE                                  
LSTCTRY  DS    XL1                 COUNTRY FILTER                               
LSTL     EQU   *-LSTD                                                           
*                                                                               
OVERWRKD DSECT                                                                  
SLSGENM  DS    XL(L'LSGENM)                                                     
SCLMHEAD DS    XL(L'ACLMHEAD)                                                   
SCLMDATA DS    XL(L'ACLMDATA)                                                   
LST#     DS    H                                                                
LST      DS    256XL(LSTL)                                                      
         DS    0X                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'155ACCLB1B   08/16/00'                                      
         END                                                                    
