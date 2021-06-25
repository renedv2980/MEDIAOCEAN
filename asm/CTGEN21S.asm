*          DATA SET CTGEN21S   AT LEVEL 023 AS OF 05/01/02                      
*PHASE TA0B21A,*                                                                
*                                                                               
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* Jun24/96 022 GLEE - Support RFP passive pointers                    *         
*                                                                     *         
* Dec23/94 021 GLEE - Disable adding new messages if max # of entries *         
*                      reached                                        *         
*              GLEE - Allow the EquateName inputted in the absence of *         
*                      the sys# & msg# (reads passives to get them)   *         
*                                                                     *         
*  ??????  020 GLEE - New Data Dictionary maintenance                 *         
***********************************************************************         
         TITLE 'CTGEN21<==>TA0B21 FILE MAINT : MSG DATA-DICT RECORDS'           
GEN21    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN21*,RA,R9,R8,RR=RE                                        
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GMSGD,R2            R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         SPACE 1                                                                
         L     R0,=A(PFXTABL)      ADJUST A(PREFIX TABLE)                       
         AR    R0,RE                                                            
         ST    R0,APFXTAB                                                       
         L     R0,=A(PFXTABLX)     ADJUST A(END OF PREFIX TABLE)                
         AR    R0,RE                                                            
         ST    R0,APFXTABX                                                      
         L     R0,=A(PFXTABLX)     ADJUST A(REPORT SPECS)                       
         AR    R0,RE                                                            
         ST    R0,AREPSPEC                                                      
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                MULTIPLY BY 4                                
         B     *+0(RF)             BRANCH TO APPROPRIATE ROUTINE                
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         SPACE 2                                                                
CCEQUAL  SR    RF,RF               TO RETURN CC = 0                             
         B     *+8                                                              
CCNEQUAL LA    RF,1                TO RETURN CC <> 0                            
         LTR   RF,RF                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*============= ROUTINE TO VALIDATE KEY OF MESSAGE RECORD =============*         
VALKEY   DS    0H                                                               
*                                                                               
         XC    GMKEY,GMKEY         CLEAR KEY                                    
         MVI   GMKREC,GMKRECQ      SET RECORD TYPE                              
*                                                                               
*------------------------------- SYSTEM ------------------------------*         
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DCTSYSH       VALIDATE MESSAGE SYSTEM                      
         BE    VK015                GO VALIDATE INPUT                           
                                                                                
         DS    0H                  NO INPUT IN SYSTEM FIELD                     
         MVI   FVMINL,1            TRY TO USE EQUNAME AS ALTERNATIVE            
         GOTO1 AFVAL,DCTQNMEH                                                   
         BE    VK012A                                                           
         LA    R1,DCTSYSH          NO EQUNAME EITHER                            
         ST    R1,APCURSOR                                                      
         B     VKXITNO              DISPLAY ERROR MESSAGE                       
VK012A   MVI   FVMINL,4            MIN L(EQUNAME) IS 4                          
         GOTO1 AFVAL,DCTQNMEH                                                   
         BNE   VKXITNO                                                          
         XC    DPRFX,DPRFX         GET SET TO GET SYSTEM PREFIX                 
         BAS   RE,CHKEQPSV         VALIDATE EQUNAME INPUTTED                    
         BNE   VK013                CC NEQ ==> EQUNAME EXISTS                   
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#QNXST)),   +        
               RR=APRELO                                                        
         B     VKXITNO                                                          
*                                                                               
VK013    DS    0H                  GET SYS# FROM PREFIX                         
         L     RE,APFXTAB                                                       
VK013A   CLI   0(RE),0             AT EOT?                                      
         BNE   *+6                                                              
         DC    H'0'                 SOMETHING'S GONE BAD                        
         CLC   DPRFX,0(RE)                                                      
         BE    *+12                                                             
         LA    RE,(PFXTABLF-PFXTABLE)(RE)                                       
         B     VK013A                                                           
                                                                                
         MVC   SVSYPRFX,0(RE)      SAVE SYSTEM PREFIX                           
         MVC   GMKSYS,2(RE)         AND SYSTEM NUMBER                           
         MVC   APBYTE,GMKSYS       NOW DISPLAY SYSTEM NAME                      
         CLI   GMKSYS,15                                                        
         BNE   *+8                                                              
         MVI   APBYTE,0                                                         
         GOTO1 ADISSYS,APBYTE      GET MESSAGE SYSTEM                           
         GOTO1 DISPFLD,DCTSYSH                                                  
         B     VK018                                                            
*                                                                               
** VALIDATE SYSTEM INPUT **                                                     
*                                                                               
VK015    DS    0H                                                               
         XC    SVSYPRFX,SVSYPRFX   CLEAR OUT SYSTEM PREFIX                      
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         TM    FVIIND,FVIALF       IF ALPHABETIC INPUT ALLOWED                  
         BZ    VKXITNO                                                          
*                                                                               
         GOTO1 AVALSYS,DCTSYSH     VALIDATE SYSTEM NAME                         
         BNE   VKXITNO                                                          
         MVC   GMKSYS,APWORK       KEY FIELD: SYSTEM NUMBER                     
         CLI   APWORK,0            CHECK FOR GENERAL SYSTEM                     
         BNE   *+8                                                              
         MVI   GMKSYS,15            ITS SYS# IS 15                              
         ZIC   R1,GMKSYS           GET SYSTEM PREFIX                            
         LA    RF,PFXTABLF-PFXTABLE                                             
         MR    RE,R1                                                            
         A     RF,APFXTAB           FROM PFXTABLE                               
         C     RF,APFXTABX         MAKE SURE DON'T GO PAST                      
         BL    *+6                  END OF TABLE                                
         DC    H'0'                                                             
         MVC   SVSYPRFX,0(RF)                                                   
*                                                                               
VK018    DS    0H                                                               
         NI    MESGFLG,X'FF'-MFX40Q-MFX20Q                                      
         CLI   GMKSYS,15                                                        
         BNE   *+8                                                              
         OI    MESGFLG,MFX40Q      FLAG TO TURN ON X'4000' IN PSV               
*                                                                               
*---------------------------- MESSAGE TYPE ---------------------------*         
*                                                                               
VK020    MVI   GMKTYP,GMKTGDIC     KEY FIELD: NEW DATA-DICTIONARY ONLY          
*                                                                               
*--------------------------- MESSAGE NUMBER --------------------------*         
*                                                                               
VK040    XC    SVHIMSG,SVHIMSG                                                  
         MVC   SVEQNAME,SPACES                                                  
         MVI   SVRFP,0                                                          
*                                                                               
** GET CONTROL (HIGH) MESSAGE NUMBER                                            
*                                                                               
         LA    R1,IOGENDIR+IO2+IOHIGH                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                NO REASON FOR ERROR                          
         CLC   IOKEY(GMKMSG-GMKEY),IOKEYSAV                                     
         BE    *+12                                                             
         BAS   RE,ADDHIMSG         ADD HIGH-MSG RECORD                          
         B     VK040               TRY AGAIN                                    
         OC    GMKMSG,GMKMSG                                                    
         BZ    *+6                                                              
         DC    H'0'                WE WANT MSG# 0                               
         MVC   IODA,GMDDA-GMSGD(R3)                                             
*                                                                               
         LA    R1,IOGENFIL+IO2+IOGET                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                NO REASON FOR ERROR                          
         L     R3,IOADDR                                                        
         XC    APELEM,APELEM       GET HIGH-MSG# ELEMENT                        
         MVI   APELEM,GMCTLELC     ELEMENT CODE                                 
         GOTO1 AGETELS,(R3)                                                     
         ICM   R3,15,APPARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING GMCTLD,R3                                                        
         MVC   APELEM(GMCTLLNQ),0(R3)                                           
         MVC   SVHIMSG,GMCTLNM1   GET CONTROL (HIGH) MSG#1                      
         TM    GMCTLFLG,GCFCNUM2  USE MSG#2                                     
         BZ    *+10                NO                                           
         MVC   SVHIMSG,GMCTLNM2   USE CONTROL (HIGH) MSG#2                      
         DROP  R3                                                               
*                                                                               
** VALIDATE MSG# INPUT                                                          
*                                                                               
VK050    CLI   APACTN,ACTADD       IS ACTION = ADD?                             
         BNE   VK060                NO, SO THERE MUST BE A MSG# INPUT           
*                                                                               
         MVI   FVMINL,0             YES, SO WE DON'T REQUIRE INPUT              
         GOTO1 AFVAL,DCTNUMH       GO VALIDATE FIELD                            
         CLI   FVILEN,0            ANY INPUT?                                   
         BH    VK065                YES, SO MAKE SURE INPUT<SVHIMSG             
*                                                                               
*** SUPPLY USER WITH NEXT MSG# FOR TYPE-'D'                                     
*                                                                               
         LA    R3,APELEM                                                        
         USING GMCTLD,R3                                                        
         TM    GMCTLFLG,GCFMXHIQ  MAX "HI" MSG# SURPASSED?                      
         BZ    VK052               NOT YET                                      
         DROP  R3                                                               
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMINF',=AL2(CI#MNEXH)),   +        
               RR=APRELO                                                        
         LA    R1,DCTSYSH                                                       
         ST    R1,APCURSOR         FORCE CURSOR TO SYSTEM FIELD                 
         B     VKXITNO             EXIT PROGRAM TO DISPLAY MSG                  
*                                                                               
VK052    DS    0H                                                               
         ZICM  R1,SVHIMSG,(3)                                                   
VK052A   EDIT  (R1),(5,DCTNUM),WRK=APWORK,DUB=APDUB,ALIGN=LEFT                  
         OI    DCTNUMH+6,X'81'             MODIFIED AND TRANSMIT                
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMINF',=AL2(CI#MNUMS)),   +        
               RR=APRELO                                                        
         LA    R1,DCTLANGH                                                      
         ST    R1,APCURSOR         FORCE CURSOR TO LANGUAGE FIELD               
         B     VKXITNO             EXIT PROGRAM TO DISPLAY MSG                  
*                                                                               
*** THERE IS INPUT TO MSG# FIELD                                                
*                                                                               
VK060    MVI   FVMINL,1            MSG# MUST BE INPUTTED                        
         GOTO1 AFVAL,DCTNUMH                                                    
         BE    VK065                                                            
*                                                                               
VK061    DS    0H                 BUT EQUATE NAME CAN BE ALTERNATIVE            
         MVI   FVMINL,1            CHECK FOR ANY INPUT                          
         GOTO1 AFVAL,DCTQNMEH                                                   
         BE    VK061A                                                           
         LA    R1,DCTNUMH           IF NONE,                                    
         ST    R1,APCURSOR           FORCE CURSOR BACK TO MSG# FIELD            
         B     VKXITNO                                                          
VK061A   MVI   FVMINL,4             BUT EQUATE NAME CAN BE ALTERNATIVE          
         GOTO1 AFVAL,DCTQNMEH                                                   
         BNE   VKXITNO                                                          
         XC    DMESGNUM,DMESGNUM   GET SET TO GET MSG#                          
         BAS   RE,CHKEQPSV         CHECK IF EQUATE NAME EXISTS                  
         BNE   VK063                CC=NEQ ==> EQUATE NAME EXISTS               
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#QNXST)),   +        
               RR=APRELO                                                        
         B     VKXITNO                                                          
*                                                                               
VK063    DS    0H                 USING INPUTTED EQUNAME AS ALTRNTVE            
         ZICM  R1,DMESGNUM,(3)     GET CORRESPONDING MSG#                       
         CLI   DCTLANGH+5,0                                                     
         BE    VK052A               AND DISPLAY IT (W/ INFO MSG)                
         ST    R1,SCFULL            OR KEEP PLOWING AHEAD                       
         EDIT  (R1),(5,DCTNUM),WRK=APWORK,DUB=APDUB,ALIGN=LEFT                  
         OI    DCTNUMH+6,X'81'    MODIFIED AND TRANSMITTED                      
         B     VK065A                                                           
*                                                                               
VK065    TM    FVIIND,FVINUM       IT MUST BE NUMERIC                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VKXITNO                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)  ASSUME INPUT IS STILL INVALID             
         OC    SCFULL(2),SCFULL       MAX MSG# = X'FFFF'                        
         BNZ   VKXITNO                                                          
*                                                                               
         OC    SCFULL+2(2),SCFULL+2   MSG# = 0 INPUTTED?                        
         BZ    VKXITNO                 YEP, ERROR                               
*                                                                               
         CLC   BNDRY,SCFULL+2         MSG# @ BOUNDARY (X'8000')?                
         BE    VK065C                  YEP, ERROR                               
*                                                                               
VK065A   DS    0H                                                               
         MVC   GMKMSG,SCFULL+2     KEY FIELD: MSG#                              
         LA    R1,IOGENDIR+IO2+IOHIGH                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                NO REASON FOR ERROR                          
         SPACE 1                                                                
         CLI   APACTN,ACTADD       THESE LAST CHECKS ARE FOR                    
         BNE   VK065B               ACTION = ADD ONLY                           
         CLC   SVHIMSG,SCFULL+2    CHECK INPUT AGAINST CONTROL #                
         BNL   VK065B               CONTROL # MUST BE >= INPUT,                 
         CLC   IOKEY(GMKLANG-GMSGD),IOKEYSAV  UNLESS THERE EXIST                
         BE    VK065B                         A MSG# WITH ANY LANG              
         MVC   FVMSGNO,=AL2(FVFNOTV)  IF BRAND NEW MESSAGE                      
         B     VKXITNO                                                          
*                                                                               
VK065B   ZICM  RF,X4000BIT,(3)     MAKE SURE X'4000' IS NOT ON                  
         N     RF,SCFULL                                                        
         BZ    VK070                                                            
VK065C   DS    0H                                                               
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#N40BT)),   +        
               RR=APRELO                                                        
         B     VKXITNO                                                          
*                                                                               
*------------ GET EQUATE NAME, RFP#, ELEM FLAG, & MAX LNGTH ----------*         
*                                                                               
VK070    OI    MESGFLG,MFNEWQ      ASSUME BRAND NEW MESSAGE                     
         CLC   IOKEYSAV(GMKLANG-GMKEY),IOKEY  EQUATE NAME IS UNIQUE             
         BNE   VK080                           UP TO LANGUAGE                   
*                                                                               
         LA    R1,IOGENFIL+IO2+IOGET                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                NO REASON FOR ERROR                          
         L     R3,IOADDR                                                        
*                                                                               
         XC    APELEM,APELEM       GET EQUATE-NAME ELEMENT                      
         MVI   APELEM,GMQSYELC                                                  
         GOTO1 AGETELS,(R3)                                                     
*                                                                               
         ICM   R3,15,APPARM        ELEMENT HAD BETTER BE THERE                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVELEM,GMDELEM      GET ELEM FLAG FROM IOKEY                     
         USING GMQSYD,R3                                                        
         MVC   SVEQNAME,GMQSYSYM   SAVE EQUATE NAME                             
         MVC   SVRFP,GMQRFP        SAVE RFP RULE#                               
         DROP  R3                                                               
         SPACE 1                                                                
         MVI   SVMXLEN,0           GET MAX LENGTH, ASSUME IT'S ZERO             
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMPRGELC     ELEMENT CODE                                 
         L     R3,IOADDR                                                        
         GOTO1 AGETELS,(R3)                                                     
         ICM   R3,15,APPARM                                                     
         BZ    VK070A                                                           
         USING GMPRGD,R3                                                        
         MVC   SVMXLEN,GMPRGLMT    SAVE MAX LENGTH AROUND                       
         DROP  R3                                                               
*                                                                               
VK070A   NI    MESGFLG,X'FF'-MFNEWQ   MESSAGE NOT BRAND NEW                     
*                                                                               
*------------------------------ LANGUAGE -----------------------------*         
*                                                                               
VK080    MVC   IOKEY,IOKEYSAV      RESTORE KEY, JUST IN CASE                    
*                                                                               
         TM    MESGFLG,MFNEWQ      IF BRAND NEW MSG,                            
         BO    VK085                THEN NO NEED TO TEST RFP CHANGEABLE         
         BAS   RE,TSTCHRFP         SET MESGFLG IF RFP CHANGEABLE                
*                                                                               
VK085    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DCTLANGH      LANGUAGE CODE                                
         BNE   VKXITNO                                                          
         GOTO1 AVALLNG,DCTLANGH    VALIDATE LANGUAGE NAME                       
         BNE   VKXITNO                                                          
         MVC   SVLANG,APWORK       SAVE LANGUAGE CODE AROUND                    
         MVC   GMKLANG,APWORK      KEY FIELD: LANGUAGE CODE                     
         XI    GMKLANG,X'FF'       INVERT LANGUAGE                              
*                                                                               
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'DCTLANG-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,DCTLANGH    DISPLAY FULL NAME                            
*                                                                               
*----------------------------- GET RECORD ----------------------------*         
*                                                                               
VK090    TM    SVFLAG,SFRFPCHQ+SFMXLCHQ                                         
         BZ    VK090A              IF ANYTHING FLAGGED TO CHANGE,               
         CLC   GMKEY,SAVRECK        AND IF KEY CHANGED,                         
         BE    VK090A               UNFLAG CHANGES                              
         MVI   SVPVRFP,0                                                        
         MVI   SVPVMXLN,0                                                       
         NI    SVFLAG,X'FF'-SFRFPCHQ-SFMXLCHQ                                   
*                                                                               
VK090A   MVC   APRECKEY,GMKEY          SAVE KEY IN APPL AREA                    
         MVC   MYSVKEY,GMKEY           SAVE KEY FOR LATER RE-READ               
         NI    SVFLAG,X'FF'-SFADDELQ   ASSUME NOT "ADDING" DEL REC              
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VKXITNO             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK100                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    VK110               NOT DEL, THEREFORE NO REC(OK TO ADD)         
         CLI   APACTN,ACTADD       SEE IF USER                                  
         BNE   *+12                                                             
         OI    SVFLAG,SFADDELQ      WANTS TO "ADD" DELETED REC                  
         B     VK100                                                            
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VK100    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         CLI   APACTN,ACTRES       IF ACTION=RESTORE,                           
         BNE   VK105                                                            
         BAS   RE,CHKRSTR           THEN CHECK IF ABLE TO RESTORE               
         BE    VK105                                                            
         NI    APINDS,X'FF'-APIOKRES                                            
*                                                                               
VK105    TM    SVFLAG,SFADDELQ     IF "ADDING" A DELETED RECORD                 
         BO    VK110                DO TASKS FOR ACTION=ADD                     
         B     VKXITYES                                                         
*                                                                               
*------------------------- TASKS FOR ACTION=ADD ----------------------*         
*                                                                               
VK110    CLI   APACTN,ACTADD       MAKE SURE IT'S ACTION ADD                    
         BNE   VKXITNO                                                          
         TM    MESGFLG,MFNEWQ      IF BRAND NEW MESSAGE,                        
         BO    VK150                THEN NO EQUNAME AND RFP TO SHOW             
VK110A   GOTO1 AFVAL,DCTQNMEH                                                   
         CLC   SVEQNAME,FVIFLD     IF SCREEN DOESN'T HAVE CORRECT               
         BE    VK112                                                            
         MVC   DCTQNME,SVEQNAME     EQUNAME, THEN DISPLAY IT                    
         OI    DCTQNMEH+6,X'81'                                                 
         CLC   SVEQNAME,SPACES     IF EQUNAME IS NON-SPACE, THEN SHOW           
         BE    VK112                                                            
         MVC   MESGNUM,=AL2(CI#EQSUP)   EQUATE NAME SUPPLIED                    
*                                                                               
VK112    XC    SCFULL,SCFULL                                                    
         GOTO1 AFVAL,DCTRFPH                                                    
         CLI   FVILEN,0            IF NO INPUT,                                 
         BE    VK112A               THEN NO NEED TO VALIDATE                    
         TM    FVIIND,FVINUM        ELSE, INPUT MUST BE NUMERIC                 
         BO    VK112A                                                           
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VKXITNO                                                          
*                                                                               
VK112A   CLC   SVRFP,SCFULL+3      DOES SCREEN SHOW EXISTING RFP#?              
         BE    VK120                YES                                         
         TM    SVFLAG,SFRFPCHQ      NO, WAS IT CHNGD IN LAST TRNSCTN?           
         BO    VK120                 YES, LEAVE IT ALONE                        
*                                                                               
VK115    EDIT  (B1,SVRFP),(3,DCTRFP),WRK=APWORK,DUB=APDUB,ALIGN=LEFT            
         OI    DCTRFPH+6,X'81'                                                  
         OC    MESGNUM,MESGNUM     GOING TO DISPLAY MSG CI#EQSUP?               
         BNZ   *+14                                                             
         MVC   MESGNUM,=AL2(CI#RFSUP)   RFP# SUPPLIED                           
         B     *+10                                                             
         MVC   MESGNUM,=AL2(CI#QRSUP)   EQUNAME & RFP# SUPPLIED                 
*                                                                               
VK120    OC    MESGNUM,MESGNUM     BY THIS TIME, THE SCREEN IS                  
         BZ    VK140                DISPLAYING CORRECT EQUNAME & RFP#           
*                                                                               
VK125    GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMINF',MESGNUM),          +        
               RR=APRELO                                                        
         LA    R1,DCTMSG1H                                                      
         ST    R1,APCURSOR         PUT CURSOR @ OKAY FIELD                      
         B     VKXITNO                                                          
*                                                                               
VK140    XC    SCFULL,SCFULL       DISPLAY MAX LEN, IF ANY                      
         GOTO1 AFVAL,DCTLMTH       FIRST SEE IF ANYTHING IN FIELD               
         CLI   FVILEN,0                                                         
         BE    VK142                                                            
         TM    FVIIND,FVINUM       WAS THAT ANYTHING NUMERIC?                   
         BO    VK142                YEP                                         
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VKXITNO                                                          
*                                                                               
VK142    CLC   SVMXLEN,SCFULL+3    DOES THING IN FIELD MATCH RECORD?            
         BE    VK150                YEP, NO NEED TO REDISPLAY IT                
         TM    SVFLAG,SFMXLCHQ      NO, WAS IT CHNGD IN LAST TRNSCTN?           
         BO    VK150                 YES, LEAVE IT ALONE                        
         EDIT  (B1,SVMXLEN),(2,DCTLMT),WRK=APWORK,DUB=APDUB,ALIGN=LEFT          
         OI    DCTLMTH+6,X'81'                                                  
*                                                                               
VK150    MVI   APINDS,APIOKADD                                                  
         B     VKXITYES                                                         
*                                                                               
*----------------------------- VALKEY EXIT ---------------------------*         
*                                                                               
VKXITYES MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     CCEQUAL                                                          
*                                                                               
VKXITNO  B     CCNEQUAL                                                         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*==================== ADD HIGH-MSG (CONTROL) RECD ====================*         
ADDHIMSG NTR1                                                                   
*                                                                               
*         IOKEYSAV HAS KEY OF CONTROL RECORD                                    
*                                                                               
         L     R2,AIOAREA2         BUILD RECORD IN IO2                          
         MVC   GMKEY,IOKEYSAV                                                   
         MVC   GMFLEN,=AL2(GMFIRST+GMCTLLNQ)                                    
         XC    APELEM,APELEM       BUILD HIGH-MSG# ELEMENT                      
         LA    R3,APELEM                                                        
         USING GMCTLD,R3                                                        
         MVI   GMCTLEL,GMCTLELC    ELEMENT CODE                                 
         MVI   GMCTLELL,GMCTLLNQ   ELEMENT LENGTH                               
         MVI   GMCTLNM1+1,1        START FROM MSG# = 1                          
         MVC   GMCTLNM2,=X'8001'   START FROM MSG# = 32769 (IF HIGH)            
         DROP  R3                                                               
         GOTO1 AADDELS,GMSGD                                                    
         SPACE 1                                                                
         LA    R1,IOGENFIL+IO2+IOADDREC                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,IOKEYSAV      RESET KEY                                    
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*==================== TEST WHETHER RFP CHANGEABLE ====================*         
TSTCHRFP NTR1                                                                   
*                                                                               
         OI    SVFLAG,SFCHRFPQ      ASSUME RFP# IS CHANGEABLE                   
         LA    R2,IOKEY                                                         
         MVC   APRECKEY,IOKEY                                                   
         MVI   GMKLANG,0                                                        
         LA    R1,IOGENDIR+IO2+IOHI                                             
         B     *+8                                                              
TCR10    LA    R1,IOGENDIR+IO2+IOSQ                                             
         GOTO1 AIO                                                              
         CLC   IOKEY(GMKLANG-GMSGD),IOKEYSAV                                    
         BNE   XTSTCHRF                                                         
         LA    R1,IOGENFIL+IO2+IOGET                                            
         GOTO1 AIO                                                              
*                                                                               
         L     R2,IOADDR                                                        
         CLI   GMSGTXT,1           ONLY 1 LITERAL ALLOW FOR RFP                 
         BE    TCR10                                                            
         NI    SVFLAG,X'FF'-SFCHRFPQ                                            
*                                                                               
XTSTCHRF MVC   IOKEY,APRECKEY                                                   
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== CHECK RESTORABLE OR NOT ======================*         
CHKRSTR  NTR1                                                                   
*                                                                               
         L     R2,AIOAREA1                                                      
         MVC   APHALF,GMKMSG       HOLD ONTO MESSAGE #                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMQSYELC     GET EQUATE-NAME ELEMENT                      
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM                                                     
         USING GMQSYD,R3                                                        
         MVC   FVIFLD(L'GMQSYSYM),GMQSYSYM                                      
         DROP  R3                                                               
         BAS   RE,CHKEQPSV         CHECK FOR PASSIVE W/ EQUNAME                 
         BE    CHKRSTRY             NONE OUT THERE                              
         MVC   MYSVKEY,IOKEY        SOME OUT THERE                              
         MVC   IOKEY,IOKEYSAV                                                   
         LA    R1,IOGENDIR+IOHIGH                                               
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         CLC   GQKQNAME,FVIFLD                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GQKMNUM,APHALF      IF IT HAS A DIFFERENT MSG#                   
         BNE   CHKRSTRN             THEN WE CAN'T RESTORE RECORD                
         MVC   IOKEY,MYSVKEY                                                    
*                                                                               
CHKRSTRY B     CCEQUAL                                                          
CHKRSTRN B     CCNEQUAL                                                         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============= ROUTINE TO ADD OR CHANGE A MESSAGE RECORD =============*         
VALREC   DS    0H                                                               
*                                                                               
         MVC   IOKEY,APRECKEY                                                   
         L     R2,AIOAREA1                                                      
         MVC   GMKEY,APRECKEY                                                   
         CLI   APACTN,ACTADD                                                    
         BNE   *+12                                                             
         TM    SVFLAG,SFADDELQ                                                  
         BZ    VR010               NO ELEMENT TO REMOVE ON PURE ADD             
*                                                                               
         MVI   APELEM,GMSGELC                                                   
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GMSGD       DELETE EXISTING MESSAGE ELEMENT              
*                                                                               
         MVI   APELEM,GMPRGELC                                                  
         GOTO1 ADELELS,GMSGD       DELETE EXISTING PROGRAM ELEMENT              
*                                                                               
         MVI   APELEM,GMQSYELC                                                  
         GOTO1 ADELELS,GMSGD       DELETE EXISTING EQUNAME ELEMENT              
*                                                                               
         MVI   APELEM,GMTXTELC     DELETE EXPANDED TEXT ELEMENTS                
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GMSGD       REMOVE OLD ELEMENTS                          
*                                                                               
         NI    GMFELEM,X'FF'-GMPRGQ-GMCOREQ-GMRFPQ-GMTRNSQ                      
         NI    GMFSTAT,X'FF'-X'80'       IN CASE SFADDELQ IS ON                 
         LA    R2,IOKEY                                                         
         NI    GMDELEM,X'FF'-GMPRGQ-GMCOREQ-GMRFPQ-GMTRNSQ                      
         NI    GMDSTAT,X'FF'-X'80'       IN CASE SFADDELQ IS ON                 
         L     R2,AIOAREA1                                                      
*                                                                               
*---------------------------- MESSAGE TEXT ---------------------------*         
*                                                                               
VR010    XC    APELEM,APELEM                                                    
         LA    R3,APELEM           R3-->ELEMENT BUFFER                          
         USING GMSGEL,R3            USED BY GMSGEL                              
         MVI   GMSGEL,GMSGELC      ELEMENT CODE                                 
*                                                                               
** GET MESSAGES FROM SCREEN                                                     
*                                                                               
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         MVI   FVMINL,1            MINIMUM LENGTH OF                            
         GOTO1 AFVAL,DCTMSG1H       LINE 1 OF MESSAGE TEXT                      
         BNE   VRXITNO                                                          
         MVC   LMESG1,FVILEN                                                    
         MVC   MESG1,FVIFLD                                                     
         MVI   FVMINL,0            MINIMUM LENGTH OF                            
         GOTO1 AFVAL,DCTMSG2H       LINE 2 OF MESSAGE TEXT                      
         MVC   LMESG2,FVILEN                                                    
         MVC   MESG2,FVIFLD                                                     
*                                                                               
** CONCATENATE MESSAGE LINES                                                    
*                                                                               
         GOTO1 =A(SPCFRMT),APPARM,(R5),(R6),(R7),(RC),0,RR=APRELO               
*                                                                               
VR020    BNE   VRXITNO                                                          
         EXMVC R1,GMSGTXT,MESGFMT  R1 = L(MESSAGE) - 1                          
         LA    R1,GMSGFXDL+1(R1)   R1 = L(MSG TEXT ELEMENT),                    
         STC   R1,GMSGELL           STORE IT                                    
*                                                                               
         LA    R1,GMFIRST(R1)      DEFINE INITIAL REC LENGTH,                   
         STCM  R1,3,GMFLEN          STORE IT                                    
         GOTO1 AADDELS,GMSGD                                                    
         DROP  R3                  DROP ELEMENT POINTER                         
*                                                                               
*---------------------------- TRANSLATION ----------------------------*         
*                                                                               
         GOTO1 AFVAL,DCTTRNSH                                                   
         SPACE 1                                                                
         CLI   FVIFLD,C'U'         UNSURE TRANSLATION                           
         BE    VR030                                                            
         CLI   FVIFLD,C'T'         TRUE TRANSLATION                             
         BE    VR025                                                            
         CLI   FVIFLD,C' '         DEFAULT TO TRUE TRANSLATION                  
         BE    VR025                                                            
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VRXITNO                                                          
*                                                                               
VR025    OI    GMFELEM,GMTRNSQ     TRUE TRANSLATION FLAG                        
         LA    R2,IOKEY                                                         
         OI    GMDELEM,GMTRNSQ                                                  
         L     R2,AIOAREA1                                                      
*                                                                               
*-------------------- PROGRAM, SUB REF, & MAX LEN --------------------*         
*                                                                               
** PROGRAM REFERENCE                                                            
*                                                                               
VR030    XC    APELEM,APELEM                                                    
         LA    R3,APELEM           R3=A(PROGRAM ELEMENT)                        
         USING GMPRGD,R3                                                        
         MVI   GMPRGPRG,C' '       CLR PRG AND SUB TO SPACES                    
         MVC   GMPRGPRG+1(L'GMPRGPRG-1),GMPRGPRG                                
         MVC   GMPRGSUB,GMPRGPRG                                                
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DCTPRGH       PROGRAM FIELD                                
         BNE   VR040                                                            
         MVI   GMPRGEL,GMPRGELC    ELEMENT CODE                                 
         GOTO1 AVALPGM,APPARM,(GMKSYS,DCTPRGH)                                  
         BE    VR035                                                            
*                                                                               
         LA    R1,PROGTAB          IF PROGRAM NAME                              
         ZIC   RF,FVXLEN           NOT IN SYSTEM LIST                           
VR030A   EX    RF,*+8              TRY TABLE FOR                                
         B     *+10                ANOTHER MATCH                                
         CLC   FVIFLD(0),0(R1)                                                  
         BE    VR035A                                                           
         LA    R1,8(R1)            NEXT ENTRY                                   
         CLI   0(R1),0                                                          
         BNE   VR030A                                                           
         MVC   FVMSGNO,=AL2(FVFEPGM)                                            
         B     VRXITNO             NOT FOUND IN EITHER                          
*                                                                               
VR035    L     R1,0(R1)                                                         
VR035A   MVC   GMPRGPRG(7),0(R1)   MOVE PGM NAME TO ELEMENT                     
         MVC   APWORK(7),0(R1)                                                  
         GOTO1 DISPFLD,DCTPRGH     MOVE PGM NAME TO FIELD                       
*                                                                               
** SUB REFERENCE                                                                
*                                                                               
VR040    GOTO1 AFVAL,DCTSUBH       SUB ID FIELD                                 
         BNE   VR050                                                            
         MVI   GMPRGEL,GMPRGELC                                                 
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8              MOVE SUB ID TO ELEMENT                       
         B     *+10                                                             
         MVC   GMPRGSUB(0),FVIFLD                                               
*                                                                               
** MAX LENGTH                                                                   
*                                                                               
VR050    MVI   MAXLEN,0            ASSUME MAX LENGTH INPUT IS ZERO              
         XC    SCFULL,SCFULL                                                    
         GOTO1 AFVAL,DCTLMTH       MAX LENGTH                                   
         SPACE 1                                                                
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   VR050A               YES                                         
         CLI   SVMXLEN,0            NO, USER WANT MAX LEN=0, SO IS              
         BNE   VR052A                MAX LEN FROM RECORD ALSO ZERO?             
         BE    VR058                  YES, ALL IS WELL                          
*                                                                               
VR050A   TM    FVIIND,FVINUM       CHECK NUMERIC                                
         BO    VR050B                                                           
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VRXITNO                                                          
VR050B   CLI   SCFULL+3,0          CHECK INPUT IS NOT ZERO                      
         BNE   VR052                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VRXITNO                                                          
*                                                                               
VR052    MVC   MAXLEN,SCFULL+3     HOLD ONTO INPUT                              
         CLC   SVMXLEN,MAXLEN      IS MAX LEN DIFF FROM RECORD?                 
         BE    VR058                NO, SO ALL IS WELL                          
VR052A   TM    MESGFLG,MFNEWQ      WAS THIS A BRAND NEW MSG?                    
         BO    VR058                YES, SO ALL IS WELL TOO                     
         TM    SVFLAG,SFMXLCHQ     WAS MAX LEN ALREADY CHANGED IN               
         BZ    *+14                 LAST TRANSACTION -- NO                      
         CLC   SVPVMXLN,MAXLEN     WAS IT CHANGED AGAIN?                        
         BE    VR058A               NOPE, DON'T DISPLAY WARNING MSG             
         SPACE 1                                                                
         MVC   SVPVMXLN,MAXLEN     HOLD ONTO RECENT INPUT                       
         OI    SVFLAG,SFMXLCHQ     MAX LENGTH HAS CHANGED                       
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMWRN',=AL2(CW#RPMXL)),   +        
               RR=APRELO                                                        
         CLI   SVMXLEN,0           IF "OLD" MAX LEN IS ZERO,                    
         BE    VR052B               THEN DON'T DISPLAY IT IN WRNING MSG         
         SPACE 1                                                                
         ZIC   R0,SVMXLEN                                                       
         EDIT  (R0),(3,APFULL),WRK=APWORK,DUB=APDUB,ALIGN=LEFT                  
         LA    R0,APFULL                                                        
         LA    R1,APPARM                                                        
         USING GETTXTD,R1                                                       
         MVI   GTLTXT,3                                                         
         STCM  R0,7,GTATXT                                                      
         DROP  R1                                                               
VR052B   B     VRXITNO                                                          
*                                                                               
VR058    NI    SVFLAG,X'FF'-SFMXLCHQ FLAG THAT MAX LEN DID NOT CHANGE           
VR058A   CLI   MAXLEN,0                                                         
         BE    VR060                                                            
         MVI   GMPRGEL,GMPRGELC                                                 
         MVC   GMPRGLMT,MAXLEN     MOVE TO ELEMENT                              
         L     R1,SCFULL                                                        
         S     R1,LENCHK                                                        
         ST    R1,LENCHK           LENCHK=MAXLEN-(L'MSG-1)                      
*                                                                               
VR060    CLI   GMPRGEL,GMPRGELC    CHECK IF ANY PROG ELEMENTS INPUT             
         BNE   VR070                                                            
         MVI   GMPRGELL,GMPRGLNQ                                                
         OI    GMFELEM,GMPRGQ      SET PRG ELEMENT FLAGS                        
         LA    R2,IOKEY                                                         
         OI    GMDELEM,GMPRGQ      SET DIRECTORY FLAGS                          
         L     R2,AIOAREA1                                                      
         GOTO1 AADDELS,GMSGD                                                    
         DROP  R3                                                               
*                                                                               
*---------------------------- EQUATE NAME ----------------------------*         
*                                                                               
VR070    CLI   APACTN,ACTADD       ACTION=ADD?                                  
         BNE   VR100                NO                                          
*                                                                               
         CLC   SVEQNAME,SPACES     IS THERE AN EQUATE NAME?                     
         BNE   VR100                YES                                         
*                                                                               
** ADDING BRAND NEW MESSAGE (NEW EQUATE NAME)                                   
*                                                                               
         MVI   FVMINL,4            MAKE SURE L(INPUT)>=4                        
         GOTO1 AFVAL,DCTQNMEH                                                   
         BNE   VRXITNO                                                          
         CLI   FVIFLD+2,C'#'       3RD CHARACTER MUST BE '#'                    
         BE    VR080                                                            
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#3RDC#)),   +        
               RR=APRELO                                                        
         B     VRXITNO                                                          
*                                                                               
VR080    CLC   SVSYPRFX,FVIFLD      SHOULD EQUAL SYSTEM PREFIX                  
         BE    VR080A                                                           
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BPRFX)),   +        
               RR=APRELO                                                        
         B     VRXITNO                                                          
*                                                                               
VR080A   CLI   GMKSYS,15           DISALLOW 'GE#GEN' AS EQUATE NAME             
         BNE   VR090                                                            
         CLI   FVILEN,6            GE#GEN IS 6 CHARS LONG                       
         BNE   VR090                                                            
         CLC   FVIFLD(6),=C'GE#GEN'                                             
         BNE   VR090                                                            
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#DDRSV)),   +        
               RR=APRELO                                                        
         B     VRXITNO                                                          
*                                                                               
VR090    BAS   RE,CHKEQPSV         EQUATE NAME SHOULD NOT BE                    
         BNE   VRXITNO              DEFINED YET                                 
         MVC   SVEQNAME,FVIFLD     NEW EQUATE NAME                              
*                                                                               
** EQUATE NAME ALREADY DEFINED (SVEQNAME<>SPACES)                               
*                                                                               
VR100    MVI   FVMINL,0            MAKE SURE THAT THE                           
         GOTO1 AFVAL,DCTQNMEH       INPUT OF EQUNAME FIELD IS THE               
         CLC   SVEQNAME,FVIFLD      SAME AS THE DEFINED EQUATE NAME             
         BE    VR110                                                            
*                                                                               
         MVC   APWORK(L'SVEQNAME),SVEQNAME                                      
         GOTO1 DISPFLD,DCTQNMEH                                                 
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMINF',=AL2(CI#QDFND)),   +        
               RR=APRELO                                                        
         B     VRXITNO                                                          
*                                                                               
** BUILD EQUATE ELEMENT                                                         
*                                                                               
VR110    XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING GMQSYD,R3                                                        
         MVI   GMQSYEL,GMQSYELC    ELEMENT CODE                                 
         MVI   GMQSYELL,GMQSYLNQ   ELEMENT LENGTH                               
         MVC   GMQSYSYM,SVEQNAME   EQUATE NAME                                  
         DROP  R3                                                               
*                                                                               
*------------------------------ RFP RULE -----------------------------*         
*                                                                               
VR130    MVI   RFPRULE,0           RESET RFP RULE TO ZERO                       
         GOTO1 AFVAL,DCTRFPH                                                    
*                                                                               
         CLI   FVILEN,0            WAS THERE ANY INPUT?                         
         BNE   VR130A               YEP                                         
         CLI   SVRFP,0             WAS THERE DATA IN RFP FIELD?                 
         BNE   VR130BA              YES, BUT NOW USER WANTS RFP# TO = 0         
         NI    SVFLAG,X'FF'-SFRFPCHQ                                            
         B     VR140                NOPE, SO RFP=0 DIDN'T CHANGE                
*                                                                               
VR130A   TM    FVIIND,FVINUM       INPUT MUST BE NUMERICS                       
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VRXITNO                                                          
*                                                                               
         SR    R1,R1               NUMBER MUST BE BETWEEN 1                     
         LA    RF,255               AND 255                                     
         C     R1,SCFULL                                                        
         BNL   *+12                ERROR, R1 >= SCFULL                          
         C     RF,SCFULL                                                        
         BNL   VR130B              RF>= SCFULL, NO ERROR                        
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BRULE)),   +        
               RR=APRELO                                                        
         B     VRXITNO                                                          
*                                                                               
VR130B   MVC   RFPRULE,SCFULL+3                                                 
VR130BA  CLC   SVRFP,RFPRULE       DID RFP# CHANGE?                             
         BE    VR130D               NO : CHECK NEXT RFP CRITERION               
         TM    MESGFLG,MFNEWQ       YES: BUT WAS MSG BRAND NEW?                 
         BO    VR130D                YES: CHECK NEXT RFP CRITERION              
         TM    SVFLAG,SFCHRFPQ       NO : IS RFP# CHANGEABLE?                   
         BO    VR130BB                YES: MOVE ON                              
*                                     NO : DISPLAY ERROR MSG                    
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#CCRFP)),   +        
               RR=APRELO                                                        
         B     VRXITNO                                                          
         SPACE 1                                                                
VR130BB  TM    SVFLAG,SFRFPCHQ     RFP# CHNGD DURING LAST TRNSCTION?            
         BZ    *+14                 NO : SET WARNING MSG                        
         CLC   SVPVRFP,RFPRULE      YES: WAS IT CHANGED AGAIN?                  
         BE    VR130E                NO : DON'T SET WARNING MSG                 
*                                                                               
VR130C   MVC   SVPVRFP,RFPRULE     UPDATE LAST TRNSCTION'S RFP#                 
         OI    SVFLAG,SFRFPCHQ     TURN ON RFP-CHANGED FLAG                     
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMWRN',=AL2(CW#RPRFP)),   +        
               RR=APRELO                                                        
         CLI   SVRFP,0             IF SVRFP=0,                                  
         BE    VR130CC              THEN DON'T APPEND RFP#                      
         SPACE 1                                                                
         ZIC   R0,SVRFP            APPEND EXISTING RFP#                         
         CVD   R0,APDUB             AT END OF WARNING MSG                       
         UNPK  APFULL(3),APDUB                                                  
         OI    APFULL+2,X'F0'      TURN ON ZONE                                 
VR130CA  CLI   APFULL,C'0'         REMOVE LEADING ZEROES                        
         BNE   VR130CB                                                          
         MVC   APFULL(2),APFULL+1                                               
         MVI   APFULL+2,C' '                                                    
         B     VR130CA                                                          
VR130CB  LA    R0,APFULL                                                        
         LA    R1,APPARM                                                        
         USING GETTXTD,R1                                                       
         MVI   GTLTXT,3                                                         
         STCM  R0,7,GTATXT                                                      
         DROP  R1                                                               
VR130CC  B     VRXITNO                                                          
*                                                                               
VR130D   NI    SVFLAG,X'FF'-SFRFPCHQ   RFP# DID NOT CHANGE                      
VR130E   CLI   GMSGTXT,1             # OF LITERALS IS LIMITED TO ONE            
         BE    VR130F                                                           
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#RFP1L)),   +        
               RR=APRELO                                                        
         LA    R1,DCTMSG1H         SET CURSOR POSITION                          
         ST    R1,FVADDR                                                        
         B     VRXITNO                                                          
*                                                                               
VR130F   MVC   BYTE,SVLANG         SET LANGUAGE FOR UPCASE TABLE                
         BAS   RE,GTAUPTAB         GET A(UPCASE TABLE)                          
         L     RF,AUPCASE          RF-->UPCASE TABLE                            
*                                                                               
         ZIC   RE,GMSGELL          L(MESSAGE ELEMENT)                           
         LA    R1,GMSGFXDL+3                                                    
         SR    RE,R1               RE=L(LITERAL)-1                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    GMSGTXT+2(0),0(RF)       CONVERT TO UPPER CASE                   
*                                                                               
         CLI   RFPRULE,0           NEED TO TURN ON RFP FLAG?                    
         BE    VR140                NOPE                                        
         OI    GMFELEM,GMRFPQ      TURN ON RFP FLAG                             
         LA    R2,IOKEY                                                         
         OI    GMDELEM,GMRFPQ      TURN ON RFP FLAG                             
         L     R2,AIOAREA1                                                      
*                                                                               
VR140    LA    R3,APELEM                                                        
         USING GMQSYD,R3                                                        
         MVC   GMQRFP,RFPRULE      STORE RFP IN RECORD                          
         DROP  R3                                                               
*                                                                               
         GOTO1 AADDELS,GMSGD       ADD ON EQUNAME ELEMENT                       
*                                                                               
*----------------------------- EXTRA EQUATES -------------------------*         
*                                                                               
VR150    LA    R4,DCTXEQUH         1ST EXTRA EQUATE LINE                        
         LA    R3,APELEM                                                        
         USING GMTXTD,R3                                                        
         XC    APELEM,APELEM       INITIALISE ELEMENT                           
         MVI   GMTXTEL,GMTXTELC                                                 
         MVI   GMTXTLNO,1          RELATIVE LINE NUMBER                         
*                                                                               
VR150A   GOTO1 AFVAL,(R4)                                                       
         BNE   VR150B              IGNORE BLANK LINES                           
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GMTXTLIN(0),FVIFLD                                               
         AH    R1,=Y(GMTXTFXD+1)   FULL ELEMENT LENGTH                          
         STC   R1,GMTXTELL                                                      
         GOTO1 AADDELS,GMSGD                                                    
         XC    GMTXTLIN(L'DCTXEQU),GMTXTLIN                                     
*                                                                               
VR150B   LA    R1,DCTXEQXH         CHECK FOR MORE LINES                         
         CR    R1,R4                BY COMPARING ADDRESSES                      
         BNH   VR160                                                            
VR150C   ZIC   R1,0(R4)            BUMP TO NEXT TWA FIELD                       
         AR    R4,R1                                                            
         TM    1(R4),X'20'         BYPASS PROTECTED FIELDS                      
         BO    VR150C                                                           
         SPACE 1                                                                
         ZIC   R1,GMTXTLNO         REDEFINE ELEMENT LINE NUMBER                 
         LA    R1,1(R1)                                                         
         STC   R1,GMTXTLNO                                                      
         B     VR150A                                                           
*                                                                               
*------------------------------ COMMENTS -----------------------------*         
*                                                                               
VR160    LA    R4,DCTCMMTH         1ST COMMENT LINE                             
         ZIC   R1,GMTXTLNO         REDEFINE ELEMENT LINE NUMBER                 
         LA    R1,1(R1)            LEAVE ELEMENT CODE, ETC.                     
         STC   R1,GMTXTLNO          IN APELEM FOR THIS PROCESS                  
*                                                                               
VR160A   GOTO1 AFVAL,(R4)                                                       
         BNE   VR160B              IGNORE BLANK LINES                           
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GMTXTLIN(0),FVIFLD                                               
         AH    R1,=Y(GMTXTFXD+1)   FULL ELEMENT LENGTH                          
         STC   R1,GMTXTELL                                                      
         GOTO1 AADDELS,GMSGD                                                    
         XC    GMTXTLIN(L'DCTCMMT),GMTXTLIN                                     
*                                                                               
VR160B   LA    R1,DCTCMMXH         CHECK FOR MORE LINES                         
         CR    R1,R4                BY COMPARING ADDRESSES                      
         BNH   VR170                                                            
VR160C   ZIC   R1,0(R4)            BUMP TO NEXT TWA FIELD                       
         AR    R4,R1                                                            
         TM    1(R4),X'20'         BYPASS PROTECTED FIELDS                      
         BO    VR160C                                                           
         SPACE 1                                                                
         ZIC   R1,GMTXTLNO         REDEFINE ELEMENT LINE NUMBER                 
         LA    R1,1(R1)                                                         
         STC   R1,GMTXTLNO                                                      
         B     VR160A                                                           
         DROP  R3                                                               
*                                                                               
*--------------------------------- OK? -------------------------------*         
*                                                                               
VR170    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DCTOKAYH      MAKE SURE THEY'RE SURE                       
         BNE   VRXITNO              BEFORE PROCEEDING                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   FVIFLD,C'Y'                                                      
         BNE   VRXITNO                                                          
         MVI   DCTOKAY,0           CLEAR OUT FIELD FOR NEXT TRANSACTION         
         OI    DCTOKAYH+6,X'80'                                                 
*                                                                               
*-------------------------- ACTIVITY ELEMENT -------------------------*         
*                                                                               
         GOTO1 ASETACT,GMSGD       DEFINE ACTIVITY ELEMENT                      
*                                                                               
*------------------------------- OUTPUT ------------------------------*         
*                                                                               
** RE-READ FILE FIRST                                                           
*                                                                               
VR190    CLI   APACTN,ACTADD       IS ACTION ADD?                               
         BNE   *+12                 NOPE, RE-READ FILE                          
         TM    SVFLAG,SFADDELQ      YEP, ADDING A DELETED RECORD?               
         BZ    VR190A                NOPE, NO NEED TO RE-READ FILE              
*                                                                               
         MVC   APRECKEY,IOKEY      SAVE UPDATED KEY                             
         MVC   IOKEY,MYSVKEY       PUT IN OLD KEY FOR RE-READ                   
         LA    R1,IOGENDIR+IO2+IORDUPD                                          
         GOTO1 AIO                                                              
         BE    VR192               IF THERE IS AN I/O ERROR,                    
         TM    SVFLAG,SFADDELQ      IT MAY BE BECAUSE USER IS                   
         BZ    *+12                 TRYING TO ADD A DELETED RECORD,             
         TM    IOERR,IOERRS-IOEDEL  IN WHICH CASE, DELETE IS THE                
         BZ    VR192                ONLY VALID ERROR                            
         DC    H'0'                                                             
VR192    LA    R1,IOGENFIL+IO2+IOGETRUP                                         
         GOTO1 AIO                                                              
         BE    VR192A              IF THERE IS AN I/O ERROR,                    
         TM    SVFLAG,SFADDELQ      IT MAY BE BECAUSE USER IS                   
         BZ    *+12                 TRYING TO ADD A DELETED RECORD,             
         TM    IOERR,IOERRS-IOEDEL  IN WHICH CASE, DELETE IS THE                
         BZ    VR192A               ONLY VALID ERROR                            
         DC    H'0'                                                             
VR192A   MVC   IOKEY,APRECKEY               RESTORE UPDATED KEY                 
         MVC   IOKEY+(GMDDA-GMSGD)(L'IODA),IODA     PUT IN DISK ADDRESS         
*                                                                               
** OUTPUT TO GENFIL                                                             
*                                                                               
VR190A   LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD       IF ACTION IS NOT ADD                         
         BNE   *+12                 THEN DO A PUTREC,                           
         TM    SVFLAG,SFADDELQ      ELSE, IF DOING A PURE ADD                   
         BZ    *+8                  THEN DO THE ADDREC                          
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APRECDA,IODA        SAVE RECORD ADDRESS                          
         LA    R2,IOKEY                                                         
*                                                                               
** OUTPUT TO GENDIR                                                             
*                                                                               
         CLI   APACTN,ACTADD       DON'T UPDATE GENDIR ON A PURE ADD,           
         BNE   *+12                                                             
         TM    SVFLAG,SFADDELQ                                                  
         BZ    VR190C               ADD PASSIVE POINTER INSTEAD                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APRECKEY,IOKEY      SAVE UPDATED KEY                             
         TM    SVFLAG,SFADDELQ                                                  
         BNO   VR200               DON'T ADD PSV PTR IF ACTION <> ADD           
*                                                                               
*** ADD PASSIVE POINTER                                                         
*                                                                               
VR190C   MVC   APHALF,GMKMSG       HOLD ONTO MSG#                               
         MVC   APBYTE,GMKLANG      HOLD ONTO LANGUAGE CODE                      
         XC    IOKEY,IOKEY         BUILD PASSIVE KEY HERE                       
         MVI   GQKREC,GQKRECQ      RECORD TYPE                                  
         MVC   GQKQNAME,SVEQNAME   EQUATE NAME                                  
         MVC   GQKMNUM,APHALF      MSG#                                         
         SPACE 1                                                                
         TM    MESGFLG,MFX40Q      NEED TO TURN ON X'4000' BIT?                 
         BZ    VR190CA              NOPE                                        
         OI    GQKMNUM,X40BITQ                                                  
         SPACE 1                                                                
VR190CA  MVC   GQKLANG,APBYTE      LANGUAGE CODE                                
         MVC   GQKRFP,RFPRULE      RFP RULE # (0 IF NONE)                       
         L     R1,AIOAREA1                                                      
         MVC   GMDELEM,(GMFELEM-GMKEY)(R1)                                      
         MVC   GMDDA,APRECDA       DISK ADDRESS                                 
         SPACE 1                                                                
         GOTO1 AIO,IOGENDIR+IORDD+IO2                                           
         BE    VR190CB                                                          
         TM    IOERR,IOERRS-IOEDEL-IOERNF  DELETE & NOT FOUND ARE SAFE          
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,IOKEYSAV      RESTORE KEY                                  
         TM    IOERR,IOERNF        IF RECORD NOT FOUND                          
         BO    VR190CB              THEN ADD IT                                 
         GOTO1 AIO,IOGENDIR+IOWRITE                                             
         BE    VR190CC                                                          
         DC    H'0'                                                             
*                                                                               
VR190CB  LA    R1,IOGENDIR+IOADD                                                
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
VR190CC  DS    0H                                                               
         CLI   RFPRULE,0           IF NO RFP RULE,                              
         BE    VR190CX              DON'T ADD RFP PASSIVE                       
                                                                                
         DS    0H                  ADD RFP PASSIVE POINTER                      
         XC    IOKEY,IOKEY                                                      
         MVI   GRKREC,GRKRECQ                                                   
         MVC   GRKSYS,APRECKEY+(GMKSYS-GMKEY)                                   
         MVC   GRKMNUM,APHALF                                                   
         TM    MESGFLG,MFX40Q                                                   
         BZ    *+8                                                              
         OI    GRKMNUM,X40BITQ                                                  
         MVC   GRKLANG,APBYTE                                                   
         MVC   GRKRFP,RFPRULE                                                   
         MVC   GRKQNAME,SVEQNAME                                                
         L     R1,AIOAREA1                                                      
         MVC   GMDELEM,(GMFELEM-GMSGD)(R1)                                      
         MVC   GMDDA,APRECDA                                                    
                                                                                
         GOTO1 AIO,IOGENDIR+IORDD+IO2                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOERRS-IOEDEL-IOERNF                                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,IOKEYSAV                                                   
         LA    R1,IOGENDIR+IOWRITE WRITE RFP PASSIVE IF IT WERE                 
         TM    IOERR,IOEDEL         MARKED FOR DELETION                         
         BO    VR190CD                                                          
         LA    R1,IOGENDIR+IOADD   ADD RFP PASSIVE IF IT                        
         TM    IOERR,IOERNF         ACTUALLY WAS NOT FOUND                      
         BO    VR190CD                                                          
         DC    H'0'                                                             
                                                                                
VR190CD  DS    0H                  MARK FILE W/ RFP PASSIVE                     
         GOTO1 AIO                                                              
         BE    VR190CX                                                          
         DC    H'0'                                                             
                                                                                
VR190CX  MVC   IOKEY,APRECKEY        RESTORE UPDATED KEY                        
                                                                                
*                                                                               
** UPDATE CONTROL MSG# RECORD (ONLY W/. BRAND NEW MSG)                          
*                                                                               
         CLC   SVHIMSG,GMKMSG      IF THEY ARE EQUAL                            
         BE    VR194                CONTROL MSG# HAVE TO BE UPDATED             
         BH    VR200                                                            
         TM    MESGFLG,MFNEWQ      IF CONTROL MSG# IS LOW, THEN                 
         BZ    VR200                IT BETTER NOT BE A BRAND                    
         DC    H'0'                 NEW MESSAGE                                 
*                                                                               
VR194    DS    0H                                                               
         MVC   APRECKEY,IOKEY      SAVE UPDATED KEY                             
         XC    MESGNUM,MESGNUM     IN CASE THERE IS MESSAGE TO DISPLAY          
         XC    GMKMSG(L'GMKMSG+L'GMKLANG),GMKMSG  MSG# = 0 IS CTRL RECD         
         LA    R1,IOGENDIR+IO2+IORDUP                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                RECORD SHOULD BE THERE                       
         LA    R1,IOGENFIL+IO2+IOGETRUP                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                RECORD SHOULD BE THERE                       
*                                                                               
         L     R3,IOADDR           GET CONTROL MSG# ELEM                        
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMCTLELC                                                  
         GOTO1 AGETELS,(R3)                                                     
         ICM   R3,15,APPARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                BETTER BE A CONTROL MSG# ELEM                
         MVC   APWORK(GMCTLLNQ),0(R3)  SAVE CONTENTS OF ELEMENT                 
*                                                                               
         L     R3,IOADDR           DELETE CONTROL MSG# ELEM                     
         GOTO1 ADELELS,(R3)                                                     
                                                                                
         DS    0H                  RE-BUILD CONTROL ELEMENT                     
         LA    R3,APWORK                                                        
         USING GMCTLD,R3                                                        
         TM    GMCTLFLG,GCFCNUM2                                                
         BO    VR193B                                                           
                                                                                
         DS    0H                  CONTROL MSG#1                                
         ZICM  RF,GMCTLNM1,(3)                                                  
         LA    RF,1(RF)             INCREMENT CONTROL #                         
         BAS   RE,SKP40BIT                                                      
         CLM   RF,3,=AL2(NUMLOWQ)  EXCEED MAX # OF "LOW" ENTRIES?               
         BH    VR193A               YEP                                         
         CLM   RF,3,BNDRY          REACHED BOUNDARY YET?                        
         BNL   VR193A               YEP                                         
         STCM  RF,3,GMCTLNM1        NO, UPDATE CONTROL #                        
         B     VR195A                AND GO UPDATE FILE                         
VR193A   OI    GMCTLFLG,GCFCNUM2   FLAG TO USE 2ND CONTROL #                    
         B     VR195A               AND UPDATE FILE                             
*                                                                               
VR193B   DS    0H                  CONTROL MSG#2                                
         ZICM  RF,GMCTLNM2,(3)                                                  
         LA    RF,1(RF)                                                         
         BAS   RE,SKP40BIT                                                      
         LR    RE,RF                                                            
         SH    RE,BNDRY                                                         
         CLM   RE,3,=AL2(NUMHIQ)   EXCEED MAX # OF "HIGH" ENTRIES?              
         BH    VR193C               YEP                                         
         CLM   RF,3,=X'FFFF'       REACHED 2ND BOUNDARY YET?                    
         BH    VR193C               YEP                                         
         STCM  RF,3,GMCTLNM2        NO, UPDATE CONTROL #                        
         B     VR195A                AND GO UPDATE FILE                         
VR193C   OI    GMCTLFLG,GCFMXHIQ                                                
         MVC   MESGNUM,=AL2(CI#DDFUL)                                           
         B     VR195A              GO UPDATE FILE                               
         DROP  R3                                                               
*                                                                               
VR195A   DS    0H                                                               
         MVC   APELEM(GMCTLLNQ),APWORK                                          
         L     R1,IOADDR                                                        
         GOTO1 AADDELS,(R1)                                                     
*                                                                               
         LA    R1,IOGENFIL+IO2+IOPUTREC    PUT RECORD BACK                      
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGENDIR+IO2+IOWRITE     WRITE RECORD BACK                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,APRECKEY              RESTORE ACTIVE KEY                   
                                                                                
         OC    MESGNUM,MESGNUM                                                  
         BZ    VR300                                                            
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMINF',MESGNUM),          +        
               RR=APRELO                                                        
         LA    R1,DCTSYSH                                                       
         ST    R1,APCURSOR         FORCE CURSOR TO SYSTEM FIELD                 
         B     VRXITNO              AND DISPLAY ERROR MESSAGE                   
*                                                                               
** IN CASE RFP#, DISK-RESIDENT STATUS, OR MAX LENGTH CHANGE                     
*                                                                               
VR200    TM    SVFLAG,SFRFPCHQ+SFMXLCHQ                                         
         BZ    VR300               NO CHANGES NEEDED                            
*                                                                               
*** UPDATE CHANGE IN OTHER RECORDS                                              
*                                                                               
VR210    MVC   IOKEY,APRECKEY      CHANGE OTHER RECORDS                         
         MVI   GMKLANG,0            WITH SAME SYS,TYP,MSG#                      
         LA    R1,IOGENDIR+IO2+IOHIUP                                           
         B     *+8                                                              
VR225    LA    R1,IOGENDIR+IO2+IOSQUP                                           
         GOTO1 AIO                                                              
         BE    VR225A                                                           
         TM    IOERR,IOEDEL        DELETE IS ONLY SAFE ERROR                    
         BO    *+6                                                              
         DC    H'0'                                                             
VR225A   CLC   IOKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BNE   VR270               EXIT CHANGING RFP#                           
*                                                                               
         LA    R1,IOGENFIL+IO2+IOGETRUP                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         MVC   APBYTE,SVLANG       DON'T UPDATE THE SAME RECORD THAT            
         XI    APBYTE,X'FF'         IS IN IOAREA1                               
         CLC   GMKLANG,APBYTE                                                   
         BE    VR240               JUST ADJUST THE PASSIVE POINTERS             
*                                                                               
**** REPLACE OLD RFP# WITH NEW RFP#                                             
*                                                                               
VR225C   TM    SVFLAG,SFRFPCHQ     UPDATE RFP#?                                 
         BZ    VR225D               NO NEED TO                                  
         XC    APELEM,APELEM       DELETE ELEM WITH THE OLD RFP#                
         MVI   APELEM,GMQSYELC                                                  
         GOTO1 ADELELS,(R2)                                                     
         XC    APELEM,APELEM       ADD ELEM WITH NEW RFP#                       
         LA    R3,APELEM                                                        
         USING GMQSYD,R3                                                        
         MVI   GMQSYEL,GMQSYELC     ELEM CODE                                   
         MVI   GMQSYELL,GMQSYLNQ    ELEM LENGTH                                 
         MVC   GMQSYSYM,SVEQNAME    EQUATE NAME                                 
         MVC   GMQRFP,RFPRULE       NEW RFP#                                    
         DROP  R3                                                               
         GOTO1 AADDELS,(R2)                                                     
*                                                                               
**** TURN MSG TEXT TO UPPERCASE                                                 
*                                                                               
         MVC   BYTE,GMKLANG        SET LANGUAGE FOR UPCASE TABLE                
         XI    BYTE,X'FF'                                                       
         BAS   RE,GTAUPTAB         GET A(UPCASE TABLE)                          
         L     RF,AUPCASE          RF-->UPCASE TABLE                            
         ZIC   RE,GMSGELL          RE=L(MESSAGE ELEMENT)                        
         LA    R1,GMSGFXDL+3                                                    
         SR    RE,R1               RE=L(LITERAL) - 1                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    GMSGTXT+2(0),0(RF)                                               
*                                                                               
**** ADJUST ELEMENT FLAG                                                        
*                                                                               
         NI    GMFELEM,X'FF'-GMRFPQ    ASSUME NO RFP RULE                       
         LA    R2,IOKEY                                                         
         NI    GMDELEM,X'FF'-GMRFPQ                                             
         L     R2,AIOAREA2                                                      
*                                                                               
         CLI   RFPRULE,0                                                        
         BE    VR225D                                                           
         OI    GMFELEM,GMRFPQ                                                   
         LA    R2,IOKEY                                                         
         OI    GMDELEM,GMRFPQ                                                   
         L     R2,AIOAREA2                                                      
*                                                                               
**** REPLACE OLD MAXLEN WITH NEW MAXLEN                                         
*                                                                               
VR225D   TM    SVFLAG,SFMXLCHQ     UPDATE MAXLEN?                               
         BZ    VR230                NO NEED TO                                  
         MVI   APWORK,C' '         INIT APWORK TO BLANKS                        
         MVC   APWORK+1(L'APWORK-1),APWORK                                      
         XC    APELEM,APELEM       GET ELEM WITH THE OLD RFP#                   
         MVI   APELEM,GMPRGELC                                                  
         GOTO1 AGETELS,(R2)                                                     
         ICM   R3,15,APPARM        PULL OUT INFO FROM ELEMENT,                  
         BZ    VR225DA              IF THERE IS AN ELEMENT                      
         USING GMPRGD,R3                                                        
         MVC   APWORK(L'GMPRGPRG),GMPRGPRG                                      
         MVC   APWORK+L'GMPRGPRG(L'GMPRGSUB),GMPRGSUB                           
         DROP  R3                                                               
         GOTO1 ADELELS,(R2)        DELETE ELEMENT                               
VR225DA  XC    APELEM,APELEM       ADD ELEM WITH NEW MAXLEN                     
         LA    R3,APELEM                                                        
         USING GMPRGD,R3                                                        
         MVI   GMPRGEL,GMPRGELC     ELEM CODE                                   
         MVI   GMPRGELL,GMPRGLNQ    ELEM LENGTH                                 
         MVC   GMPRGPRG,APWORK      PROGRAM NAME                                
         MVC   GMPRGSUB,APWORK+L'GMPRGPRG   SUB ID                              
         MVC   GMPRGLMT,MAXLEN      NEW MAXLEN                                  
         DROP  R3                                                               
         GOTO1 AADDELS,(R2)                                                     
         SPACE 1                                                                
         OI    GMFELEM,GMPRGQ      SET ELEMENT FLAGS                            
         LA    R2,IOKEY                                                         
         OI    GMDELEM,GMPRGQ                                                   
         L     R2,AIOAREA2                                                      
*                                                                               
VR230    GOTO1 ASETACT,GMSGD       SET ACTIVITY ELEMENT                         
         LA    R1,IOGENFIL+IO2+IOPUT                                            
         GOTO1 AIO                                                              
         LA    R1,IOGENDIR+IO2+IOWRITE                                          
         GOTO1 AIO                                                              
*                                                                               
**** GET PASSIVE RECORD FOR MODIFICATION                                        
*                                                                               
VR240    TM    SVFLAG,SFRFPCHQ     MODIFY PASSIVES ON RFP CHANGE ONLY           
         BZ    VR225               GET NEXT RECORD                              
         MVC   MYSVKEY,IOKEY       HOLD KEY FOR CHANGING RFP#                   
         LR    R3,R2               R3-->IOAREA2                                 
         MVC   APHALF,GMKMSG       HOLD ONTO MSG#                               
         MVC   APBYTE,GMKLANG      HOLD ONTO LANGUAGE CODE                      
         XC    IOKEY,IOKEY         GET PASSIVE RECORD                           
         LA    R2,IOKEY                                                         
         MVI   GQKREC,GQKRECQ      RECORD TYPE                                  
         MVC   GQKQNAME,SVEQNAME   EQUATE NAME                                  
         MVC   GQKMNUM,APHALF      MSG#                                         
         SPACE 1                                                                
         TM    MESGFLG,MFX40Q      NEED TO TURN ON X'4000' BIT?                 
         BZ    VR240A               NOPE                                        
         OI    GQKMNUM,X40BITQ                                                  
         SPACE 1                                                                
VR240A   MVC   GQKLANG,APBYTE      LANGUAGE CODE                                
         MVC   GQKRFP,SVRFP        RFP SYMBOL                                   
*                                                                               
         LA    R1,IOGENDIR+IO3+IORD                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                PSV RECORD HAD BETTER BE THERE               
         TM    SVFLAG,SFRFPCHQ     ADDING PASSIVE REQUIRED ON                   
         BO    VR250B               CHANGES TO RFP                              
*                                                                               
VR250A   MVC   GMDELEM,(GMFELEM-GMSGD)(R3)   ONLY DISK RESIDENCY                
         LA    R1,IOGENDIR+IO3+IOWRITE        STATUS CHANGED, UPDATE            
         GOTO1 AIO                            AND WRITE RECORD BACK             
         B     VR268                                                            
*&&DO                                                                           
         B     VR260                                                            
*&&                                                                             
*                                                                               
VR250B   OI    GMDSTAT,X'80'       MARK OLD PSV FOR DELETION                    
         LA    R1,IOGENDIR+IO3+IOWRITE                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APRECDA,GMDDA       HOLD ONTO DISK ADDRESS                       
         MVC   GQKRFP,RFPRULE      ADD PASSIVE WITH NEW RFP                     
         MVI   GMDSTAT,0           CLEAR STATUS BYTE                            
         LA    R1,IOGENDIR+IO3+IORDD+IOLOCK                                     
         GOTO1 AIO                                                              
         BNE   *+6                 RECORD SHOULD NOT EXIST                      
         DC    H'0'                 OR SHOULD'VE BEEN DELETED                   
         TM    IOERR,IOEDEL        MARKED FOR DELETION?                         
         BNZ   VR255                YEP, SO DO A RESTORE                        
*                                                                               
         MVC   IOKEY,IOKEYSAV      NOT DEL, THEREFORE NO REC                    
         MVC   GMDELEM,(GMFELEM-GMSGD)(R3)   SET ELEMENT FLAG                   
         MVC   GMDDA,APRECDA       SET DISK ADDRESS                             
         LA    R1,IOGENDIR+IO3+IOADD                                            
         GOTO1 AIO                                                              
         BE    VR260                                                            
         DC    H'0'                                                             
*                                                                               
VR255    NI    GMDSTAT,X'FF'-X'80'      UNMARK DELETION                         
         MVC   GMDELEM,(GMFELEM-GMSGD)(R3)    SET ELEMENT FLAG                  
         MVC   GMDDA,APRECDA            SET DISK ADDRESS                        
         LA    R1,IOGENDIR+IO3+IOWRITE                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
VR260    DS    0H                  PROCESS RFP PASSIVE POINTERS                 
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         MVI   GRKREC,GRKRECQ              RECORD TYPE                          
         MVC   GRKSYS,(GMKSYS-GMKEY)(R3)   MESSAGE SYSTEM #                     
         MVC   GRKMNUM,(GMKMSG-GMKEY)(R3)  MESSAGE NUMBER                       
         TM    MESGFLG,MFX40Q                                                   
         BZ    *+8                                                              
         OI    GRKMNUM,X40BITQ                                                  
         MVC   GRKLANG,(GMKLANG-GMKEY)(R3) LANGUAGE                             
         MVC   GRKRFP,SVRFP                                                     
         MVC   GRKQNAME,SVEQNAME                                                
*                                                                               
         CLI   SVRFP,0             IF THERE WERE NO RFP PREVIOUSLY,             
         BE    VR262                THERE IS NOTHING TO DELETE                  
                                                                                
         GOTO1 AIO,IOGENDIR+IO3+IORD       GET PASSIVE FOR OLD RFP              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APRECDA,GMDDA                HOLD ONTO DISK ADDRESS              
         OI    GMDSTAT,X'80'                MARK FOR DELETION                   
         GOTO1 AIO,IOGENDIR+IO3+IOWRITE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR262    DS    0H                  ADD PASSIVE FOR NEW RFP RULE #               
         CLI   RFPRULE,0                                                        
         BE    VR262X                                                           
                                                                                
         MVC   GRKRFP,RFPRULE                                                   
         GOTO1 AIO,IOGENDIR+IO3+IORDD+IOLOCK                                    
         TM    IOERR,IOERNF                                                     
         BO    VR262A                                                           
         TM    IOERR,IOEDEL                                                     
         BO    VR262B                                                           
         DC    H'0'                                                             
                                                                                
VR262A   DS    0H                  ADD RFP PASSIVE                              
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   GMDSTAT,0                                                        
         LA    R1,IOGENDIR+IO3+IOADD                                            
         B     VR262G                                                           
                                                                                
VR262B   DS    0H                  RESTORE RFP PASSIVE                          
         NI    GMDSTAT,X'FF'-X'80'                                              
         LA    R1,IOGENDIR+IO3+IOWRITE                                          
         B     VR262G                                                           
                                                                                
VR262G   DS    0H                                                               
         MVC   GMDELEM,(GMFELEM-GMSGD)(R3)                                      
         MVC   GMDDA,APRECDA                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
VR262X   EQU   *                                                                
                                                                                
*                                                                               
VR268    DS    0H                                                               
         MVC   IOKEY,MYSVKEY       RESTORE KEY FOR CHANGING RFP#                
         LA    R2,IOKEY                                                         
         LA    R1,IOGENDIR+IO3+IORD                                             
         GOTO1 AIO                                                              
         BE    VR225               BETTER BE NO ERRORS                          
         DC    H'0'                                                             
*                                                                               
VR270    NI    SVFLAG,X'FF'-SFRFPCHQ-SFCHRFPQ-SFMXLCHQ                          
         MVI   SVPVRFP,0           RESET FLAGS AND VALUES                       
         MVI   SVPVMXLN,0                                                       
         L     R2,AIOAREA1                                                      
         MVC   IOKEY,APRECKEY      RESTORE KEY OF UPDATED RECORD                
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
VR300    NI    MESGFLG,X'FF'-MFNEWQ  RESET FLAG                                 
         XC    APPARM(8*L'APPARM),APPARM       SET UP GETTXT BLOCK              
         LA    R1,APPARM                                                        
         MVI   8(R1),C'W'          WARNING                                      
         MVI   3(R1),1             RECORD ADDED BUT TOO LONG                    
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         MVI   3(R1),2             RECORD CHANGED BUT TOO LONG                  
*                                                                               
         ICM   R1,15,LENCHK        CHECK MSG LENGTH                             
         BP    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     *+10                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R1,GENACTH                                                       
         ST    R1,APCURSOR         FORCE CURSOR TO ACTION FIELD                 
*                                                                               
VRXITYES B     CCEQUAL                                                          
*                                                                               
VRXITNO  B     CCNEQUAL                                                         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============= ROUTINE TO DISPLAY KEY OF MESSAGE RECORD ==============*         
DISKEY   DS    0H                                                               
*                                                                               
         LA    R2,APRECKEY         APRECKEY MAY BE A PASSIVE KEY                
         GOTO1 VDMGR,APPARM,=C'DMREAD',=C'GENDIR',APRECKEY,APRECKEY             
         CLI   APPARM+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDMGR,APPARM,=C'GETREC',=C'GENFIL',GMDDA,AIOAREA2,      +        
               MYDMWORK                                                         
         L     R2,AIOAREA2                                                      
*                                                                               
*------------------------------- SYSTEM ------------------------------*         
*                                                                               
         MVC   APBYTE,GMKSYS                                                    
         CLI   GMKSYS,15                                                        
         BNE   *+8                                                              
         MVI   APBYTE,0                                                         
         GOTO1 ADISSYS,APBYTE      GET MESSAGE SYSTEM                           
         GOTO1 DISPFLD,DCTSYSH                                                  
*                                                                               
*-------------------------------- TYPE -------------------------------*         
*                                                                               
         CLI   GMKTYP,GMKTGDIC     CHECK FOR NEW DICT REFERENCE                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*-------------------------------- MSG# -------------------------------*         
*                                                                               
         ZICM  R1,GMKMSG,(3)                                                    
         EDIT  (R1),(6,DCTNUM),WRK=APWORK,DUB=APDUB,ALIGN=LEFT                  
         OI    DCTNUM,C'0'         FOR MESSAGE ZERO                             
*                                                                               
*------------------------------ LANGUAGE -----------------------------*         
*                                                                               
         MVC   APWORK(1),GMKLANG                                                
         XI    APWORK,X'FF'        INVERT LANGUAGE                              
         GOTO1 ADISLNG,APWORK      GET LANGUAGE TABLE ENTRY                     
*                                                                               
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'DCTLANG-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,DCTLANGH    DISPLAY FULL NAME                            
*                                                                               
DISKEYX  B     CCEQUAL             RETURN CC=0                                  
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*================= ROUTINE TO DISPLAY MESSAGE RECORD =================*         
DISREC   DS    0H                                                               
*                                                                               
         L     R2,AIOAREA1                                                      
         TWAXC DCTQNMEH                                                         
*                                                                               
*----------------------------- TRANSLATION ---------------------------*         
*                                                                               
         MVI   DCTTRNS,C'T'        ASSUME TRUE TRANSLATION                      
         TM    GMFELEM,GMTRNSQ                                                  
         BO    *+8                                                              
         MVI   DCTTRNS,C'U'        IT'S NOT TRUE TRANSLATION                    
*                                                                               
*---------------------------- MESSAGE TEXT ---------------------------*         
*                                                                               
         XC    MESGTXT,MESGTXT                                                  
         ZIC   R1,GMSGELL          NOTE MSG ELEM ALWAYS FIXED POSN              
         LA    R0,GMSGFXDL         R0=L(ELEMENT OVERHEAD)                       
         SR    R1,R0               R1=L(MESSAGE)                                
         STC   R1,LMESGTXT                                                      
         BCTR  R1,0                                                             
         EXMVC R1,MESGTXT,GMSGTXT                                               
*                                                                               
         OI    MESGFLG,MFTRUEQ     SET TRUE TRANS FLAG TO DISPLAY               
*                                   '>' START DELIMITER                         
         GOTO1 =A(SPCFRMT),APPARM,(R5),(R6),(R7),(RC),(1,0),           +        
               RR=APRELO                                                        
         NI    MESGFLG,X'FF'-MFTRUEQ                                            
*                                                                               
DREC100  ZIC   R1,LMESG1                                                        
         BCTR  R1,0                                                             
         EXMVC R1,DCTMSG1,MESG1                                                 
         CLI   LMESG2,0                                                         
         BE    DREC020                                                          
         ZIC   R1,LMESG2                                                        
         BCTR  R1,0                                                             
         EXMVC R1,DCTMSG2,MESG2                                                 
*                                                                               
*----------------- PROGRAM, SUB-REF, AND MAX LENGTH ------------------*         
*                                                                               
DREC020  XC    APELEM,APELEM                                                    
         MVI   APELEM,GMPRGELC     LOOK PROGRAM ELEMENT                         
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM        R3=A(PROGRAM ELEMENT)                        
         USING GMPRGD,R3                                                        
         BZ    DREC60                                                           
         MVC   DCTPRG,GMPRGPRG                                                  
         MVC   DCTSUB,GMPRGSUB                                                  
         EDIT  (B1,GMPRGLMT),(2,DCTLMT),WRK=APWORK,DUB=APDUB,ALIGN=LEFT         
         DROP  R3                                                               
*                                                                               
*--------------------------- EQUNAME & RFP ---------------------------*         
*                                                                               
DREC60   XC    APELEM,APELEM                                                    
         MVI   APELEM,GMQSYELC     LOOK FOR EQUNAME ELEMENT                     
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM        R3=A(TEXT ELEMENT)                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GMQSYD,R3                                                        
         MVC   DCTQNME,GMQSYSYM                                                 
         EDIT  (B1,GMQRFP),(3,DCTRFP),WRK=APWORK,DUB=APDUB,ALIGN=LEFT           
         DROP  R3                                                               
*                                                                               
*---------------------- COMMENTS & EXTRA EQUATES ---------------------*         
*                                                                               
DREC70   XC    APELEM,APELEM                                                    
         MVI   APELEM,GMTXTELC     LOOK FOR FULL-TEXT ELEMENT                   
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM        R3=A(TEXT ELEMENT)                           
         BZ    DISRECX             EXIT IF THERE IS NONE                        
         SPACE 1                                                                
         USING GMTXTD,R3                                                        
DREC70A  ZIC   RF,GMTXTLNO         PLACE TEXT ON SCREEN ACCORDING               
         OR    RF,RF                TO LINE NUMBER                              
         BNZ   *+6                                                              
         DC    H'0'                IT SHOULD NEVER BE ZERO                      
         LA    R4,DCTXEQUH         FIRST DISPLAY LINE                           
         LA    RE,DCTCMMXH         LAST DISPLAY LINE                            
DREC70B  BCTR  RF,0                ZERO-BASED                                   
         OR    RF,RF                                                            
         BZ    DREC73                                                           
DREC70C  ZIC   R1,0(R4)            GET TO THE GMTXTLNO'NTH LINE                 
         AR    R4,R1               BUMP TO NEXT FIELD                           
         TM    1(R4),X'20'         IF PROTECTED FIELD,                          
         BO    DREC70C              BYPASS IT                                   
         CR    R4,RE               MAKE SURE WE DON'T GO                        
         BNH   DREC70B              PASS THE LAST LINE                          
         DC    H'0'                                                             
*                                                                               
DREC73   ZIC   R1,GMTXTELL         R4-->RESPECTIVE DISPLAY LINE                 
         LA    R0,GMTXTFXD+1       PULL TEXT OUT FROM ELEMENT                   
         SR    R1,R0                                                            
         EXMVC R1,8(R4),GMTXTLIN    AND PUT ON SCREEN                           
         OI    6(R4),X'80'                                                      
*                                                                               
DREC76   ZIC   R1,GMTXTELL         GET NEXT ELEMENT                             
         AR    R3,R1                                                            
         CLI   0(R3),0             END OF RECORD?                               
         BE    DISRECX              YEP                                         
         CLI   0(R3),GMTXTELC      ELEMENT WE'RE LOOKING FOR?                   
         BE    DREC70A              YEP, GO DISPLAY IT                          
         B     DREC76               NOPE, CHECK NEXT ELEMENT                    
         DROP  R3                                                               
*                                                                               
DISRECX  GOTO1 ADISACT,GMSGD       DISPLAY ACTIVITY DATE                        
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========== ROUTINES TO DELETE OR RESTORE A MESSAGE RECORD ==========*         
*                                                                               
*------------------------- DELETE A RECORD ---------------------------*         
*                                                                               
DELREC   MVC   MYSVKEY,IOKEY       SAVE KEY AROUND                              
         MVC   APHALF,GMKMSG                                                    
         MVC   APBYTE,GMKLANG                                                   
         XC    IOKEY,IOKEY                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMQSYELC                                                  
         L     R2,AIOAREA1         POINT R2 TO RECORD                           
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM                                                     
         SPACE 1                                                                
         USING GMQSYD,R3                                                        
         LA    R2,IOKEY            MARK PASSIVE RECORD FIRST                    
         MVI   GQKREC,GQKRECQ      RECORD TYPE                                  
         MVC   GQKQNAME,GMQSYSYM   EQUATE NAME                                  
         MVC   GQKMNUM,APHALF      MSG#                                         
         MVC   GQKLANG,APBYTE      LANGUAGE                                     
         MVC   GQKRFP,GMQRFP       RFP RULE # (IF ANY)                          
         DROP  R3                                                               
         SPACE 1                                                                
         LA    R1,IOGENDIR+IORD                                                 
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                PASSIVE HAS TO BE THERE                      
         OI    GMDSTAT,X'80'       MARK FOR DELETION                            
         LA    R1,IOGENDIR+IOWRITE                                              
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVC   IOKEY,MYSVKEY       MARK ACTIVE POINTER                          
         OI    GMDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     R2,AIOAREA1         MARK RECORD                                  
         GOTO1 ASETACT,GMSGD                                                    
         OI    GMFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     XDELRES                                                          
         EJECT                                                                  
*------------------------- RESTORE A RECORD --------------------------*         
*                                                                               
RESREC   MVC   MYSVKEY,IOKEY       SAVE KEY AROUND                              
         MVC   APHALF,GMKMSG                                                    
         MVC   APBYTE,GMKLANG                                                   
         XC    IOKEY,IOKEY                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMQSYELC                                                  
         L     R2,AIOAREA1         POINT R2 TO RECORD                           
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM                                                     
         SPACE 1                                                                
         USING GMQSYD,R3                                                        
         LA    R2,IOKEY            UNMARK PASSIVE RECORD FIRST                  
         MVI   GQKREC,GQKRECQ      RECORD TYPE                                  
         MVC   GQKQNAME,GMQSYSYM   EQUATE NAME                                  
         MVC   GQKMNUM,APHALF      MSG#                                         
         MVC   GQKLANG,APBYTE      LANGUAGE                                     
         MVC   GQKRFP,GMQRFP       RFP RULE # (IF ANY)                          
         DROP  R3                                                               
         SPACE 1                                                                
         LA    R1,IOGENDIR+IORDD+IOLOCK                                         
         GOTO1 AIO                                                              
         BE    *+12                MUST COME BACK AS DELETED                    
         TM    IOERR,IOEDEL                                                     
         BO    *+6                                                              
         DC    H'0'                PASSIVE HAS TO BE THERE                      
         NI    GMDSTAT,X'FF'-X'80'  UNMARK FOR RESTORE                          
         LA    R1,IOGENDIR+IOWRITE                                              
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         MVC   IOKEY,MYSVKEY                                                    
         NI    GMDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GMSGD                                                    
         NI    GMFSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     XDELRES                                                          
*                                                                               
XDELRES  B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============== ROUTINE TO VALIDATE SELECT PARAMETERS ===============*         
VALSEL   DS    0H                                                               
*                                                                               
         XC    APRECKEY,APRECKEY                                                
         LA    R2,APRECKEY                                                      
         MVI   SELFLG,0            CLEAR SELECT-FLAG                            
         XC    GMKEY,GMKEY                                                      
         MVI   GMKMAJ,X'FF'        FLAG FOR FIRST PASS                          
         XC    SELKEY(SELX-SELKEY),SELKEY                                       
         MVI   FVMINL,0                                                         
*                                                                               
*---------------------------- EQUATE NAME ---------------------------*          
*                                                                               
VS000    MVC   SELQNAME,SPACES     ASSUME NOT FILTERING VIA EQUNAME             
         GOTO1 AFVAL,DCLQNMEH      GET EQUNAME (IF INPUT)                       
         BNE   VS010                NO INPUT, VALIDATE SYSTEM                   
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)   ASSUME INPUT NOT VALID                   
         CLI   FVILEN,3            MIN L(EQUNAME) = 3                           
         BL    VSXITNO             EXIT WITH ERROR                              
*                                                                               
         CLI   FVIFLD+2,C'#'       3RD CHAR MUST BE '#'                         
         BE    VS004                                                            
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#3RDC#)),   +        
               RR=APRELO                                                        
         B     VSXITNO             EXIT WITH ERROR CONDITION                    
*                                                                               
VS004    L     RE,APFXTAB          VALIDATE PREFIX OF EQUNAME ENTERRED          
VS005    CLC   FVIFLD(L'SVSYPRFX),0(RE)                                         
         BE    VS005A                                                           
         LA    RE,(PFXTABLF-PFXTABLE)(RE)    BUMP TO NEXT ENTRY                 
         CLI   0(RE),0             REACHED EOT YET?                             
         BNE   VS005                   NO                                       
*                                      YES, ERROR!                              
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BPRFX)),   +        
               RR=APRELO                                                        
         LA    R1,DCLQNMEH         RE-POSITION CURSOR                           
         ST    R1,APCURSOR                                                      
         B     VSXITNO             EXIT WITH ERROR CONDITION                    
*                                                                               
VS005A   MVC   SELSYS,2(RE)        SYSTEM DEFINED BY EQUNAME                    
         CLI   2(RE),0             CHECK FOR GENERAL SYSTEM,                    
         BNE   *+8                                                              
         MVI   SELSYS,15            ITS SYS# IS 15                              
         ZIC   R1,FVXLEN           SET SELECTED EQUNAME                         
         EXMVC R1,SELQNAME,FVIFLD                                               
         OI    SELFLG,SFEQNMQ      LIST VIA EQUATE NAMES                        
*                                                                               
*------------------------------- SYSTEM -----------------------------*          
*                                                                               
VS010    GOTO1 AFVAL,DCLSYSH       CHECK FOR INPUT                              
         BNE   VS020                NO INPUT                                    
*                                                                               
         GOTO1 AVALSYS,DCLSYSH     VALIDATE SYSTEM NAME                         
         BNE   VSXITNO                                                          
         CLI   APWORK,0            CHECK IF GENERAL SYSTEM,                     
         BNE   *+8                                                              
         MVI   APWORK,15            ITS SYS# IS 15                              
         CLC   SELQNAME,SPACES     IF SELQNAME HAS SOMETHING,                   
         BE    VS010A                                                           
         CLC   SELSYS,APWORK        MATCH SYSTEM TO EQUNAME PREFIX              
         BE    VS010B                                                           
         SPACE 1                                                                
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BPXSY)),   +        
               RR=APRELO                                                        
         B     VSXITNO             EXIT WITH ERROR CONDITION                    
*                                                                               
VS010A   MVC   SELSYS,APWORK       HOLD ONTO SYSTEM NUMBER                      
VS010B   MVC   APBYTE,SELSYS                                                    
         CLI   SELSYS,15                                                        
         BNE   *+8                                                              
         MVI   APBYTE,0                                                         
         GOTO1 ADISSYS,APBYTE                                                   
         GOTO1 DISPFLD,DCTSYSH                                                  
         OI    SELFLG,SFSYSQ       FILTER ON SYSTEM                             
*                                                                               
*------------------------------ LANGUAGE -----------------------------*         
*                                                                               
VS020    GOTO1 AFVAL,DCLLANGH      CHECK FOR LANGUAGE INPUT                     
         BNE   VS030                NO INPUT                                    
*                                                                               
         GOTO1 AVALLNG,DCLLANGH    VALIDATE LANGUAGE INPUT                      
         BNE   VSXITNO                                                          
         MVC   SELLNG,APWORK       HOLD ONTO LANGUAGE                           
         XI    SELLNG,X'FF'         AND INVERT IT                               
         OI    SELFLG,SFLNGQ       FILTER ON LANGUAGE                           
*                                                                               
         L     R1,APPARM                                                        
         MVC   APWORK,SPACES                                                    
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         GOTO1 DISPFLD,DCLLANGH    DISPLAY FULL LANGUAGE NAME                   
*                                                                               
*----------------------------- MESSAGE # -----------------------------*         
*                                                                               
VS030    GOTO1 AFVAL,DCLMSGH       GET MSG# INPUT                               
         BNE   VS050                                                            
*                                                                               
         TM    FVIIND,FVINUM       IS INPUT NUMERIC?                            
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VSXITNO                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)   ASSUME INPUT IS NOT VALID                
         OC    SCFULL(2),SCFULL         MAX MSG# IS X'FFFF'                     
         BNZ   VSXITNO                                                          
*                                                                               
         OC    SCFULL+2(2),SCFULL      INPUT MSG# = 0 ==> ERROR                 
         BZ    VSXITNO                                                          
*                                                                               
         MVC   SELMSG,SCFULL+2                                                  
         OI    SELFLG,SFMSGQ           START @ THE MSG# INPUTTED                
*                                                                               
*---------------------------- TRANSLATION ----------------------------*         
*                                                                               
VS050    MVI   SELTRNS,C'A'        DEFAULT TO <A>LL                             
         GOTO1 AFVAL,DCLTRNSH                                                   
         BNE   VS060                                                            
*                                                                               
         CLI   FVIFLD,C'A'         WAS 'A',         (ALL)                       
         BE    VS060                                                            
         CLI   FVIFLD,C'T'          'T', OR         (TRUE)                      
         BNE   *+12                                                             
         MVI   SELTRNS,C'T'                                                     
         B     VS060                                                            
         CLI   FVIFLD,C'U'          'U' INPUTTED?   (UNSURE)                    
         BNE   *+12                                                             
         MVI   SELTRNS,C'U'                                                     
         B     VS060                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV)   IF NOT, THEN INPUT IS NOT VALID          
         B     VSXITNO                                                          
*                                                                               
*-------------------------------- RFP --------------------------------*         
*                                                                               
VS060    MVI   SELRFP,0            ASSUME RFP RULE NOT REQUESTED                
         GOTO1 AFVAL,DCLRFPH       CHECK FOR ANY RFP INPUT                      
         BNE   VS070                                                            
*                                                                               
         TM    FVIIND,FVINUM       INPUT MUST EITHER BE NUMERIC                 
         BO    VS060B                                                           
         TM    FVIIND,FVIALF        OR ALPHABETIC                               
         BO    VS060C                                                           
VS060A   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VSXITNO                                                          
*                                                                               
VS060B   SR    R1,R1               NUMBER MUST BE BETWEEN 1                     
         LA    RF,255               AND 255                                     
         C     R1,SCFULL                                                        
         BNL   *+12                IF R1 >= SCFULL                              
         C     RF,SCFULL                                                        
         BNL   VS065                OR RF < SCFULL,                             
*                                    THEN BAD RULE ERROR                        
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BRULE)),   +        
               RR=APRELO                                                        
         B     VSXITNO                                                          
*                                                                               
VS060C   ZIC   RF,FVXLEN           INPUT MUST BE YES OR NO                      
         EXCLC RF,FVIFLD,=CL3'NO'                                               
         BE    VS065A              ONLY WANT RECORDS W/. RFP#=0                 
         EXCLC RF,FVIFLD,=CL3'YES'                                              
         BNE   VS060A                                                           
         MVI   SELRFP,1            WANT RECORDS W/. RFP#>=1                     
         B     VS065A                                                           
*                                                                               
VS065    MVC   SELRFP,SCFULL+3     START @ INPUTTED RFP#                        
VS065A   OI    SELFLG,SFRFPQ                                                    
*                                                                               
*------------------------- BUILD INITIAL KEY -------------------------*         
*                                                                               
VS070    TM    SELFLG,SFEQNMQ      LIST VIA EQUATE NAMES?                       
         BZ    VS075                NO, BUILD ACTIVE KEY                        
*                                                                               
** BUILD PASSIVE KEY                                                            
*                                                                               
         MVI   GQKREC,GQKRECQ      RECORD TYPE = X'94'                          
         MVI   GQKREC,GQKRECQ      PASSIVE RECORD TYPE                          
         MVC   GQKQNAME,SELQNAME   EQUATE NAME IS IN PASSIVE KEY                
         B     VSXITYES                                                         
*                                                                               
** BUILD ACTIVE KEY                                                             
*                                                                               
VS075    MVI   GMKREC,GMKRECQ      RECORD TYPE = 'M'                            
         MVI   GMKTYP,GMKTGDIC                                                  
         TM    SELFLG,SFSYSQ       FILTER ON SYSTEM?                            
         BZ    VSXITYES             NO                                          
         MVC   GMKSYS,SELSYS                                                    
         TM    SELFLG,SFMSGQ       START AT SOME MSG#?                          
         BZ    VSXITYES             NO                                          
         MVC   GMKMSG,SELMSG                                                    
         TM    SELFLG,SFLNGQ       FILTER ON LANGUAGE?                          
         BZ    VSXITYES             NO                                          
         MVC   GMKLANG,SELLNG                                                   
*                                                                               
VSXITYES LA    R3,L'GMKEY(R2)      BUILD THIS DUMB KEY AT END OF                
         USING DUMBKEYD,R3          APRECKEY SO GENERAL WILL KNOW               
         MVC   DMBKMSG,SELMSG       WHEN THE LIST FILTER FIELDS                 
         MVC   DMBKLNG,SELLNG       HAVE CHANGED                                
         MVC   DMBKTRNS,SELTRNS                                                 
         MVC   DMBKRFP,SELRFP                                                   
         DROP  R3                                                               
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R0,DCLACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,DCLACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         B     CCEQUAL             RETURN W/. CC = 0                            
*                                                                               
VSXITNO  B     CCNEQUAL            RETURN W/. CC <> 0                           
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*==================== GET NEXT LIST/SELECT RECORD ====================*         
GETSEL   DS    0H                                                               
*                                                                               
*----------------------- ADMINISTRATIVE STUFF ------------------------*         
*                                                                               
         MVC   MYSVKEY,APRECKEY                                                 
         LA    R2,IOKEY                                                         
         MVC   GMKEY,APRECKEY                                                   
         CLI   GMKMAJ,X'FF'        TEST FIRST TIME FLAG                         
         BNE   GS010                                                            
         MVI   GMKMAJ,0                                                         
         MVI   MYSVKEY+(GMKMAJ-GMSGD),0                                         
         B     GS016               READ HIGH                                    
*                                                                               
GS010    TM    APINDS,2            TEST SEQUENCE BROKEN                         
         BZ    GS014                                                            
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    GS018                                                            
         B     GSXITNO                                                          
*                                                                               
GS014    TM    APINDS,1            TEST READ OR READ HIGH                       
         BNZ   GS018                                                            
GS016    LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GS018    LA    R1,IOGENDIR+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GSXITNO                                                          
*                                                                               
*----------------------------- FILTERING -----------------------------*         
*                                                                               
GS018A   LA    R2,IOKEY                                                         
         MVC   APRECKEY,IOKEY      SAVE "PRIMARY" KEY                           
         TM    SELFLG,SFEQNMQ      LIST VIA PASSIVE RECORDS?                    
         BZ    GS050                NO, GET RECORD DIRECTLY                     
*                                                                               
** LIST VIA PASSIVE RECORDS                                                     
*                                                                               
GS020    CLI   GMKREC,GQKRECQ      CHECK FOR CORRECT RECORD TYPE                
         BNE   GSXITNO                                                          
*                                                                               
*** APPLY FILTERS ON PASSIVE RECORDS                                            
*                                                                               
         CLC   GQKQNAME(2),SELQNAME  PREFIX OF EQUNAME MUST MATCH               
         BNE   GSXITNO                                                          
*                                                                               
         TM    SELFLG,SFMSGQ       DOES USER WANT TO START @                    
         BZ    GS020A               A CERTAIN MSG #?                            
         MVC   APHALF,GQKMNUM      IF SO, THEN TEST MESSAGE NUMBER,             
         NI    APHALF,X'FF'-X40BITQ   W/O THE X'4000' BIT ON                    
         CLC   SELMSG,APHALF                                                    
         BH    GS018               TEST FAILED, READ NEXT RECORD                
*                                                                               
GS020A   TM    SELFLG,SFRFPQ       START @ A CERTAIN RFP RULE #?                
         BZ    GS020D               NO, DON'T FILTER ON RFP#                    
         CLI   SELRFP,0            ONLY RFP#=0 RECORDS?                         
         BNE   GS020B               NO                                          
         CLI   GQKRFP,0             YES, SO IS RFP#=0?                          
         BE    GS020D                YES, PASS RFP# TEST                        
         B     GS018                                                            
GS020B   CLC   GQKRFP,SELRFP                                                    
         BL    GS018                                                            
*                                                                               
GS020D   TM    SELFLG,SFLNGQ       IF LANGUAGE FILTER PRESENT,                  
         BZ    GS020E                                                           
         CLC   GQKLANG,SELLNG       CHECK AGAINST REQUESTED LANG                
         BNE   GS018                                                            
*                                                                               
GS020E   MVC   APBYTE,GMDELEM      FILTER ON TRANSLATION                        
         MVC   BYTE,SELTRNS        PASS SELECTED OPTION                         
         MVC   OPT1(2),=C'TU'      SET POSSIBLE OPTIONS (TRUE/UNSURE)           
         LA    RF,GMTRNSQ          PASS CORRESPONDING FLAG BIT                  
         BAS   RE,FILT1            DO FILTER TEST                               
         BNE   GS018                DIDN'T PASS, GET NEXT RECORD                
*                                                                               
*** GET APPROPRIATE RECORD                                                      
*                                                                               
         LA    R1,IOGENFIL+IO1+IOGET                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         CLI   OPTSCAN,0           SEE IF 'SCAN=' INVOKED                       
         BE    GSXITYES             NOPE                                        
         BAS   RE,SCANTXT                                                       
         BE    GSXITYES            TEXT STRING WAS FOUND...EXIT                 
         BNE   GS018                OR ELSE, GET NEXT RECORD                    
         SPACE 2                                                                
*                                                                               
** LIST VIA ACTIVE RECORDS                                                      
*                                                                               
GS050    CLI   GMKREC,GMKRECQ      CHECK FOR CORRECT RECORD TYPE                
         BNE   GSXITNO                                                          
         OC    GMKMSG,GMKMSG       EXCLUDE MSG# = 0                             
         BZ    GS018                                                            
         CLI   GMKTYP,GMKTGDIC     ONLY DICTIONARY RECORDS                      
         BNE   GS018                                                            
*                                                                               
*** APPLY FILTERS ON ACTIVE RECORDS                                             
*                                                                               
         TM    SELFLG,SFSYSQ       FILTER ON SYSTEM?                            
         BZ    GS050A               NOPE                                        
         CLC   GMKSYS,SELSYS                                                    
         BNE   GS018                                                            
*                                                                               
GS050A   TM    SELFLG,SFMSGQ       START @ MSG# GIVEN?                          
         BZ    GS050B                                                           
         CLC   GMKMSG,SELMSG                                                    
         BL    GS018                                                            
*                                                                               
GS050B   TM    SELFLG,SFLNGQ       FILTER ON LANGUAGE?                          
         BZ    GS050C                                                           
         CLC   GMKLANG,SELLNG                                                   
         BNE   GS018                                                            
*                                                                               
GS050C   MVC   APBYTE,GMDELEM      FILTER ON TRANSLATON                         
         MVC   BYTE,SELTRNS        PASS SELECTED OPTION                         
         MVC   OPT1(2),=C'TU'      SET POSSIBLE OPTIONS (TRUE/UNSURE)           
         LA    RF,GMTRNSQ          PASS CORRESPONDING FLAG BIT                  
         BAS   RE,FILT1            DO FILTER TEST                               
         BNE   GS018                                                            
*                                                                               
*** GET RECORD                                                                  
*                                                                               
GS055    LA    R1,IOGENFIL+IO1+IOGET                                            
         GOTO1 AIO                                                              
         L     R3,IOADDR                                                        
*                                                                               
         CLI   OPTSCAN,0           SEE IF 'SCAN=' INVOKED                       
         BE    *+12                 NOPE, DON'T SCAN TEXT                       
         BAS   RE,SCANTXT                                                       
         BNE   GS018                                                            
*                                                                               
         TM    SELFLG,SFRFPQ       START AT CERTAIN RFP#?                       
         BZ    GSXITYES             NO                                          
         XC    APELEM,APELEM        YES, GET ELEM AND CHECK RFP#                
         MVI   APELEM,GMQSYELC     EQUNAME ELEMENT CODE                         
         GOTO1 AGETELS,(R3)                                                     
         ICM   R3,15,APPARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING GMQSYD,R3                                                        
         CLI   SELRFP,0            ONLY THOSE W/. RFP#=0?                       
         BNE   GS055A               NO                                          
         CLI   GMQRFP,0             YES, SO IS THE RFP#=0?                      
         BE    GSXITYES             YES                                         
         B     GS018                                                            
GS055A   CLC   GMQRFP,SELRFP                                                    
         BL    GS018                                                            
         DROP  R3                                                               
*                                                                               
GSXITYES MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   IOKEY,APRECKEY      RESTORE "PRIMARY" KEY                        
         LA    R1,IOGENDIR+IO2+IORD   RESTORE DATAMGR READ                      
         GOTO1 AIO                     FOR NEXT TIME AROUND                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     CCEQUAL                                                          
*                                                                               
GSXITNO  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
         B     CCNEQUAL                                                         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== DISPLAY LIST/SELECT LINE =====================*         
DISSEL   DS    0H                                                               
*                                                                               
         L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
*------------------------------- SYSTEM ------------------------------*         
*                                                                               
         MVC   APBYTE,GMKSYS       CHECK IF GENERAL SYSTEM                      
         CLI   GMKSYS,15           THE DISSYS ROUTINE THINKS                    
         BNE   *+8                  THE GENERAL SYSTEM IS SYS#0                 
         MVI   APBYTE,0                                                         
         GOTO1 ADISSYS,APBYTE      GET SYSTEM SHORT NME                         
         ICM   R1,15,APPARM        R1=A(SYSLST ENTRY)                           
         BNZ   *+14                                                             
         MVC   LISTSYS,APWORK      UNKNOWN SYSTEM                               
         B     *+10                                                             
         MVC   LISTSYS,SYSLSHRT-SYSLSTD(R1)                                     
*                                                                               
*-------------------------------- MSG# -------------------------------*         
*                                                                               
         ZICM  R1,GMKMSG,(3)                                                    
         EDIT  (R1),(5,LISTMNO),WRK=APWORK,DUB=APDUB,ALIGN=LEFT                 
*                                                                               
*------------------------------ LANGUAGE -----------------------------*         
*                                                                               
         MVC   APWORK(1),GMKLANG                                                
         XI    APWORK,X'FF'        INVERT LANGUAGE                              
         GOTO1 ADISLNG,APWORK      GET LANGUAGE NAME                            
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVC   LISTLANG,LANGSHR-LANGTABD(R1)                                    
*                                                                               
*------------------------------ MESSAGE ------------------------------*         
*                                                                               
         XC    MESGTXT,MESGTXT                                                  
         ZIC   R1,GMSGELL          NOTE MSG ELEM ALWAYS FIXED POSN              
         LA    R0,GMSGFXDL         R0=L(ELEMENT OVERHEAD)                       
         SR    R1,R0               R1=L(MESSAGE)                                
         STC   R1,LMESGTXT                                                      
         BCTR  R1,0                                                             
         EXMVC R1,MESGTXT,GMSGTXT                                               
*                                                                               
         NI    MESGFLG,X'FF'-MFTRUEQ                                            
         TM    GMFELEM,GMTRNSQ     IF TRUE TRANSLATION,                         
         BZ    *+8                                                              
         OI    MESGFLG,MFTRUEQ      SET TRUE TRANS FLAG                         
*                                                                               
         GOTO1 =A(SPCFRMT),APPARM,(R5),(R6),(R7),(RC),(1,0),           +        
               RR=APRELO                                                        
*                                                                               
         ZIC   RE,LMESG1                                                        
         LA    RF,L'LISTMESG                                                    
         CR    RE,RF                                                            
         BH    *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                                                             
         EXMVC RF,LISTMESG,MESG1                                                
*                                                                               
*--------------------------- EQUNAME & RFP ---------------------------*         
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMQSYELC     ELEMENT CODE                                 
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING GMQSYD,R3                                                        
         MVC   LISTQNME,GMQSYSYM   EQUATE SYMBOL                                
         OC    GMQRFP,GMQRFP                                                    
         BZ    DISSELX                                                          
         EDIT  (B1,GMQRFP),(3,LISTRFP),WRK=APWORK,DUB=APDUB                     
*                                                                               
DISSELX  B     EXIT                                                             
         DROP  R3,R4                                                            
         SPACE 3                                                                
*                                                                               
*== ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS) =*         
*                                                                               
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============= ROUTINE TO VALIDATE REPORT REQUEST SCREEN =============*         
VALREQ   DS    0H                                                               
*                                                                               
         XC    SELKEY(SELX-SELKEY),SELKEY  SELECTION CRITERION                  
         XC    APRECKEY,APRECKEY                                                
         MVI   SELFLG,0            CLEAR SELECT-FLAG                            
*                                                                               
*----------------------------- REQUESTOR -----------------------------*         
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DCRREQH       VALIDATE REQUESTOR                           
         BNE   VQXITNO                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
*-------------------------------- WHEN -------------------------------*         
*                                                                               
         GOTO1 AVALWHEN,DCRWHENH   VALIDATE WHEN                                
         BNE   VQXITNO                                                          
*                                                                               
*----------------------------- DESTINATION ---------------------------*         
*                                                                               
         GOTO1 AVALDEST,DCRDESTH   VALIDATE DESTINATION ID                      
         BNE   VQXITNO                                                          
*                                                                               
*------------------------------- SYSTEM ------------------------------*         
*                                                                               
         NI    SELFLG,X'FF'-SFSYSQ ASSUME NO FILTER ON SYSTEM                   
         MVI   FVMINL,1            MUST HAVE SYSTEM INPUT                       
         GOTO1 AFVAL,DCRSYSH       VALIDATE SYSTEM                              
         BNE   VQXITNO                                                          
         GOTO1 AVALSYS,DCRSYSH     VALIDATE SYSTEM NAME                         
         BNE   VQXITNO                                                          
         SPACE 1                                                                
         OI    SELFLG,SFSYSQ       FILTER ON SYSTEM                             
         MVC   SELSYS,APWORK                                                    
         CLI   APWORK,0            CHECK IF GENERAL SYSTEM                      
         BNE   *+8                                                              
         MVI   SELSYS,15            ITS SYS# IS 15                              
         GOTO1 ADISSYS,APWORK      GET SYSTEM NAME                              
         GOTO1 DISPFLD,DCRSYSH     REDISPLAY                                    
*                                                                               
*-------------------------------- TYPE -------------------------------*         
*                                                                               
VRQ20    MVI   SELTYPE,GMKTGDIC    ONLY DATA DICTIONARY                         
*                                                                               
*------------------------------ LANGUAGE -----------------------------*         
*                                                                               
VRQ40    NI    SELFLG,X'FF'-SFLNGQ ASSUME NO FILTER ON LANGUAGE                 
         SPACE 1                                                                
         LA    RE,NMLNGLN                                                       
         LA    R1,DCRLNG1H         CLEAR THOSE FIELDS WHICH WILL                
VRQ40A   XC    8(L'DCRLNG1,R1),8(R1)                                            
         OI    6(R1),X'80'                                                      
         ZIC   R0,0(R1)             DISPLAY THE FULL NAME OF THE                
         AR    R1,R0                                                            
         BCT   RE,VRQ40A            LANGUAGE(S) SELECTED                        
         SPACE 1                                                                
         GOTO1 AFVAL,DCRLNGFH      CHECK FOR ANY INPUT                          
         CLI   FVILEN,0                                                         
         BE    VRQ50                NO INPUT                                    
         XC    SCANTBL,SCANTBL      SCAN IN INPUT                               
         LA    R3,SCANTBL                                                       
         GOTO1 VSCANNER,APPARM,(0,DCRLNGFH),('NMLNGLN',(R3)),0                  
         CLI   APPARM+4,0          ALLOW MULTIPLE LANGUAGE SELECT               
         BNE   VRQ43                                                            
         DC    H'0'                                                             
*                                                                               
VRQ43    ZIC   RF,APPARM+4         RF=# OF LANGUAGES SELECTED                   
         LA    R1,DCRLNG1H                                                      
         LA    R0,DCRLNGXH                                                      
         ST    R0,XADDRSS          A(LAST LANGUAGE DISPLAY FIELD)               
         LA    R0,DCRLNGFH         IN CASE INPUT FIELD                          
         ST    R0,FVADDR            IS INVALID                                  
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VRQ43B   CLI   1(R3),0             DON'T WANT ANY 2ND HALF                      
         BNE   VQXITNO              OF DIVIDED FIELDS                           
         CLI   0(R3),L'DCRLNG1     MAKE SURE L(INPUT)<=L(DISP LINE)             
         BH    VQXITNO                                                          
         CLI   0(R3),0              AND THAT IT ISN'T ZERO                      
         BE    VQXITNO                                                          
         ZIC   RE,0(R3)            MOVE DIVIDED FIELD INTO DISP LINE            
         BCTR  RE,0                                                             
         EXMVC RE,8(R1),12(R3)                                                  
         LA    R3,32(R3)           BUMP TO NEXT BLOCK                           
         ZIC   RE,0(R1)                                                         
         AR    R1,RE               BUMP TO NEXT DISPLAY FIELD                   
         C     R1,XADDRSS          DON'T GO PASS LAST DISPLAY FIELD             
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCT   RF,VRQ43B                                                        
         SPACE 1                                                                
         XC    SELLNGST,SELLNGST   BUILD LIST OF LANGUAGES TO                   
         LA    R1,DCRLNG1H          FILTER ON                                   
         LA    R3,SELLNGST+1                                                    
VRQ45    GOTO1 AFVAL,(R1)                                                       
         CLI   FVILEN,0                                                         
         BE    VRQ45B                                                           
         GOTO1 AVALLNG,(R1)        VALIDATE LANGUAGE NAME                       
         BE    VRQ45A                                                           
         SPACE 1                                                                
         LA    R0,DCRLNGFH         INVALID LANGUAGE NAME                        
         ST    R0,FVADDR            RESET CURSOR POSITIONING ADDRESS            
         B     VQXITNO                                                          
*                                                                               
VRQ45A   MVC   0(1,R3),APWORK      MOVE LANG CODE INTO TABLE                    
         XI    0(R3),X'FF'         INVERT LANGUAGE CODE                         
         LA    R3,1(R3)            BUMP LANG TABLE POINTER                      
         ZIC   RE,SELLNGST         INCREMENT # OF VALID                         
         LA    RE,1(RE)             LANGUAGES SELECTED                          
         STC   RE,SELLNGST                                                      
         OI    SELFLG,SFLNGQ       FILTER ON LANGUAGE FLAG                      
         L     RF,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'DCRLNGF-1),APWORK                                     
         MVC   APWORK(L'LANGFUL),LANGFUL-LANGTABD(RF)                           
         GOTO1 DISPFLD,(R1)        DISPLAY FULL NAME                            
*                                                                               
VRQ45B   ZIC   R0,0(R1)            BUMP TO NEXT DISPLAY FIELD                   
         AR    R1,R0                                                            
         C     R1,XADDRSS          ARE WE DONE YET?                             
         BNH   VRQ45                NOPE                                        
*                                                                               
*------------------------------ EQUNAME ------------------------------*         
*                                                                               
VRQ50    NI    SELFLG,X'FF'-SFEQNMQ  ASSUME NOT LISTING VIA EQUNAMES            
         XC    SELQNAME,SELQNAME                                                
         GOTO1 AFVAL,DCRQNMEH                                                   
         BNE   VRQ60                                                            
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFNOTV) ASSUME INPUT IS NOT VALID                  
         CLI   FVILEN,3             MIN L(EQUNAME) = 3                          
         BL    VQXITNO                                                          
         SPACE 1                                                                
         CLI   FVIFLD+2,C'#'       3RD CHAR MUST BE '#'                         
         BE    VRQ55                                                            
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#3RDC#)),   +        
               RR=APRELO                                                        
         B     VQXITNO                                                          
*                                                                               
VRQ55    L     RE,APFXTAB          START OF PREFIX TABLE                        
VRQ55A   CLC   0(2,RE),SPACES                                                   
         BE    VRQ55B                                                           
         CLC   0(2,RE),FVIFLD                                                   
         BE    VRQ57                                                            
VRQ55B   LA    RE,PFXTABLF-PFXTABLE(RE)   BUMP TO NEXT ENTRY                    
         CLI   0(RE),0             CHECK EOT                                    
         BNE   VRQ55A                                                           
*                                  COULDN'T FIND PREFIX IN TABLE                
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BPRFX)),   +        
               RR=APRELO                                                        
         LA    R1,DCRQNMEH         RE-POSITION CURSOR                           
         ST    R1,APCURSOR                                                      
         B     VQXITNO             EXIT WITH ERROR CONDITION                    
*                                                                               
VRQ57    MVC   APBYTE,2(RE)        GET SYSTEM NUMBER                            
         TM    SELFLG,SFSYSQ       WAS A SYSTEM NAME INPUTTED?                  
         BZ    VRQ57A               NOPE                                        
         SPACE 1                                                                
         CLC   APBYTE,SELSYS        YES, ENSURE MATCH                           
         BE    VRQ57A                                                           
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BPXSY)),   +        
               RR=APRELO                                                        
         B     VQXITNO             EXIT WITH ERROR CONDITION                    
*                                                                               
VRQ57A   NI    SELFLG,X'FF'-SFSYSQ  EQUNAME TAKES PRECEDENCE                    
         OI    SELFLG,SFEQNMQ                                                   
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,SELQNAME,FVIFLD                                               
*                                                                               
*-------------------------- HOLES AND BLANKS -------------------------*         
*                                                                               
VRQ60    NI    SELFLG,X'FF'-SFHOLEQ  ASSUME HOLES NOT REQUESTED                 
         GOTO1 AFVAL,DCRHOLEH                                                   
         BE    VRQ60A                                                           
         MVI   DCRHOLE,C'N'        SHOW AN 'N' IN FIELD                         
         OI    DCRHOLEH+6,X'80'                                                 
         B     VRQ70                                                            
         SPACE 1                                                                
VRQ60A   CLI   FVIFLD,C'N'         VALIDATE INPUT                               
         BE    VRQ70                                                            
         CLI   FVIFLD,C'Y'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)  INVALID INPUT                             
         B     VQXITNO                                                          
         SPACE 1                                                                
         TM    SELFLG,SFEQNMQ      CAN'T HAVE EQUNAME AND HOLES                 
         BZ    VRQ60B                                                           
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#DCR01)),   +        
               RR=APRELO                                                        
         B     VQXITNO                                                          
*                                                                               
VRQ60B   OI    SELFLG,SFHOLEQ      REPORT ON HOLES                              
         SPACE 2                                                                
VRQ70    NI    SELFLG,X'FF'-SFBLNKQ  ASSUME BLANKS NOT REQUESTED                
         GOTO1 AFVAL,DCRBLNKH                                                   
         BE    VRQ70A                                                           
         MVI   DCRBLNK,C'N'        SHOW AN 'N' IN FIELD                         
         OI    DCRBLNKH+6,X'80'                                                 
         B     VRQXIT                                                           
         SPACE 1                                                                
VRQ70A   CLI   FVIFLD,C'N'         VALIDATE INPUT                               
         BE    VRQXIT                                                           
         CLI   FVIFLD,C'Y'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)  INVALID INPUT                             
         B     VQXITNO                                                          
         SPACE 1                                                                
         TM    SELFLG,SFEQNMQ      CAN'T HAVE EQUNAME AND BLANKS                
         BZ    VRQ70B                                                           
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#DCR02)),   +        
               RR=APRELO                                                        
         B     VQXITNO                                                          
*                                                                               
VRQ70B   OI    SELFLG,SFBLNKQ      REPORT ON BLANKS                             
*                                                                               
*----------------------------- BUILD KEY -----------------------------*         
*                                                                               
VRQXIT   TM    SELFLG,SFHOLEQ      IF REPORTING ON HOLES,                       
         BZ    VRQXITA                                                          
         XC    SVHINUM1(L'SVHINUM1+L'SVHINUM2),SVHINUM1                         
         LA    R2,IOKEY             GET HIGH MSG#S                              
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ      RECORD TYPE                                  
         MVC   GMKSYS,SELSYS       SYSTEM                                       
         MVI   GMKTYP,GMKTGDIC     MESSAGE TYPE                                 
         GOTO1 AIO,IOGENDIR+IOHI                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(L'GMKEY),IOKEYSAV                                          
         BNE   VRQXITA             NO DICTIONARY FOR SYSTEM                     
         SPACE 1                                                                
         GOTO1 AIO,IOGENFIL+IOGET+IO2                                           
         L     R2,AIOAREA2                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMCTLELC     GET CONTROL ELEMENT                          
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING GMCTLD,R3                                                        
         MVC   SVHINUM1,GMCTLNM1   CONTROL MSG#1                                
         MVC   SVHINUM2,GMCTLNM2   CONTROL MSG#2                                
         DROP  R3                                                               
*                                                                               
VRQXITA  LA    R2,APRECKEY         SET UP INITIAL KEY                           
         XC    APRECKEY,APRECKEY                                                
         TM    SELFLG,SFEQNMQ      REPORTING BY EQUNAMES?                       
         BO    VRQXITB                                                          
         SPACE 1                                                                
         MVI   GMKREC,GMKRECQ      RECORD TYPE - BY ACTIVE POINTERS             
         MVC   GMKSYS,SELSYS       SYSTEM                                       
         MVC   GMKTYP,SELTYPE      DATA DICTIONARY TYPE                         
         MVI   GMKMSG+(L'GMKMSG-1),1  FORGET ABOUT MSG#=0                       
         B     VQXITYES                                                         
*                                                                               
VRQXITB  MVI   GQKREC,GQKRECQ      RECORD TYPE - BY PASSIVE PTRS                
         MVC   GQKQNAME,SELQNAME                                                
*                                                                               
VQXITYES L     R1,AREP                                                          
         USING REPD,R1             R1=A(REPORT WORK AREA)                       
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         L     R0,AREPSPEC                                                      
         ST    R0,REPAPHS                                                       
         DROP  R1                                                               
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     CCEQUAL                                                          
*                                                                               
VQXITNO  B     CCNEQUAL                                                         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============== ROUTINE TO GENERATE MESSAGE REPORT ==================*         
PRTREP   DS    0H                                                               
         L     R1,AREP                                                          
         USING REPD,R1                                                          
         MVI   REPPRNSA,1          ONE SPACE LINE AFTER PRINTING                
         OI    REPPRNTI,REPPSPAC                                                
         DROP  R1                                                               
         SPACE 1                                                                
         MVI   RPTFLG,0            CLEAR REPORT FLAG                            
         L     R2,AIOAREA1                                                      
         MVC   IOKEY,APRECKEY      SET INITIAL KEY VALUE                        
*                                                                               
PR010    LA    R1,IOHI+IOGENDIR+IO1                                             
         B     *+8                                                              
PR020    LA    R1,IOSQ+IOGENDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
*----------------------------- FILTERING -----------------------------*         
*                                                                               
         LA    R2,IOKEY                                                         
         TM    SELFLG,SFHOLEQ+SFBLNKQ  REPORT HOLES AND/OR BLANKS?              
         BZ    *+14                                                             
         XC    MESGNUM,MESGNUM        YES, INITIALIZE MSG# SEQUENCING           
         B     PR100                                                            
         TM    SELFLG,SFEQNMQ      REPORT VIA PASSIVES?                         
         BZ    PR025                YES                                         
         SPACE 1                                                                
         CLI   GQKREC,GQKRECQ      TEST STILL A PASSIVE PTR                     
         BNE   PRTREPX                                                          
         CLC   SELQNAME(3),GQKQNAME  PREFIX OF EQUNAME                          
         BNE   PRTREPX                                                          
         TM    SELFLG,SFLNGQ       FILTER ON LANGUAGE?                          
         BZ    PR035                NO, GO GET RECORD                           
         MVC   APBYTE,GQKLANG       YES, GO DO FILTER TEST                      
         BAS   RE,PRFLTLNG                                                      
         BE    PR035                                                            
         BNE   PR020                                                            
*                                                                               
PR025    CLI   GMKREC,GMKRECQ      TEST STILL A MESSAGE RECORD                  
         BNE   PRTREPX                                                          
         CLC   GMKSYS,SELSYS       FILTER ON SYSTEM, ALWAYS                     
         BNE   PRTREPX                                                          
         CLI   GMKTYP,GMKTGDIC     TEST STILL A DATA DICT ITEM                  
         BNE   PRTREPX                                                          
         OC    GMKMSG,GMKMSG       DON'T WANT MSG# = 0                          
         BZ    PR020               (IT SHOULDN'T HAPPEN THOUGH)                 
*                                                                               
PR030    TM    SELFLG,SFLNGQ       TEST IF FILTER ON LANGUAGE                   
         BZ    PR035                NOPE, NOT ON                                
         MVC   APBYTE,GMKLANG      LOOK UP TABLE OF SELECTED                    
         BAS   RE,PRFLTLNG          LANGUAGES                                   
         BE    PR035               PASSED FILTER                                
         BNE   PR020               READ SEQUENTIAL FOR NEXT MESSAGE             
         SPACE 3                                                                
PRFLTLNG ZIC   R1,SELLNGST         R1=# OF SELECTED LANGUAGES                   
         LA    R3,SELLNGST+1                                                    
PRFLA    CLC   APBYTE,0(R3)                                                     
         BER   RE                                                               
         LA    R3,1(R3)                                                         
         BCT   R1,PRFLA                                                         
         BR    RE                                                               
*                                                                               
*----------------------- GET & PROCESS RECORD ------------------------*         
*                                                                               
PR035    GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
         SPACE 1                                                                
         CLI   OPTSCAN,0           SEE IF 'SCAN=' INVOKED                       
         BE    PR035A               NOPE                                        
         BAS   RE,SCANTXT                                                       
         BNE   PR020               TEXT STRING WAS FOUND...FORMAT PRINT         
*                                   LINE, ELSE, GET NEXT RECORD                 
PR035A   BAS   RE,RPTRECRD         GO REPORT RECORD                             
         B     PR020                                                            
*                                                                               
*----------------------- HOLES AND/OR BLANKS -------------------------*         
*                                                                               
PR100    CLC   IOKEY(GMKMSG-GMSGD),IOKEYSAV                                     
         BNE   PRTREPX                                                          
         TM    SELFLG,SFHOLEQ      IF NOT REPORTING ON HOLES,                   
         BZ    PR105                THEN SKIP THIS AND READ FILE                
*                                                                               
** MOVE TO NEXT NON-HOLE MSG#                                                   
*                                                                               
PR100A   LH    RF,MESGNUM          MOVE TO NEXT MSG# FOR HOLES                  
         LA    RF,1(RF)                                                         
         BAS   RE,SKP40BIT                                                      
         CLM   RF,3,BNDRY          CHECK NEW MSG# WITH                          
         BL    PR100B               RESPECTIVE CONTROL NUMBERS                  
         BH    *+8                                                              
         LA    RF,1(RF)            DON'T WANT MSG# ON BOUNDARY                  
         SPACE 1                                                                
         CLM   RF,3,SVHINUM2       RF > X'8000'                                 
         BL    PR100C                                                           
         BE    PRTREPX              NO MORE MSGS AFTER CONTROL #                
         DC    H'0'                                                             
PR100B   CLM   RF,3,SVHINUM1       RF < X'8000'                                 
         BL    PR100C                                                           
         BE    *+6                  NO MORE MSGS AFTER CONTROL #,               
         DC    H'0'                                                             
         ICM   RF,3,=X'8001'         PUT NEW MSG# ABOVE BOUNDARY                
*                                                                               
PR100C   STH   RF,MESGNUM          RF BECOMES NEW MSG#                          
         CLC   MESGNUM,GMKMSG      CHECK AGAINST MSG# IN KEY                    
         BE    PR105                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     R2,AIOAREA2         A HOLE WAS FOUND, PREPARE                    
         MVC   GMKSYS,SELSYS        TO REPORT RECORD                            
         MVC   GMKMSG,MESGNUM                                                   
         MVI   GMKLANG,0                                                        
         OI    RPTFLG,RFHOLEQ      INDICATE HOLE-REPORTING                      
         BAS   RE,RPTRECRD                                                      
         LA    R2,IOKEY                                                         
         B     PR100A                                                           
*                                                                               
** READ FILE TO LOCATE GAPS                                                     
*                                                                               
PR105    GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    FILLNGST,FILLNGST                                                
         B     PR110                                                            
PR105A   LA    R2,IOKEY                                                         
         GOTO1 AIO,IOGENDIR+IOSQ+IO1                                            
         CLC   IOKEY(GMKMSG-GMSGD),IOKEYSAV                                     
         BNE   PR120                                                            
         CLC   GMKMSG,IOKEYSAV+(GMKMSG-GMSGD)                                   
         BNE   PR120                                                            
*                                                                               
** BUILDING FILLNGST                                                            
*                                                                               
PR110    TM    SELFLG,SFHOLEQ      REPORT ON HOLES?                             
         BZ    PR115                NOPE, CHECK BLANKS                          
         LA    RF,FILLNGST+1       PUT LANG OF RECORD IN FILLNGST               
         ZICM  RE,FILLNGST,(1)                                                  
         BZ    PR110A                                                           
         LR    R0,RE               R0 = # OF LANGS IN LIST (>0)                 
         LA    RF,1(RF)            BUMP TO POSITION TO PUT LANG CODE            
         BCT   RE,*-4                                                           
         LR    RE,R0                                                            
PR110A   MVC   0(L'GMKLANG,RF),GMKLANG                                          
         LA    RE,1(RE)            INCREMENT # OF LANG CODES                    
         STC   RE,FILLNGST                                                      
*                                                                               
** REPORTING ON BLANKS                                                          
*                                                                               
PR115    TM    SELFLG,SFBLNKQ      REPORT ON BLANKS?                            
         BZ    PR105A               NOPE, READ NEXT RECORD                      
         GOTO1 AIO,IOGENFIL+IO1+IOGET                                           
         BE    *+6                  YEP, GET RECORD AND CHECK                   
         DC    H'0'                  FOR BLANK ENTRY                            
         L     R2,AIOAREA1                                                      
         ZIC   RE,GMSGELL          RE=L(MSG ELEMENT)                            
         LA    R0,GMSGFXDL+1       R0=L(ELEM OVERHEAD)                          
         SR    RE,R0               RE=L(GMSGTXT)-1                              
         LA    RF,GMSGTXT          RF-->MESSAGE                                 
         ZIC   R1,0(RF)            R1=# OF LITERALS                             
         SR    RE,R1               RE=COMBINED LENGTH OF LITERALS               
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RF,1(RF,R1)         RF-->FIRST LITERAL                           
PR115A   CLI   0(RF),C' '          IS ENTRY A BLANK?                            
         BNE   PR105A               NO, GET NEXT RECORD                         
         LA    RF,1(RF)             SO FAR IT IS                                
         BCT   RE,PR115A                                                        
         BAS   RE,RPTRECRD         RECORD IS A BLANK ENTRY                      
         B     PR105A                                                           
*                                                                               
** LOOKING FOR HOLES TO REPORT ON                                               
*                                                                               
PR120    TM    SELFLG,SFHOLEQ      IF NOT REPORTING ON HOLES,                   
         BZ    PR130                THEN SKIP THIS CODE                         
         SPACE 1                                                                
         CLI   FILLNGST,0          DEFINITELY A HOLE ENTRY,                     
         BNE   PR125                                                            
         L     R2,AIOAREA2          PREPARE TO REPORT IT                        
         MVC   GMKSYS,SELSYS       SYSTEM                                       
         MVC   GMKMSG,MESGNUM      MSG#                                         
         MVI   GMKLANG,0           LEAVE LANG NAME BLANK                        
         OI    RPTFLG,RFHOLEQ      INDICATE HOLE-REPORTING                      
         BAS   RE,RPTRECRD                                                      
         LA    R2,IOKEY                                                         
         B     PR130               READY TO CHECK NEXT MSG#                     
*                                                                               
*** AT LEAST 1 LANG FOR MSG#                                                    
*                                                                               
PR125    CLI   SELLNGST,0          REPORT HOLES ON LANGUAGE TOO?                
         BE    PR130                NOPE                                        
         SPACE 1                                                                
         ZIC   R3,SELLNGST         YEP, PICK OUT THOSE SELECTED                 
         LA    RE,SELLNGST+1        LANGS WHICH ARE HOLES                       
PR125A   ZIC   R4,FILLNGST                                                      
         LA    RF,FILLNGST+1                                                    
PR125B   CLC   0(1,RE),0(RF)       SEE IF SELECTED LANGUAGE                     
         BE    PR125C                                                           
         LA    RF,1(RF)             IS IN FILLNGST                              
         BCT   R4,PR125B                                                        
         LR    R0,RE               IT IS NOT IN FILLNGST,                       
         L     R2,AIOAREA2          PREPARE TO REPORT THIS HOLE                 
         MVC   GMKLANG,0(RE)       LANGUAGE                                     
         MVC   GMKSYS,SELSYS       SYSTEM                                       
         MVC   GMKMSG,MESGNUM      MSG#                                         
         OI    RPTFLG,RFHOLEQ      INDICATE HOLE-REPORTING                      
         BAS   RE,RPTRECRD                                                      
         LA    R2,IOKEY                                                         
         LR    RE,R0               RESTORE PLACE IN SELLNGST                    
PR125C   LA    RE,1(RE)            BUMP TO NEXT SELECTED LANG                   
         BCT   R3,PR125A           DO REST OF SELLNGST                          
*                                                                               
PR130    BE    PR100                                                            
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------- REPORT RECORD ---------------------------*         
*                                                                               
RPTRECRD NTR1                                                                   
*         FORMATS A SINGLE RECORD ON REPORT LINES AND REPORTS IT                
*         AT ENTRY,                                                             
*           R2-->RECORD                                                         
*                                                                               
         L     R1,AREP                                                          
         USING REPD,R1                                                          
         LA    R4,REPPS            R4-->PRINT LINES                             
         DROP  R1                                                               
         USING PRTD,R4                                                          
*                                                                               
** SYSTEM NAME                                                                  
*                                                                               
         MVC   APBYTE,GMKSYS       DISSYS ROUTINE RECOGNIZES                    
         CLI   GMKSYS,15            SYS#=0 IS GENERAL SYSTEM                    
         BNE   *+8                                                              
         MVI   APBYTE,0                                                         
         GOTO1 ADISSYS,APBYTE      GET SYSTEM NAME                              
         MVC   PRTSYS,APWORK                                                    
*                                                                               
** MESSAGE NUMBER                                                               
*                                                                               
         ZICM  R1,GMKMSG,(3)       PRINT MESSAGE NUMBER                         
         EDIT  (R1),(5,PRTMNUM),WRK=APWORK,DUB=APDUB                            
*                                                                               
** LANGUAGE                                                                     
*                                                                               
         TM    RPTFLG,RFHOLEQ      IF REPORTING HOLES,                          
         BZ    *+12                                                             
         CLI   GMKLANG,0            AND GMKLANG=0,                              
         BE    RPR05                THEN GO STRAIGHT TO PRINT TEXT              
         SPACE 1                                                                
         MVC   APBYTE,GMKLANG      LANGUAGE CODE                                
         XI    APBYTE,X'FF'        INVERT                                       
         GOTO1 ADISLNG,APBYTE                                                   
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVC   PRTLANG,LANGFUL-LANGTABD(R1)                                     
*                                                                               
** EQUATE NAME                                                                  
*                                                                               
         TM    RPTFLG,RFHOLEQ      SKIP EQUATE NAMES IF                         
         BO    RPR05                REPORTING HOLES                             
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GMQSYELC     GET EQUATE NAME                              
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING GMQSYD,R3                                                        
         MVC   PRTEQNM,GMQSYSYM                                                 
         DROP  R3                                                               
*                                                                               
** DICTIONARY TEXT                                                              
*                                                                               
RPR05    TM    RPTFLG,RFHOLEQ             IF REPORTING HOLES,                   
         BZ    RPR05A                                                           
         MVC   PRTMSG(10),=C'** HOLE **'   PRINT THIS MESSAGE                   
         ZAP   PRTLNCNT,=P'1'                                                   
         B     RPR20                                                            
*                                                                               
RPR05A   XC    MESGTXT,MESGTXT     GET DICTIONARY TEXT                          
         ZIC   R1,GMSGELL          NOTE MSG ELEM ALWAYS FIXED POSN              
         LA    R0,GMSGFXDL         R0=L(ELEMENT OVERHEAD)                       
         SR    R1,R0               R1=L(MESSAGE)                                
         STC   R1,LMESGTXT                                                      
         BCTR  R1,0                                                             
         EXMVC R1,MESGTXT,GMSGTXT                                               
         SPACE 1                                                                
         NI    MESGFLG,X'FF'-MFTRUEQ                                            
         TM    GMFELEM,GMTRNSQ     IF TRUE TRANSLATION,                         
         BZ    *+8                                                              
         OI    MESGFLG,MFTRUEQ      SET TRUE TRANS FLAG                         
         SPACE 1                                                                
         GOTO1 =A(SPCFRMT),APPARM,(R5),(R6),(R7),(RC),(1,0),           +        
               RR=APRELO                                                        
         SPACE 1                                                                
         ZIC   RE,LMESG1           1ST MESSAGE LINE                             
         LA    RF,L'PRTMSG                                                      
         CR    RE,RF                                                            
         BH    *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                                                             
         EXMVC RF,PRTMSG,MESG1                                                  
         ZAP   PRTLNCNT,=P'1'                                                   
         SPACE 1                                                                
         OC    LMESG2,LMESG2                                                    
         BZ    RPR10                                                            
         LA    R4,L'REPPS(R4)      BUMP TO NEXT PRINT LINE                      
         ZIC   RE,LMESG2           2ND MESSAGE LINE                             
         LA    RF,L'PRTMSG                                                      
         CR    RE,RF                                                            
         BH    *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                                                             
         EXMVC RF,PRTMSG,MESG2                                                  
         AP    PRTLNCNT,=P'1'                                                   
*                                                                               
** EXTRA EQUATES AND COMMENTS                                                   
*                                                                               
RPR10    XC    APELEM,APELEM       GET FULL TEXT                                
         MVI   APELEM,GMTXTELC      (EXTRA EQUATES AND COMMENTS)                
         GOTO1 AGETELS,GMSGD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    RPR20                                                            
         SPACE 1                                                                
         USING GMTXTD,R3                                                        
RPR10A   LA    R4,L'REPPS(R4)      BUMP TO NEXT PRINT LINE                      
         AP    PRTLNCNT,=P'1'                                                   
         ZIC   RF,GMTXTELL         RF=L(ELEMENT)                                
         LA    RE,GMTXTFXD         RF=L(FIXED OVERHEAD OF ELEM)                 
         SR    RF,RE               RF=L(TEXT)                                   
         LA    RE,L'PRTCMMT        MAKE SURE L(TEXT) IS                         
         CR    RF,RE                NOT GREATER THAN COMMENT LINE               
         BL    *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                                                             
         EXMVC RF,PRTCMMT,GMTXTLIN                                              
*                                                                               
RPR15    ZIC   RF,GMTXTELL         GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         CLI   GMTXTEL,0           IF END OF RECORD,                            
         BE    RPR20                GO PRINT STUFF                              
         CLI   GMTXTEL,GMTXTELC    IF FULL TEXT ELEMENT,                        
         BE    RPR10A               MOVE STUFF INTO LINE                        
         B     RPR15               ELSE, GET NEXT ELEMENT                       
         DROP  R3                                                               
*                                                                               
RPR20    XC    APDUB,APDUB                                                      
         MVC   APDUB+(L'APDUB-L'PRTLNCNT)(L'PRTLNCNT),PRTLNCNT                  
         CVB   RE,APDUB                                                         
         L     R1,AREP                                                          
         USING REPD,R1                                                          
         STC   RE,REPALLN          ALLOW THIS MANY LINES                        
         SPACE 1                                                                
         GOTO1 VREPORT,(R1)                                                     
         MVI   RPTFLG,0            CLEAR REPORT FLAG BEFORE RETURNING           
         B     EXIT                                                             
         DROP  R1,R4                                                            
***********************************************************************         
         SPACE 3                                                                
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
*============================== SCAN TEXT ============================*         
SCANTXT  NTR1                                                                   
*                                                                               
*         AT ENTRY, IOAREA1 CONTAINS TEXT TO SEARCH IN                          
*                                                                               
         L     R2,AIOAREA1                                                      
         ZIC   R1,GMSGELL          L'MESSAGE ELEMENT                            
         SH    R1,=Y(GMSGFXDL+1+1)                                              
         ZIC   R0,GMSGTXT                                                       
         SR    R1,R0               R1=TOTAL L(ACTUAL MSG TEXT) - 1              
         MVI   APWORK,C' '         PRESET WORK TO SPACES                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK+1(0),APWORK                                               
         LA    R2,GMSGTXT+1                                                     
         AR    R2,R0               R2-->START OF ACTUAL MSG TEXT                
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    APWORK(0),0(R2)     MOVE IN MESSAGE AND CONVERT TO U/C           
         LA    R6,OPTSCAN          R6-->SCAN TEXT(S)                            
*                                                                               
STXT10   CLI   0(R6),0                                                          
         BE    CCEQUAL                                                          
         SPACE 1                                                                
         ZIC   R1,0(R6)            R1 = L(SCAN STRING)                          
         L     R2,AIOAREA1         RESET TO SCAN ENTIRE MSG TEXT                
         LA    R2,GMSGTXT                                                       
         ZIC   R0,0(R2)            R0 = # OF ENTRIES TO SCAN                    
         SPACE 1                                                                
         LA    R3,APWORK           R3-->START OF MSG TEXT IN APWORK             
STXT20   LA    R2,1(R2)            R2-->L(ENTRY)                                
         ZIC   RF,0(R2)            RF = L(ENTRY) = L'TEXT                       
         SLL   RF,25               REMOVE HIGH ORDER BIT IN THE LENGTH          
         SRL   RF,25                IN CASE OF SPLITTER ENTRIES                 
         CR    RF,R1               CHECK L'TEXT NOT LESS THAN L'SCAN            
         BL    STXT40               IF LESS, GET NEXT ENTRY                     
         SR    RF,R1                                                            
         LA    RF,1(RF)            RF = # OF COMPARES REQUIRED                  
*                                                                               
STXT30   LR    R4,R3                                                            
         LA    R5,1(R6)                                                         
STXT30A  CLI   0(R5),C'*'          WILDCARD IN SCANTEXT                         
         BE    STXT30C                                                          
         CLC   0(1,R4),0(R5)       MATCH CHAR BY CHAR                           
         BNE   STXT30D              MISMATCH OCCURRED                           
STXT30C  LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R1,STXT30A          MATCH REST OF SEQUENCE                       
         B     STXT50              GOT GOOD MATCH, GET NEXT SCAN TEXT           
STXT30D  LA    R3,1(R3)            GET NEXT SEQUENCE OF CHARS TO MATCH          
         ZIC   R1,0(R6)            RESET L(SCAN TEXT), AND                      
         BCT   RF,STXT30            GET NEXT SEQUENCE OF CHARS FOR MTCH         
         SPACE 1                                                                
         LR    RF,R1               NOT IN THIS ENTRY, TRY NEXT ENTRY            
         BCTR  RF,0                                                             
STXT40   AR    R3,RF               R3-->NEXT ENTRY TO TRY                       
         BCT   R0,STXT20                                                        
         B     CCNEQUAL            TEXT IS NOT IN ANY ENTRY, EXIT               
*                                                                               
STXT50   ZIC   R1,0(R6)                                                         
         LA    R6,1(R1,R6)         GET NEXT SCAN TEXT STRING                    
         B     STXT10                                                           
***********************************************************************         
         SPACE 3                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*========================= VALIDATE EQUNAME ==========================*         
CHKEQPSV NTR1                                                                   
*         MAKE SURE USER DOESN'T ADD DUPLICATE EQUATE NAMES                     
*                                                                               
         MVC   APRECKEY,IOKEY                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING GMSGD,R2                                                         
         MVI   GQKREC,GQKRECQ      RECORD TYPE                                  
         MVC   GQKQNAME,FVIFLD     EQUATE NAME                                  
         LA    R1,IOGENDIR+IO2+IOHIGH                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                NO REASON FOR ERROR                          
         CLC   GQKQNAME,FVIFLD     EQUATE NAMES THE SAME?                       
         BNE   XCHKEQPS             NOPE, NO PROBLEM HERE                       
         SPACE 1                                                                
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#QEXST)),   +        
               RR=APRELO                                                        
         MVC   DPRFX,GQKQNAME      HOLD THE SYSTEM PREFIX                       
         MVC   DMESGNUM,GQKMNUM     AND THE MSG# AROUND                         
         MVC   IOKEY,APRECKEY      RESTORE ORIGINAL KEY                         
         B     CCNEQUAL                                                         
*                                                                               
XCHKEQPS MVC   IOKEY,APRECKEY      RESTORE ORIGINAL KEY                         
         B     CCEQUAL                                                          
                                                                                
         DROP  R2                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== LIST-FILTERING ROUTINES ======================*         
FILT1    NTR1                                                                   
*         AT ENTRY,                                                             
*           APBYTE CONTAINS ELEMENT FLAG                                        
*           BYTE   CONTAINS SELECTED OPTION                                     
*           OPT1   CONTAINS OPTION 1                                            
*           OPT2   CONTAINS OPTION 2                                            
*           RF   = RESPECTIVE FLAG BIT IN ELEMENT FLAG                          
*         AT EXIT,                                                              
*           CC = 0  IF FILTERED OKAY                                            
*           CC <>0  IF FILTERED NOT OKAY                                        
*                                                                               
         CLI   BYTE,C'A'           WANT ALL OPTIONS?                            
         BE    FLT1XYES             YEP, SKIP TO NEXT STEP                      
*                                                                               
         CLC   BYTE,OPT1           OPTION 1 SPECIFIED                           
         BNE   FLT1A                                                            
         EX    RF,*+8              DO AN EXECUTED                               
         B     *+8                                                              
         TM    APBYTE,0             TEST-UNDER-MASK                             
         BO    FLT1XYES                                                         
         BZ    FLT1XNO                                                          
*                                                                               
FLT1A    CLC   BYTE,OPT2           OPTION 2 SPECIFIED                           
         BE    *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+8              DO AN EXECUTED                               
         B     *+8                                                              
         TM    APBYTE,0            TEST-UNDER-MASK                              
         BZ    FLT1XYES                                                         
         BO    FLT1XNO                                                          
*                                                                               
FLT1XYES B     CCEQUAL                                                          
FLT1XNO  B     CCNEQUAL                                                         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== GET A(UPCASE TABLE) ========================*         
GTAUPTAB NTR1                                                                   
*         AT ENTRY,                                                             
*           BYTE = LANGUAGE CODE                                                
*                                                                               
         GOTO1 VGETFACT,APPARM,0   GET ADDRESS                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   AXLATAB,FAXLATES     OF A(TRANSLATE TABLES)                      
         DROP  R1                                                               
*                                                                               
         L     RF,AXLATAB          GET A(RESPECTIVE TRANSLATE TABLE)            
GTAUP10  CLI   0(RF),15            THERE ARE A MAX OF 15 LANGUAGES              
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,0(RF)          MATCH AGAINST LANGUAGE CODE                  
         BE    GTAUP20                                                          
         LA    RF,4*4(RF)          FOUR ADDRESSES PER ENTRY                     
         B     GTAUP10             BUMP TO & COMPARE WITH NEXT ENTRY            
*                                                                               
GTAUP20  L     RF,2*4(RF)          3RD ADDRESS = A(UPCASE TABLE)                
         ST    RF,AUPCASE                                                       
*                                                                               
XGTAUPTA B     EXIT                                                             
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*========================== SKIP X'4000' BIT =========================*         
SKP40BIT NTR1                                                                   
*         RF  CONTAINS NUMBER UNDER SCRUTINY                                    
*                                                                               
         ZICM  R0,X4000BIT,(3)                                                  
SKP10    NR    R0,RF                                                            
         BZ    XSKP                                                             
         LA    RF,1(RF)                                                         
         B     SKP10                                                            
*                                                                               
XSKP     XIT1  REGS=(RF)                                                        
***********************************************************************         
         EJECT                                                                  
**********************************************************************          
* GENERAL FIELD XMT IF CHANGED                                       *          
* R1=A(TWAHDR)                                                       *          
* APWORK MUST CONTAIN THE NEW TEXT                                   *          
**********************************************************************          
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     KNOCK OFF HEADER EXTENSION                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         COMPARE FIELD CONTENTS                       
         BER   RE                                                               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW FIELD                            
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ LITERAL POOL ===========================*         
         LTORG                                                                  
***********************************************************************         
         SPACE 3                                                                
***********************************************************************         
*========================= DEFINED CONSTANTS =========================*         
         DS    0H                  FORCE HALF-WORD BOUNDARY                     
BNDRY    DC    AL2(BNDRYQ)         A MSG# BOUNDARY                              
X4000BIT DC    X'4000'                                                          
X40BITQ  EQU   X'40'                                                            
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
PROGTAB  EQU   *                   EXTRA VALID PROGRAM ENTRIES                  
         DC    CL8'OFFLINE'                                                     
         DC    CL8'ALL'                                                         
         DC    H'0'                                                             
         SPACE 1                                                                
REPDESCL DC    C'DCTNRY LIST'                                                   
         SPACE 1                                                                
NUMER1   EQU   (DCRLNGX+L'DCRLNGX)-DCRLNG1H                                     
DENOM1   EQU   (DCRLNG1+L'DCRLNG1)-DCRLNG1H                                     
NMLNGLN  EQU   NUMER1/DENOM1                                                    
         SPACE 1                                                                
         DS    0CL(L'APWORKX-GMCTLLNQ+1)  CAN APWORK HOLD GMCTLD ELEM?          
***********************************************************************         
         SPACE 3                                                                
         DROP  R7,R8,R9,RA,RB,RC                                                
         EJECT                                                                  
***********************************************************************         
*========================= TYPE 'G' MESSAGES =========================*         
         DS    0CL(X'4000'-(*-GEN21))                                           
         ORG   GEN21+X'4000'                                                    
SPCFRMT  NMOD1 0,*SPCFRMT,RR=RE                                                 
         SPACE 1                                                                
         LM    R5,R7,0(R1)         GET ADDRESS OF WORKING STORAGES              
         USING TWAD,R5                                                          
         USING SAVAREA,R6                                                       
         USING WORKD,R7                                                         
         L     RC,12(R1)                                                        
         USING LOCALD,RC                                                        
         ST    RE,MYRELO                                                        
         SPACE 1                                                                
         CLI   16(R1),0            CAME HERE FOR VALIDATING                     
         BE    VALTYPD                                                          
         CLI   16(R1),1            CAME HERE FOR DISPLAYING                     
         BE    DISPIC                                                           
         DC    H'0'                                                             
*                                                                               
EXIT2    XIT1                                                                   
         EJECT                                                                  
*========================= VALIDATE MESSAGES =========================*         
VALTYPD  DS    0H                                                               
*                                                                               
** CHECK LENGTH OF INPUT(S)                                                     
*                                                                               
         MVC   FVMSGNO,=AL2(FVFSHRT) ASSUME INPUT IS TOO SHORT                  
         LA    R1,DCTMSG1H         FOR DICTIONARY-TYPE MESSAGES,                
         ST    R1,FVADDR                                                        
         CLI   5(R1),3              L(INPUT)>=3                                 
         BL    VALTYPNX                                                         
*                                                                               
** CHECK FOR CORRECT DELIMITERS                                                 
*                                                                               
         LA    R3,MESG1                                                         
         CLI   0(R3),DELIM         START DELIMITER SHOULD BE C'>'               
         BE    VTD15                                                            
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BSDLM)),   +        
               RR=MYRELO                                                        
         LA    R1,DCTMSG1H                                                      
         ST    R1,FVADDR           PLACE CURSOR HERE                            
         B     VALTYPNX                                                         
*                                                                               
VTD15    MVI   0(R3),DELIM         CHECK END DELIMITER                          
         NI    MESGFLG,X'FF'-MFCNTDQ  INITIALIZE CONTINUATION FLAG              
         CLI   MESG2,CONTD         DOES LINE 2 CONTAIN CONT. SYMBOL?            
         BNE   *+8                                                              
         OI    MESGFLG,MFCNTDQ      YEP!                                        
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BEDLM)),   +        
               RR=MYRELO                                                        
         LA    R1,DCTMSG1H                                                      
         ST    R1,FVADDR           PLACE CURSOR HERE                            
         ZIC   R0,LMESG1                                                        
         AR    R3,R0                                                            
         BCTR  R3,0                                                             
         LA    R0,MESG1                                                         
         BAS   RE,CNTDLIM          COUNT # OF C'>' @ END.  IF                   
         BZ    *+16                                                             
         TM    MESGFLG,MFCNTDQ      ODD, THERE SHOULD NOT BE ANY                
         BNZ   VALTYPNX             CONTINUATION                                
         B     *+12                                                             
         TM    MESGFLG,MFCNTDQ      EVEN, THERE SHOULD BE A                     
         BNO   VALTYPNX             CONTINUATION                                
         SPACE 1                                                                
         CLI   LMESG2,0            CHECK DELIMITERS ON 2ND LINE                 
         BE    VTD30               NO INPUT ON 2ND LINE                         
         SPACE 1                                                                
         LA    R3,MESG2                                                         
         TM    MESGFLG,MFCNTDQ     NEED TO CHECK DELIM OF 2ND LINE?             
         BO    VTD20A               NOPE                                        
         CLI   0(R3),DELIM         START DELIMITER SHOULD BE C'>'               
         BE    VTD20                                                            
         LA    R1,DCTMSG2H                                                      
         ST    R1,FVADDR           SET CURSOR POSITION                          
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BSDLM)),   +        
               RR=MYRELO                                                        
         B     VALTYPNX                                                         
*                                                                               
VTD20    MVC   FVMSGNO,=AL2(FVFSHRT)   FOR THE SECOND LINE,                     
         CLI   5(R1),4                  L(INPUT)>=4                             
         BL    VALTYPNX                                                         
*                                                                               
** CHECK END DELIMITER                                                          
*                                                                               
VTD20A   GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#BEDLM)),   +        
               RR=MYRELO                                                        
         ZIC   R0,LMESG2                                                        
         AR    R3,R0                                                            
         BCTR  R3,0                                                             
         LA    R0,MESG2                                                         
         BAS   RE,CNTDLIM          COUNT # OF END DELIMITERS @ END OF           
         BZ    VALTYPNX             2ND LINE, SHOULD BE ODD # OF THEM           
*                                                                               
** CONCATENATE MESG1 AND MESG2                                                  
*                                                                               
VTD30    XC    MESGTXT,MESGTXT                                                  
         LA    R3,MESGTXT                                                       
         ZIC   R1,LMESG1           EXECUTED LENGTH ON 1ST MESG                  
         LR    RF,R1                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),MESG1                                                   
*                                                                               
         CLI   LMESG2,0                                                         
         BE    VTD40                                                            
         AR    R3,R1               R3-->END DELIMITER (S/B C'>')                
         ZIC   R1,LMESG2           EXECUTED LENGTH ON 2ND MESG                  
         BCTR  R1,0                                                             
         AR    RF,R1               RF=L(TEXT) IN MESGTXT                        
         BCTR  R1,0                EXCLUDE START DELIMITER                      
         EXMVC R1,1(R3),MESG2+1                                                 
         SPACE 1                                                                
         NI    MESGFLG,X'FF'-MFCNTDQ  TURN OFF CONTINUATION FLAG                
         LA    R1,DCTMSG1H         ANY ERRORS REGARDING MSG TEXT                
         ST    R1,FVADDR            WILL HAVE CURSOR ON 1ST MSG LINE            
*                                                                               
** COUNT # OF LITERALS                                                          
*                                                                               
VTD40    STC   RF,LMESGTXT                                                      
         SR    RE,RE               RE COUNTS # OF LITERALS                      
         LA    R3,MESGTXT                                                       
VTD45    CLC   0(2,R3),SPDELIM     IF '>>'                                      
         BE    VTD45B                                                           
         CLC   0(2,R3),SPCOLON      OR '::' IS ENCOUNTERED,                     
         BE    VTD45B               SKIP OVER IT                                
         CLI   0(R3),SPLITTER      IF SPLITTER (:), MOVE TO                     
         BE    VTD45C               NEXT CHARACTER                              
         CLI   0(R3),DELIM         IF '>', INCREMENT RE                         
         BNE   VTD45C                                                           
*                                                                               
VTD45A   LA    RE,1(RE)            INCREMENT # OF LITERALS COUNTER              
         B     VTD45C                                                           
VTD45B   LA    R3,1(R3)            BUMP EXTRA TIME FOR SPEC CHARS               
         BCTR  RF,0                                                             
VTD45C   LA    R3,1(R3)            BUMP NORMAL                                  
         BCT   RF,VTD45                                                         
         BCTR  RE,0                #(LITERALS) = #(DELIMITERS) - 1              
*                                                                               
** INITIALIZE MESGFMT, POINTERS, ETC.                                           
*                                                                               
VTD100   XC    MESGFMT,MESGFMT                                                  
         MVI   APBYTE,0            APBYTE=PREV L(LITERAL)=0 @ START             
         SR    R1,R1               R1 KEEPS L(LITERAL)                          
         LA    R2,MESGTXT+1        R2-->START OF MSG IN NORMAL FORMAT           
         LA    R3,MESGFMT+1        R3-->START OF L(LITERALS) ENTRIES            
         LA    R4,0(R3,RE)         R4-->START OF LITERALS ENTRIES               
         LA    R6,1(RE)            R6=L(MESGFMT)=#(LITERALS)+1, SO FAR          
         ZIC   RF,LMESGTXT                                                      
         BCTR  RF,0                STARTING AT MESGTXT+1 (IGNORE DELIM)         
         NI    MESGFLG,X'FF'-MFSPLTQ   TURN OFF SPLITTER FLAG                   
*                                                                               
** CONVERT TO SPECIALIZE FORMAT                                                 
*                                                                               
         STC   RE,MESGFMT          #(LITERALS) GOES IN 1ST BYTE                 
VTD50    CLC   0(2,R2),SPDELIM     '>>' BECOMES '>'                             
         BNE   VTD50A                                                           
         MVI   0(R4),DELIM                                                      
         B     VTD80A                                                           
VTD50A   CLC   0(2,R2),SPCOLON     '::' BECOMES ':'                             
         BNE   VTD60A                                                           
         MVI   0(R4),SPLITTER                                                   
         B     VTD80A                                                           
*                                                                               
VTD60A   CLI   0(R2),SPLITTER      IF SPLITTER,                                 
         BNE   VTD60B                                                           
         OI    MESGFLG,MFSPLTQ      TURN ON SPLITTER FLAG                       
         MVI   0(R4),SPLITTER                                                   
         B     VTD80B              BUMP POINTERS                                
*                                                                               
VTD60B   CLI   0(R2),DELIM         DELIMITER ENCOUNTERED                        
         BE    VTD70                                                            
*                                                                               
         MVC   0(1,R4),0(R2)       MOVE IN THE NORMAL CHARACTER                 
         B     VTD80B               AND BUMP POINTERS                           
*                                                                               
VTD70    TM    MESGFLG,MFSPLTQ     SPLITTER PRESENT IN LITERAL?                 
         BZ    VTD70A                                                           
         BAS   RE,DOSPLIT          YES, PROCESS SPLITTER SITUATION              
         BNE   VALTYPNX            IF WE CAME BACK OK,                          
         BCTR  R1,0                 DECREMENT L(LITERAL)--SPLTTR GONE,          
         BCTR  R4,0                 BUMP DESTINATION POINTER BACK ONE,          
         B     *+8                  AND SKIP STC INSTRUCTION                    
VTD70A   STC   R1,0(R3)            STORE L(LITERAL)                             
         CLC   APBYTE,0(R3)        COMPARE AGAINST PREV LENGTH                  
         BL    VTD70B               NEW LENGTH IS HIGHER, OK                    
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#NORDR)),   +        
               RR=MYRELO                                                        
         B     VALTYPNX                                                         
VTD70B   MVC   APBYTE,0(R3)        NEW LEN BECOMES PREV LEN                     
         AR    R6,R1               UPDATE L(MESGFMT)                            
         LA    R3,1(R3)            BUMP L(LITERAL)-ENTRY POINTER                
         SR    R1,R1               RESET L(LITERAL)                             
         B     VTD80C              BUMP POINTERS AND COUNTERS                   
*                                                                               
VTD80A   LA    R2,1(R2)            BUMP AN EXTRA TIME FOR                       
         BCTR  RF,0                 SPECIAL CHARACTERS                          
VTD80B   LA    R4,1(R4)            BUMP DESTINATION POINTER                     
         LA    R1,1(R1)            INCREMENT L(LITERAL)                         
VTD80C   LA    R2,1(R2)            BUMP SOURCE POINTER                          
         BCT   RF,VTD50            DECREMENT COUNTER AND REPEAT PROCESS         
*                                                                               
VTD90    IC    RF,APBYTE           SAVE L(LONGEST LITERAL) - 1                  
         BCTR  RF,0                                                             
         ST    RF,LENCHK            IN LENCHK FOR LATER COMPARISON              
*                                                                               
VTD95    LR    R1,R6               RETURN L(MESGFMT) IN R1                      
         BCTR  R1,0                GET SET FOR EX INSTRUCTION                   
*                                                                               
VALTYPYX CR    R1,R1               EXIT VALTYPD WITH CC=0                       
         XIT1  REGS=(R1)            AND SAVING R1                               
*                                                                               
VALTYPNX LA    RF,1                EXIT VALTYPD WITH CC<>0                      
         LTR   RF,RF                                                            
         B     EXIT2                                                            
         SPACE 2                                                                
*------------------------- COUNT # DELIMITERS ------------------------*         
*                                                                               
*         AT ENTRY,                                                             
*           R0 --> MESSAGE                                                      
*           R3 --> END OF MESSAGE                                               
*         AT EXIT,                                                              
*           CC = 0 IF EVEN # OF DELIMITERS                                      
*           CC <> 0 IF ODD # OF DELIMITERS                                      
*                                                                               
CNTDLIM  ST    RE,SAVERE                                                        
         SR    RF,RF               RF COUNTS # OF DELIMITERS                    
CNTDLIM1 CLI   0(R3),DELIM                                                      
         BNE   CNTDLIM2                                                         
         LA    RF,1(RF)                                                         
         BCTR  R3,0                                                             
         CR    R0,R3                                                            
         BL    CNTDLIM1                                                         
CNTDLIM2 LA    RE,1                                                             
         NR    RE,RF                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
*------------------------- SPLITTER HANDLING -------------------------*         
DOSPLIT  NTR1                                                                   
*         AT ENTRY,                                                             
*           R1-->L(LITERAL), INCLUDING SPLITTER                                 
*           R3-->LOCATION TO STORE L(LITERAL)                                   
*           R4-->NEXT LOCATION TO PUT NEXT TEXT                                 
*                                                                               
         LR    R0,R1               SAVE VALUE IN R1                             
         GOTO1 =A(DOMSG),MYDMCB,(R7),(RC),('GTMERR',=AL2(CE#SPLTR)),   +        
               RR=MYRELO                                                        
         LR    R1,R0               RESTORE R1                                   
         LA    RF,1                SPLITTER MUST BE 1/2WAY IN MESSAGE,          
         NR    RF,R1                TEST PARITY OF L(LITERAL)                   
         BZ    XDOSPLTN            IF EVEN, THEN EXIT WITH ERROR                
*                                                                               
         BCTR  R1,0                MAKE SPECIAL LENGTH                          
         LR    RF,R1                                                            
         LA    R0,X'80'                                                         
         OR    R1,R0               R1=SPECIAL LENGTH                            
*                                                                               
         LR    RE,R4                                                            
         BCTR  RE,0                RE-->LAST CHAR OR LITERAL                    
         SRL   RF,1                RF=(L(LITERAL)-1)/2                          
         SR    RE,RF               MOVE BACK HALFWAY IN LITERAL                 
         CLI   0(RE),SPLITTER      IS SPLITTER THERE?                           
         BNE   XDOSPLTN             NO, SPLITTER ISN'T 1/2WAY==>ERROR           
         EXMVC RF,0(RE),1(RE)      SHIFT TEXT TO LEFT BY 1 (OVERWRT             
         STC   R1,0(R3)             SPLITTER), AND STORE SPECIAL LENGTH         
         NI    MESGFLG,X'FF'-MFSPLTQ   RESET FLAG FOR NEXT LITERAL              
*                                                                               
XDOSPLTY SR    RF,RF               EXIT DOSPLIT WITH CC=0                       
         B     *+8                                                              
XDOSPLTN LA    RF,1                EXIT DOSPLIT WITH CC<>0                      
         LTR   RF,RF                                                            
         B     EXIT2                                                            
         EJECT                                                                  
*====================== DISPLAY TYPE'G' MESSAGE ======================*         
*                                                                               
DISPIC   DS    0H                                                               
*DISPIC   NTR1                                                                  
*         CONVERT DATA DICTIONARY TYPE COMPLEX MESSAGE DATA FOR                 
*          DISPLAY.  ON ENTRY, FORMAT OF DATA IS:                               
*           <STRNG CNT><LIST(<STRNG LEN>)><LIST(<STRNG TEXT>)>                  
*          ALSO DEAL WITH IMBEDDED DELIMITER CHARACTER AND                      
*          SPLIT STRINGS AND MARK TRUNCATED STRINGS                             
*         AT ENTRY,                                                             
*           MESGTXT  CONTAINS MESSAGE FROM RECORD                               
*           LMESGTXT  = LENGTH OF MESSAGE                                       
*         AT EXIT,                                                              
*           MESG1  CONTAINS MESSAGE FOR LINE1                                   
*           MESG2  CONTAINS MESSAGE FOR LINE2                                   
*           LMESG1 = L(MESG1)                                                   
*           LMESG2 = L(MESG2)                                                   
*                                                                               
** INITIALIZE POINTERS, COUNTERS, AND BUFFERS                                   
*                                                                               
         LA    R0,L'DCTMSG1        HOLD ONTO MAX LENGTH                         
         STH   R0,APHALF            OF DISPLAY LINE                             
*                                                                               
         MVI   LMESG1,0                                                         
         MVI   LMESG2,0                                                         
         MVC   MESG1,SPACES2                                                    
         MVC   MESG2,SPACES2                                                    
*                                                                               
         ZIC   RF,MESGTXT          RF = # OF LITERALS                           
         LA    R3,MESGTXT+1        R3-->L(LITERALS) ENTRIES                     
         LA    R4,0(RF,R3)         R4-->LITERALS ENTRIES                        
         LA    R2,MESGFMT          R2-->CONVERTED-MESSAGE BUFFER                
         ST    R2,AMESG2                                                        
         SR    R1,R1               R1 COUNTS L(DISPLAY MESSAGE)                 
         NI    MESGFLG,X'FF'-MFSPLTQ   ASSUME NO SPLITTER                       
*                                                                               
** START CONVERTING                                                             
*                                                                               
         MVI   0(R2),DELIM         START OFF WITH START-DELIMITER               
         LA    R1,1(R1)             AND BUMP COUNTER                            
         LA    R2,1(R2)             AND POINTER                                 
*                                                                               
*** GET L(LITERAL) AS LOOP COUNTER                                              
*                                                                               
DSPC5    XC    SCANTBL,SCANTBL     USE SCANTBL TO HOLD 1 LITERAL                
         NI    MESGFLG,X'FF'-MFSPLTQ                                            
         ZIC   R0,0(R3)            R0 = L(LITERAL)                              
         LA    RE,X'80'            CHECK TO SEE IF SPECIAL LENGTH               
         NR    RE,R0                I.E. SPLITTER PRESENT                       
         BZ    DSPC5A              NOT SPECIAL LENGTH                           
         XR    R0,RE               TURN OFF X'80', UNDO SPEC LEN                
         OI    MESGFLG,MFSPLTQ     TURN ON SPLITTER FLAG                        
DSPC5A   LR    RE,R0               RE = LOOP COUNTER = L(LITERAL)               
         OR    RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                DIE IF L(LITERAL) = 0                        
         STC   RE,APBYTE           HOLD ONTO L(CURRENT LITERAL)                 
         BCTR  RE,0                                                             
         EXMVC RE,SCANTBL,0(R4)    MOVE LITERAL INTO SCANTBL                    
         LA    R4,1(R4,RE)         BUMP R4 TO NEXT LITERAL                      
         TM    MESGFLG,MFSPLTQ                                                  
         BZ    DSPC5B              SKIP SPLITTER PROCESSES                      
         LA    RE,1(RE)                                                         
         SRL   RE,1                RE=0.5*L(LITERAL)                            
         ST    RE,APFULL           APFULL SET TO 0.5*L(LITERAL)                 
DSPC5B   LA    R8,SCANTBL          USE R8 AS POINTER TO SCANTBL                 
         ZIC   RE,APBYTE           SET RE UP FOR BCT LOOP                       
*                                                                               
*** GET A(TEXT FOR 2ND DISPLAY LINE)                                            
*                                                                               
         CH    R1,APHALF           IF L(MSG)>L(DISPLAY LINE)                    
         BH    *+8                  THEN WE KNOW WHERE THE TEXT                 
         ST    R2,AMESG2            FOR 2ND DISPLAY LINE BEGINS                 
*                                                                               
*** CHECK FOR CHARACTERS TO BE IN SPECIAL FORM                                  
*                                                                               
DSPC10   TM    MESGFLG,MFSPLTQ     A SPLITTER SITUATION?                        
         BZ    DSPC10A              NOPE                                        
         C     RE,APFULL            YEP, GET SPLITTER IN TEXT                   
         BNE   DSPC10A              AT 1/2WAY MARK                              
         MVI   0(R2),SPLITTER                                                   
         LA    R1,1(R1)            INCREMENT L(DISPLAY MESSAGE)                 
         LA    R2,1(R2)            BUMP POINTER TO PUT NEXT TEXT                
*                                                                               
DSPC10A  CLI   0(R8),DELIM         '>' BECOMES '>>'                             
         BNE   DSPC10B                                                          
         MVC   0(L'SPDELIM,R2),SPDELIM                                          
         B     DSPC30A                                                          
DSPC10B  CLI   0(R8),SPLITTER      ':' BECOMES '::'                             
         BNE   DSPC20                                                           
         MVC   0(L'SPCOLON,R2),SPCOLON                                          
         B     DSPC30A                                                          
*                                                                               
*** NORMAL CHARACTERS                                                           
*                                                                               
DSPC20   MVC   0(1,R2),0(R8)                                                    
         B     DSPC30B                                                          
*                                                                               
*** UPDATE POINTERS AND COUNTERS                                                
*                                                                               
DSPC30A  LA    R2,1(R2)            BUMP XTRA TIME FORM SPEC CHAR                
         LA    R1,1(R1)             I.E. COMPENSATE FOR '>'                     
DSPC30B  LA    R2,1(R2)            BUMP DESTINATION POINTER,                    
         LA    R1,1(R1)             L(DISPLAY MESSAGE) COUNTER, AND             
         LA    R8,1(R8)             LITERALS POINTER                            
         BCT   RE,DSPC10                                                        
*                                                                               
*** END OF CONVERTING A LITERAL                                                 
*                                                                               
         MVI   0(R2),DELIM         INSERT DELIM AND                             
         LA    R2,1(R2)             BUMP POINTERS FOR IT                        
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)            GET L(NEXT LITERAL)                          
         BCT   RF,DSPC5            REPEAT LOOP UNTIL ALL LITERALS DONE          
*                                                                               
** END OF CONVERTING ENTIRE MESSAGE                                             
*                                                                               
         STC   R1,APBYTE                                                        
         CH    R1,APHALF           COMPARE L(DISPLAY MESSAGE)                   
         BH    DSPC50               AGAINST MAX L(DISPLAY LINE)                 
*                                                                               
         STC   R1,LMESG1           TEXT CAN FIT ON ONE LINE                     
         BCTR  R1,0                                                             
         EXMVC R1,MESG1,MESGFMT                                                 
         B     DSPC60                                                           
*                                                                               
DSPC50   CLI   MESGTXT,1           CHECK IF IT'S ONLY ONE LITERAL               
         BH    DSPC55               NOPE, DON'T NEED SPECIAL PROCESS            
         SPACE 1                                                                
         OI    MESGFLG,MFCNTDQ     FLAG CONTINUATION                            
         LA    RE,MESGFMT          FIND SUITABLE PLACE TO SPLIT                 
         LR    R0,RE                LITERAL (SPLIT AT BLANKS)                   
         ST    R0,AMESG2            AND STORE ITS ADDRESS                       
         LA    R1,1                R1 COUNTS # OF CHARACTERS                    
*                                                                               
DSPC50A  CH    R1,APHALF           REACHED LIMIT YET?                           
         BNL   DSPC50C              YEP                                         
         LA    R1,1(R1)             NOPE, INCREMENT COUNTER                     
         LA    RE,1(RE)              AND BUMP POINTER                           
         CLI   0(RE),C' '          AT A BLANK YET?                              
         BNE   DSPC50A              NOPE                                        
         ST    RE,AMESG2            YEP, HOLD ONTO ADDRESS                      
DSPC50B  LA    R1,1(R1)            CHECK FOR A STRING OF BLANKS                 
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BE    DSPC50B                                                          
         B     DSPC50A                                                          
*                                                                               
DSPC50C  C     R0,AMESG2           WERE THERE ANY BLANKS IN LITERAL?            
         BNE   DSPC55               NOPE                                        
         LA    RE,1(RE)             YES, SPLIT LITERAL AT THE LIMIT             
         ST    RE,AMESG2                                                        
*                                                                               
DSPC55   L     RE,AMESG2           RE-->TEXT FOR 2ND LINE                       
         LR    RF,R2               RF-->1 BYTE BEYOND END DELIM                 
         SR    RF,RE               RF = L(TEXT) FOR 2ND LINE                    
         ZIC   R1,APBYTE                                                        
         SR    R1,RF               R1 = EX LENGTH FOR MSG 1                     
         BCTR  RF,0                                                             
         SPACE 1                                                                
         EXMVC RF,MESG2+1,0(RE)                                                 
         MVI   MESG2,DELIM         PUT START DELIM ON 2ND LINE                  
         TM    MESGFLG,MFCNTDQ                                                  
         BZ    *+12                                                             
         MVI   MESG2,CONTD         PUT CONTINUATIN SYMBOL ON 2ND LINE           
         NI    MESGFLG,X'FF'-MFCNTDQ                                            
         LA    RF,2(RF)            RF = L(TEXT) + L(DELIM)                      
         STC   RF,LMESG2           SAVE LENGTH                                  
         SPACE 1                                                                
         BCTR  R1,0                                                             
         EXMVC R1,MESG1,MESGFMT                                                 
         LA    R1,1(R1)                                                         
         STC   R1,LMESG1           SAVE LENGTH                                  
*                                                                               
** SET FOR TRUE TRANSLATION                                                     
*                                                                               
DSPC60   TM    MESGFLG,MFTRUEQ     FOR LIST AND REPORT ONLY                     
         BO    XDISPIC                                                          
         MVI   MESG1,COMMON        START DELIM = '!'                            
*                                                                               
XDISPIC  B     EXIT2                                                            
         EJECT                                                                  
*==================== LITERAL POOL AND CONSTANTS =====================*         
         LTORG                                                                  
         SPACE 1                                                                
COMMON   EQU   C'!'                                                             
CONTD    EQU   C'<'                                                             
DELIM    EQU   C'>'                                                             
SPLITTER EQU   C':'                                                             
SPCOLON  DC    CL2'::'                                                          
SPDELIM  DC    CL2'>>'                                                          
SPACES2  DC    CL80' '                                                          
         SPACE 3                                                                
         DROP  R5,R6,R7,RB,RC                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= SET GETTXT PARAM ==========================*         
         ORG   GEN21+X'5000'                                                    
DOMSG    NMOD1 0,**DOMSG**,RR=RE                                                
*         AT ENTRY,                                                             
*           PARAM1 --> WORKD                                                    
*           PARAM2 --> LOCALD                                                   
*           PARAM3  =  (BYTE   0 ) MESSAGE TYPE                                 
*                      (BYTES 1-3) A(MESSAGE NUMBER)                            
*                                                                               
         L     R7,0(R1)            R7-->WORKD                                   
         USING WORKD,R7                                                         
         L     RC,4(R1)            RC-->LOCALD                                  
         USING LOCALD,RC                                                        
         LR    R0,RE               R0=RELO FACTOR                               
         SPACE 1                                                                
         XC    APPARM(8*L'APPARM),APPARM                                        
         LA    RF,APPARM                                                        
         USING GETTXTD,RF                                                       
         ZICM  RE,9(R1),(7)        RE-->MESSAGE NUMBER                          
         MVC   GTMSGNO,0(RE)                                                    
         MVC   GTMTYP,8(R1)                                                     
         MVI   GTMAXL,60           MAX LENGTH = 60                              
         CLI   ASONOFF,ASOFF                                                    
         BNE   *+10                                                             
         MVC   GTAOUT,AGETOUT+1    A(OUTPUT AREA)                               
         DROP  RF                                                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         DROP  R7,RB,RC                                                         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== TABLES ===============================*         
*                                                                               
*--------------------------- SYSTEM PREFIX ---------------------------*         
*                                                                               
PFXTABL  DS    0H                                                               
       ++INCLUDE DDPFXTBLE                                                      
         SPACE 1                                                                
PFXTABLX EQU   *                                                                
         DC    X'00'               END OF TABLE                                 
         EJECT                                                                  
*--------------------------- REPORT SPECS ----------------------------*         
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,54,C'DICTIONARY RECORD LIST'                                  
         SPEC  H2,54,C'----------------------'                                  
         SPEC  M1,1,C'SYSTEM  MSG#   LANGUAGE      EQUNAME   MESSAGE TE+        
               XT/COMMENTS/EXTRA EQUATES'                                       
         SPEC  M2,1,C'------  ----   --------      -------   ----------+        
               -------------------------'                                       
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== CTGENWRK =============================*         
         PRINT OFF                                                              
       ++INCLUDE CTGENWRK                                                       
         PRINT ON                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== GEGENMSG =============================*         
* GEGENMSG                                                                      
       ++INCLUDE GEGENMSG                                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= TWA DSECTS ============================*         
TWAD     DSECT                                                                  
*                                                                               
*------------------------- MAINTENANCE SCREEN ------------------------*         
*                                                                               
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE1D                                                       
         EJECT                                                                  
*                                                                               
*---------------------------- LIST SCREEN ----------------------------*         
*                                                                               
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE2D                                                       
         EJECT                                                                  
*                                                                               
*--------------------------- REPORT SCREEN ---------------------------*         
*                                                                               
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE6D                                                       
         ORG                                                                    
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== GETFACT PARAMETER BLOCK ======================*         
       ++INCLUDE FAFACTS                                                        
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== GETTXT PARAMETER BLOCK =======================*         
       ++INCLUDE FAGETTXTD                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= DDBLDICTD =============================*         
                                                                                
* DDBLDICTD has some equates which is used by CTGEN21                           
                                                                                
       ++INCLUDE DDBLDICTD                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
*                                                                               
*----------------------------- LIST LINE -----------------------------*         
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL2                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'DCLLIN1)                                                   
LISTSYS  DS    CL(L'SYSLSHRT)      SYSTEM                                       
         DS    CL1                                                              
LISTMNO  DS    CL(L'DCTNUM)        MESSAGE NUMBER                               
         DS    CL1                                                              
LISTLANG DS    CL1                 LANGUAGE                                     
         DS    CL1                                                              
LISTQNME DS    CL(L'GQKQNAME)      EQUATE NAME                                  
         DS    CL1                                                              
LISTRFP  DS    CL3                 RFP RULE #                                   
         DS    CL1                                                              
LISTMESG DS    CL(L'DCLLIN1-(LISTMESG-LISTLIN))                                 
         ORG   LISTLIN+L'LISTLIN                                                
*                                                                               
*----------------------------- PRINT LINE ----------------------------*         
PRTD     DSECT                     ** PRINT LINE LAYOUT **                      
PRTLIN   DS    0CL(L'REPP1)                                                     
PRTSYS   DS    CL(L'SYSLNAME)      SYSTEM NAME                                  
         DS    CL1                                                              
PRTMNUM  DS    CL(L'DCTNUM)        MESSAGE NUMBER                               
         DS    CL2                                                              
PRTLANG  DS    CL(L'LANGFUL)       LANGUAGE NAME                                
         DS    CL1                                                              
PRTEQNM  DS    CL(L'GQKQNAME)      EQUATE NAME                                  
         DS    CL2                                                              
PRTMSG   DS    CL(L'REPP1-(PRTMSG-PRTD))                                        
         ORG   PRTMSG+2                                                         
PRTCMMT  DS    CL(L'REPP1-(PRTCMMT-PRTD))                                       
*                                                                               
*----------------------------- DUMB KEY -----------------------------*          
DUMBKEYD DSECT                     ** FOR LIST APRECKEY **                      
DMBKMSG  DS    CL(L'SELMSG)        MSG#                                         
DMBKLNG  DS    CL(L'SELLNG)        LANGUAGE CODE                                
DMBKTRNS DS    CL(L'SELTRNS)       TRANSLATION                                  
DMBKRFP  DS    CL(L'SELRFP)        RFP RULE #                                   
DMBKLENQ EQU   *-DUMBKEYD                                                       
         DS    0CL((L'APRECKEY-L'GMKEY)-DMBKLENQ)                               
*               MAKE SURE APRECKEY HAS ENOUGH ROOM AT END TO                    
*                FIT ALL OF DUMBKEYD THINGS                                     
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== CTGEN21 WORKING STORAGE ======================*         
LOCALD   DSECT                     ADDRESSED BY RC                              
SELKEY   DS    0C                                                               
SELSYS   DS    XL(L'GMKSYS)        MESSAGE SYSTEM                               
SELTYPE  DS    XL(L'GMKTYP)        MESSAGE TYPE                                 
SELMSG   DS    XL(L'GMKMSG)        MESSAGE NUMBER TO START                      
SELLNG   DS    XL(L'GMKLANG)       MESSAGE LANGUAGE                             
         SPACE 1                                                                
SELQNAME DS    CL(L'GQKQNAME)      EQUATE NAME                                  
SELMNUM  DS    XL(L'GQKMNUM)       MESSAGE NUMBER                               
         SPACE 1                                                                
SELLNGST DS    CL7                 FOR SELECTING MULTIPLE LANGUAGES             
*                                   +0 : # OF SELECTED LANGUAGES                
*                                   +1..+6 : LANGUAGE CODES                     
         DS    0CL((L'SELLNGST-1)-NMLNGLN+1)                                    
         DS    0CL(NMLNGLN-(L'SELLNGST-1)+1)                                    
SELTRNS  DS    CL1                 WHICH TRANSLATION (DEFAULT = <A>LL)          
SELDRES  DS    CL1                 CORE/DISK RESIDNT (DEFAULT = <A>LL)          
SELRFP   DS    XL1                 START AT RFP RULE#                           
SELFLG   DS    XL1                 SELECT FLAG                                  
SFEQNMQ  EQU   X'80'                LIST BY EQUATES (SYS IS AUTO. SET)          
SFSYSQ   EQU   X'40'                FILTER ON SYSTEM                            
SFMSGQ   EQU   X'20'                START AT MSG# GIVEN                         
SFLNGQ   EQU   X'10'                FILTER ON LANGUAGE                          
SFRFPQ   EQU   X'08'                START AT RFP# GIVEN                         
SFHOLEQ  EQU   X'04'                REPORT HOLES IN MSG# (ACTN=REPRT)           
SFBLNKQ  EQU   X'02'                REPORT ON BLANK ENTRIES (RPRTING)           
SELX     EQU   *                                                                
*                                                                               
*----------------------------- ADDRESSES -----------------------------*         
*                                                                               
*                                  THESE ARE SET IN THE BEGINNING               
APFXTAB  DS    A                    A(PREFIX TABLE)                             
APFXTABX DS    A                    A(END OF PREFIX TABLE)                      
AREPSPEC DS    A                    A(REPORT SPECS)                             
         SPACE 1                                                                
AGETOUT  DS    A                   A(OUTPUT AREA) FOR GTAOUT (GETTXT)           
AUPCASE  DS    A                   A(UPCASE TABLE)                              
AXLATAB  DS    A                   ADDRESS TO CTRYXLAT (IN FATI3270)            
AMESG2   DS    A                   A(START OF TEXT FOR 2ND LINE)                
SAVERE   DS    A                                                                
XADDRSS  DS    A                   A MULTIPURPOSE END ADDRESS FIELD             
MYRELO   DS    F                   2ND LEVEL NMOD RELO FACTOR                   
*                                                                               
*--------------------------- MISCELLANEOUS ---------------------------*         
*                                                                               
MYDMWORK DS    12D                                                              
MYDMCB   DS    6F                                                               
LENCHK   DS    F                   LENGTH CHECK                                 
MESGNUM  DS    H                   MSG#, CTGEN21 BUILDS GETTXT PARMS            
DMESGNUM DS    H                   DICTIONARY MSG#                              
DPRFX    DS    CL2                 DICTIONARY PREFIX                            
BYTE     DS    XL1                 ONE BYTE TEMP STORAGE                        
LMESG1   DS    XL1                 L(MESG1)                                     
LMESG2   DS    XL1                 L(MESG2)                                     
LMESGFMT DS    XL1                 L(MESGFMT)                                   
LMESGTXT DS    XL1                 L(MESGTXT)                                   
MAXLEN   DS    XL(L'GMPRGLMT)      MAX LENGTH                                   
MTYPE    DS    CL1                 GETTXT MSG TYPE                              
OPT1     DS    XL1                 OPTION 1 (FOR LIST FILTERING)                
OPT2     DS    XL1                 OPTION 2 (FOR LIST FILTERING)                
RECTYPE  DS    XL1                 RECORD TYPE (X'94' OR X'D4')                 
RFPRULE  DS    XL1                 RFP RULE                                     
LASTSYS  DS    XL1                 CONTROL TOF ON CHANGE OF SYSTEM              
PRTLNCNT DS    PL1                 # OF LINES IN A "BLOCK" IN RPRTING           
FILLNGST DS    XL8                 LIST OF EXISTING LANGS FOR A MSG#            
*                                   +0     : # OF ENTRIES IN LIST               
*                                   +1..+7 : LANGUAGE CODES                     
*                                                                               
*------------------------------- FLAGS -------------------------------*         
*                                                                               
MESGFLG  DS    XL1                 MESSAGE FLAG                                 
MFSPLTQ  EQU   X'80'                SPLITTER PRESENT                            
MFTRUEQ  EQU   X'40'                TRUE TRANSLATION                            
MFADPSVQ EQU   X'20'                ADD EQUATE PASSIVE POINTER                  
MFNEWQ   EQU   X'10'                BRAND NEW MESSAGE                           
MFX40Q   EQU   X'04'                TURN ON X'4000' BIT IN PSV RECORD           
MFX20Q   EQU   X'02'                TURN ON X'2000' BIT IN PSV RECORD           
MFCNTDQ  EQU   X'01'                DISPLAY LINE CONTINUATION                   
         SPACE 1                                                                
RPTFLG   DS    XL1                 REPORT FLAG                                  
RFHOLEQ  EQU   X'80'                CALLED RPTRECRD FOR HOLE                    
*                                                                               
*------------------------------ BUFFERS ------------------------------*         
*                                                                               
MYSVKEY  DS    CL(L'IOKEY)                                                      
MESG1    DS    CL(L'DCTMSG1)       BUFFER FOR 1ST LINE                          
MESG2    DS    CL(L'DCTMSG2)       BUFFER FOR 2ND LINE                          
MESGFMT  DS    CL256               BUFFER FOR FORMATTED TEXT                    
SCANTBL  DS    CL(L'MESGFMT)       FOR SCANNER'S USE                            
MESGTXT  DS    CL(L'MESG1+L'MESG2) BUFFER FOR NON-FORMATTED TEXT                
LOCALX   EQU   *                                                                
         EJECT                                                                  
*                                                                               
*======================= CTGEN21 SAVED STORAGE =======================*         
*                                                                               
TWAD     DSECT                                                                  
         ORG   SAVOVER                                                          
SVHIMSG  DS    H                   HIGH-MSG#                                    
SVHINUM1 DS    H                   HIGH MSG#1                                   
SVHINUM2 DS    H                   HIGH MSG#2                                   
SVEQNAME DS    CL(L'GMQSYSYM)      EQUATE NAME                                  
SVSYPRFX DS    CL2                 SYSTEM PREFIX                                
SVLANG   DS    XL1                 LANGUAGE CODE                                
SVRFP    DS    XL1                 RFP #                                        
SVPVRFP  DS    XL1                 RFP FROM LAST TRANSACTION                    
SVELEM   DS    XL1                 ELEMENT FLAG                                 
SVPVDRES DS    CL1                 PREVIOUS DISK RESIDENT STATUS                
SVMXLEN  DS    XL(L'GMPRGLMT)      MAX LENGTH                                   
SVPVMXLN DS    XL(L'SVMXLEN)       MAX LENGTH FROM PREV TRANSACTION             
SVFLAG   DS    XL1                                                              
SFRFPCHQ EQU   X'80'               RFP WAS CHANGED                              
SFCHRFPQ EQU   X'40'               RFP IS CHANGEABLE                            
SFDRSCHQ EQU   X'20'               DISK RESIDENT STATUS WAS CHANGED             
SFMXLCHQ EQU   X'10'               MAX LEN VALUE WAS CHANGED                    
SFADDELQ EQU   X'08'               USER IS "ADDING" A DELETED RECORD            
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023CTGEN21S  05/01/02'                                      
         END                                                                    
