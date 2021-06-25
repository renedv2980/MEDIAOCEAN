*          DATA SET SPNWS11    AT LEVEL 156 AS OF 11/27/07                      
*PHASE T20711A,*                                                                
         TITLE 'BWS11 - BUYERS WORK SHEET - CAMPAIGN COPY'                      
T20711   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 EXTRAWKL,T20711**,RA,RR=RE                                       
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         LR    R1,RC                                                            
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    R6,ASAVAREA         SAVE A(EXTRA SAVED STORAGE)                  
         ST    R1,AEXTRAWK         SAVE A(EXTRA WORKING STORAGE)                
         USING EXTRAWKD,R1                                                      
         XC    LSTDARKY,LSTDARKY                                                
         DROP  R1                                                               
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LA    R2,IOKEY            R2=A(HEADER KEY)                             
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY         R3=A(DETAIL KEY)                             
         USING BWDRECD,R3                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         B     DISREC                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         B     EXIT     FSTRPT                                                  
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   MVI   LFLAG,0                                                          
         MVI   LFLAG2,0                                                         
         XC    CABLETAB,CABLETAB   CLEAR THE CABLE TABLE                        
         XC    BWHKEY,BWHKEY       BUILD HEADER KEY                             
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         L     RE,AEXTRAWK                                                      
         LA    RF,EXTRAWKL                                                      
         XCEFL ,                                                                
*                                                                               
         XC    CPYMDN,CPYMDN       VALIDATE MEDIA                               
         OI    CPYMDNH+6,FVOXMT                                                 
         GOTO1 AVALMED,CPYMEDH                                                  
         BNE   VALKX                                                            
         MVC   CPYMDN,MEDNM                                                     
         MVC   BWHKAGMD,BAGYMD                                                  
         MVC   LMED,BAGYMD                                                      
*                                                                               
         XC    CPYBYN,CPYBYN       VALIDATE BUYER                               
         OI    CPYBYNH+6,FVOXMT                                                 
         GOTO1 AVALBYR,CPYBYRH                                                  
         BNE   VALKX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   CPYBYN,BYRNM                                                     
         MVC   BWHKBYR,BBYR                                                     
         MVC   LBYR,BBYR                                                        
         OC    BYRPW,BYRPW         CHECK BUYER'S PASSWORD                       
         BZ    VALK1                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALKX                                                            
*                                                                               
VALK1    XC    CPYCNM,CPYCNM       VALIDATE CAMPAIGN NUMBER                     
         OI    CPYCNMH+6,FVOXMT                                                 
         GOTO1 AVALCAM,CPYNUMH                                                  
         BNE   VALKX                                                            
         MVC   CPYCNM,CMPNM                                                     
         MVC   BWHKCAM,BCAM                                                     
         MVC   LCAM,BCAM                                                        
         MVC   LFRSLN,CMPSLN                                                    
         MVC   LFRUF,CMPUF                                                      
         MVC   LFRUP,CMPUP                                                      
         MVC   LFRFB,CMPFB                                                      
         MVC   LFRFBTP,CMPFBTP     CMPFB BOOKTYPE                               
         MVC   LFRFBLST,CMPFBLST                                                
         MVC   LFRUPIN,CMPUPIN                                                  
         MVC   LFRPUT,CMPUPUT                                                   
         MVC   LFRSHR,CMPUSHR                                                   
*                                                                               
         L     RE,ATWA             SAVE CAMPAIGN DATES                          
         AHI   RE,CMPDATSP-TWAD                                                 
         MVC   LFRDATES,0(RE)                                                   
*                                                                               
         TM    CMPOPTS,CAMODLY          EXCEPT FOR DAILY SCHEDULE,              
         BO    *+10                                                             
         MVC   LFRDATES(2),CMPSTMNP     CAMPAIGN START MONDAY                   
         MVC   LFREND,CMPND             CAMPAIGN END                            
         MVC   LFRENDMN,CMPNDMNP        CAMPAIGN END MONDAY                     
         MVC   LFRCAMOP,CMPOPTS         CAMPAIGN OPTION BYTE                    
         MVC   LFRU2DAT,CMPU2DAT        CAMPAIGN 2ND UPGRADE DATE               
         GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BE    *+16                                                             
         LA    R1,CPYNUMH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
         MVC   LQCLT,QCLT                                                       
         CLI   CLTBWPRO+14,C'Y'    TEST COSTS ARE NETTED DOWN                   
         BNE   *+8                                                              
         OI    LFLAG,LNETCOST                                                   
         GOTO1 AGETPRD,CMPPRDN     (GET PRODUCT)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BE    VALK1A                                                           
         CLC   FVMSGNO,=AL2(FVIEST)    INVALID ESTIMATE??                       
         BE    VALKX                   YES, CAN'T COPY                          
         CLC   FVMSGNO,=AL2(FVCMPEST)  TEST CAMPAIGN DATES DON'T MATCH          
         BE    *+6                     ESTIMATE DATES                           
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)     YES-THAT'S OK                            
*                                                                               
VALK1A   XC    LSTA,LSTA           INITIALIZE STATION FILTERS                   
         XC    LSTACD,LSTACD                                                    
         MVI   FVMINL,1            MARKET/STATION IS MANDATORY                  
         GOTO1 AFVAL,CPYMKTH                                                    
         BNE   VALKX                                                            
         TM    FVIIND,FVINUM       TEST FOR MARKET (NUMERIC FIELD)              
         BZ    VALK2                                                            
         GOTO1 AVALMKT,CPYMKTH     YES-VALIDATE MARKET                          
         BNE   VALKX                                                            
         MVC   LFRMKT,BMKT                                                      
         OI    LFLAG,LMKTCPY       X'02' - THIS IS A MARKET COPY                
         B     VALK4                                                            
*                                                                               
VALK2    L     R4,AIOAREA1         VALIDATE STATION                             
         XC    0(64,R4),0(R4)                                                   
*****                                                                           
* CHECK FOR  9999/AAA=1111 WHERE 9999 IS THE CABLE SYSTEM                       
*                                AAA  IS THE NETWORK                            
*                                1111 IS THE OVERRIDE MARKET                    
*****                                                                           
         GOTO1 VSCANNER,APPARM,CPYMKTH,(2,AIOAREA1)                             
         CLI   32(R4),0                                                         
         BNE   ESTA                                                             
         CLI   1(R4),0             ANY '=' SIGN IN INPUT                        
         BE    VALK2A              NONE, USE OLD WAY                            
         TM    3(R4),X'80'         YES, MARKET HAD BETTER BE NUMERIC            
         BZ    ESTA                                                             
         ZIC   RF,1(R4)            YES-VALIDATE IT                              
         LA    R1,22(R4)                                                        
         BAS   RE,VALKSETF                                                      
         GOTO1 AVALMKT,APWORK                                                   
         BNE   VALKX                                                            
         MVC   LFRMKT,BMKT                                                      
         ZIC   RF,0(R4)            VALIDATE THE STATION                         
         LA    R1,12(R4)                                                        
         BAS   RE,VALKSETF                                                      
         GOTO1 AVALSTA,APWORK                                                   
         BNE   VALKX                                                            
         MVC   LSTA,QSTA                                                        
         CLC   LFRMKT,BMKT         CHECK OVERRIDE MKT IS NOT SAME               
         BE    EMKT                AS STATION'S CURRENT MKT                     
         B     VALK4                                                            
*****                                                                           
VALK2A   GOTO1 VSCANNER,APPARM,CPYMKTH,(2,AIOAREA1),C',=,/'                     
         CLI   32(R4),0                                                         
         BNE   ESTA                                                             
         CLI   1(R4),0             TEST FOR OVERRIDE MARKET                     
         BE    VALK3                                                            
         TM    3(R4),X'80'                                                      
         BZ    VALK3                                                            
         ZIC   RF,1(R4)            YES-VALIDATE IT                              
         LA    R1,22(R4)                                                        
         BAS   RE,VALKSETF                                                      
         GOTO1 AVALMKT,APWORK                                                   
         BNE   VALKX                                                            
         MVC   LFRMKT,BMKT                                                      
         ZIC   RF,0(R4)            VALIDATE THE STATION                         
         LA    R1,12(R4)                                                        
         BAS   RE,VALKSETF                                                      
         GOTO1 AVALSTA,APWORK                                                   
         BNE   VALKX                                                            
         MVC   LSTA,QSTA                                                        
         CLC   LFRMKT,BMKT         CHECK OVERRIDE MKT IS NOT SAME               
         BE    EMKT                AS STATION'S CURRENT MKT                     
         B     VALK4                                                            
*                                                                               
* SUBROUTINE CABLEALL ADDED 5/22/02 FOR CABLE SYS CAMPAIGN COPIES               
VALK3    TM    2(R4),X'80'         IS THE FIELD NUMERIC?                        
         BZ    VALK3A                                                           
         CLC   22(3,R4),SPACES     IS THE NETWORK BLANK?                        
         BNE   VALK3A                                                           
         BRAS  RE,CVRTCAB          CONVERT CABLE SYSTE NUMBER FORMAT            
         OI    LFLAG,LCBLSYS       WE ARE DOING CABLE SYSTEM COPY X'01'         
         BRAS  RE,CABLEALL         ALL CABLE COPY                               
         BE    CABLEOK                                                          
         TM    LFLAG2,LINVSTA      DOES THE CABLE SYSTEM EXIST?                 
         BO    ESTA                 - NOPE, INVALID STATION                     
         B     ENCP                 - NO RECORDS TO COPY!                       
*                                                                               
CABLEOK  MVC   IOKEY,SAVEKEY       PUT THE HEADER KEY BACK IN IOKEY             
         B     VALK3B                                                           
*                                                                               
VALK3A   GOTO1 AVALSTA,CPYMKTH     VALIDATE STATION                             
         BNE   VALKX                                                            
         MVC   LSTA,QSTA                                                        
VALK3B   MVC   LFRMKT,BMKT                                                      
*                                                                               
VALK4    MVC   BWHKMKT,LFRMKT      SET 'FROM' MARKET                            
         MVC   LMKT,BMKT           SET 'TO' MARKET                              
         MVC   LMKTLKUP,MKTLKUP                                                 
         MVI   BDPT,0                                                           
         MVI   BSLN,0                                                           
         CLI   CPYDPLH+5,0         TEST DAYPART ENTERED                         
         BE    VALK7                                                            
*                                                                               
VALK5    GOTO1 AVALDPL,CPYDPLH     VALIDATE DAYPART/LENGTH                      
         BE    VALK6                                                            
         CLC   FVMSGNO,=AL2(FVNOSLN)   TEST SLN MISSING                         
         BNE   *+12                                                             
         MVI   CMPSLN,255              YES-FUDGE CAMPAIGN SPOT LENGTH           
         B     VALK5                                                            
         CLC   FVMSGNO,=AL2(FVNODPT)   TEST DAYPART MISSING                     
         BNE   VALKX                                                            
         MVC   FVMSGNO,=AL2(FVFOK)     YES-THAT'S OK                            
         B     VALK7                                                            
*                                                                               
VALK6    CLI   CMPSLN,255                                                       
         BNE   VALK7                                                            
         MVI   CMPSLN,0                                                         
         MVI   BSLN,0                                                           
         XC    QSLN,QSLN                                                        
*                                                                               
VALK7    MVC   LDPT,BDPT           SAVE DAYPART/LENGTH FILTERS                  
         MVC   LSLN,BSLN                                                        
         MVI   APINDS,0                                                         
         TM    LFLAG,LCBLSYS       IS IT CABLE SYSTEM COPY?                     
         BO    VALK11A              YUP IT IS                                   
*                                                                               
         GOTO1 AIO,DIRHI+IO1       READ HIGH FOR HEADER RECORD                  
         BNE   VALKX                                                            
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BNE   ENCP                NO RECORDS TO COPY                           
         GOTO1 (RF),FILGET1        GET THE HEADER RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   HDRKEY,IOKEY        SAVE THE HEADER KEY                          
         OC    LSTA,LSTA           TEST STATION FILTER                          
         BZ    VALK9                                                            
         L     R2,AIOAREA1         YES-LOOK FOR STATION IN HEADER               
         LA    R4,BWHFSTEL                                                      
         SR    R0,R0                                                            
*                                                                               
VALK8    CLI   0(R4),0                                                          
         BE    ENCP                NOT FOUND - NO RECORDS TO COPY               
         CLI   0(R4),BWHELCDQ                                                   
         BNE   VALK8A                                                           
         USING BWHEL,R4                                                         
         CLC   LSTA,BWHSTA                                                      
         BNE   VALK8A                                                           
         MVC   LSTACD,BWHSEQ       FOUND - SAVE STATION CODE                    
         B     VALK9                                                            
*                                                                               
VALK8A   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALK8                                                            
*                                                                               
VALK9    XC    APRECKEY,APRECKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    SET UP DETAIL KEY                            
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ     CAMPAIGN/MARKET SEQ NUM FROM HDR KEY         
         MVI   BWDKELCD,BWDELCDQ                                                
         MVC   BWDKELST,LSTACD                                                  
         LA    R4,BWDKELST-BWDKEY-1  SET KEY COMPARE LENGTH                     
         CLI   LSTACD,0                                                         
         BE    *+8                                                              
         LA    R4,L'BWDKELST(R4)                                                
         STC   R4,LKEYCOMP                                                      
         MVC   IOKEY,APRECKEY                                                   
         LA    R1,MINHI2                                                        
         B     VALK10+4                                                         
*                                                                               
VALK10   LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                READ HIGH FOR DETAIL RECORD                  
         BNE   ENCP                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   ENCP                NO DETAIL RECORDS FOUND                      
         L     R3,AIOAREA2                                                      
         CLI   BDPT,0              TEST DAYPART FILTER                          
         BE    VALK11                                                           
         CLC   BWDDPT,BDPT         YES-MATCH THE DAYPART                        
         BE    VALK11                                                           
         CLC   BWDSUBDP,BDPT       ALSO ACCEPT SUB-DAYPART MATCH                
         BNE   VALK10                                                           
*                                                                               
VALK11   CLI   BSLN,0                                                           
         BE    *+14                                                             
         CLC   BWDSLN,BSLN                                                      
         BNE   VALK10                                                           
         XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(13),IOKEY  SAVE FIRST DETAIL RECORD KEY                 
VALK11A  OI    APINDS,APIOKDIS                                                  
*                                                                               
         MVI   FVMINL,1            VALIDATE COPY DEMOS OPTION                   
         MVI   FVMAXL,1                                                         
         GOTO1 AFVAL,CPYDEMH                                                    
         BNE   VALKX                                                            
         CLI   FVIFLD,C'N'                                                      
         BE    VALK12                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EINV                                                             
         OI    LFLAG,LCPYDEM       COPY DEMOS = YES                             
*                                                                               
VALK12   GOTO1 AFVAL,CPYSKDH       VALIDATE SCHEDULE OPTION                     
         BH    VALKX                                                            
         BL    VALK12A                                                          
         CLI   FVIFLD,C'Y'                                                      
         BE    VALK12A                                                          
         CLI   FVIFLD,C'N'                                                      
         BNE   EINV                                                             
         OI    LFLAG,LNOSKD                                                     
         B     VALK12D             NO NEED TO WORK ABOUT IGNORE DATES           
*                                                                               
VALK12A  GOTO1 AFVAL,CPYIGNDH      VALIDATE SAME (IGNORE) DATE OPTION           
         BH    VALKX                                                            
         BL    VALK12D                                                          
         CLI   FVIFLD,C'Y'                                                      
         BE    VALK12D                                                          
         CLI   FVIFLD,C'N'                                                      
         BNE   EINV                                                             
         OI    LFLAG2,LIGNDAT      IGNORE THE DATES                             
*                                                                               
VALK12D  GOTO1 AFVAL,CPYCSTH       VALIDATE COST OPTION                         
         BH    VALKX                                                            
         BL    VALK13                                                           
         CLI   FVIFLD,C'Y'                                                      
         BE    VALK13                                                           
         CLI   FVIFLD,C'N'                                                      
         BNE   EINV                                                             
         OI    LFLAG2,LNOCST                                                    
*                                                                               
VALK13   LA    R4,LHDRTAB          VALIDATE TO CAMPAIGNS                        
         USING HDRTABD,R4                                                       
         LA    R8,CPYTMDH                                                       
         LA    R0,CPYMAX           R0=MAX N'TO CAMPAIGNS                        
         XI    LIND,X'FF'-LNFPASS   NOT FIRST PASS FLAG                         
         XC    LCMSEQ,LCMSEQ       INIT SAVED CAMPAIGN/MARKET SEQ NUM           
         B     VALK15                                                           
*                                                                               
         USING CPYTM2H,R8                                                       
VALK14   MVI   HDRAM,0                                                          
****     CLI   CPYTM2H+5,0         TEST END OF TO CAMPAIGNS                     
         LA    RF,CPYTMDH          THE BEGINNING                                
         CR    R8,RF               ARE WE AT THE BEGINNING?                     
         BE    VALK15               - YUP                                       
         CLI   5(R8),0             TEST END OF TO CAMPAIGNS                     
         BE    VALK40                                                           
*                                                                               
VALK15   LA    R2,IOKEY                                                         
         XC    BWHKEY,BWHKEY       HEADER KEY                                   
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         LA    R1,CPYTMDH          MEDIA                                        
         TM    LIND,LNFPASS        IS IT THE FIRST PASS?                        
         BZ    VALK15E              - YES                                       
***      LA    R1,CPYTM2H                                                       
         LR    R1,R8               R8 ALREADY POINTING TO WHAT WE WANT          
         ST    R1,APFULL           SAVE OFF THE ADDRESS                         
VALK15E  GOTO1 AVALMED                                                          
         BNE   VALKX                                                            
*                                                                               
         CLC   CPYMED,8(R1)        FROM- AND TO- MEDIA THE SAME??               
         BE    *+14                 - YUP, NO PROBLEMS, CONTINUE ON             
         MVC   FVMSGNO,=AL2(FVSAMEAM)   FROM- AND TO- MEDIA NOT SAME            
         B     VALKX               GET OUTTA HERE                               
*                                                                               
         MVC   BWHKAGMD,BAGYMD                                                  
         MVC   HDRAM,BAGYMD                                                     
*                                                                               
         LA    R1,CPYTBYH          BUYER                                        
         TM    LIND,LNFPASS        IS IT THE FIRST PASS?                        
         BZ    VALK15K              - YES                                       
***      LA    R1,CPYTB2H                                                       
         L     R1,APFULL           RESTORE THE ADDRESS                          
         XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         AR    R1,RF               NEXT HEADER                                  
         ST    R1,APFULL           SAVE OFF THE ADDRESS                         
VALK15K  GOTO1 AVALBYR                                                          
         BNE   VALKX                                                            
         OC    HDRAM,BBYRMASK                                                   
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   HDRBYR,BBYR                                                      
*                                                                               
         LA    R1,CPYTCMH          CAMPAIGN                                     
         TM    LIND,LNFPASS        IS IT THE FIRST PASS?                        
         BZ    VALK15P              - YES                                       
***      LA    R1,CPYTC2H                                                       
         L     R1,APFULL           RESTORE THE ADDRESS                          
         XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         AR    R1,RF               NEXT HEADER                                  
         ST    R1,APFULL           SAVE OFF THE ADDRESS                         
VALK15P  GOTO1 AVALCAM                                                          
         BNE   VALKX                                                            
*                                                                               
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD                          
         BZ    VALK16                                                           
         CLC   BAGYMD,LMED         YES-TEST BUYER SAME AS FROM BUYER            
         BNE   *+14                                                             
         CLC   BBYR,LBYR                                                        
         BE    VALK16                                                           
         GOTO1 AVALPWD             TEST PASSWORD IN SCREEN PWD FIELD            
         BE    VALK16              YES                                          
         LA    R1,CPYTPSH          NO-VALIDATE PASSWORD                         
         TM    LIND,LNFPASS        IS IT THE FIRST PASS?                        
         BZ    VALK15V              - YES                                       
***      LA    R1,CPYTP2H                                                       
         L     R1,APFULL           RESTORE THE ADDRESS                          
         XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         AR    R1,RF               NEXT HEADER                                  
         ST    R1,APFULL           SAVE OFF THE ADDRESS                         
VALK15V  GOTO1 AFVAL                                                            
         BH    VALKX                                                            
         BL    ENOPWD             PASSWORD MISSING                              
         CLC   FVIFLD(L'CPYTPS),BYRPW                                           
         BNE   EIPWD              PASSWORD INVALID                              
*                                                                               
VALK16   GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BE    VALK16E                                                          
         LA    R1,CPYTCMH                                                       
         TM    LIND,LNFPASS        IS IT THE FIRST PASS?                        
         BZ    VALK16A                                                          
         LR    R1,R8                                                            
         XR    RF,RF               GOING FROM MED TO BYR TO CAMP                
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
****     LA    R1,CPYTC2H                                                       
VALK16A  ST    R1,FVADDR                                                        
         B     VALKX                                                            
*                                                                               
VALK16E  GOTO1 AGETPRD,CMPPRDN     (GET PRODUCT)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BNE   VALKX                                                            
         MVC   BWHKCAM,BCAM                                                     
         MVC   HDRCAM,BCAM                                                      
*****                                                                           
         L     RE,ATWA             END OF CAMPAIGN START WEEK                   
         AHI   RE,CMPDATSP+2-TWAD                                               
         MVC   HDRSTRT,0(RE)                                                    
         MVC   HDREND,CMPND        CAMPAIGN END                                 
*****                                                                           
         TM    ESTIND,ESTIBTYP     STABKTYP HAS ESTIMATE BOOK TYPE?             
         BZ    *+10                                                             
         MVC   HDRBKTYP,STABKTYP   YES, NEED THIS FOR LATER                     
*****                                                                           
         STC   R0,APWORK           SAVING OFF THE COUNTER                       
         L     RE,ATWA             END OF CAMPAIGN START WEEK                   
         AHI   RE,CMPDATSP-TWAD                                                 
         LA    RF,L'CMPDATSP                                                    
         LA    R0,HDRDATSP                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE               CAMPAIGN LIST OF WEEKS                       
         XR    R0,R0                                                            
         IC    R0,APWORK           RESTORING THE COUNTER                        
*****                                                                           
         GOTO1 VDATCON,APPARM,(3,CMPND),(2,HDRENDP)                             
*                                                                               
         TM    CMPOPTS,CAMOWKS     TEST FLIGHT WEEKS                            
         BO    VALK17                                                           
         GOTO1 (RF),(R1),(3,CMPST),(2,HDRDATSP)  NO-CAMPAIGN START              
*                                                                               
VALK17   MVC   HDRNWKS,CMPNWKS     N'CAMPAIGN WEEKS                             
         MVC   HDRSLN,CMPSLN       CAMPAIGN SPOT LENGTH                         
         MVC   HDRSLEQU,CMPSLEQU   CAMPAIGN SPOT LENGTH EQUIVALENCES            
         TM    CMPIND,CMPISKD      TEST ANY SPOTS SCHEDULED                     
         BO    *+8                                                              
         OI    HDRIND2,HDRINOSP    YES-INDICATE NO SPOTS YET                    
         MVC   HDRUF,CMPUF         UPGRADE VALUES                               
         MVC   HDRUP,CMPUP                                                      
         MVC   HDRFB,CMPFB                                                      
         TM    CMPFB+1,BTY2CHAR    2 CHARACTER BOOKTYPES?                       
         BNO   VALK17G                                                          
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   HDRFBTP,CMPFBTP     CMPFB BOOKTYPE                               
VALK17G  MVC   HDRFBLST,CMPFBLST                                                
         MVC   HDRUPUT,CMPUPUT                                                  
         MVC   HDRUSHR,CMPUSHR                                                  
         MVC   HDRU2DAT,CMPU2DAT   2ND UPGRADE VALUES (IF ANY)                  
         MVC   HDRUF2,CMPUF2                                                    
         MVC   HDRUP2,CMPUP2                                                    
         MVC   HDRFB2,CMPFB2                                                    
         TM    CMPFB2+1,BTY2CHAR   2 CHARACTER BOOKTYPES?                       
         BNO   VALK17K                                                          
         CLI   CMPFB2TP,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   HDRFB2TP,CMPFB2TP   CMPFB2 BOOKTYPE                              
VALK17K  MVC   HDRFB2L,CMPFB2L                                                  
         MVC   HDRIN2,CMPUPIN2                                                  
         MVC   HDRUPUT2,CMPUPUT2                                                
         MVC   HDRUSHR2,CMPUSHR2                                                
         MVC   HDRCLT,QCLT                                                      
         MVC   HDRSRC,CLTSRC                                                    
         MVC   HDRBWPRO,CLTBWPRO   CLIENT BW PROFILE                            
         MVC   HDRDMENU,ESTDMENU   ESTIMATE DEMO MENU                           
         MVC   HDRBOOK,ESTBOOK     ESTIMATE DEFAULT BOOK                        
         MVC   HDRDEMS,ESTDEMS     ESTIMATE DEMOS                               
         CLI   CMPDPOPT,C'M'       TEST SUB-DPTS GROUPED UNDER MASTER           
         BNE   *+8                                                              
         OI    HDRIND,HDRIMADP                                                  
         CLI   CMPDPOPT,C'S'       TEST SUB-DPTS SCHEDULED SEPARATELY           
         BNE   *+8                                                              
         OI    HDRIND,HDRISUDP                                                  
         MVI   HDRDEMAD,0                                                       
         TM    CMPOPTS,CAMOAIMP    SET AUTO DEMO ADJUST INDICATOR               
         BZ    *+12                                                             
         MVI   HDRDEMAD,C'I'                                                    
         B     VALK18                                                           
         TM    CMPOPTS,CAMOAALL                                                 
         BZ    *+12                                                             
         MVI   HDRDEMAD,C'A'                                                    
         B     VALK18                                                           
         TM    CMPOPTS,CAMOATGT                                                 
         BZ    VALK18                                                           
         MVI   HDRDEMAD,C'T'                                                    
         B     VALK18                                                           
*                                                                               
VALK18   TM    LFLAG,LCPYDEM       IF COPYING DEMOS,                            
         BZ    VALK18A                                                          
         TM    LFRCAMOP,CAMOAIMP   TEST FROM CAMPAIGN HAS AUTOADJ=IMP           
         BZ    *+12                                                             
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT YES-TO CAMPAIGN MUST          
         BZ    EADJ                               HAVE AUTO ADJUTMENTS          
         TM    LFRCAMOP,CAMOAALL   TEST FROM CAMPAIGN HAS AUTOADJ=ALL           
         BZ    *+12                                                             
         TM    CMPOPTS,CAMOAALL    YES - TO CMP MUST HAVE AUTOADJ=ALL           
         BZ    EADJ                                                             
         TM    LFRCAMOP,CAMOATGT   TEST FROM CAMPAIGN HAS AUTOADJ=TGT           
         BZ    *+12                                                             
         TM    CMPOPTS,CAMOATGT    YES - TO CMP MUST HAVE AUTOADJ=TGT           
         BZ    EADJ                                                             
*                                                                               
VALK18A  MVC   BWHKMKT,LMKT        MARKET                                       
         GOTO1 AIO,DIRHI+IO3       SEE IF CAMPAIGN/MKT HEADER EXISTS            
         BNE   VALKX                                                            
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BE    VALK21              YES                                          
         OI    HDRIND,HDRINEW+HDRINEWD   NO-INDICATE NEW HEADER AND             
         SR    RE,RE                        NEW SET OF DETAIL RECORDS           
         ICM   RE,3,LCMSEQ         GET NEXT CAMPAIGN/MKT SEQ NUM                
         BNZ   VALK19                                                           
         XC    BWHKEY,BWHKEY                                                    
         MVI   BWHPTYP,BWHPTYPQ                                                 
         MVI   BWHPSUB,BWHPSUBQ                                                 
         MVC   BWHPAGMD,HDRAM                                                   
         MVC   BWHPBYR,BBYR                                                     
****     GOTO1 (RF),DIRHID         NEED READ FOR UPDATES!!    MHC               
****  11/04/03          SO THAT 2 PEOPLE CAN'T HAVE SAME SEQUENCE NUM           
**       GOTO1 (RF),DIRHIU+IORDEL   READ FOR UPDATES AND DELETES                
**   05/17/05  THAT'S NOT RIGHT YOU IDIOT                                       
         GOTO1 (RF),DIRHIU         READ FOR UPDATES AND DELETES                 
***      BNE   VALKX                                                            
         BE    VALK18G                                                          
         TM    IOERR,IOEDEL        RECORD IS DELETED?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK) SET OK RETURN CODE                           
*                                                                               
VALK18G  MVC   HDRSEQ,EFFS                                                      
         CLC   IOKEY(BWHPSEQ-BWHPKEY),IOKEYSAV                                  
         BNE   VALK20                                                           
         SR    RE,RE                                                            
         ICM   RE,3,BWHPSEQ                                                     
*                                                                               
VALK19   BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    ECMSEQ                                                           
         STCM  RE,3,HDRSEQ         SET NEW CAMPAIGN/MARKET SEQ NUM              
*                                                                               
VALK20   MVC   LCMSEQ,HDRSEQ       SAVE LATEST CAMPAIGN/MARKET SEQ NUM          
         B     VALK28                                                           
*                                                                               
VALK21   MVC   HDRSEQ,BWHKSEQ      READ CAMPAIGN/MKT HEADER RECORD              
         GOTO1 (RF),FILGET3                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA3         SET STATION DISPLACEMENTS                    
         MVI   HDRSTA,X'FF'                                                     
         MVC   HDRSTA+1(L'HDRSTA-1),HDRSTA   FILL IT ALL WITH X'FF'             
         SR    RF,RF                                                            
         L     R1,AIOAREA1                                                      
         LA    R1,BWHFSTEL-BWHKEY(R1)                                           
*                                                                               
VALK22   CLI   0(R1),0                                                          
         BE    VALK27                                                           
         CLI   0(R1),BWHELCDQ                                                   
         BNE   VALK26                                                           
         MVC   APDUB,BWHSTA-BWHEL(R1)                                           
         ZIC   R3,BWHSEQ-BWHEL(R1)                                              
         CHI   R3,HDRSTMAX                                                      
         B     VALK23                                                           
         XC    BWSMSG,BWSMSG                                                    
         OI    BWSMSGH+6,X'88'     TRANSMIT AND HIGH INTENSITY                  
         MVC   BWSMSG(32),=C'Too many stations on this copy!!'                  
         DC    H'0',C'$ABEND'      INCREASE HDRSTMAX                            
VALK23   BCTR  R3,0                                                             
         LA    R3,HDRSTA(R3)                                                    
         LA    RE,BWHFSTEL                                                      
         SR    R9,R9                                                            
*                                                                               
VALK24   CLI   0(RE),0                                                          
         BNE   *+16                                                             
         LA    R9,1(R9)                                                         
         STC   R9,HDRNXTST         SET NEXT AVAILABLE STATION CODE              
         B     VALK26                                                           
         CLI   0(RE),BWHELCDQ                                                   
         BNE   VALK25                                                           
         IC    RF,BWHSEQ-BWHEL(RE)                                              
         CR    RF,R9                                                            
         BNH   *+6                                                              
         LR    R9,RF                                                            
         CLC   APDUB,BWHSTA-BWHEL(RE)                                           
         BNE   VALK25                                                           
         STC   RF,0(R3)                                                         
*                                                                               
VALK25   IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VALK24                                                           
*                                                                               
VALK26   IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VALK22                                                           
*                                                                               
VALK27   XC    IOKEY,IOKEY         SEE IF SET OF MINIO DETAIL RECORDS           
         LA    R3,IOKEY            EXISTS                                       
         MVI   BWDKTYP,BWDKTYPQ                                                 
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,HDRAM                                                   
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ                                                  
         GOTO1 AMIN,MINHI2                                                      
         BNE   *+14                                                             
         CLC   IOKEY(BWDKEL-BWDKEY),IOKEYSAV                                    
         BE    VALK28                                                           
         OI    HDRIND,HDRINEWD     NO                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALK28   TM    LFLAG,LNOSKD        TEST FOR NO SCHEDULE COPY                    
         BO    VALK29                                                           
****  IGNORE DATES!!  ALWAYS COPY SCHEDULE UNLESS THEY DON'T WANT               
         TM    LFLAG2,LIGNDAT                                                   
         BO    VALK30               - WE'RE IGNORING DATES, SKIP CHECKS         
*                                                                               
VALK28C  CLC   LFRENDMN,CMPSTMNP   NO - TEST FROM/TO DATES OVERLAP              
         BL    VALK29                                                           
         CLC   CMPNDMNP,LFRDATES                                                
         BL    VALK29                                                           
         CLC   CMPST,LFREND        FINAL TEST-TO CAMPAIGN START AFTER           
         BNH   VALK29A                        FROM CAMPAIGN END                 
*                                                                               
VALK29   OI    HDRIND,HDRINOSK     FLAG NO SCHEDULE COPY                        
         B     VALK34                                                           
VALK29A  CLC   LFRDATES(2),CMPSTMNP  TEST START AND END WEEKS THE SAME          
         BNE   VALK30                                                           
         CLC   LFRENDMN,CMPNDMNP                                                
         BNE   VALK30                                                           
         OI    HDRIND,HDRISAMD     FLAG CAMPAIGNS HAVE SAME DATES               
*                                                                               
VALK30   LA    RF,53               FIND WHICH WEEKS OVERLAP                     
         SR    R1,R1                                                            
         LA    R3,HDRSKDSP                                                      
         MVI   0(R3),FF                                                         
         MVC   1(L'HDRSKDSP-1,R3),0(R3)                                         
         LA    R6,LFRDATES                                                      
         L     R9,ATWA                                                          
         AHI   R9,CMPDATSP-TWAD                                                 
         TM    CMPOPTS,CAMODLY     AVOID SCHEDULE COPY FOR MIXTURE              
         BO    VALK31              OF DAILY AND WEEKLY SCHEDULES                
         MVC   0(2,R9),CMPSTMNP                                                 
         TM    LFRCAMOP,CAMODLY                                                 
         BZ    VALK32                                                           
         B     VALK34                                                           
*                                                                               
VALK31   TM    LFRCAMOP,CAMODLY                                                 
         BZ    VALK34                                                           
*                                                                               
VALK32   OC    0(4,R6),0(R6)                                                    
         BZ    VALK34                                                           
         OC    0(4,R9),0(R9)                                                    
         BZ    VALK34                                                           
         TM    LFLAG2,LIGNDAT      WE IGNORING DATES?                           
         BNO   VALK32E              - NOPE, CONTINUE NORMALLY                   
         CLI   0(R9),X'FF'         NO MORE DESTINATION WEEKS?                   
         BE    VALK34               - NOPE, NO MORE                             
         STC   R1,0(R3)             - YUP, STORE IT NO MATTER WHAT              
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         LA    R9,4(R9)            DESTINATION CAMPAIGN DATES                   
         B     VALK33A             BUMP R1 AND R3 NO MATTER WHAT                
*                                                                               
VALK32E  CLC   0(2,R6),0(R9)       EVENTUALLY CAN BE 0(R6) AND 0(R9)            
         BNE   VALK33                ARE BOTH  X'FF00'  SO HDRDATDSP            
         STC   R1,0(R3)              SHOULD BE XL(53+1) OTHERWISE               
VALK33   BH    *+12                  FIRST BYTE OF HDRDATSP GETS TRASHD         
         LA    R3,1(R3)                                                         
         LA    R6,4(R6)                                                         
         BL    *+12                                                             
         LA    R1,1(R1)                                                         
         LA    R9,4(R9)                                                         
*                                                                               
VALK33A  LTR   RF,RF               DID WE RUN OUT ALREADY?                      
         BZ    *+8                 BECAUSE OF THE 2 LINE AFTER THE BCT          
         BCT   RF,VALK32                                                        
*                                                                               
         CHI   R1,53               COMBINED OF 53 WEEKS                         
         BL    VALK32              NOT YET, KEEP GOING TILL IT IS               
*                                                                               
VALK34   OC    CMPUP,CMPUP         SEE IF CAMPAIGN UPGRADES DIFFERENT           
         BZ    VALK38                                                           
         CLC   LFRUF,CMPUF                                                      
         BNE   VALK36                                                           
         CLC   LFRUP,CMPUP                                                      
         BNE   VALK36                                                           
         CLC   LFRFB,CMPFB                                                      
         BNE   VALK36                                                           
         CLC   LFRFBLST,CMPFBLST                                                
         BNE   VALK36                                                           
         CLC   LFRPUT,CMPUPUT                                                   
         BNE   VALK36                                                           
         CLC   LFRSHR,CMPUSHR                                                   
         BE    VALK38                                                           
*                                                                               
VALK36   OI    HDRIND,HDRIUPGR     YES-USE NEW CAMPAIGN UPGRADE                 
*                                                                               
VALK38   DS    0H                                                               
         TM    LIND,LNFPASS        IS IT THE FIRST PASS?                        
         BO    VALK38E                                                          
         OI    LIND,LNFPASS                                                     
         DROP  R8                                                               
         LA    R8,CPYTM2H          ADVANCE TO SECOND CAMPAIGN                   
         B     *+8                                                              
VALK38E  LA    R8,CPYTM3H-CPYTM2H(R8)  ADVANCE TO NEXT CAMPAIGN                 
         LA    R4,HDRTABL(R4)                                                   
         BCT   R0,VALK14                                                        
*                                                                               
VALK40   B     VALKX                                                            
*                                                                               
VALKX    B     EXIT                                                             
         SPACE 2                                                                
VALKSETF XC    APWORK,APWORK                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APWORK+L'FVIHDR(0),0(R1)                                         
         LA    RF,1(RF)                                                         
         STC   RF,FVILEN-FVIHDR+APWORK                                          
         LA    RF,L'FVIHDR(RF)                                                  
         STC   RF,APWORK                                                        
         BR    RE                                                               
         EJECT                                                                  
         DROP  R4                                                               
***********************************************************************         
* DISPLAY RECORDS - THIS MODE IS USED TO DO THE COPY                  *         
***********************************************************************         
         SPACE 1                                                                
DISREC   MVC   LFSTKEY,APRECKEY    SAVE FIRST ELIGIBLE DETAIL KEY               
***  CAN SURPASS MAXIMUM I/OS NOW                                               
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL                                         
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,(X'80',0),F#MIOST   TURN ON CAN EXCEED I/OS          
***  CAN SURPASS MAXIMUM I/OS NOW                                               
*****                                                                           
         LA    R4,LHDRTAB                                                       
         USING HDRTABD,R4                                                       
*                                                                               
         TM    HDRIND,HDRINEW      READ THE DEST. HEADER REC TO AIO3            
         BO    DISREC00            ...IF THE HEADER IS NOT NEW                  
         BRAS  RE,READHDR                                                       
*                                                                               
***   DEL02CBL IS FOR CABLE SYSTEM COPIES ONLY!                                 
DISREC00 TM    LFLAG,LCBLSYS       IS IT CABLE SYSTEM COPY?                     
         BZ    DISREC0C             YES, SKIP NEXT FEW LINES OF GARBAGE         
*        BRAS  RE,CABLEALL                                                      
*        BE    *+6                 SHOULD BE FINE BECAUSE THIS CHECKED          
*        DC    H'0'                ...BEFORE                                    
*  RENSEQH RENAMES THE SEQUENCE ACCORDING TO "TO" CAMPAIGN                      
         BRAS  RE,RENSEQH          NOTHING HAPPENS IF NEW CAMPAIGN              
*ISREC0A BRAS  RE,DEL02CBL         KILL THEM ALL! (NON CABLE/NETWORKS)          
*                                                                               
         TM    HDRIND,HDRINEW      TEST NEW CAMPAIGN/MARKET                     
         BZ    DISREC0B                                                         
         BRAS  RE,RNHDRNEW         FOR NEW CAMPAIGNS ONLY                       
*                                                                               
DISREC0B LA    R8,CABLETAB                                                      
         B     DISREC02            CABLE SYS COPY DOESN'T NEED DEL02STA         
*                                                                               
***   DEL02STA IS FOR SINGLE STATION COPIES ONLY!                               
DISREC0C TM    LFLAG,LMKTCPY       IS IT MARKET COPY?                           
         BO    DISREC02             YES IT IS, SKIP THE SUBROUTINE              
         TM    HDRIND,HDRINEW      NEW CAMPAIGN?                                
         BO    DISREC0D             YES, DON'T GO TO DEL02STA                   
         BRAS  RE,DEL02STA         SINGLE STA COPY, DELETE BAD 02 ELS           
         B     DISREC02                                                         
DISREC0D BRAS  RE,DEL02NEW         FOR NEW CAMPAIGNS                            
*                                                                               
DISREC02 L     R2,AEXTRAWK         MARKET COPIES DON'T NEED RENSEQH             
         USING EXTRAWKD,R2                                                      
         LA    RE,STASNBKS         STATIONS AND BOOKS                           
         LA    RF,L'STASNBKS                                                    
         XCEFL                                                                  
         NI    ESTIND,X'FF'-ESTIBTYP   NEW ESTIMATE IS OVERRIDE BKTYP           
*                                                                               
         OC    LSTA,LSTA           ANY STATION FILTER?                          
         BZ    DISR1               NONE, A BUNCH OF STATIONS                    
         MVC   STASNBKS(L'LSTA),LSTA                                            
         LA    R1,LSTA                                                          
         BAS   RE,GETLNGTH                                                      
         GOTO1 AVALSTA,APWORK                                                   
         MVC   STASNBKS+L'LSTA(1),STABKTYP  REMEMBER, EST BOOK TYPE             
         B     DISR1X                         SUPERCEDES STA BOOK TYPE          
*                                                                               
GETLNGTH XC    APWORK,APWORK                                                    
         MVC   APWORK+8(L'LSTA),0(R1)                                           
         LA    R1,APWORK+8+L'LSTA-1                                             
         MVI   APWORK+7,C'A'       BREAKER SO NO ENDLESS LOOP                   
GLNGTH10 CLI   0(R1),C' '                                                       
         BH    GLNGTH20                                                         
         BCTR  R1,0                                                             
         B     GLNGTH10                                                         
GLNGTH20 LA    R1,1(R1)                                                         
         LA    RF,APWORK+8                                                      
         SR    R1,RF                                                            
         STC   R1,APWORK+5                                                      
         LA    R1,L'FVIHDR(R1)                                                  
         STC   R1,APWORK                                                        
         BR    RE                                                               
*                                                                               
DISR1    L     R9,AIOAREA1                                                      
         LA    R9,24(R9)                                                        
         LA    R2,STASNBKS                                                      
         XR    R0,R0                                                            
         DROP  R2                                                               
*                                                                               
DISR1A   CLI   0(R9),0             END OF RECORD                                
         BE    DISR1X                                                           
         CLI   0(R9),BWHELCDQ                                                   
         BE    DISR1C                                                           
DISR1B   IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     DISR1A                                                           
*                                                                               
         USING BWHEL,R9            STATION ELEMENT                              
DISR1C   LA    R1,BWHSTA                                                        
         L     RF,AEXTRAWK         MARKET COPIES DON'T NEED RENSEQH             
         USING EXTRAWKD,RF                                                      
         LA    RF,STASNBKS                                                      
         DROP  RF                                                               
         AHI   RF,L'STASNBKS                                                    
         CR    R2,RF                                                            
         BL    DISR1D                                                           
         XC    BWSMSG,BWSMSG                                                    
         OI    BWSMSGH+6,X'88'     TRANSMIT AND HIGH INTENSITY                  
         MVC   BWSMSG(32),=C'Too many stations on this copy!!'                  
         DC    H'0',C'$ABEND'      STASNBKS IS B4 NEXT D-CHAIN ENTRY            
DISR1D   MVC   0(L'LSTA,R2),0(R1)                                               
         BAS   RE,GETLNGTH                                                      
         GOTO1 AVALSTA,APWORK                                                   
         MVC   L'LSTA(1,R2),STABKTYP  REMEMBER, EST BOOK TYPE                   
         LA    R2,9(R2)            NEXT STATION ENTRY                           
         B     DISR1B                     SUPERCEDES STA BOOK TYPE              
         DROP  R9                                                               
*****                                                                           
DISR1X   MVC   LAMINBLK,AMINBLK    SAVE MINIO AREA ADDRESSES                    
         MVC   LAMINBUF,AMINBUFF                                                
         MVC   LAMINRTB,AMINRTAB                                                
*                                                                               
         L     RE,ATIA                                                          
         LH    RF,=H'6144'                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R1,ATIA                                                          
         USING MIN2D,R1                                                         
         MVI   MIN2BLK+MINBF2-MINBLKD,C'Y'                                      
*                                                                               
         LA    RE,MIN2BLK          SET SECOND MINIO AREA ADDRESSES              
         ST    RE,LAMN2BLK                                                      
         LA    RE,MIN2RTAB                                                      
         ST    RE,LAMN2RTB                                                      
         LA    RE,MIN2BUFF                                                      
         ST    RE,LAMN2BUF                                                      
         DROP  R1                                                               
***  DONE IN THE BEGINNING OF DISREC    MHC 06/25/02                            
*        LA    R4,LHDRTAB          COPY TO ALL CAMPAIGNS                        
*        USING HDRTABD,R4                                                       
         LA    R0,CPYMAX                                                        
         LA    R1,CPYTCMH                                                       
         ST    R1,LACAMP           SET A(TO CAMPAIGN HEADER)                    
*                                                                               
DISR2    CLI   HDRAM,0             TEST COPY FINISHED                           
         BE    DISR20                                                           
****  DO WE REALLY NEED THE ABOVE CHECK???!!?                                   
*ISR2    CLI   5(R1),0             ANY INPUT IN THE CAMPAIGN NUM FIELD?         
*        BE    DISR20               - NOPE, WE'RE DONE!!                        
****  NEW CHECK AS OF         05/27/03    MHC                                   
***      MVC   BAGYMD,HDRAM        MOVE BACK AGENCY MEDIA W/ BBYRMASK           
***  I FORGOT ABOUT THAT BBYRMASK, SHOULD NEVER NEED TO RESET BAGYMD!!          
****  OVERWRITING THE BUYER MASK, SHOULDN'T NEED REAL ONE ANYMORE               
         MVC   LSVBYRMK,BBYRMASK   SAVE OFF CURRENT MASK                        
         MVI   BBYRMASK,0          RESET BBYRMASK                               
         TM    HDRAM,X'08'         WE HAVE A BBYRMASK?                          
         BZ    DISR2G               - NOPE                                      
         OI    BBYRMASK,X'08'                                                   
DISR2G   MVC   BBYR,HDRBYR         MOVE BACK THE RIGHT BUYER                    
******  HAVE TO MOVE THEM BACK IN CASE 2ND DEST. CAMP HAS DIFF BUYER            
         XR    R3,R3                                                            
**                                                                              
         CLI   8(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
**                                                                              
         IC    R3,0(R1)            GET TOTAL LENGTH                             
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),0(R1)                                                  
         GOTO1 AVALCAM,APWORK      NEED THIS FOR CORRECT CMP INFO               
         BE    *+6                 (CMPSTMNP) SPECIFICLY                        
         DC    H'0'                                                             
         MVC   BBYRMASK,LSVBYRMK   RESTORE THE ACTIVE BUYER MASK                
**  ONLY USEFUL FOR CABLE SYSTEM CAMPAIGN COPIES                                
         XC    SAVESTA,SAVESTA                                                  
         XC    SAVESC,SAVESC                                                    
**                                                                              
         MVC   CMPSLEQU,HDRSLEQU   SET CAMPAIGN SPOT LENGTH EQUIVS              
         MVC   CLTBWPRO,HDRBWPRO   SET CLIENT BW PROFILE                        
         OI    LFLAG,LFSTREC       INDICATE FIRST RECORD                        
         MVI   LPKORNUM,0          INITIALIZE PKG/ORB NUMBER                    
         MVI   LASTPKOR,0                                                       
         XC    LSVUP1KY,LSVUP1KY                                                
         XC    LASTKEY,LASTKEY     INITIALIZE LAST KEY                          
         XC    IOKEY,IOKEY         CLEAR OUT IOKEY                              
         MVC   IOKEY(13),LFSTKEY   READ FIRST ELIGIBLE DETAIL RECORD            
*                                                                               
DISR3    LA    R1,MINHI2           READ HIGH                                    
         B     DISR4+4                                                          
*                                                                               
DISR4    LA    R1,MINSEQ2          READ ALL RECORDS                             
         GOTO1 AMIN                                                             
         BE    DISR4A                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISR12                                                           
DISR4A   ZIC   RE,LKEYCOMP                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV   TEST REACHED END                             
         BNE   DISR12                                                           
         CLC   IOKEY(BWDKELPO-BWDKEY),IOKEYSAV  TEST NEW STATION                
         BE    *+12                                                             
         MVI   LPKORNUM,0          YES-INIT PKG/ORB NUMBERS                     
         MVI   LASTPKOR,0                                                       
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
*                                                                               
*  ADDED 5/22/02 TEST ONLY FOR CABLE SYSTEM CAMPAIGN COPY VS CABLETAB           
         TM    LFLAG,LCBLSYS       IS IT CABLE SYSTEM CAMPAIGN COPY?            
         BZ    DISR4X               NO IT ISN'T                                 
*                                                                               
DISR4C   CLI   0(R8),X'FF'                                                      
         BE    DISR4BMP            CANT BELIEVE I LEFT THE STUPID CHECK         
         CLI   0(R8),0             MISSING STATION CODE                         
         BE    DISR4BMP            CANT BELIEVE I LEFT THE STUPID CHECK         
*                                                                               
DISR4E   CLC   BWDSTACD,0(R8)      TEST STATION CODE                            
         BE    DISR4X               PERFECT, CONTINUE                           
         BL    DISR4                BWDSTACD IS LOWER, DO A MINSEQ              
*                                                                               
DISR4BMP LA    R8,1(R8)            MISSING STATION CODE                         
         LA    RF,CABLETAB+L'CABLETAB   THE NEXT FIELD                          
         CR    R8,RF               IS IT THE END OF THE TABLE?                  
         BL    DISR4C               NO, NOT YET, KEEP GOING                     
         B     DISR12              IT'S OVER, COPY HEADER RECORD                
*                                                                               
DISR4X   CLI   LDPT,0              TEST DAYPART/LENGTH FILTERS                  
         BE    DISR5                                                            
         CLC   BWDDPT,LDPT                                                      
         BE    DISR5                                                            
         CLC   BWDSUBDP,LDPT       TRY SUB-DAYPART                              
         BNE   DISR6                                                            
*                                                                               
DISR5    CLI   LSLN,0              TEST SPOT LENGTH FILTER                      
         BE    *+18                                                             
         CLC   BWDSLN,LSLN         YES                                          
         BNE   DISR6                                                            
         B     DISR7                                                            
         CLI   HDRSLN,0            NO-TEST TO CAMPAIGN SINGLE SLN               
         BE    DISR7               NO-ACCEPT                                    
         CLI   LFRSLN,0            YES-TEST FROM CAMPAIGN SINGLE SLN            
         BNE   DISR7               YES-ACCEPT                                   
         CLC   BWDSLN,HDRSLN       NO-MATCH THE SPOT LENGTHS                    
         BE    DISR7                                                            
*                                                                               
DISR6    CLI   BWDKELPO,0          RECORD REJECTED-TEST PKG/ORB HDR             
         BE    DISR4               NO-NEXT RECORD                               
         LA    R3,IOKEY            YES-SKIP ENTIRE PACKAGE/ORBIT                
         MVC   BWDKELDY(4),EFFS                                                 
         B     DISR3                                                            
*                                                                               
DISR7    B     DISR8               ** SKIP THIS DUBIOUS CODE **                 
         TM    BWDINDS,BWDIXFR+BWDIUP1  TEST TRANSFERRED FROM NSID              
         BNO   *+10                     UPGRADE 1                               
         MVC   LSVUP1KY,BWDKEY     YES-SAVE ITS KEY                             
         TM    BWDINDS,BWDIUP2     TEST RECORD BELONGS TO 2ND UPGRADE           
         BZ    DISR8                                                            
         OC    HDRU2DAT,HDRU2DAT   YES-TEST TO CAMPAIGN HAS 2ND UPGRADE         
         BNZ   DISR8                                                            
         CLC   LSVUP1KY(BWDKELSQ-BWDKEY),BWDKEY  NO-REJECT IF READ A            
         BE    DISR4                             REC FOR 1ST UPGRADE            
         NI    BWDINDS,255-BWDIUP2   NO LONGER FROM 2ND UPGRADE                 
*                                                                               
DISR8    CLI   LSTACD,0            RECORD ACCEPTED-TEST STATION FILTER          
         BE    *+16                                                             
         CLC   BWDSTA,LSTA         YES-CHECK STATIONS CALL LETTERS              
         BE    *+6                                                              
         DC    H'0'                DISASTER                                     
*                                                                               
         XC    LAFRDMEL,LAFRDMEL                                                
         XC    LAFRSPEL,LAFRSPEL                                                
         XC    LAFRUPEL,LAFRUPEL                                                
         XC    LAFROVEL,LAFROVEL                                                
         XC    LAFRSHEL,LAFRSHEL                                                
         SR    RF,RF                                                            
         LA    RE,BWDEL                                                         
*                                                                               
DISR9    CLI   0(RE),0             LOCATE FROM RECORD ELEMENTS                  
         BE    DISR10                                                           
         CLI   0(RE),DMOELCDQ                                                   
         BNE   *+8                                                              
         ST    RE,LAFRDMEL                                                      
         CLI   0(RE),SPWELCDQ                                                   
         BNE   *+8                                                              
         ST    RE,LAFRSPEL                                                      
         CLI   0(RE),UPGELCDQ                                                   
         BNE   *+8                                                              
         ST    RE,LAFRUPEL                                                      
         CLI   0(RE),ODTELCDQ                                                   
         BNE   *+8                                                              
         ST    RE,LAFROVEL                                                      
         CLI   0(RE),SHPELCDQ                                                   
         BNE   *+8                                                              
         ST    RE,LAFRSHEL                                                      
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     DISR9                                                            
*                                                                               
DISR10   MVC   DTLKEY,BWDKEY       SAVE DETAIL KEY                              
         TM    LFLAG,LMKTCPY       IS IT MARKET COPY?                           
         BO    DISR11               YES, SKIP NEXT FEW LINES                    
         TM    HDRIND,HDRINEW      NEW CAMPAIGN?                                
         BZ    DISR11               NO, NO NEED FOR RENSEQD                     
*                                                                               
         CLC   SAVESTA,BWDSTA      NO NEED FOR SUBROUTINE IF SAME STA           
         BNE   DISR10A              NOT THE SAME, GO TO SUBROUTINE              
         MVC   BWDSTACD,SAVESC     SAVE AS PREVIOUS, MOVE IT IN                 
         MVC   BWDKELST,SAVESC     NEED IT FOR KEY AS WELL                      
         B     DISR11                                                           
*                                                                               
DISR10A  BRAS  RE,RENSEQD          RENUMBERING THE SEQUENCE FOR DETAILS         
*        CLI   0(R5),9                                                          
*        BNE   *+6                                                              
*        DC    H'0'                                                             
DISR11   BRAS  RE,COPYREC          COPY DETAIL RECORD                           
         BNE   DISR99                                                           
         MVC   IOKEY(13),DTLKEY    NEXT RECORD                                  
         GOTO1 AMIN,MINHI2                                                      
         B     DISR4                                                            
*                                                                               
DISR12   BAS   RE,COPYHDR          COPY HEADER AND CAMPAIGN DETAILS             
         MVC   AMINBLK,LAMN2BLK    SWITCH TO SECOND MINIO                       
         MVC   AMINBUFF,LAMN2BUF                                                
         MVC   AMINRTAB,LAMN2RTB                                                
         GOTO1 AMIN,MINCLS         CLOSE THAT MINIO                             
         MVC   AMINBLK,LAMINBLK    SWITCH BACK TO FIRST MINIO                   
         MVC   AMINBUFF,LAMINBUF                                                
         MVC   AMINRTAB,LAMINRTB                                                
*                                                                               
         LA    R4,HDRTABL(R4)      NEXT CAMPAIGN/MARKET                         
         LA    R1,CPYTCMH                                                       
         C     R1,LACAMP           SET A(NEXT CAMPAIGN HEADER)                  
         BNE   *+12                                                             
         LA    R1,CPYTC2H                                                       
         B     *+12                                                             
         L     R1,LACAMP                                                        
         LA    R1,CPYTM3H-CPYTM2H(R1)                                           
         ST    R1,LACAMP                                                        
         TM    LFLAG,LCBLSYS       IS IT CABLE SYSTEM CAMPAIGN COPY?            
         BZ    *+8                  - NOPE, DON'T RESET R8                      
         LA    R8,CABLETAB                                                      
         BCT   R0,DISR2                                                         
*                                                                               
DISR20   MVC   FVMSGNO,=AL2(FVCPYDON)   COMPLETE MESSAGE                        
         LA    R1,CPYMEDH                                                       
         ST    R1,FVADDR                                                        
*                                                                               
**                                                                              
         TM    APROFBTS,ASDNOWRK   DO WE NEED CHKAUTH?                          
         BO    *+8                                                              
         BRAS  RE,CHKAUTH                                                       
**                                                                              
*                                                                               
*        TM    LFLAG,LCBLSYS       IS IT A CABLE SYSTEM CAMPAIGN COPY?          
*        BNE   DISRX                NO, EXIT                                    
*        B     DISR4                                                            
         B     DISRX                                                            
*                                                                               
DISR99   OI    FVERRIND,FVEUNWND   ERROR EXIT-UNWIND THE TRANSACTION            
*                                                                               
DISRX    DS    0H                                                               
***                                                                             
**       L     RF,ACPARMA                                                       
**       L     RF,16(RF)           CALL                                         
**       L     RF,CGETFACT-COMFACSD(RF)                                         
**       GOTO1 (RF),APPARM,(X'80',0),F#MIOUN   TURN OFF EXCEED I/OS             
***                                                                             
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COPY HEADER AND CAMPAIGN DETAILS                         *         
* INPUT  : IOAREA1 CONTAINS CAMPAIGN/MARKET HEADER RECORD             *         
*          R4=A(HEADER RECORD ENTRY)                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING HDRTABD,R4                                                       
COPYHDR  NTR1  ,                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            YES-BUILD HEADER KEY                         
         USING BWHRECD,R2                                                       
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,HDRAM                                                   
         MVC   BWHKBYR,HDRBYR                                                   
         MVC   BWHKCAM,HDRCAM                                                   
         MVC   BWHKMKT,LMKT                                                     
         MVC   BWHKSEQ,HDRSEQ                                                   
         TM    HDRIND,HDRINEW      TEST NEW HEADER                              
         BZ    COPH2                                                            
         L     R2,AIOAREA3         YES-COPY HEADER RECORD                       
         LR    R0,R2                                                            
****     LA    R1,L'IOAREA1        USED TO BE 2000                              
         LHI   R1,L'IOAREA1        USED TO BE 4000, NOW 6000                    
         L     RE,AIOAREA1                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,UPD06EL                                                       
         BRAS  RE,UPDATE06         THIS IS TO SAVE OFF SOURCE BUYER...          
*                                  ...AND CAMPAIGN NUMBER  MHC 06/06/03         
*        XR    RE,RE                                                            
*        L     RF,AIOAREA3                                                      
*        LA    RF,BWHFSTEL-BWHKEY(RF)                                           
*OPH1A   CLI   0(RF),0                                                          
*        BE    COPH1Z                                                           
*        CLI   0(RF),INFELCDQ      INFO ELEMENT (X'06')?                        
*        BE    COPH1B                                                           
*        IC    RE,1(RF)                                                         
*        AR    RF,RE                                                            
*        B     COPH1A                                                           
*                                                                               
*        USING INFELD,RF                                                        
*OPH1B   NI    INFFLAG1,X'FF'-IFF1BYRV  THIS CAN'T BE                           
*        XC    INFADDED,INFADDED   CLEAR DATE FIELD                             
*        DROP  RF                                                               
*                                                                               
COPH1Z   MVC   BWHKEY,IOKEY        SET NEW KEY                                  
         GOTO1 AIO,FILADD3         ADD THE RECORD                               
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    IOKEY,IOKEY         ADD PASSIVE POINTER                          
         LA    R2,IOKEY                                                         
         MVI   BWHPTYP,BWHPTYPQ                                                 
         MVI   BWHPSUB,BWHPSUBQ                                                 
         MVC   BWHPAGMD,HDRAM                                                   
         MVC   BWHPBYR,HDRBYR                                                   
         MVC   BWHPSEQ,HDRSEQ                                                   
         MVC   BWHKCNTL+1(4),IODA                                               
         GOTO1 AIO,DIRADD                                                       
         BNL   COPH10                                                           
         DC    H'0'                                                             
*                                                                               
COPH2    GOTO1 AIO,DIRRD+IO3       READ CAMPAIGN/MARKET HEADER                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,FILGETU3                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
***    NO NEW STATION ADDED, BUT STILL NEED TO UPDATE 06 COPY INFO              
         BRAS  RE,UPDATE06         THIS IS TO SAVE OFF SOURCE BUYER...          
*                                  ...AND CAMPAIGN NUMBER  MHC 06/06/03         
         TM    HDRIND,HDRINEW+HDRINSTA   TEST NEW RECORD, OR STATION(S)         
         BZ    COPH9                     TO ADD TO RECORD                       
*                                                                               
         L     R2,AIOAREA3                                                      
         L     R1,AIOAREA1                                                      
         LA    R8,BWHFSTEL-BWHKEY(R1)                                           
         SR    R0,R0                                                            
*                                                                               
COPH4    CLI   0(R8),0             TEST END OF STATIONS                         
         BE    COPH9               YES                                          
         CLI   0(R8),BWHELCDQ                                                   
         BNE   COPH8                                                            
         ZIC   RF,BWHSEQ-BWHEL(R8) NO-TEST THIS STATION COPIED                  
         BCTR  RF,0                                                             
         LA    RF,HDRSTA(RF)                                                    
         CLI   0(RF),X'FF'                                                      
         BE    COPH8               NO                                           
         LA    R3,BWHFSTEL         YES-TEST STATION CODE EXISTS IN              
*                                      TO RECORD                                
COPH5    CLI   0(R3),0                                                          
         BE    COPH7                                                            
         CLI   0(R3),BWHELCDQ                                                   
         BNE   COPH6                                                            
         USING BWHEL,R3                                                         
         CLC   BWHSEQ,0(RF)                                                     
         BNE   COPH6                                                            
         CLC   BWHSTA,BWHSTA-BWHEL(R8)  YES-DOUBLE CHECK IT'S NOT               
         BE    COPH8                        ANOTHER STATION!                    
         DC    H'0'                                                             
*                                                                               
COPH6    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     COPH5                                                            
*                                                                               
COPH7    LA    R3,APELEM           ADD STATION TO 'TO' RECORD                   
         XC    APELEM,APELEM                                                    
         MVI   BWHELCD,BWHELCDQ                                                 
         MVI   BWHELLN,BWHELLNQ                                                 
         MVC   BWHSEQ,0(RF)                                                     
*                                                                               
         ZIC   R0,BWHSEQ                                                        
         CH    R0,=Y(HDRSTMAX)                                                  
         BNH   COPH7A                                                           
         XC    BWSMSG,BWSMSG                                                    
         OI    BWSMSGH+6,X'88'     TRANSMIT AND HIGH INTENSITY                  
         MVC   BWSMSG(32),=C'Too many stations on this copy!!'                  
         DC    H'0',C'$ABEND'      INCREASE HDRSTMAX                            
*                                                                               
COPH7A   MVC   BWHSTA,BWHSTA-BWHEL(R8)                                          
         GOTO1 AADDELS,BWHRECD                                                  
*                                                                               
COPH8    IC    R0,1(R8)            NEXT STATION                                 
         AR    R8,R0                                                            
         B     COPH4                                                            
*                                                                               
COPH9    GOTO1 AIO,FILPUT3         PUT CAMPAIGN/MARKET HEADER                   
         BE    COPH10                                                           
         DC    H'0'                                                             
*                                                                               
COPH10   TM    HDRIND2,HDRICOPU+HDRISKD  TEST NEED TO CHANGE CAMPAIGN           
         BZ    COPHX                     RECORD                                 
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY            YES-READ THE CAMPAIGN RECORD                 
         USING CAMRECD,R3                                                       
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,HDRAM                                                   
         MVC   CAMKBYR,HDRBYR                                                   
         MVC   CAMKCAM,HDRCAM                                                   
         GOTO1 AIO,DIRRD+IO3       READ CAMPAIGN/MARKET HEADER                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,FILGETU3                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA3                                                      
         TM    HDRIND2,HDRISKD     TEST SPOTS SCHEDULED                         
         BZ    *+8                                                              
         OI    CAMINDS,CAMISKD     YES                                          
         TM    HDRIND2,HDRICOPU    TEST COPY UPGRADE                            
         BZ    COPH12                                                           
         MVC   CAMUPFIL,LFRUF      YES                                          
         MVC   CAMUPGRD,LFRUP                                                   
         MVC   CAMFRBK,LFRFB                                                    
         MVC   CAMFRBKL,LFRFBLST                                                
         MVC   CAMINPUT,LFRUPIN                                                 
         MVC   CAMUPUT,LFRPUT                                                   
         MVC   CAMUSHR,LFRSHR                                                   
***                                THIS WILL FORCE VALCAM TO GO THROUGH         
         XC    BCAM,BCAM           COMPLETELY WHEN YOU DO WORK SKED             
***                                WITH THE "TO" CAMPAIGN  MHC 06/28/04         
*                                                                               
COPH12   GOTO1 AIO,FILPUT3         PUT CAMPAIGN RECORD                          
         BE    COPHX                                                            
         DC    H'0'                                                             
*                                                                               
COPHX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET DAYPART DETAILS                                      *         
* INPUT  : R3=AIOAREA3=A(TO DETAIL RECORD)                            *         
*          R4=A(HEADER TABLE ENTRY)                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING BWDRECD,R3                                                       
GETDPT   LR    R0,RE                                                            
         MVC   ESTDMENU,HDRDMENU                                                
         GOTO1 AGETDPT,BWDDPT      GET THE DAYPART DETAILS                      
         BE    GETDPTX                                                          
         MVC   FVMSGNO,=AL2(FVIDPT)   DPT NOT VALID FOR TO CAMPAIGN             
         MVC   FVXTRA(10),=C'- Daypart='                                        
         MVC   FVXTRA+10(1),BWDDPT                                              
*                                                                               
GETDPTX  LR    RE,R0                                                            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BR    RE                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GROSS UP A COST FROM NET (=85% OF GROSS)                 *         
* INPUT  : R1=A(COST)                                                 *         
***********************************************************************         
         SPACE 1                                                                
GROSSUP  LR    R0,RE                                                            
         ICM   RF,15,0(R1)                                                      
         BZ    GROSSUPX                                                         
         M     RE,=F'200'                                                       
         D     RE,=F'85'                                                        
         LTR   RF,RF                                                            
         BM    *+8                                                              
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         ST    RF,0(R1)                                                         
GROSSUPX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ENONE    MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                                                             
*                                                                               
EINV     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
*                                                                               
EIPWD    MVC   FVMSGNO,=AL2(FVIPWD)                                             
         B     EXIT                                                             
*                                                                               
ENOPWD   MVC   FVMSGNO,=AL2(FVNOPWD)                                            
         B     EXIT                                                             
*                                                                               
ECMSEQ   MVC   FVMSGNO,=AL2(FVCMSEQ)                                            
         B     EXIT                                                             
*                                                                               
EADJ     MVC   FVMSGNO,=AL2(FVCPYADJ)                                           
         B     EXIT                                                             
*                                                                               
ESTA     MVC   FVMSGNO,=AL2(FVISTA)                                             
         LA    R1,CPYMKTH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
EMKT     MVC   FVMSGNO,=AL2(FVIMKT)                                             
         LA    R1,CPYMKTH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
ENCP     MVC   FVMSGNO,=AL2(FVCPYMIS)                                           
         LA    R1,CPYMEDH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
EFFS     DC    XL8'FFFFFFFFFFFFFFFF'                                            
SPACES   DC    CL80' '                                                          
*                                                                               
FF       EQU   X'FF'                                                            
CPYMAX   EQU   4                   MAX N'COPY CAMPAIGNS                         
NDEMOS   EQU   14                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REFORMAT CABLE SYSTEM NUMBER INTO NNNN (EX. 0034 NOT 34) *         
* OUTPUT : SAVECABL WILL HAVE THE CABLE SYSTEM NUMBER IN NNNN FORMAT  *         
***********************************************************************         
CVRTCAB  NTR1                                                                   
         ZIC   R3,CPYMKTH+5                                                     
         SHI   R3,2                1 BYTE FOR SLASH 1 BYTE FOR EX               
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,CPYMKT(0)                                                  
         CVB   R1,APDUB                                                         
*                                                                               
         EDIT  (R1),SAVECABL,WRK=APWORK,DUB=APDUB                               
         OC    SAVECABL,=C'0000'   TO INSERT LEADING ZEROES                     
         XIT1                                                                   
***********************************************************************         
* ROUTINE TO READ THE DESTINATION HEADER RECORD TO AIOAREA3(IF EXIST) *         
* OUTPUT : IOAREA3 CONTAINS DESTINATION CAMPAIGN/MARKET HEADER RECORD *         
*          R4=A(HEADER RECORD ENTRY)                                  *         
***********************************************************************         
READHDR  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LHDRTAB                                                       
         USING HDRTABD,R3                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            YES-BUILD HEADER KEY                         
         USING BWHRECD,R2                                                       
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,HDRAM                                                   
         MVC   BWHKBYR,HDRBYR                                                   
         MVC   BWHKCAM,HDRCAM                                                   
         MVC   BWHKMKT,LMKT                                                     
         MVC   BWHKSEQ,HDRSEQ                                                   
*                                                                               
         GOTO1 AIO,DIRRD+IO3       READ CAMPAIGN/MARKET HEADER                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,FILGETU3                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RHDRX    XIT1                                                                   
         DROP  R2,R3                                                            
***********************************************************************         
* ROUTINE TO ERASE ALL STATIONS BUT THE ONE THAT'S BEING COPIED       *         
* INPUT  : IOAREA1 CONTAINS CAMPAIGN/MARKET HEADER RECORD             *         
*          R4=A(HEADER RECORD ENTRY)                                  *         
***********************************************************************         
         SPACE 1                                                                
DEL02NEW NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIOAREA1                                                      
         LA    R8,BWHFSTEL-BWHKEY(R4)                                           
         USING BWHEL,R8                                                         
*                                                                               
DELN05   CLI   0(R8),0             ARE WE DONE WITH ALL THE ELEMENTS?           
         BE    DELNX                YUP, SURE IS                                
         CLI   0(R8),2             IS IT THE X'02' ELEMENT?                     
         BNE   DELNBUMP             NAH, BUMP                                   
*                                                                               
         CLC   BWHSTA,LSTA         IS THIS THE ONE WE'RE LOOKING FOR?           
         BNE   DELN10               NEGATIVE, SIRE, DELETE IT PLZ               
         MVI   BWHSEQ,1            'TIS THE FIRST ELEMENT SINCE NEW             
         B     DELNBUMP                                                         
*                                                                               
DELN10   GOTO1 VRECUP,IODMCB,AIOAREA1,(R8),(R8)   DELETE THE ELEMENT            
         B     DELN05                                                           
*                                                                               
DELNBUMP ZIC   R0,1(R8)            BUMP THE STATION ELEMENT....                 
         AR    R8,R0               ...                                          
         B     DELN05                                                           
*                                                                               
DELNX    XIT1                                                                   
         EJECT                                                                  
         DROP  R8                                                               
***********************************************************************         
* ROUTINE TO ERASE ALL STATIONS BUT THE ONE THAT'S BEING COPIED       *         
*         THIS ROUTINE UPDATES THE HDRSTA AS WELL                     *         
* INPUT  : IOAREA1 CONTAINS CAMPAIGN/MARKET HEADER RECORD             *         
*          R4=A(HEADER RECORD ENTRY)                                  *         
***********************************************************************         
         SPACE 1                                                                
DEL02STA NTR1  BASE=*,LABEL=*                                                   
         LA    R1,LHDRTAB                                                       
         USING HDRTABD,R1                                                       
         LA    R2,HDRSTA                                                        
*                                                                               
         L     R4,AIOAREA1                                                      
         LA    R8,BWHFSTEL-BWHKEY(R4)                                           
NEWONE   USING BWHEL,R8                                                         
         L     R6,AIOAREA3                                                      
         LA    R5,BWHFSTEL-BWHKEY(R6)                                           
         USING BWHEL,R5                                                         
         MVI   SEQCOUNT,1                                                       
*                                                                               
DELS05   CLI   0(R8),0             ARE WE DONE WITH ALL THE ELEMENTS?           
         BE    DELSX                YUP, SURE IS                                
         CLI   0(R8),2             IS IT THE X'02' ELEMENT?                     
         BNE   DELSBUMP             NAH, BUMP                                   
*                                                                               
         CLC   NEWONE.BWHSTA,LSTA   IS THIS THE ONE WE'RE LOOKING FOR?          
         BNE   DELS10               NEGATIVE, SIRE, DELETE IT PLZ               
*                                                                               
DELS07   CLI   0(R5),0                                                          
         BE    R5DONE                                                           
         CLI   0(R5),2                                                          
         BNE   R5BUMPB                                                          
         CLC   NEWONE.BWHSTA,BWHSTA                                             
         BNE   R5BUMPA                                                          
         MVC   0(1,R2),BWHSEQ                                                   
         B     DELSBUMP                                                         
*                                                                               
R5DONE   ZIC   R0,SEQCOUNT                                                      
         AHI   R0,1                                                             
         STC   R0,0(R2)            REPLACE THE HDRSTA                           
         LA    R1,LHDRTAB                                                       
         OI    HDRIND,HDRINSTA     THERE IS A NEW STATION                       
         B     DELSBUMP                                                         
*                                                                               
R5BUMPA  MVC   SEQCOUNT,BWHSEQ     SAVE OFF THE STATION CODE                    
R5BUMPB  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DELS07                                                           
*                                                                               
DELSBUMP ZIC   R0,1(R8)            BUMP THE STATION ELEMENT....                 
         AR    R8,R0               ...                                          
         LA    R2,1(R2)                                                         
         B     DELS05                                                           
*                                                                               
DELS10   GOTO1 VRECUP,IODMCB,AIOAREA1,(R8),(R8)   DELETE THE ELEMENT            
         MVI   0(R2),X'FF'                                                      
         LA    R2,1(R2)                                                         
         B     DELS05                                                           
*                                                                               
DELSX    XIT1                                                                   
         EJECT                                                                  
         DROP  R1,R5                                                            
*&&DO                                                                           
***********************************************************************         
* ROUTINE TO PERGE STATIONS THAT DOES NOT BELONG IN THE CABLE         *         
* INPUT  : IOAREA1 CONTAINS CAMPAIGN/MARKET HEADER RECORD             *         
*          R4=A(HEADER RECORD ENTRY)                                  *         
***********************************************************************         
         SPACE 1                                                                
DEL02CBL NTR1  BASE=*,LABEL=*                                                   
         L     R9,ASAVAREA         RESTORE A(EXTRA SAVED STORAGED)              
         USING SAVAREA,R9                                                       
*                                                                               
         LA    R5,CABLETAB                                                      
         SR    R3,R3               R3 WILL BE COUNTER                           
*                                                                               
         L     R4,AIOAREA1                                                      
         LA    R8,BWHFSTEL-BWHKEY(R4)                                           
         USING BWHEL,R8                                                         
*                                                                               
DELC05   CLI   0(R8),0                                                          
         BE    DELCX               IT'S OVA                                     
         CLI   0(R8),2             IS IT THE X'02' ELEMENT?                     
         BNE   DELCBUMP             NAH, BUMP                                   
*                                                                               
         CHI   R3,169                                                           
         BNL   DELCERR                                                          
         CLI   0(R5),X'FF'                                                      
         BE    DELC15                                                           
         LA    R5,1(R5)            BUMP THE TABLE                               
         LA    R3,1(R3)            BUMP THE COUNTER SINCE NOT DELETING          
DELCBUMP ZIC   R0,1(R8)            BUMP THE STATION ELEMENT...                  
         AR    R8,R0               ....                                         
         B     DELC05                                                           
*                                                                               
DELC15   GOTO1 VRECUP,IODMCB,AIOAREA1,(R8),(R8)   DELETE THE ELEMENT            
         LA    R5,1(R5)            BUMP THE TABLE                               
***  WE'RE NOT BUMPING THE COUNTER!!   MHC  10/30/02                            
*        LA    R3,1(R3)            BUMP THE COUNTER WHEN R5 GOES UP             
         B     DELC05                                                           
*                                                                               
DELCX    XIT1                                                                   
*                                                                               
DELCERR  DC    H'0'                                                             
         DROP  R8,R9                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ROUTINE TO RENUMBER THE SEQUENCE FOR *NEW* HEADER RECORDS ONLY      *         
* INPUT  : IOAREA1 CONTAINS CAMPAIGN/MARKET HEADER RECORD             *         
*          R4=A(HEADER RECORD ENTRY)                                  *         
***********************************************************************         
         SPACE 1                                                                
RNHDRNEW NTR1  BASE=*,LABEL=*                                                   
*  NO NEED FOR HDRSTA BECAUSE IT'S A NEW CAMPAIGN                               
         L     R9,ASAVAREA         RESTORE A(EXTRA SAVED STORAGED)              
         USING SAVAREA,R9          (FOR CABLETAB)                               
***      LA    R5,CABLETAB                                                      
*                                                                               
         L     R4,AIOAREA1                                                      
         USING BWHRECD,R4                                                       
         LA    R8,BWHFSTEL-BWHKEY(R4)                                           
         USING BWHEL,R8                                                         
         LA    R3,1                COUNTER FOR THE SEQUENCE                     
*                                                                               
***      CLI   0(R5),X'FF'                                                      
***      BE    RNHDR07                                                          
***      CLI   0(R5),0             MISSING STATION CODE                         
***      BNE   RNHDR10                                                          
***DR07  LA    R5,1(R5)                                                         
***      B     RNHDR05                                                          
*                                                                               
RNHDR10  CLI   0(R8),0                                                          
         BE    RNHDRX                                                           
         CLI   0(R8),2                                                          
         BNE   RNHDR20                                                          
*                                                                               
         XR    R5,R5                                                            
         IC    R5,BWHSEQ                                                        
         BCTR  R5,0                                                             
         LA    R5,CABLETAB(R5)                                                  
*                                                                               
         CLC   BWHSEQ,0(R5)                                                     
         BNE   RNHDR20                                                          
         STC   R3,BWHSEQ           UPDATE THE STATION CODE                      
         LA    R3,1(R3)            BUMP THE COUNTER                             
***      LA    R5,1(R5)            BUMP CABLETAB                                
*                                                                               
RNHDR20  XR    R0,R0               TRY TO FIND THE NEXT X'02' ELEMENT           
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     RNHDR10                                                          
*                                                                               
RNHDRX   XIT1                                                                   
         EJECT                                                                  
         DROP  R4,R8                                                            
***********************************************************************         
* ROUTINE TO RENUMBER THE SEQUENCE FOR HEADER RECORD                  *         
*         HDRSTA SHOULD BE READY FOR EXISTING STATIONS                *         
*         JUST NEED TO RENUMBER FOR NEW STATIONS                      *         
* INPUT  : IOAREA1 CONTAINS CAMPAIGN/MARKET HEADER RECORD             *         
*          IOAREA3 CONTAINS DESTINATION HEADER RECORD                 *         
*          R4=A(HEADER RECORD ENTRY)                                  *         
***********************************************************************         
         SPACE 1                                                                
RENSEQH  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING HDRTABD,R1                                                       
         LA    R1,LHDRTAB                                                       
*                                                                               
         TM    HDRIND,HDRINEW      TEST NEW CAMPAIGN/MARKET                     
         BO    RENHX               GET OUT IF IT'S A NEW CAMPAIGN               
*                                                                               
         L     R4,AIOAREA1                                                      
         USING BWHRECD,R4                                                       
         LA    R8,BWHFSTEL-BWHKEY(R4)                                           
         USING BWHEL,R8                                                         
*                                                                               
RENH10   CLI   0(R8),0                                                          
         BE    RENHX                - WE'RE DONE                                
         CLI   0(R8),2                                                          
         BNE   RENHBUMP                                                         
*                                                                               
RENH20   ZIC   RE,BWHSEQ                                                        
         BCTR  RE,0                                                             
         LA    RE,HDRSTA(RE)                                                    
         CLI   0(RE),X'FF'         DO WE HAVE X'FF'                             
         BNE   RENHBUMP             - NOPE, BUMP                                
*                                                                               
****   NOW WE NEED THE NEXT STATION CODE                                        
         MVC   0(1,RE),HDRNXTST                                                 
****   BUMP SEQCOUNT                                                            
         XR    RF,RF                                                            
         IC    RF,HDRNXTST                                                      
         LA    RF,1(RF)                                                         
         STC   RF,HDRNXTST                                                      
****                                                                            
         OI    HDRIND,HDRINSTA     NEW STATION(S) EXISTS, TURN ON FLAG          
*                                                                               
RENHBUMP XR    R0,R0               TRY TO FIND THE NEXT X'02' ELEMENT           
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     RENH10                                                           
*                                                                               
RENHX    XIT1                                                                   
         EJECT                                                                  
         DROP  R1                                                               
***********************************************************************         
* ROUTINE TO RENUMBER THE SEQUENCE FOR DETAIL RECORD                  *         
* INPUT  : IOAREA2 CONTAINS CAMPAIGN/MARKET DETAIL RECORD             *         
*          R4=A(DETAIL RECORD ENTRY)                                  *         
***********************************************************************         
         SPACE 1                                                                
RENSEQD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING BWDRECD,R4                                                       
         L     R4,AIOAREA2                                                      
         LA    R8,BWDFSTEL-BWDKEY(R4)                                           
         USING BWDEL,R8                                                         
         USING BWHRECD,R6                                                       
         L     R6,AIOAREA1                                                      
         LA    R5,BWHFSTEL-BWHKEY(R6)                                           
         USING BWHEL,R5                                                         
*                                                                               
REND05   CLI   0(R5),0                                                          
         BE    RENDX                                                            
         CLI   0(R5),2             IS THIS AN X'02' ELEMENT                     
         BNE   REND10                                                           
         CLC   BWDSTA,BWHSTA                                                    
         BNE   REND10                                                           
         MVC   BWDKELST,BWHSEQ     REPLACE THE SEQUENCE CODE                    
         MVC   BWDSTACD,BWHSEQ     ONCE FOR KEY, ONCE FOR ELEMENT               
         MVC   SAVESTA,BWDSTA                                                   
         MVC   SAVESC,BWDSTACD                                                  
         B     RENDX                                                            
*                                                                               
REND10   ZIC   R0,1(R5)            GET TO THE NEXT X'02' ELEMENT                
         AR    R5,R0                                                            
         B     REND05                                                           
*                                                                               
         DROP  R4,R8,R5,R6                                                      
RENDX    XIT1                                                                   
***********************************************************************         
* ROUTINE CHECKS 06 ELEM IF THERE IS ANY DATE                         *         
* IF IT FINDS NO DATE OR NO 06 ELEM IT PUTS/ADDS 06 ELEM W/ CURR DATE *         
***********************************************************************         
         SPACE 1                                                                
UPD06EL  NTR1  BASE=*,LABEL=*                                                   
         USING BWHRECD,R2                                                       
         L     R2,AIOAREA3                                                      
         LA    R6,BWHFSTEL         POINT TO 1ST ELEM                            
*                                  LOOK FOR X'06' ELEM                          
UPD06A   CLI   0(R6),0                                                          
         BE    NEW06               DIDN'T FIND, ADD W/ TODAY'S DATE             
         CLI   0(R6),INFELCDQ                                                   
         BE    OLD06               FOUND OLD, CK DATE, ADD TODAYS' IF 0         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               CK NXT ELEM                                  
         B     UPD06A                                                           
*                                                                               
NEW06    DS    0H                                                               
         TM    LFLAG,LNOSKD                                                     
         BO    UPDX                                                             
         XC    APELEM,APELEM       ADD ELEM W/ TODAYS DATE                      
         LA    R4,APELEM                                                        
         USING INFELD,R4                                                        
         MVI   INFELCD,INFELCDQ                                                 
         MVI   INFELLN,INFELLNQ                                                 
         GOTO1 VDATCON,APPARM,(5,0),(3,INFADDED)                                
         MVC   SVDAT,INFADDED      SAVE FOR CHKAUTH                             
         GOTO1 AADDELS,BWHRECD     ADD ELEM                                     
         B     UPDX                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
OLD06    DS    0H                                                               
         USING INFELD,R6                                                        
         MVC   SVDAT,INFADDED                      SAVE FOR CHKAUTH             
         NI    INFFLAG1,X'FF'-IFF1BYRV             THIS CAN'T BE                
         TM    LFLAG,LNOSKD                                                     
         BO    UPDX                                                             
         GOTO1 VDATCON,APPARM,(5,0),(3,INFADDED)    PUT TODAY'S DATE            
         MVC   SVDAT,INFADDED                       SAVE FOR CHKAUTH            
*                                                                               
UPDX     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE LOOKS FOR 06 ELEM, BETTER BE THERE!                         *         
* SAVES/UPDATES THE SOURCE BUYER AND CAMPAIGN NUMBERELEM W/ CURR DATE *         
*                                  MHC  06/06/03                      *         
***********************************************************************         
         SPACE 1                                                                
UPDATE06 NTR1  BASE=*,LABEL=*                                                   
OLDHDR   USING BWHRECD,R3          NEED THIS TO STORE SOURCE BUYER...           
         L     R3,AIOAREA1         ...AND CAMPAIGN NUMBER DETAILS               
*                                                                               
         USING BWHRECD,R2                                                       
         L     R2,AIOAREA3                                                      
         LA    R6,BWHFSTEL         POINT TO 1ST ELEM                            
*                                  LOOK FOR X'06' ELEM                          
UP06A    CLI   0(R6),0                                                          
         BE    NEW06A              DIDN'T FIND, ADD W/ TODAY'S DATE             
         CLI   0(R6),INFELCDQ                                                   
         BE    OLD06A              FOUND OLD, CK DATE, ADD TODAYS' IF 0         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               CK NXT ELEM                                  
         B     UP06A                                                            
*                                                                               
NEW06A   DS    0H                                                               
         XC    APELEM,APELEM       ADD ELEM W/ TODAYS DATE                      
         LA    R4,APELEM                                                        
         USING INFELD,R4                                                        
         MVI   INFELCD,INFELCDQ    X'06' TYPE                                   
         MVI   INFELLN,INFELLNQ                                                 
         MVC   INFFRBYR,OLDHDR.BWHKBYR   SAVE SOURCE BUYER                      
         MVC   INFFRCAM,OLDHDR.BWHKCAM   SAVE SOURCE CAMPAIGN NUMBER            
         GOTO1 VDATCON,APPARM,(5,0),(19,INFCPDAT)   LAST COPY DATE              
*                                                   (UNPACKED JULIAN)           
         TM    LFLAG2,LIGNDAT      WE IGNORING DATES?                           
         BZ    *+8                                                              
         OI    INFFLAG2,IFF2SPDT    - USING SPECIAL 'BY DATE' COPY              
***                                                                             
         GOTO1 AADDELS,BWHRECD     ADD ELEM                                     
         B     UPDATEX                                                          
         DROP  R4                                                               
*                                                                               
OLD06A   DS    0H                                                               
         USING INFELD,R6                                                        
*                                                                               
         MVC   INFFRBYR,OLDHDR.BWHKBYR   SAVE SOURCE BUYER                      
         MVC   INFFRCAM,OLDHDR.BWHKCAM   SAVE SOURCE CAMPAIGN NUMBER            
         GOTO1 VDATCON,APPARM,(5,0),(19,INFCPDAT)   LAST COPY DATE              
*                                                     (JULIAN)                  
         MVI   INFFLAG2,0          RESET FLAG EVERY TRANSFER                    
         TM    LFLAG2,LIGNDAT      WE IGNORING DATES?                           
         BZ    *+8                                                              
         OI    INFFLAG2,IFF2SPDT    - USING SPECIAL 'BY DATE' COPY              
***                                                                             
UPDATEX  J     EXIT                                                             
         DROP  OLDHDR,R6                                                        
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
* CALL SPAUTH TO UPDATE SUPERDESK AUTHORIZATION RECORDS               *         
***********************************************************************         
         SPACE 1                                                                
CHKAUTH  NTR1  BASE=*,LABEL=*                                                   
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         TM    XTRAFLG1,XAUTH      ALREADY WENT THROUGH CODE?                   
         BNZ   CHKAUTHZ            YES, GET OUT                                 
         TM    XTRAFLG1,XSDE       SDESK AUTH OPEN FOR PRD OPTION?              
         BO    CKAUTH10            YES                                          
         DROP  RF                                                               
*                                                                               
         L     RF,ATWA                                                          
         AHI   RF,SVINDS-TWAD                                                   
         TM    0(RF),SVIEAUTH      SDESK AUTH OPEN FOR CAMPAIGN PRD?            
         BNO   CHKAUTHZ            NO, GET OUT                                  
CKAUTH10 MVC   APHALF+0(1),BPRD                                                 
         MVC   APHALF+1(1),CMPPRD2                                              
         BRAS  RE,SETPRD                                                        
         CLI   APHALF+1,0          IS THERE A PIGGYBACK PRODUCT?                
         BE    CKAUTH50            NO                                           
         TM    CLTIND2,CLTPONLY    POL ONLY?                                    
         BZ    CKAUTH20                                                         
         CLI   CLTPROF+0,C'0'      TRUE POL?                                    
         BE    CKAUTH50            IF TRUE POL DON'T READ EST RECORD            
*                                                                               
CKAUTH20 MVC   APBYTE,APHALF+1                                                  
         BRAS  RE,GETPIG           GET 3 BYTE PRODUCT MNEMONIC                  
*                                                                               
         USING ESTHDRD,R2                                                       
         LA    R2,IOKEY            READ ESTIMATE RECORD FOR PIGGYBACK           
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,APFULL      PIGGYBACK PRODUCT                            
         MVC   EKEYEST,BEST                                                     
*                                                                               
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   *+14                                                             
         CLC   EKEY,IOKEYSAV                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 AIO,FILGET1         GET ESTIMATE HDR                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         TM    EFLAG1,EF1SDE       PIGGYBACK AUTHORIZATION OPEN?                
         BZ    CHKAUTHZ            NO, GET OUT                                  
         DROP  R2                                                               
*                                                                               
* NEED TO ALPHABETIZE PRD1 AND PRD2                                             
         CLI   INOPRD,0            PRD CODE HERE?                               
         BE    CKAUTH30            NO, CHECK CAMPAIGN                           
*                                                                               
*                                                                               
         CLC   POLPRD1,POLPRD2     ALREADY ALPHABETIZED?                        
         BH    CKAUTH40            NO                                           
         B     CKAUTH50            YES                                          
*                                                                               
CKAUTH30 CLC   CMPPRDC,APFULL      ALREADY ALPHABETIZED?                        
         BNH   CKAUTH50            YES                                          
*                                                                               
CKAUTH40 MVC   APBYTE,APHALF       SWAP THEM                                    
         MVC   APHALF(1),APHALF+1                                               
         MVC   APHALF+1(1),APBYTE                                               
*                                                                               
         PUSH  USING                                                            
CKAUTH50 XC    APWORK,APWORK       CALL SPAUTH                                  
*                                                                               
         USING SPAUTHD,APWORK                                                   
         MVC   SPACOM,ACOM                                                      
         L     RF,AIOAREA1                                                      
         ST    RF,SPAIO                                                         
         MVC   SPAKAM,BAGYMD                                                    
         MVC   SPAKCLT,BCLT                                                     
         MVC   SPAKPRD(2),APHALF                                                
         MVC   SPAKEST,BEST                                                     
***      MVC   SPAKMKT,BMKT                                                     
         MVC   SPAKMKT,LFRMKT                                                   
         MVI   SPAUPDT,SPAUPDWK    UPDATE NWS WORK REC ADDED DATE               
         GOTO1 VDATCON,APPARM,(3,SVDAT),(2,SPAWRKDT)                            
         GOTO1 VDATCON,APPARM,(3,CMPST),(2,SPASDTE)                             
         GOTO1 (RF),(R1),(3,CMPND),(2,SPAEDTE)                                  
         GOTO1 VSPAUTH,APWORK                                                   
         CLI   SPAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
CHKAUTHX L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         OI    XTRAFLG1,XAUTH      TURN FLAG ON                                 
         DROP  RF                                                               
CHKAUTHZ XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*******************************************************************             
*                                                                               
SETPRD   DS    0H                                                               
         TM    LIND,LPOL           TEST POL BUY                                 
         JZ    SETPRDX             NO-THERE'S NO BDMASPRD                       
         XC    APHALF,APHALF                                                    
         MVC   APHALF(1),BPRD      CAMPAIGN PRODUCT                             
         CLI   CMPPRD1,0           TEST FOR CAMPAIGN PIGGYBACKS                 
         JE    SETPRD2                                                          
         MVC   APHALF(1),CMPPRD1   YES                                          
         MVC   APHALF+1(1),CMPPRD2                                              
         J     SETPRD4                                                          
*                                                                               
SETPRD2  CLI   BPRD,FF             TEST CAMPAIGN PRODUCT = POL                  
         JNE   SETPRD4                                                          
         CLI   INOPRD,0            YES-SET MASTER PRODUCT FROM THE              
         JE    SETPRDX                 PRD OPTION IF IT'S GIVEN                 
         MVC   APHALF,INOPRD                                                    
*                                                                               
SETPRD4  MVC   LMASPRD,APHALF                                                   
*                                                                               
SETPRDX  BR    RE                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
GETPIG   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 AIO,FILRD1          READ CLIENT RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA1                                                      
         LA    R1,CLIST            LOOK FOR PASSIVE PRODUCT CODE                
*                                                                               
GETPIG10 CLI   0(R1),C'A'                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   APBYTE,3(R1)                                                     
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     GETPIG10                                                         
         MVC   APFULL,0(R1)        FOUND - PRODUCT MNEMONIC                     
         XIT1                                                                   
*                                                                               
SVDAT    DS    XL3                 SAVE INFADDED FOR AUTH                       
         EJECT                                                                  
***********************************************************************         
* ROUTINE FOR COMPLETE CABLE SYSTEM CAMPAIGN COPIES                   *         
* OUTPUT : CABLETAB WILL BE FILLED UP WITH FILTERED STATION SEQUENCE  *         
*          NUMBERS FOR CABLE SYSTEM TOTAL COMPAIGN COPY               *         
***********************************************************************         
CABLEALL NTR1  BASE=*,LABEL=*                                                   
         L     R9,ASAVAREA         RESTORE A(EXTRA SAVED STORAGED)              
         USING SAVAREA,R9                                                       
         XC    SAVEKEY,SAVEKEY                                                  
         USING BWHRECD,R2                                                       
         LA    R2,SAVEKEY                                                       
         MVI   BWHKTYP,BWHKTYPQ    X'0D' TYPE RECORDS                           
         MVI   BWHKSUB,BWHKSUBQ    X'67' SUBTYPE                                
         MVC   BWHKAGMD,LMED       AGENCY/MEDIA                                 
         OC    BWHKAGMD,BBYRMASK   DON'T FORGET THIS IDIOT!  MHC                
         MVC   BWHKBYR,LBYR        BUYER                                        
         MVC   BWHKCAM,LCAM        CAMPAIGN NUMBER                              
*                                                                               
         TM    LFLAG2,LMKTDONE     X'40' - STATION RECORD IS READ               
         BZ    CABLE00                                                          
         OC    BMKT,BMKT           IS BMKT SET YET?                             
         BNZ   CABLE05              YES IT IS                                   
*                                                                               
*  WE NEED TO GRAB A STATION RECORD TO GET THE MARKET NUMBER                    
         USING STAHDRD,R4                                                       
CABLE00  XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         MVI   STAKTYPE,STAKTYPQ   RECORD TYPE (C'S')                           
         MVI   STAKMED,C'T'        MEDIA IS ALWAYS T FOR CABLE SYSTEM           
         MVC   STAKCALL(4),SAVECABL                                             
         MVI   STAKCALL+4,C'T'     ALWAYS T                                     
         MVC   STAKAGY,CUAALF      MOVE IN AGENCY ALPHA                         
         MVC   STAKCLT,LQCLT       3 LETTER CLIENT SPECIFIC CODE                
*                                                                               
         GOTO1 AIO,IOSTAFIL+IOHI+IO2   AIOAREA2 HAS MASTER RECORD NOW           
         L     R4,AIOAREA2                                                      
         CLC   IOKEY(STAKCLT-STAKEY),0(R4)                                      
         BE    CABLE02                                                          
         OI    LFLAG2,LINVSTA      INVALID CABLE SYSTEM                         
         B     CABLENO             SET "NOT EQUAL" CONDITION..                  
*                                  ..AND GET THE HELL OUTTA HERE                
CABLE02  CLC   STAKCLT,LQCLT       DOES CLIENT SPECIFIC STATION EXIST?          
         BE    CABLE03              - YES, CONTINUE                             
         CLC   STAKCLT,=C'000'      - NO, IS IT GENERIC MASTER RECORD?          
         BE    CABLE03                - YES, CONTINUE                           
         LA    R4,IOKEY                                                         
         MVC   STAKCLT,=C'000'                                                  
*                                                                               
         GOTO1 AIO,IOSTAFIL+IOHI+IO2   GO GET GENERIC MASTER RECORD             
         L     R4,AIOAREA2                                                      
         CLC   IOKEY(STAKFILL-STAKEY),0(R4)                                     
         BE    *+6                                                              
         DC    H'0'                SHOULD DEFINITELY AGREE                      
*                                                                               
CABLE03  PACK  SCDUB,SMKT                                                       
         CVB   R0,SCDUB                                                         
         STCM  R0,3,BMKT                                                        
         OI    LFLAG2,LMKTDONE     X'40'                                        
*  THE MARKET NUMBER IS FINALLY CONSTRUCTED                                     
*                                                                               
CABLE05  MVC   BWHKMKT,BMKT        MARKET                                       
         MVC   LMKT,BMKT                                                        
         MVC   IOKEY,SAVEKEY                                                    
         GOTO1 AIO,DIRHI+IO1                                                    
*                                                                               
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BE    CABLE07                                                          
         B     CABLENO             NO RECORDS TO COPY, SET TO NOT EQUAL         
*                                                                               
CABLE07  GOTO1 AIO,FILGET1         PUT THE RECORD IN AIOAREA1                   
*                                                                               
         L     R2,AIOAREA1         LOOK FOR/SAVE ALL THE STATION..              
         LA    R4,BWHFSTEL         ..SEQUENCE CODES IN CABLETAB                 
*                                                                               
CABLE10  CLI   0(R4),0                                                          
         BE    CABLE30             NO MORE STATIONS, CONTINUE                   
         CLI   0(R4),BWHELCDQ      X'02' ELEMENT                                
         BNE   CABLE20              - BUMP                                      
         USING BWHEL,R4                                                         
         CLC   SAVECABL,BWHSTA     CHECK FOR CABLE SYSTEM ONLY(4 BYTES)         
         BNE   CABLE25              - WE'RE GONNA DELETE THIS ELEMENT           
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING BWDRECD,R3                                                       
         MVI   BWDKTYP,BWDKTYPQ    RECORD TYPE X'0D'                            
         MVI   BWDKSUB,BWDKSUBQ    SUB-TYPE X'68'                               
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK   DON'T FORGET THIS IDIOT!  MHC                
         MVC   BWDKBYR,BBYR        BUYER                                        
         MVC   BWDKSEQ,BWHKSEQ     CAMPAIGN/MARKET SEQ NUM FROM HDR KEY         
         MVI   BWDKELCD,BWDELCDQ   X'01' DESCRIPTION ELEMENT                    
         MVC   BWDKELST,BWHSEQ                                                  
         LA    R6,BWDKELST-BWDKEY-1   SET KEY COMPARE LENGTH                    
         CLI   BWHSEQ,0                                                         
         BE    *+8                                                              
         LA    R6,L'BWDKELST(R6)                                                
         LR    R1,R6               COPY R6 TO DECREMENT                         
         BCTR  R1,0                                                             
         STC   R1,LKEYCOMP                                                      
*        STC   R6,LKEYCOMP                                                      
         LA    R1,MINHI2                                                        
         B     CABLE15+4                                                        
*                                                                               
CABLE15  LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                READ HIGH FOR DETAIL RECORD                  
         BNE   CABLE25              - DELETE                                    
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   CABLE25             NO DETAIL RECORDS FOUND - DELETE             
*  RECORD IS FOUND, SAVE THE SEQUENCE NUMBER IN THE TABLE                       
*  ....IF IT PASSING THE DAYPART FILTER                                         
*        CLI   BDPT,0              ANY DAYPART FILTER?                          
*        BE    CABLE18              NOPE                                        
*        CLC   BWDDPT,BDPT         DOES THE DAYPART MATCH?                      
*        BE    CABLE18              YES, CONTINUE                               
*        CLC   BWDSUBDP,BDPT        NO, BUT DOES SUB-DAYPART MATCH?             
*        BNE   CABLE20               NO, NEXT RECORD PLZ                        
*                                                                               
         TM    LFLAG2,LFSTTIME     HAVE WE DONE THIS ALREADY??                  
         BO    CABLE18              - YUP, SKIP                                 
         MVC   APRECKEY(13),IOKEY   SAVE OFF FIRST DETAIL RECORD KEY            
         OI    LFLAG2,LFSTTIME     SET THE X'80' BIT                            
*                                                                               
CABLE18  ZIC   RF,BWHSEQ                                                        
         BCTR  RF,0                                                             
         LA    RF,CABLETAB(RF)                                                  
         MVC   0(1,RF),BWHSEQ      SAVE OFF THE STATION CODE                    
*                                                                               
CABLE20  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CABLE10                                                          
*                                                                               
CABLE25  ZIC   RF,BWHSEQ                                                        
         BCTR  RF,0                                                             
         LA    RF,CABLETAB(RF)                                                  
         MVI   0(RF),X'FF'         NOT NEEDED                                   
         GOTO1 VRECUP,IODMCB,AIOAREA1,(R4),(R4)   DELETE THE ELEMENT            
         B     CABLE10             CHECK OTHER STATIONS NOW                     
*                                                                               
CABLE30  DS    0H                                                               
         TM    LFLAG2,LFSTTIME     WE HAVE SOMETHING FOR COPYING"               
         BNO   CABLENO              - NO WE DON'T                               
*                                                                               
CABLEYES CR    R2,R2               SET CONDITION CODE TO EQUAL                  
         B     *+6                                                              
*                                                                               
CABLENO  CR    RE,RD                                                            
CABLEX   XIT1                                                                   
         LTORG                                                                  
         DROP  R2,R3,R4,R9                                                      
***********************************************************************         
* ROUTINE TO CHECK IF STATIONS ALREADY EXISTS IN DESTINATION HEADER   *         
* INPUT  : IOAREA2 CONTAINS DETAIL RECORD                             *         
*          IOAREA3 CONTAINS DESTINATION HEADER RECORD                 *         
***********************************************************************         
HDRCHECK NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LHDRTAB                                                       
         USING HDRTABD,R3                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R5,IOKEY                                                         
         USING BWHRECD,R5                                                       
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,HDRAM                                                   
         MVC   BWHKBYR,HDRBYR                                                   
         MVC   BWHKCAM,HDRCAM                                                   
         MVC   BWHKMKT,LMKT                                                     
         MVC   BWHKSEQ,HDRSEQ                                                   
*                                                                               
         GOTO1 AIO,DIRRD+IO3       READ CAMPAIGN/MARKET HEADER                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,FILGETU3                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIOAREA3                                                      
         L     R4,AIOAREA2                                                      
         USING BWDRECD,R4                                                       
*                                                                               
         LA    R8,BWHFSTEL-BWHKEY(R5)                                           
HDRC10   CLI   0(R8),0                                                          
         BE    HDRCX                                                            
         CLI   0(R8),2                                                          
         BNE   HDRC20                                                           
         CLC   BWHSTA,BWDSTA       IS IT THE SAME STATION                       
         BNE   HDRC20                                                           
         MVC   BWDSTACD,BWHSEQ                                                  
         MVC   BWDKELCD,BWHSEQ                                                  
         B     HDRCX                                                            
*                                                                               
HDRC20   ZIC   R0,1(R8)            BUMP R8 TO THE NEXT ELEMENT                  
         AR    R8,R0                                                            
         B     HDRC10                                                           
*                                                                               
HDRCX    XIT1                                                                   
         DROP  R3,R4,R5                                                         
***********************************************************************         
* ROUTINE TO COPY A DETAIL RECORD                                     *         
* INPUT  : IOAREA2 CONTAINS DETAIL RECORD                             *         
*          R4=A(HEADER RECORD ENTRY)                                  *         
* OUTPUT : NEW RECORD BUILT IN IOAREA3                                *         
*          CC EQ RECORD ADDED                                         *         
*          CC HI RECORD NOT ADDED                                     *         
*          CC LO ERROR                                                *         
***********************************************************************         
         USING HDRTABD,R4                                                       
COPYREC  NTR1  BASE=*,LABEL=*                                                   
         MVC   AMINBLK,LAMN2BLK    SWITCH TO SECOND MINIO                       
         MVC   AMINBUFF,LAMN2BUF                                                
         MVC   AMINRTAB,LAMN2RTB                                                
         L     R8,AIOAREA2                                                      
         LA    R3,APRECKEY         BUILD DETAIL KEY                             
         USING BWDRECD,R3                                                       
         XC    BWDKEY,BWDKEY                                                    
         MVI   BWDKTYP,BWDKTYPQ                                                 
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,HDRAM                                                   
         MVC   BWDKBYR,HDRBYR                                                   
         MVC   BWDKSEQ,HDRSEQ                                                   
         MVI   BWDKELCD,BWDELCDQ                                                
         LA    RE,BWDKELST-BWDKEY(R8)                                           
         TM    HDRIND,HDRINEW      TEST NEW CAMPAIGN/MARKET                     
         BO    COPR2               YES-USE SAME STATION CODES                   
**  WE NEED TO CHECK EXISTING DESTINATION HEADER RECORD TO SEE IF...            
**  THE X'02' ELEMENT IS THERE ALREADY, WE JUST CAN'T AUTOMATICALLY             
**  BUMP TO THE NEXT NUMBER!!!            MHC  06/20/02                         
*        BRAS  RE,HDRCHECK                                                      
*        LA    RE,BWDKELST-BWDKEY(R8)                                           
**                                                                              
         ZIC   RE,BWDKELST-BWDKEY(R8) NO-DETERMINE STATION CODE                 
         BCTR  RE,0                                                             
         LA    RE,HDRSTA(RE)                                                    
         CLI   0(RE),FF                                                         
         BNE   COPR2                                                            
         SR    RF,RF               USE NEW STATION CODE                         
         ICM   RF,1,HDRNXTST                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,HDRNXTST                                                      
         OI    HDRIND,HDRINSTA                                                  
*                                                                               
COPR2    MVC   BWDKELST,0(RE)                                                   
         CLI   BWDKELPO-BWDKEY(R8),0    TEST PACKAGE/ORBIT                      
         BE    COPR6                                                            
         CLI   BWDKELSQ-BWDKEY(R8),0    YES-TEST PKG/ORB HEADER RECORD          
         BE    COPR3                                                            
         CLC   LASTPKOR,BWDKELPO-BWDKEY(R8) SLAVE-TEST HDR CAME BEFORE          
         BNE   COPR70                       NO-SLAVE W/O MASTER-IGNORE!         
         MVC   BWDKELPO,LPKORNUM        PKG/ORB NUMBER WAS SAVED                
         B     COPR6                                                            
*                                                                               
COPR3    MVC   LASTPKOR,BWDKELPO-BWDKEY(R8)  SAVE 'FROM' PKG/ORB NUM            
         SR    R9,R9                                                            
         ICM   R9,1,LPKORNUM       TEST FIRST PKG/ORB                           
         BNZ   COPR5               NO-INCREMENT PKG/ORB NUMBER                  
         MVC   IOKEY(13),APRECKEY  YES-READ RECORDS TO GET NEXT                 
         SR    R9,R9                   AVAILABLE PACKAGE/ORBIT NUMBER           
         LA    R1,MINHI3                                                        
         B     COPR4+4                                                          
*                                                                               
COPR4    LA    R1,MINSEQ3                                                       
         IC    R9,IOKEY+BWDKELPO-BWDKEY                                         
         GOTO1 AMIN                                                             
         BNE   *+14                                                             
         CLC   IOKEY(BWDKELPO-BWDKEY),IOKEYSAV                                  
         BE    COPR4                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
COPR5    LA    R9,1(R9)            NEXT PKG/ORB NUMBER                          
         CHI   R9,255                                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R9,BWDKELPO                                                      
         STC   R9,LPKORNUM         SAVE PKG/ORB NUMBER                          
*                                                                               
COPR6    MVC   BWDKELDY,BWDKELDY-BWDKEY(R8)    DAYS                             
         MVC   BWDKELTM,BWDKELTM-BWDKEY(R8)    TIMES                            
         CLI   BWDKELPO,0                      TEST PACKAGE/ORBIT               
         BE    *+14                                                             
         MVC   BWDKELSQ,BWDKELSQ-BWDKEY(R8)    YES-USE SAME SEQ NUMBER          
         B     COPR10                                                           
         SR    R9,R9                           NO-                              
         CLC   BWDKEY(BWDKELSQ-BWDKEY),LASTKEY TEST SAME STA/DAYS/TIMES         
         BNE   *+12                                                             
         IC    R9,LASTKEY+BWDKELSQ-BWDKEY      YES-INCREMENT SEQ NUMBER         
         B     COPR9                                                            
         MVC   IOKEY(13),BWDKEY       NO-FIND NEXT AVAILABLE SEQUENCE           
         LA    R1,MINHI3                 NUMBER FOR THIS STA/DAYS/TIMES         
         B     COPR8+4                                                          
*                                                                               
COPR8    LA    R1,MINSEQ3                                                       
         LA    R3,IOKEY                                                         
         IC    R9,BWDKELSQ                                                      
         GOTO1 AMIN                                                             
         BNE   *+14                                                             
         CLC   BWDKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                 
         BE    COPR8                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
COPR9    LA    R9,1(R9)            NEXT SEQUENCE NUMBER                         
         CH    R9,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    R3,APRECKEY                                                      
         STC   R9,BWDKELSQ                                                      
         MVC   LASTKEY,BWDKEY      SAVE LAST KEY                                
*                                                                               
COPR10   L     R3,AIOAREA3                                                      
         LR    R0,R3               COPY RECORD TO NEW RECORD AREA               
****     LA    R1,L'IOAREA1        USED TO BE 2000                              
         LHI   R1,L'IOAREA1        USED TO BE 4000, NOW 6000                    
         L     RE,AIOAREA2                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   BWDKEY,APRECKEY     SET ELEMENT KEY VALUES                       
         MVC   BWDSTACD,BWDKELST                                                
         MVC   BWDPKOR,BWDKELPO                                                 
         MVC   BWDSEQ,BWDKELSQ                                                  
*                                                                               
         OI    BWDINDS2,BWDICPY    INDICATE CREATED BY CAMPAIGN COPY            
         NI    BWDINDS2,X'FF'-BWDINBR   NEED TO RESET FLAG, NO LONGER..         
***                                ... LINKED WITH NBR SINCE NEW CMP            
         CLI   BWDSUBDP,0          TEST FOR SUB-DAYPART                         
         BE    COPR12                                                           
         TM    HDRIND,HDRISUDP     YES-TEST SUB-DPTS SCHEDULED SEP              
         BZ    *+18                                                             
         MVC   BWDDPT,BWDSUBDP     YES-MAKE SUBDAYPART THE MAIN DPT             
         MVI   BWDSUBDP,0                                                       
         B     COPR14                                                           
         BAS   RE,GETDPT           NO-GET DAYPART DETAILS                       
         BNE   COPR99                                                           
         LA    RE,DPTSUBS          TEST IT'S STILL VALID SUB-DPT                
         LA    RF,L'DPTSUBS                                                     
         CLC   BWDSUBDP,0(RE)                                                   
         BE    COPR14                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,*-14                                                          
         MVI   BWDSUBDP,0          NO-TRASH THE SUB DAYPART                     
         B     COPR14                                                           
*                                                                               
COPR12   TM    HDRIND,HDRIMADP     TEST SUB-DPTS GROUPED UNDER MASTER           
         BZ    COPR14                                                           
         BAS   RE,GETDPT           YES-GET DAYPART DETAILS                      
         BNE   COPR99                                                           
         CLI   DPTTYPE,C'S'        TEST DAYPART IS A SUB-DAYPART                
         BNE   COPR14                                                           
         MVC   BWDSUBDP,BWDDPT     YES-                                         
         MVC   BWDDPT,DPTMAS       MAIN DAYPART BECOMES THE MASTER DPT          
*                                                                               
COPR14   MVI   APELEM,BTRELCDQ     DELETE BUY TRANSFER ELEMENT                  
         GOTO1 ADELELS,BWDRECD                                                  
         MVI   APELEM,DTRELCDQ     DELETE DAILY TRANSFER ELEMENT(S)             
         GOTO1 ADELELS,BWDRECD                                                  
         MVI   APELEM,SPWELCDQ     DELETE SPOTS/WEEK ELEMENT                    
         GOTO1 ADELELS,BWDRECD                                                  
         MVI   APELEM,DMOELCDQ     DELETE DEMO ELEMENT                          
         GOTO1 ADELELS,BWDRECD                                                  
         MVI   APELEM,SHPELCDQ     DELETE SHARE/PUT ELEMENT                     
         GOTO1 ADELELS,BWDRECD                                                  
*                                                                               
         TM    HDRIND,HDRISAMD     TEST CAMPAIGNS HAVE SAME DATES               
         BO    COPR20              YES - EFFECTIVE DATES ARE OK                 
*                                                                               
COPR16   OC    BWDEFDT2,BWDEFDT2   NO - TEST EFFECTIVE DATES ARE VALID          
         BZ    COPR20                   IN NEW CAMPAIGN PERIOD                  
         CLC   BWDEFDT2,HDREND                                                  
         BH    COPR18                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(2,APHALF)                           
         CLC   APHALF,HDRSTRT                                                   
         BH    COPR17                                                           
         MVC   BWDCOST1,BWDCOST2                                                
         MVC   BWDCOST2,BWDCOST3                                                
         MVC   BWDEFDT2,BWDEFDT3                                                
         XC    BWDCOST3,BWDCOST3                                                
         XC    BWDEFDT3,BWDEFDT3                                                
         B     COPR16                                                           
*                                                                               
COPR17   OC    BWDEFDT3,BWDEFDT3                                                
         BZ    COPR20                                                           
         CLC   BWDEFDT3,HDREND                                                  
         BH    COPR19                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(2,APHALF)                           
         CLC   APHALF,HDRSTRT                                                   
         BH    COPR20                                                           
         MVC   BWDCOST1,BWDCOST3                                                
*                                                                               
COPR18   XC    BWDEFDT2,BWDEFDT2                                                
         XC    BWDCOST2,BWDCOST2                                                
COPR19   XC    BWDEFDT3,BWDEFDT3                                                
         XC    BWDCOST3,BWDCOST3                                                
*                                                                               
COPR20   CLI   LFRSLN,0            TEST SINGLE LENGTH FROM CAMPAIGN             
         BE    COPR20A                                                          
         CLI   HDRSLN,0            AND SINGLE LENGTH TO CAMPAIGN                
         BE    COPR21                                                           
         CLC   LFRSLN,HDRSLN       AND THEY'RE DIFFERENT                        
         BNE   COPR20B             YES-CHANGE THE SPOT LENGTH                   
         B     COPR21                                                           
*                                                                               
COPR20A  CLI   HDRSLN,0            TEST SINGLE LENGTH TO CAMPAIGN               
         BE    COPR21                                                           
         CLI   LSLN,0              AND SPOT LENGTH FILTER                       
         BE    COPR21                                                           
         CLC   LSLN,HDRSLN         AND THEY'RE DIFFERENT                        
         BE    COPR21                                                           
*                                                                               
COPR20B  MVC   BWDSLN,HDRSLN       SET SPOT LENGTH & EQUIV THE COSTS            
         GOTO1 ACOSTEQU,APPARM,(LSLN,BWDCOST1),(HDRSLN,BWDCOST1)                
         OC    BWDCOST2,BWDCOST2                                                
         BZ    COPR21                                                           
         GOTO1 (RF),(R1),(LSLN,BWDCOST2),(HDRSLN,BWDCOST2)                      
         OC    BWDCOST3,BWDCOST3                                                
         BZ    COPR21                                                           
         GOTO1 (RF),(R1),(LSLN,BWDCOST3),(HDRSLN,BWDCOST3)                      
*                                                                               
COPR21   TM    LFLAG,LNETCOST      TEST FROM RECORD HAS NET COST                
         BZ    COPR22                                                           
         CLI   CLTBWPRO+14,C'Y'    YES-TEST TO RECORD SHOULD HAVE NET           
         BE    COPR23                                                           
         GOTO1 GROSSUP,BWDCOST1    NO-GROSS UP THE COSTS                        
         GOTO1 GROSSUP,BWDCOST2                                                 
         GOTO1 GROSSUP,BWDCOST3                                                 
         B     COPR23                                                           
*                                                                               
COPR22   CLI   CLTBWPRO+14,C'Y'    TEST TO RECORD SHOULD HAVE NET               
         BNE   COPR23                                                           
         GOTO1 ANETCOST,BWDCOST1   YES-NET DOWN THE COSTS                       
         GOTO1 (RF),BWDCOST2                                                    
         GOTO1 (RF),BWDCOST3                                                    
*                                                                               
COPR23   NI    BWDINDS,FF-BWDITRLK RE-INITIALIZE CERTAIN FIELDS                 
******   XC    BWDWKS,BWDWKS                                                    
         XC    BWDTRED2,BWDTRED2                                                
         XC    BWDTREC2,BWDTREC2                                                
         XC    BWDTRED3,BWDTRED3                                                
         XC    BWDTREC3,BWDTREC3                                                
*                                                                               
         TM    LFLAG2,LNOCST       ARE WE COPYING COSTS OVER?                   
         BNO   COPR23A              - YUP WE ARE                                
         XC    BWDCOST1,BWDCOST1    - NOPE, GET RID OF ALL COSTS                
         XC    BWDEFDT2,BWDEFDT2      TAKE OUT EFFECTIVE DATES AS WELL          
         XC    BWDCOST2,BWDCOST2      (NEW COPY COST OPTION)                    
         XC    BWDEFDT3,BWDEFDT3                                                
         XC    BWDCOST3,BWDCOST3              MHC  06/24/03                     
*                                                                               
COPR23A  NI    LFLAG,FF-LUPGRAD1-LUPGRAD2                                       
         OC    HDRU2DAT,HDRU2DAT   TEST TO CAMPAIGN HAS 2ND UPGRADE             
         BNZ   COPR24                                                           
         TM    BWDINDS,BWDIUP2     NO-REJECT RECORD IF IT BELONGS TO            
         BO    COPR70                 2ND UPGRADE                               
         TM    BWDINDS,BWDIUP1     TEST RECORD BELONGS TO 1ST UPGRADE           
         BZ    COPR24                                                           
         NI    BWDINDS,255-BWDIUP1 YES-REMOVE EFFECTIVE DATES                   
         XC    BWDDATES,BWDDATES                                                
         XC    BWDWKS,BWDWKS                                                    
*                                                                               
         CLI   BWDELLN,BWDELLNQ    CAN WE DO THIS FOR 53 WEEKS?                 
         BNH   COPR28              NO, WE MIGHT OVERWRITE THE NEXT ELEM         
         XC    BWDWKS2,BWDWKS2                                                  
         B     COPR28                                                           
*                                                                               
COPR24   OC    BWDDATES,BWDDATES   ANY BWDDATES?                                
         BZ    COPR24B              - NOPE, NO POSSIBLE OVERRIDES               
         CLC   BWDDATES(2),BWDDATES+2   SINGLE DAY SCHEDULE?                    
         BNE   COPR24B                                                          
         BAS   RE,FIXSNGDS         FIX SINGLE DAY SCHEDULE                      
         B     COPR24C                                                          
*                                                                               
COPR24B  TM    LFLAG2,LIGNDAT      ARE WE IGNORING DATES?                       
         BZ    COPR24C              - NOPE, SKIP!                               
         BRAS  RE,MVIGNDAT         NEED TO MOVE IGNORE DATES                    
*                                                                               
COPR24C  OC    BWDDATES,BWDDATES   TEST DATES                                   
         BZ    COPR25                                                           
         CLC   BWDDATES(2),HDRDATSP     YES-MAKE SURE DATES ARE                 
         BNL   *+10                         WITHIN CAMPAIGN DATES               
         MVC   BWDDATES(2),HDRDATSP                                             
         CLC   BWDDATES+2(2),HDRENDP                                            
         BNH   *+10                                                             
         MVC   BWDDATES+2(2),HDRENDP                                            
*                                                                               
         TM    BWDINDS,BWDIUP1+BWDIUP2  TEST DATES ARE FOR 1ST/2ND UPGR         
         BNZ   COPR25                                                           
         CLC   BWDDATES(2),BWDDATES+2   NO-TEST ANY OVERLAP                     
         BH    COPR70                   NO-REJECT THIS RECORD                   
*                                                                               
COPR25   OC    HDRU2DAT,HDRU2DAT   TEST TO CAMPAIGN HAS 2ND UPGRADE             
         BZ    COPR27              NO                                           
         TM    BWDINDS,BWDIUP1+BWDIUP2  YES-TEST RECORD BELONGS TO              
         BNZ   COPR26                       1ST OR 2ND UPGRADE                  
         OC    LFRU2DAT,LFRU2DAT   NO-TEST FROM CAMPAIGN HAS 2ND UPGR           
         BNZ   COPR27                                                           
         CLI   BWDKELPO,0          NO-CREATE 1ST OF TWO RECORDS                 
         BNE   COPR27              ***EXCEPT FOR ORBITS & PACKAGES***           
         OI    BWDINDS,BWDIUP1                                                  
         OI    LFLAG,LUPGRAD1                                                   
*                                                                               
COPR26   BRAS  RE,DATES            SET EFFECTIVE DATES FOR 1ST/2ND              
         BE    COPR27                                      UPGRADE              
         TM    LFLAG,LUPGRAD1      NO SUCCESS-TEST 1ST OF NEW PAIR              
         BO    COPR60              YES-CREATE RECORD FOR 2ND UPGRADE            
         TM    LFLAG,LUPGRAD2      TEST 2ND OF NEW PAIR                         
         BO    COPR70              YES-DONE                                     
         DC    H'0'                ELSE, BONKERS!                               
*&&DO                                                                           
COPR27   OC    BWDWKS,BWDWKS       TEST INACTIVE WEEKS MASK SET                 
         BNZ   *+14                                                             
         OC    BWDWKS2,BWDWKS2                                                  
         BZ    *+8                                                              
*&&                                                                             
COPR27   TM    HDRIND,HDRISAMD     SAME DATES AS FROM CAMPAIGN?                 
         BO    *+8                  - YUP, NO NEED TO GO TO SUBROUTINE          
         BRAS  RE,WEEKS            YES-SET NEW INACTIVE WEEKS MASK              
*                                                                               
         TM    CLTIND2,CLTIXOUT    TEST TO X-OUT SCHEDULED WEEKS                
         BZ    COPR28                                                           
         BRAS  RE,XOUT             YES                                          
*                                                                               
COPR28   TM    HDRIND,HDRINOSK     TEST FOR SCHEDULE TRANSFER                   
         BO    COPR33                                                           
****  WE NEED TO CHECK LAFRSPEL IF USING IGNORE DATES!                          
         TM    LFLAG2,LIGNDAT      WE IGNORING DATES?                           
         BNO   COPR28C              - NOPE!  CONTINUE NORMALLY                  
         BRAS  RE,IGNSPTFX                                                      
****                                                                            
COPR28C  ICM   R2,15,LAFRSPEL      YES - TEST FOR SPOTS/WEEK ELEMENT            
         BZ    COPR33                                                           
         LA    R9,APELEM           YES - BUILD NEW SPOTS/WEEK ELEMENT           
         XC    APELEM,APELEM                                                    
         USING SPWEL,R9                                                         
         MVI   SPWELCD,SPWELCDQ                                                 
         LA    R6,HDRSKDSP                                                      
****  USE SVHDRSKD IF IT'S SINGLE DAY SCHEDULE AND WE'RE IGNORING DATES         
****                                                                            
         OC    BWDDATES,BWDDATES   ANY BWDDATES?                                
         BZ    COPR28E              - NOPE, NO POSSIBLE OVERRIDE                
         CLC   BWDDATES(2),BWDDATES+2 SINGLE DAY SCHEDULE?                      
         BNE   COPR28E              - NOPE, USE HDSKDSP                         
         LA    R6,SVHDRSKD          - YEAH, USE SPECIAL SKED INSTEAD            
****                                                                            
COPR28E  ZIC   RE,1(R2)                                                         
         AR    RE,R2                                                            
         LA    R2,SPWPERWK-SPWEL(R2)                                            
*                                                                               
COPR29   CR    R2,RE                                                            
         BNL   COPR32                                                           
         CLI   0(R2),0                                                          
         BE    COPR31                                                           
         CLI   0(R6),FF                                                         
         BE    COPR31                                                           
         ZIC   RF,0(R6)                                                         
         SR    R1,R1                                                            
         ICM   R1,12,BWDWKS        TEST INACTIVE WEEKS MASK SET                 
         BZ    COPR30                                                           
         LA    RF,1(RF)            YES-TEST THE WEEK IS INACTIVE                
         SR    R0,R0                                                            
         SLDL  R0,1                                                             
         BCT   RF,*-6                                                           
         LTR   R0,R0                                                            
         BNZ   COPR31              WEEK IS INACTIVE                             
         IC    RF,0(R6)                                                         
*                                                                               
COPR30   LA    RF,SPWPERWK(RF)                                                  
         MVC   0(1,RF),0(R2)                                                    
*                                                                               
COPR31   LA    R2,1(R2)                                                         
         LA    R6,1(R6)                                                         
         B     COPR29                                                           
*                                                                               
COPR32   ZIC   RE,HDRNWKS                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    SPWPERWK(0),SPWPERWK TEST ANY SPOTS                              
         BZ    COPR33                                                           
         LA    RE,SPWPERWK-SPWEL+1(RE)  YES - ADD SPOTS/WEEK ELEMENT            
         STC   RE,SPWELLN                                                       
         GOTO1 AADDELS,BWDRECD                                                  
         TM    HDRIND2,HDRINOSP    TEST NO CAMPAIGN SPOTS ORIGINALLY            
         BZ    COPR33                                                           
         OI    HDRIND2,HDRISKD     YES-INDICATE THERE ARE SPOTS NOW             
*                                                                               
COPR33   TM    LFLAG,LUPGRAD2      TEST 2ND RECORD OF 2 UPGRADE PAIR            
         BO    COPR38              YES-UPGRADE VALUES ALREADY SET               
         XC    LUF,LUF             CLEAR UPGRADE VALUES                         
         XC    LUP,LUP                                                          
         XC    LFB,LFB                                                          
         MVI   LFBTP,0                                                          
         XC    LFBLST,LFBLST                                                    
         XC    LUPPUT,LUPPUT                                                    
         XC    LUPSHR,LUPSHR                                                    
         XC    LOVDAY,LOVDAY                                                    
         XC    LOVTIME,LOVTIME                                                  
         TM    BWDINDS,BWDIUP2     TEST 2ND UPGRADE                             
         BZ    COPR34                                                           
         MVI   APELEM,UPGELCDQ     YES-DELETE UPGRADE ELEMENT                   
         GOTO1 ADELELS,BWDRECD                                                  
         BAS   RE,SETUP2           AND SET UP FOR 2ND TO UGRADE                 
         B     COPR36                                                           
*                                                                               
COPR34   TM    HDRIND,HDRIUPGR     TEST NEW CAMPAIGN UPGRADE                    
         BZ    COPR35                                                           
         MVC   LUF,HDRUF           YES - USE THOSE VALUES                       
         MVC   LUP,HDRUP                                                        
         MVC   LFB,HDRFB                                                        
         TM    HDRFB+1,BTY2CHAR    2 CHARACTER BOOKTYPE?                        
         BNO   COPR34G              - NOPE                                      
         CLI   HDRFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LFBTP,HDRFBTP       HDRFB BOOKTYPE                               
COPR34G  MVC   LFBLST,HDRFBLST                                                  
         MVC   LUPPUT,HDRUPUT                                                   
         MVC   LUPSHR,HDRUSHR                                                   
         MVI   APELEM,UPGELCDQ           DELETE UPGRADE ELEMENT                 
         GOTO1 ADELELS,BWDRECD                                                  
         B     COPR36                                                           
*                                                                               
COPR35   ICM   R2,15,LAFRUPEL      NO - TEST DETAIL UPGRADE                     
         BZ    COPR37                                                           
         USING UPGEL,R2                                                         
         MVC   LUF,UPGFILE         YES - USE DETAIL UPGRADE VALUES              
         MVC   LUP,UPGRADE                                                      
         MVC   LFB,UPGFRBK                                                      
         TM    UPGFRBK+1,BTY2CHAR   2 CHARACTER BOOKTYPE?                       
         BNO   COPR35G                                                          
         CLI   UPGELLN,UPGELLNQ    EXTENDED LENGTH?                             
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   UPGFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
COPR35G  MVC   LFBTP,UPGFRBKT                                                   
         CLI   UPGELLN,51                                                       
         BL    *+10                                                             
         MVC   LFBLST,UPGFRBKL                                                  
         MVC   LUPPUT,BWDUPUT-BWDRECD(R8)                                       
         MVC   LUPSHR,BWDUSHR-BWDRECD(R8)                                       
*                                                                               
COPR36   ICM   R2,15,LAFROVEL                                                   
         BZ    COPR38                                                           
         USING ODTEL,R2                                                         
         MVC   LOVDAY,ODTDAY                                                    
         MVC   LOVTIME,ODTTIME                                                  
         B     COPR38                                                           
*                                                                               
COPR37   MVC   LUF,LFRUF           ELSE USE OLD CAMPAIGN UPGRADE VALUES         
         MVC   LUP,LFRUP                                                        
         MVC   LFB,LFRFB                                                        
         TM    LFRFB+1,BTY2CHAR    2 CHARACTER BOOKTYPE?                        
         BNO   COPR37G                                                          
         CLI   LFRFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LFBTP,LFRFBTP                                                    
COPR37G  MVC   LFBLST,LFRFBLST                                                  
         MVC   LUPPUT,LFRPUT                                                    
         MVC   LUPSHR,LFRSHR                                                    
         OC    HDRUP,HDRUP         TEST NEW CAMPAIGN HAS AN UPGRADE             
         BNZ   COPR38                                                           
         OI    HDRIND2,HDRICOPU    NO-FLAG TO WRITE UPGRADE TO CAMP REC         
*                                                                               
COPR38   OC    LFB,LFB             TEST ZERO SHARE BOOK                         
         BNZ   COPR38K                                                          
         MVC   LFB,HDRBOOK         YES-USE ESTIMATE BOOK                        
         TM    HDRBOOK+1,BTY2CHAR                                               
         BNO   COPR38G                                                          
         CLI   HDRBKTYP,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LFBTP,HDRBKTYP                                                   
COPR38G  XC    LFBLST,LFBLST                                                    
COPR38K  XC    APELEM,APELEM       BUILD DEMO ELEMENT                           
         XC    LDEMS,LDEMS                                                      
         LA    R2,HDRDEMS                                                       
         LA    R5,LOLDDEMS                                                      
         XC    LOLDDEMS,LOLDDEMS                                                
         LA    R6,LDEMS                                                         
         LA    RE,LADJDEMS                                                      
         LA    RF,LADJNEW                                                       
         LA    R9,APELEM                                                        
         USING DMOEL,R9                                                         
         MVI   DMOELCD,DMOELCDQ                                                 
         LA    R9,DMODEMO-DMOEL(R9)                                             
*                                                                               
COPR40   OC    0(3,R2),0(R2)       TEST END OF DEMOS                            
         BZ    COPR44                                                           
         MVC   0(3,R9),0(R2)       MOVE DEMO CODE TO ELEMENT                    
         TM    LFLAG,LCPYDEM       TEST COPY DEMOS                              
         BZ    COPR42              NO - UPGRADE NEEDED                          
         ICM   R1,15,LAFRDMEL      FIND DEMO IN OLD DEMO ELEMENT                
         BZ    COPR42                                                           
         SR    R0,R0                                                            
         DROP  R9                                                               
         USING DMOEL,R1                                                         
         STM   RE,RF,APDUB                                                      
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,DMOELLN                                                       
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO                                                       
         DROP  R1                                                               
*                                                                               
         CLC   1(2,R1),1(R2)                                                    
         BE    *+16                                                             
         BXLE  R1,RE,*-10                                                       
         LM    RE,RF,APDUB                                                      
         B     COPR42                                                           
         LM    RE,RF,APDUB                                                      
         MVC   4(4,R9),4(R1)       MOVE IN OLD DEMO VALUE                       
         MVC   0(3,R5),0(R2)       SAVE DEMO CATEGORY                           
         LA    R5,3(R5)                                                         
         CLI   HDRDEMAD,0          TEST AUTO DEMO ADJUSMENTS                    
         BE    COPR43                                                           
         TM    4(R1),X'80'         YES-TEST THIS DEMO IS OVERRIDDEN             
         BZ    COPR43                                                           
         CLI   HDRDEMAD,C'T'       YES-TEST TARGET DEMO ADJUSTS                 
         BNE   *+18                                                             
         LA    R0,HDRDEMS          YES-TEST THIS IS THE TARGET                  
         CR    R0,R2                                                            
         BE    COPR41                                                           
         B     COPR43                                                           
         CLI   1(R2),C'R'          TEST THIS IS A RATING                        
         BNE   COPR43                                                           
         CLI   HDRDEMAD,C'I'       YES-TEST IMPS ADJUSTS                        
         BE    COPR41                                                           
         CLI   HDRDEMAD,C'A'       TEST RHOMES ADJUSTS                          
         BNE   COPR43                                                           
         CLI   2(R2),1             YES-TEST THIS IS RHOMES                      
         BNE   COPR43                                                           
*                                  FOR AUTO ADJUSTS ON NEW DEMOS-               
COPR41   MVC   0(3,RE),0(R2)       SAVE DEMO CATEGORY                           
         LA    RE,3(RE)                                                         
         MVC   0(4,RF),4(R9)       AND OVERRIDE DEMO VALUE                      
         NI    0(RF),X'7F'                                                      
         LA    RF,4(RF)                                                         
         B     COPR43                                                           
*                                                                               
COPR42   MVC   0(3,R6),0(R2)       UPGRADE NEEDED - ADD DEMO CODE TO            
         LA    R6,3(R6)                             DEMO LIST                   
*                                                                               
COPR43   LA    R2,3(R2)            NEXT ESTIMATE DEMO                           
         LA    R9,L'DMODEMO(R9)                                                 
         B     COPR40                                                           
*                                                                               
COPR44   MVI   0(R6),FF                                                         
         MVI   0(RE),FF                                                         
         LA    RE,APELEM           SET DEMO ELEMENT LENGTH                      
         SR    R9,RE                                                            
         STC   R9,APELEM+1                                                      
         LA    R9,APELEM                                                        
         CLI   LDEMS,FF            TEST ANY UPGRADE NEEDED                      
         BE    COPR50                                                           
         OC    LUP,LUP             YES-THERE MUST BE AN UPGRADE                 
         BNZ   COPR44A                                                          
         CLI   QMED,C'R'                                                        
         BE    COPR47                                                           
         B     COPR90              FOR RADIO, UPGRADE NOT NEEDED                
*                                                                               
COPR44A  LA    R6,LDMUPBLK                                                      
         USING SPDEMUPD,R6                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         L     R1,AIOAREA4                                                      
         ST    R1,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,HDRCLT                                                   
         MVC   SPUPMKT,LMKTLKUP                                                 
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   BWDSTA,C'0'         IS IT A NUMBER?                              
         BL    COPR44E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),BWDSTA+5   MOVE THE NETWORK IN                        
         MVC   SPUPSYSE,BWDSTA                                                  
         B     COPR44G                                                          
*****  CABLE/FUSION DATE LOOKUP                                                 
COPR44E  MVC   SPUPSTA,BWDSTA                                                   
COPR44G  MVC   SPUPDAY,BWDDAYS                                                  
         MVC   SPUPTIM,BWDTIMES                                                 
         MVC   SPUPFIL,LUF                                                      
         MVC   SPUPSRC,HDRSRC                                                   
         MVC   SPUPFBK,LFB                                                      
         MVC   SPUPFBKL,LFBLST                                                  
         MVC   SPUPUDAY,LOVDAY                                                  
         MVC   SPUPUTIM,LOVTIME                                                 
         MVC   SPUPTYPE(L'LUP),LUP                                              
*****                                                                           
         MVC   SPUPBTYP,STABKTYP                                                
         CLI   LFBTP,0             EST FOR THIS CAM HAD BOOK TYPE?              
         BE    *+14                                                             
         MVC   SPUPBTYP,LFBTP      YES, OVERRIDES ANY STATION BK TYPE           
         B     COPR44X                                                          
*                                                                               
         L     R1,AEXTRAWK         LOOK FOR STATION BOOKTYPE                    
         USING EXTRAWKD,R1                                                      
         LA    R1,STASNBKS                                                      
         DROP  R1                                                               
COPR44T  OC    0(L'LSTA,R1),0(R1)                                               
         BZ    COPR44X             NONE                                         
         CLC   0(L'LSTA,R1),BWDSTA                                              
         BE    *+12                                                             
         LA    R1,9(R1)                                                         
         B     COPR44T                                                          
         MVC   SPUPBTYP,L'LSTA(R1)   STATION HAS AN OVERRIDE BOOKTYPE           
*****                                                                           
COPR44X  CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
         TM    SPUPFBK+1,BTYBITSQ    SPECIAL BOOK?                              
         BZ    COPR44XX                                                         
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   COPR44XR             - NOPE                                      
         MVC   SPUPBTYP,CMPFBTP     - YUP                                       
         B     COPR44XT                                                         
*                                                                               
COPR44XR GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
COPR44XT NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
COPR44XX CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         CLI   LUPPUT,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   LUPPUT,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   LUPSHR,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   LUPSHR,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
         XC    LDEMVALS(NDEMOS*4),LDEMVALS                                      
*                                                                               
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   COPR44Z                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
COPR44Z  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS                          
         TM    BWDINDS,BWDIPRG                                                  
         BO    *+16                                                             
         MVC   BWDPROG,SPACES                                                   
         MVC   BWDPROG(L'SPUPPRG),SPUPPRG                                       
         MVC   BWDBOOK,SPUPFBK     ACTUAL SHARE BOOK                            
*                                                                               
         CLI   LADJDEMS,FF         TEST NEED RAW DEMOS FOR ADJUSTS              
         BE    COPR47                                                           
         GOTO1 (RF),(R1),LDMUPBLK,LADJDEMS,LADJVALS  YES-GET THEM               
         LA    R2,LADJDEMS                                                      
         LA    R5,LADJNEW                                                       
         LA    R6,LADJVALS         FIGURE OUT ADJUSTMENT VALUES                 
*                                                                               
COPR45   CLI   0(R2),FF                                                         
         BE    COPR47                                                           
***  2 DECIMAL                                                                  
         XC    APWORK+64(16),APWORK+64                                          
         MVC   APWORK+64(4),0(R6)                                               
         MVC   APWORK+68(4),0(R5)                                               
*                                                                               
         NI    APWORK+64,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         NI    APWORK+68,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         OC    APWORK+64(4),APWORK+64   TEST ORIGINAL DEMO VALUE = 0            
         BZ    COPR46               - YUP, DON'T ADJUST                         
***                                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BZ    COPR45K                                                          
*                                                                               
         TM    0(R6),X'40'         2 DECIMAL?                                   
         BNZ   COPR45E              - YUP, NOTHING TO DO                        
         L     R1,APWORK+64                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+64                                                     
COPR45E  TM    0(R5),X'40'         2 DECIMALS?                                  
         BNZ   COPR45K              - YUP, NOTHING TO DO                        
         L     R1,APWORK+68                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+68                                                     
***  2 DECIMAL                                                                  
COPR45K  L     RF,=F'1000'                                                      
         ICM   R1,15,APWORK+64     TEST ORIGINAL DEMO VALUE = 0                 
         BZ    COPR46              YES-DON'T ADJUST                             
         L     RF,APWORK+68                                                     
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
COPR46   ST    RF,0(R6)            FIGURED OUT ADJVAL                           
         LA    R2,3(R2)                                                         
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         B     COPR45                                                           
*                                                                               
COPR47   LA    R2,LDEMVALS         MOVE NEW DEMO VALUES TO DEMO ELEM            
         LA    R6,LDEMS                                                         
         LA    R8,LADJVALS                                                      
         USING DMOEL,R9                                                         
         LA    R1,DMODEMO                                                       
         ZIC   RF,DMOELLN                                                       
         AR    RF,R9                                                            
         BCTR  RF,0                                                             
         LA    RE,L'DMODEMO                                                     
*                                                                               
COPR48   MVI   APBYTE,0            USED FOR 2D/OVERRIDE BITS                    
         CLI   0(R6),FF                                                         
         BE    COPR50                                                           
         CLC   0(3,R1),0(R6)       FIND DEMO IN DEMO ELEMENT                    
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         CLI   LADJDEMS,FF         TEST ANY ADJUSTS NEEDED                      
         BE    COPR49K                                                          
         CLI   HDRDEMAD,C'I'       YES-TEST AUTOADJ=IMP                         
         BNE   COPR49A             NO-THEN ALL GET ADJUSTED                     
         CLI   1(R6),C'I'          YES-TEST THIS IS AN IMPRESSION               
         BNE   COPR49K                                                          
         LA    R5,LADJDEMS         YES-ADJUST IMPS IF RATING WAS                
         LA    R8,LADJVALS             ORIGINALLY OVERRIDDEN                    
*                                                                               
COPR49   CLI   0(R5),FF                                                         
         BE    COPR49K                                                          
         CLC   2(1,R6),2(R5)                                                    
         BE    COPR49A                                                          
         LA    R5,3(R5)                                                         
         LA    R8,4(R8)                                                         
         B     COPR49                                                           
*                                                                               
COPR49A  STM   RE,RF,APDUB                                                      
***  2 DECIMAL                                                                  
         MVC   APBYTE,0(R2)        SAVING OFF 2D/OVERRIDE BITS                  
         NI    APBYTE,X'FF'-X'40'   TURN OFF 2D BIT!!                           
         L     RF,0(R2)            ADJUST                                       
         ICM   RF,8,APBYTE         MAKE SURE RF DOESN'T HAVE 2D BIT             
         TM    0(R2),X'40'         2 DECIMAL BIT ON?                            
         BZ    COPR49B              - NOPE                                      
         MVC   APBYTE,0(R2)         - YUP, RETAIN THE 2D BIT THIS TIME          
***  2 DECIMAL                                                                  
***  WE'RE NOT WORRIED ABOUT OVERRIDING BIT APPARENTLY...                       
COPR49B  SLA   RF,1                                                             
         M     RE,0(R8)                                                         
         D     RE,=F'1000'                                                      
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,0(R2)                                                         
***  2 DECIMAL                                                                  
         TM    APBYTE,X'40'        WE NEED 2D FLAG?                             
         BZ    COPR49E              - NOPE                                      
         OI    0(R2),X'40'          - YUP, TURN ON THE FLAG!                    
***  2 DECIMAL                                                                  
COPR49E  LM    RE,RF,APDUB                                                      
*                                                                               
COPR49K  MVC   4(4,R1),0(R2)       MOVE IN DEMO VALUE                           
         LA    R2,4(R2)            NEXT DEMO                                    
         LA    R6,3(R6)                                                         
         B     COPR48                                                           
*                                                                               
COPR50   GOTO1 AADDELS,BWDRECD     ADD DEMO ELEMENT                             
*                                                                               
         ICM   R2,15,LAFRSHEL      TEST SHARE/PUT OVERRIDE ELEMENT              
         BZ    COPR58                                                           
         OC    LOLDDEMS,LOLDDEMS   YES-TEST ANY DEMOS COPIED                    
         BZ    COPR58                                                           
         XC    APELEM,APELEM       YES-ALSO COPY ANY SHARE/PUT                  
         LA    R9,APELEM               OVERRIDES FOR THOSE DEMOS                
         USING SHPEL,R9                                                         
         MVI   SHPELCD,SHPELCDQ                                                 
         LA    R9,SHPDEMO                                                       
         LA    R6,LOLDDEMS                                                      
         LA    R0,NDEMOS                                                        
*                                                                               
COPR52   OC    0(3,R6),0(R6)                                                    
         BZ    COPR56                                                           
         ZIC   RF,1(R2)                                                         
         AR    RF,R2                                                            
         BCTR  RF,0                                                             
         LA    RE,L'SHPDEMO                                                     
         LA    R1,SHPDEMO-SHPEL(R2)                                             
         CLC   1(2,R1),1(R6)                                                    
         BE    *+12                                                             
COPR53   BXLE  R1,RE,*-10                                                       
         B     COPR54                                                           
         TM    4(R1),SHPDEMOV                                                   
         BZ    COPR53                                                           
         MVC   0(L'SHPDEMO,R9),0(R1)                                            
         LA    R9,L'SHPDEMO(R9)                                                 
         B     COPR53                                                           
*                                                                               
COPR54   LA    R6,3(R6)            NEXT DEMO                                    
         BCT   R0,COPR52                                                        
*                                                                               
COPR56   LA    RE,APELEM                                                        
         SR    R9,RE                                                            
         STC   R9,1(RE)                                                         
         CH    R9,=Y(SHPDEMO-SHPEL)                                             
         BNH   COPR58                                                           
         GOTO1 AADDELS,BWDRECD                                                  
*                                                                               
COPR58   GOTO1 AMIN,MINADD3        ADD THE RECORD                               
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    HDRIND,HDRINEWD     TEST NEW SET OF MINIO DETAIL RECORDS         
         BZ    COPR60                                                           
         TM    LFLAG,LFSTREC       YES-TEST FIRST RECORD                        
         BZ    COPR60                                                           
         GOTO1 AMIN,MINCLS         YES-CLOSE MINIO (TO AVOID ERROR ON           
         NI    LFLAG,FF-LFSTREC                     FIRST RECORD SPLIT)         
*                                                                               
COPR60   TM    LFLAG,LUPGRAD1      TEST CREATING 1ST OF 2 RECORDS               
         BZ    COPR70                                                           
         NI    LFLAG,FF-LUPGRAD1   YES-NOW CREATE 2ND RECORD                    
         OI    LFLAG,LUPGRAD2          FOR 2ND CAMPAIGN UPGRADE                 
         NI    BWDINDS,FF-BWDIUP1                                               
         OI    BWDINDS,BWDIUP2                                                  
         MVI   APELEM,DMOELCDQ     DELETE DEMO ELEMENT                          
         GOTO1 ADELELS,BWDRECD                                                  
         BAS   RE,SETUP2           SET UP FOR 2ND UPGRADE                       
         ZIC   RE,BWDKELSQ         INCREMENT THE SEQUENCE NUMBER                
         LA    RE,1(RE)                                                         
         CH    RE,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   RE,BWDKELSQ                                                      
         STC   RE,BWDSEQ                                                        
         MVC   LASTKEY,BWDKEY                                                   
         B     COPR26              GO BACK AND ADD 2ND RECORD                   
*                                                                               
COPR70   MVC   AMINBLK,LAMINBLK    SWITCH BACK TO FIRST MINIO                   
         MVC   AMINBUFF,LAMINBUF                                                
         MVC   AMINRTAB,LAMINRTB                                                
         B     COPRX                                                            
*                                                                               
COPR90   MVC   FVMSGNO,=AL2(FVNOCNUP)   NO UPGRADE EXPRESSION                   
*                                                                               
COPR99   MVC   FVADDR,LACAMP       SET A(TO CAMPAIGN HEADER)                    
*                                                                               
COPRX    CLC   FVMSGNO,=AL2(FVFOK)                                              
COPRXIT  XIT1                                                                   
***********************************************************************         
* SET UP FOR SECOND UPGRADE                                           *         
***********************************************************************         
         SPACE 1                                                                
SETUP2   LR    R0,RE                                                            
         MVC   LUF,HDRUF2                                                       
         MVC   LUP,HDRUP2                                                       
         MVC   LFB,HDRFB2                                                       
         TM    HDRFB2+1,BTY2CHAR   2 CHARACTER BOOKTYPES                        
         BNO   SETUP2G              - NOPE                                      
         CLI   HDRFB2TP,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LFBTP,HDRFB2TP      HDRFB2 BOOKTYPE                              
SETUP2G  MVC   LFBLST,HDRFB2L                                                   
         MVC   LUPPUT,HDRUPUT2                                                  
         MVC   LUPSHR,HDRUSHR2                                                  
         XC    APELEM,APELEM       ADD UPGRADE ELEMENT                          
         LA    R9,APELEM                                                        
         USING UPGEL,R9                                                         
         MVI   UPGELCD,UPGELCDQ                                                 
         MVI   UPGELLN,UPGELLNQ                                                 
         MVC   UPGFILE,HDRUF2                                                   
         MVC   UPGRADE,HDRUP2                                                   
         MVC   UPGFRBK,HDRFB2                                                   
         TM    HDRFB2+1,BTY2CHAR   2 CHARACTER BOOKTYPES                        
         BNO   SETUP4                                                           
         CLI   HDRFB2TP,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   UPGFRBKT,HDRFB2TP                                                
         MVI   UPGELLN,UPGELLQ2    EXTENDED LENGTH                              
SETUP4   MVC   UPGFRBKL,HDRFB2L                                                 
         MVC   UPGINPUT,HDRIN2                                                  
         GOTO1 AADDELS,BWDRECD                                                  
         MVC   BWDUPUT,HDRUPUT2                                                 
         MVC   BWDUSHR,HDRUSHR2                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIX SCHEDULING FOR SINGLE DAY BWDDATES FOR IGNORE DATES  *         
* OUTPUT : CC EQ - OK                                                 *         
*          CC NE - NO DATES, THEREFORE REJECT THE RECORD              *         
***********************************************************************         
***  THIS CAME DIRECTLY FROM VALK28 - VALK33  ABOVE  ********                   
         SPACE 1                                                                
FIXSNGDS NTR1                                                                   
         L     R5,ATWA             MAKE SURE R5 POINTS TO THE TWA               
         CLC   LFRENDMN,CMPSTMNP   NO - TEST FROM/TO DATES OVERLAP              
         BL    FIXSNGNO                                                         
         CLC   CMPNDMNP,LFRDATES                                                
         BL    FIXSNGNO                                                         
         CLC   CMPST,LFREND        FINAL TEST-TO CAMPAIGN START AFTER           
         BNH   FIXSNG10                       FROM CAMPAIGN END                 
         B     FIXSNGNO                                                         
*                                                                               
FIXSNG10 LA    RF,53               FIND WHICH WEEKS OVERLAP                     
         SR    R1,R1                                                            
         LA    R3,SVHDRSKD                                                      
         MVI   0(R3),FF                                                         
         MVC   1(L'SVHDRSKD-1,R3),0(R3)                                         
         LA    R6,LFRDATES                                                      
         L     R9,ATWA                                                          
         AHI   R9,CMPDATSP-TWAD                                                 
         TM    CMPOPTS,CAMODLY     AVOID SCHEDULE COPY FOR MIXTURE              
         BO    FIXSNG20            OF DAILY AND WEEKLY SCHEDULES                
         MVC   0(2,R9),CMPSTMNP                                                 
         TM    LFRCAMOP,CAMODLY                                                 
         BZ    FIXSNG30                                                         
         B     FIXSNGYS                                                         
*                                                                               
FIXSNG20 TM    LFRCAMOP,CAMODLY                                                 
         BZ    FIXSNGYS                                                         
*                                                                               
FIXSNG30 OC    0(4,R6),0(R6)                                                    
         BZ    FIXSNGYS                                                         
         OC    0(4,R9),0(R9)                                                    
         BZ    FIXSNGYS                                                         
         CLC   0(2,R6),0(R9)                                                    
         BNE   FIXSNG50                                                         
         STC   R1,0(R3)                                                         
*                                                                               
FIXSNG50 BH    *+12                                                             
         LA    R3,1(R3)                                                         
         LA    R6,4(R6)                                                         
         BL    *+12                                                             
         LA    R1,1(R1)                                                         
         LA    R9,4(R9)                                                         
         LTR   RF,RF               DID WE RUN OUT ALREADY?                      
         BZ    *+8                                                              
         BCT   RF,FIXSNG30                                                      
         CHI   R1,53               COMBINED OF 53 WEEKS                         
         BL    FIXSNG30            NOT YET, KEEP GOING TILL IT IS               
*                                                                               
FIXSNGYS SR    RC,RC               CC EQ - OK                                   
FIXSNGNO LTR   RC,RC               CC NE - REJECT THIS RECORD                   
FIXSNGX  B     COPRXIT                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET EFFECTIVE DATES FOR 1ST OR 2ND CAMPAIGN UPGRADE                
* OUTPUT : CC EQ - OK                                                           
*          CC NE - NO DATES, THEREFORE REJECT THE RECORD                        
***********************************************************************         
DATES    NTR1  BASE=*,LABEL=*                                                   
         TM    BWDINDS,BWDIUP1     TEST 1ST UPGRADE                             
         BZ    DATE4                                                            
         TM    LFLAG,LUPGRAD1      TEST 1ST OF NEW PAIR                         
         BZ    DATE1                                                            
         MVC   LSVDATES,BWDDATES   YES-SAVE ORIGINAL DATES                      
         OC    BWDDATES,BWDDATES   TEST ANY DATES ALREADY                       
         BZ    DATE1                                                            
         CLC   BWDDATES(2),HDRDATSP     YES-START DATE OK IF AFTER              
         BH    DATE1A                       FLIGHT START                        
*                                                                               
DATE1    MVC   BWDDATES(2),HDRDATSP     START DATE=FLIGHT START                 
DATE1A   L     RE,ATWA                                                          
         AHI   RE,CMPDATSP-TWAD                                                 
         LA    R1,1                DETERMINE END DATE                           
*                                                                               
DATE2    CLI   4(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   HDRU2DAT,6(RE)                                                   
         BNH   *+16                                                             
         LA    R1,1(R1)                                                         
         LA    RE,4(RE)                                                         
         B     DATE2                                                            
         TM    LFLAG,LUPGRAD1      TEST 1ST OF NEW PAIR                         
         BZ    DATE3                                                            
         MVC   LUP2DATE,4(RE)      YES-SAVE START OF SECOND RECORD              
         OC    BWDDATES+2(2),BWDDATES+2     TEST ALREADY AN END DATE            
         BZ    DATE3                                                            
         CLC   BWDDATES+2(2),2(RE) YES-END DATE OK IF BEFORE                    
         BL    DATE6                                                            
*                                                                               
DATE3    MVC   BWDDATES+2(2),2(RE) WEEK BEFORE 2ND UPGRADE EFF DATE             
         B     DATE6                                                            
*                                                                               
DATE4    TM    BWDINDS,BWDIUP2     TEST 2ND UPGRADE                             
         BZ    DATEYES                                                          
         TM    LFLAG,LUPGRAD2      YES-TEST 2ND OF NEW PAIR                     
         BO    *+14                                                             
         MVC   BWDDATES(2),HDRU2DAT  NO-START ON 2ND UPGRADE EFF DATE           
         B     DATE5                                                            
         MVC   BWDDATES,LSVDATES   YES-MOVE BACK ORIGINAL DATES                 
         OC    BWDDATES,BWDDATES   TEST ANY DATES ALREADY                       
         BZ    *+14                                                             
         CLC   BWDDATES(2),LUP2DATE   YES-START DATE OK IF AFTER                
         BH    *+10                       START DAY OF EFFECTIVE WEEK           
         MVC   BWDDATES(2),LUP2DATE                                             
         OC    BWDDATES+2(2),BWDDATES+2   TEST ALREADY AN END DATE              
         BZ    DATE5                                                            
         CLC   BWDDATES+2(2),HDRENDP  YES-END DATE OK IF BEFORE                 
         BL    DATE6                      CAMPAIGN END                          
*                                                                               
DATE5    MVC   BWDDATES+2(2),HDRENDP  END DATE=CAMPAIGN END                     
*                                                                               
DATE6    CLC   BWDDATES(2),BWDDATES+2   TEST DATES ARE OK                       
         BH    DATENO                                                           
*                                                                               
DATEYES  SR    RC,RC               CC EQ - OK                                   
DATENO   LTR   RC,RC               CC NE - REJECT THIS RECORD                   
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIX SCHEDULED SPOTS IF USING IGNORE DATES                *         
***********************************************************************         
         SPACE 1                                                                
IGNSPTFX NTR1  BASE=*,LABEL=*                                                   
***  INITIAL SETUP                                                              
         XC    APWORK,APWORK                                                    
         XC    APDUB,APDUB                                                      
         NI    LFLAG2,X'FF'-LHAVESPT                                            
         L     R3,AIOAREA2                                                      
*****  R3 ALREADY HAS ACTIVE USING OF BWDRECD                                   
         MVC   APBYTE,BWDDAYS      SAVE OFF BWDDAYS                             
         ICM   R2,15,LAFRSPEL      TEST FOR SPOTS/WEEK ELEMENT                  
         BZ    IGNSPTX                                                          
         ZIC   R0,1(R2)            YES-LOOP THROUGH THE SCHEDULE                
         AR    R0,R2                                                            
         SHI   R0,1                                                             
         LA    R2,SPWPERWK-SPWEL(R2)                                            
*                                                                               
         TM    CMPOPTS,CAMODLY     IS IT A DAILY CAMPAIGN?                      
         BO    IGNDLY05                                                         
******************  THIS IS A WEEKLY CAMPAIGN  ************************         
         L     R9,ATWA                                                          
         AHI   R9,CMPDATSP-TWAD                                                 
         LR    R4,R9               KEEP R4 AS A(1ST WEEK IN CAMPAIGN)           
**                                                                              
         CLI   0(R9),X'FF'         ANYMORE WEEKS?                               
         BE    IGNSPTX              - NOPE, DONE!                               
IGNWKY10 CLI   0(R2),0             ANY SPOTS THIS WEEK?                         
         BE    IGNWKBMP             - NOPE, BUMP IT!                            
         CLC   0(4,R9),0(R4)       IS IT THE FIRST WEEK?                        
         BNE   IGNWKY19             - NOPE, NO NEED TO CHECK BWDDAYS            
*                                                                               
         OC    APWORK,APWORK       FIRST TIME THROUGH?                          
         BNZ   IGNWKY13             - NOPE                                      
         L     RE,ATWA                                                          
         AHI   RE,CMPDATSD-TWAD                                                 
         MVC   APWORK+2(6),0(RE)    - YUP, FIRST WEEK, USE REAL FIRST           
         B     IGNWKY15               DATE INSTEAD OF A MONDAY                  
IGNWKY13 MVC   APWORK(2),0(R9)                                                  
         GOTO1 VDATCON,APPARM,(2,APWORK),(0,APWORK+2)                           
IGNWKY15 GOTO1 VGETDAY,APPARM,APWORK+2,APWORK+8                                 
         CLC   APWORK+8(3),SPACES   DO WE HAVE ANYTHING?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   APWORK(2),2(R9)                                                  
         GOTO1 VDATCON,APPARM,(2,APWORK),(0,APWORK+2)                           
         GOTO1 VGETDAY,APPARM,APWORK+2,APWORK+12                                
         CLC   APWORK+12(3),SPACES   DO WE HAVE ANYTHING?                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   APWORK+8(3),APWORK+12   SAME DAY?                                
         BNE   IGNWKY17             - NOPE, NORMAL                              
         GOTO1 VDAYPAK,APPARM,(3,APWORK+8),APFLAG,APDUB   != EXPRESSION         
         B     IGNWKY18            ...ONLY 1 DAY, NOT EXPRESSION                
*                                                                               
IGNWKY17 MVI   APWORK+11,C'-'      MOVE IN DASH, WE HAVE EXPRESSION!            
         GOTO1 VDAYPAK,APPARM,(7,APWORK+8),APFLAG,APDUB                         
IGNWKY18 CLI   APFLAG,0            WE HAVE ANYTHING?                            
         BNE   *+6                                                              
         DC    H'0'                KILL IT IF DATE IS MESSED UP                 
         MVC   APDUB(1),APBYTE                                                  
         NC    APDUB(1),APFLAG                                                  
         CLI   APDUB,0             IS IT PART OF BWDDAYS?                       
         BE    IGNWKY20             - NOPE, TAKE IT OUT                         
IGNWKY19 OI    LFLAG2,LHAVESPT     WE HAVE A SPOT!                              
         B     IGNWKY20+4                                                       
IGNWKY20 MVI   0(R2),0             TAKE OUT THE SPOT                            
         CR    R2,R0               WE ALREADY AT THE LAST WEEK?                 
         BE    IGNSPTX              - YA, A ONE WEEK CAMPAIGN                   
         B     IGNWKBMP                                                         
*                                                                               
IGNWKBMP LA    R9,4(R9)            NEXT WEEK'S DATES                            
         CLI   0(R9),X'FF'         ANYMORE WEEKS?                               
         BE    IGNSPTX              - NOPE, DONE!                               
         LA    R2,1(R2)            NEXT WEEK'S SPOTS                            
         CR    R2,R0               UP TO LAST WEEK?                             
         BL    IGNWKY10             - NOPE, GO BACK                             
*                                                                               
****  WEEKLY CAMPAIGN NEEDS SPECIAL TREATMENT FOR THE LAST WEEK                 
         MVC   APWORK(2),0(R9)                                                  
         GOTO1 VDATCON,APPARM,(2,APWORK),(0,APWORK+2)                           
         GOTO1 VGETDAY,APPARM,APWORK+2,APWORK+8                                 
         CLC   APWORK+8(3),SPACES   DO WE HAVE ANYTHING?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   APWORK(2),2(R9)                                                  
         GOTO1 VDATCON,APPARM,(2,APWORK),(0,APWORK+2)                           
         GOTO1 VGETDAY,APPARM,APWORK+2,APWORK+12                                
         CLC   APWORK+12(3),SPACES   DO WE HAVE ANYTHING?                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   APWORK+8(3),APWORK+12   SAME DAY?                                
         BNE   IGNWKY47             - NOPE, NORMAL                              
         GOTO1 VDAYPAK,APPARM,(3,APWORK+8),APFLAG,APDUB   != EXPRESSION         
         B     IGNWKY48            ...ONLY 1 DAY, NOT EXPRESSION                
*                                                                               
IGNWKY47 MVI   APWORK+11,C'-'      MOVE IN DASH, WE HAVE EXPRESSION!            
         GOTO1 VDAYPAK,APPARM,(7,APWORK+8),APFLAG,APDUB                         
IGNWKY48 CLI   APFLAG,0            WE HAVE ANYTHING?                            
         BNE   *+6                                                              
         DC    H'0'                KILL IT IF DATE IS MESSED UP                 
         MVC   APDUB(1),APBYTE                                                  
         NC    APDUB(1),APFLAG                                                  
         CLI   APDUB,0             IS IT PART OF BWDDAYS?                       
         BE    IGNWKY50             - NOPE, TAKE IT OUT                         
IGNWKY49 OI    LFLAG2,LHAVESPT     WE HAVE A SPOT!                              
         B     IGNWKY50+4                                                       
IGNWKY50 MVI   0(R2),0             TAKE OUT THE SPOT                            
         B     IGNSPTX             GET OUT                                      
**********************  END OF WEEKLY CAMPAIGN  ***********************         
*                                                                               
*********************  THIS IS A DAILY CAMPAIGN  **********************         
IGNDLY05 L     R9,ATWA                                                          
         AHI   R9,CMPDATSD-TWAD    YYMMDD FORMAT                                
IGNDLY10 CLI   0(R9),X'FF'         ANYMORE DAYS?                                
         BE    IGNSPTX              - NOPE!                                     
         CLI   0(R2),0             ANY SPOTS THIS DAY?                          
         BE    IGNDLBMP             - NOPE, BUMP IT!                            
*                                                                               
         MVC   APWORK(6),0(R9)                                                  
         GOTO1 VGETDAY,APPARM,APWORK,APWORK+6                                   
         CLC   APWORK+6(3),SPACES   DO WE HAVE ANYTHING?                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDAYPAK,APPARM,(3,APWORK+6),APFLAG,APDUB                         
         CLI   APFLAG,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APDUB(1),APBYTE                                                  
         NC    APDUB(1),APFLAG                                                  
         CLI   APDUB,0             IS IT PART OF BWDDAYS?                       
         BE    IGNDLY20             - NOPE, TAKE IT OUT                         
IGNDLY17 OI    LFLAG2,LHAVESPT     WE HAVE A SPOT!                              
         B     IGNDLY20+4                                                       
IGNDLY20 MVI   0(R2),0             TAKE OUT THE SPOT                            
         CR    R2,R0               WE ALREADY AT THE LAST DAY?                  
         BE    IGNSPTX              - YA, A ONE DAY CAMPAIGN                    
         B     IGNDLBMP                                                         
*                                                                               
IGNDLBMP LA    R9,6(R9)            NEXT DAY'S DATES                             
         LA    R2,1(R2)            NEXT DAY'S SPOTS                             
         CR    R2,R0               UP TO LAST DAY?                              
         BNH   IGNDLY10             - NOPE, GO BACK                             
****  DAILY CAMPAIGN DOES NOT NEED SPECIAL TREATMENT FOR THE LAST DAY           
****                                                                            
IGNSPTX  TM    LFLAG2,LHAVESPT     DO WE HAVE AT LEAST 1 SPOT?                  
         BO    IGNSPTXX             - YUP, WE DO                                
         XC    LAFRSPEL,LAFRSPEL    - NOPE, DELETE THIS                         
         B     IGNSPTXX                                                         
*                                                                               
*  THE FOLLOWING CODE MIGHT NOT BE NEEDED                                       
*&&D0                                                                           
IGNSPTX5 CR    R2,R0               MORE WEEKS OF SPOTS SKED THAN CMPGN?         
         BNL   IGNSPTXX             - NOPE, JUST END THIS                       
         SR    R0,R2                                                            
         L     R1,LAFRSPEL                                                      
         USING SPWEL,R1                                                         
         IC    RE,SPWELLN          WE'RE GONNA SHORTEN THE ELEMENT              
         SR    RE,R0                                                            
         STC   RE,SPWELLN                                                       
         B     IGNSPTX                                                          
*&&                                                                             
*                                                                               
IGNSPTXX J     COPRXIT                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET INACTIVE WEEKS MASK                                            
*                                                                               
* ON EXIT:     APDUB               BITS MASK                                    
***********************************************************************         
         SPACE 1                                                                
WEEKS    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
***   HDRDATSP - TO CAMP DATES    LFRDATES - FROM CAMP DATES                    
         LA    R0,53               SET INACTIVE WEEKS MASK (ASSUME 14)          
         LA    R1,HDRDATSP                                                      
         LA    R2,LFRDATES                                                      
*                                                                               
         XR    RF,RF               R8,R9  RE,RF USED FOR BWDWKS...              
         LR    RE,RF               ...SETUP                                     
         LR    R8,RF                                                            
         LR    R9,RF                                                            
         ICM   R8,12,BWDWKS                                                     
*                                                                               
         CLI   CMPNWKS,14          MORE THAN 14 WEEKS?                          
         BNH   WEEK10                                                           
         CLI   BWDELLN,BWDELLNQ    CAN THIS ELEMENT HANDLE 53 WEEKS?            
         BNH   WEEK10              NO, WE MIGHT OVERWRITE NEXT ELEM             
         ICM   R8,3,BWDWKS2        5 MORE BYTES USED FOR 53 BITS                
         ICM   R9,14,BWDWKS2+2                                                  
*                                                                               
WEEK10   CLI   0(R1),X'FF'         'TO' IS DONE                                 
         BE    WEEK85               - WE'RE DONE!                               
*                                                                               
********************  SEE IF BWDDATES IS ACTIVE  ********************           
DATECHK  DS    0H                                                               
         OC    BWDDATES,BWDDATES   ANY DATES?                                   
         BZ    DATCHKYS             - NOPE                                      
         CLC   BWDDATES(2),2(R1)   TEST START DATE AFTER THIS WEEK              
         BH    DATCHKNO             - YUP, INACTIVE                             
         CLC   BWDDATES+2(2),0(R1) TEST END DATE BEFORE                         
         BL    DATCHKNO             - YUP, INACTIVE                             
*                                                                               
DATCHKYS OI    LFLAG2,LDATESON     ** ACTIVE **                                 
         B     *+8                                                              
DATCHKNO NI    LFLAG2,X'FF'-LDATESON   ** INACTIVE **                           
********************  SEE IF BWDDATES IS ACTIVE  ********************           
         CLI   0(R2),X'FF'         'FROM' IS DONE                               
         BNE   WEEK20               - LET'S CONTINUE                            
         LA    R1,4(R1)            BUMP                                         
         B     WEEK80              ACTIVE UNLESS BWDDATES SAY OTHERWISE         
*                                                                               
WEEK20   OC    BWDDATES,BWDDATES   ANY BWDDATES?                                
         BZ    WEEK20C              - NOPE, NO POSSIBLE OVERRIDE                
         CLC   BWDDATES(2),BWDDATES+2   SINGLE DAY SCHEDULE?                    
         BE    WEEK23               - YUP, IT IS                                
WEEK20C  TM    LFLAG2,LIGNDAT      WE IGNORING DATES?                           
         BO    WEEK25               - YUP, WE'RE IGNORING DATES                 
WEEK23   CLC   0(4,R2),0(R1)       LET'S COMPARE THE 'FROM' AND 'TO'            
         BL    WEEK40               - 'FROM' STARTS FIRST                       
         BH    WEEK50               - 'TO' STARTS FIRST                         
*                                                                               
WEEK25   LA    R2,4(R2)            BUMP BOTH 'FROM' AND 'TO'                    
         LA    R1,4(R1)                                                         
         B     WEEK80              NEXT WEEK                                    
*                                                                               
WEEK40   LA    R2,4(R2)            NEED TO BUMP TO NEXT WEEK                    
         SLDL  R8,1                SHIFT INACTIVE WEEKS ACCORDINGLY             
         B     WEEK10              ...GET RID OF CURRENT INACT. WEEK            
****                               ...NO NEED TO CHECK LFLAG BWDDATES           
****                               ...DON'T BUMP RESULT BWDWKS EITHER           
*                                                                               
WEEK50   LA    R1,4(R1)            BUMP 'TO' WEEK                               
         SRDL  R8,1                MOVE BWDWKS BACK ONE SLOT                    
****  WEEK80 - INACTIVE      WEEK90 - ACTIVE                                    
*EEK80   TM    LFLAG2,LDATESON     DO WE HAVE AN ACTIVE WEEK?                   
*        BO    WEEK90                                                           
*        STCM  R8,8,APBYTE         TURN ON HIGH ORDER BIT (INACTIVE)            
*        OI    APBYTE,X'80'                                                     
*        ICM   R8,8,APBYTE         PUT IT BACK                                  
*        B     WEEK90                                                           
WEEK80   STCM  R8,8,APBYTE         TURN ON HIGH ORDER BIT (INACTIVE)            
         OI    APBYTE,X'80'                                                     
         TM    LFLAG2,LDATESON     DO WE HAVE AN ACTIVE WEEK?                   
         BZ    WEEK80G              - NOPE WE DON'T                             
         NI    APBYTE,X'FF'-X'80'   TAKE OFF THE HIGH ORDER BIT                 
WEEK80G  ICM   R8,8,APBYTE         PUT IT BACK                                  
         B     WEEK90                                                           
*                                                                               
WEEK85   XR    R8,R8               SO FUTURE COPIES OF THIS COPY WILL           
         LR    R9,R8                  NOT HAVE STRANGE INACTIVE WEEKS           
*EEK90   CLC   0(4,R1),HDRDATSP    IS IT STILL FIRST WEEK?                      
*        BNH   WEEK10               - NOPE, DON'T SUBTRACT COUNTER              
WEEK90   STCM  R8,8,APBYTE                                                      
         ICM   RF,1,APBYTE         LUCKY WE ONLY NEED 7 BYTES                   
         SLDL  RE,1                MOVE ON TO NEXT WEEK                         
         SLDL  R8,1                                                             
         BCT   R0,WEEK10                                                        
*                                                                               
         MVI   APBYTE,0            LOB OF RF NO USED                            
         ICM   RF,1,APBYTE                                                      
         SLDL  RE,3                TO ALIGN FOR 56 BITS                         
         STCM  RE,12,BWDWKS        SAVE OFF BWDWKS                              
         CLI   CMPNWKS,14          DO WE CARE ABOUT BWDWKS2?                    
         BNH   WEEKX               NO, ALL GOOD                                 
         CLI   BWDELLN,BWDELLNQ    CAN THIS ELEMENT HANDLE 53 WEEKS?            
         BNH   WEEKX               NO, WE MIGHT OVERWRITE NEXT ELEM             
         STCM  RE,3,BWDWKS2                                                     
         STCM  RF,14,BWDWKS2+2                                                  
*                                                                               
WEEKX    J     COPRXIT                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO X-OUT SCHEDULED WEEKS                                    *         
***********************************************************************         
         SPACE 1                                                                
XOUT     NTR1  BASE=*,LABEL=*                                                   
         ICM   R2,15,LAFRSPEL      TEST FOR SPOTS/WEEK ELEMENT                  
         BZ    XOUTX                                                            
         ZIC   R0,1(R2)            YES-LOOP THROUGH THE SCHEDULE                
         AR    R0,R2                                                            
         LA    R2,SPWPERWK-SPWEL(R2)                                            
         LA    R1,HDRSKDSP                                                      
         XC    APHALF,APHALF                                                    
****  USE SVHDRSKD IF IT'S SINGLE DAY SCHEDULE AND WE'RE IGNORING DATES         
****                                                                            
         OC    BWDDATES,BWDDATES   ANY BWDDATES?                                
         BZ    XOUT2                - NOPE, NO POSSIBLE OVERRIDE                
         CLC   BWDDATES(2),BWDDATES+2 SINGLE DAY SCHEDULE?                      
         BNE   XOUT2                - NOPE, USE HDSKDSP                         
         LA    R1,SVHDRSKD          - YEAH, USE SPECIAL SKED INSTEAD            
****                                                                            
*                                                                               
XOUT2    CR    R2,R0               TEST ELEMENT END                             
         BNL   XOUT6                                                            
         CLI   0(R2),0             NO-TEST SPOTS THIS WEEK                      
         BE    XOUT4               NO                                           
         ZIC   RE,0(R1)            YES-GET DISPLACEMENT TO NEW CAMPAIGN         
         CLI   0(R1),X'FF'         IS THERE A CORRESPONDING WEEK?               
         BE    XOUT4               NO                                           
         LA    RE,1(RE)            YES-MAKE THAT WEEK INACTIVE                  
         LA    R8,1                                                             
         SR    R9,R9                                                            
         SRDL  R8,1                                                             
         BCT   RE,*-4                                                           
         ST    R9,APFULL                                                        
         OC    APHALF,APFULL                                                    
*                                                                               
XOUT4    LA    R1,1(R1)            NEXT WEEK                                    
         LA    R2,1(R2)                                                         
         B     XOUT2                                                            
*                                                                               
XOUT6    OC    BWDWKS,APHALF       OR IN INACTIVE WEEKS                         
*                                                                               
XOUTX    J     COPRXIT                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SHIFT THE BWDDATES ACCORDINGLY IF WE'RE IGNORING DATES   *         
* OUTPUT : CC EQ -                                                    *         
*          CC NE -                                                    *         
***********************************************************************         
         SPACE 1                                                                
MVIGNDAT NTR1  BASE=*,LABEL=*                                                   
         OC    BWDDATES,BWDDATES   NOTHING IN BWDDATES?                         
         BZ    MVIGNX               - YUP, NOTHING TO DO, GET OUT               
***   HDRDATSP - TO CAMP DATES    LFRDATES - FROM CAMP DATES   ***              
*                                                                               
         GOTO1 VDATCON,APPARM,(2,LFRDATES),(0,APWORK+1)                         
         GOTO1 VDATCON,APPARM,(2,HDRDATSP),(0,APWORK+7)                         
         GOTO1 VDATCON,APPARM,(2,BWDDATES),(0,APWORK+13)                        
         GOTO1 VDATCON,APPARM,(2,BWDDATES+2),(0,APWORK+19)                      
*   THIS IS FOR THE BEGINNING OF EFFECTIVE DATE RANGE                           
         GOTO1 VPERVERT,APPARM,APWORK+1,APWORK+13                               
         XR    RF,RF                                                            
         ICM   RF,3,APPARM+8                                                    
         BCTR  RF,0                SUBTRACT 1 DAY                               
*                                                                               
         GOTO1 VADDAY,APPARM,APWORK+7,APWORK+13,(RF)                            
*                                                                               
         GOTO1 VDATCON,APPARM,(0,APWORK+13),(2,BWDDATES)   COMPRESSED           
*                                                                               
*   THIS IS FOR THE ENDING OF EFFECTIVE DATE RANGE                              
         GOTO1 VPERVERT,APPARM,APWORK+1,APWORK+19                               
         XR    RF,RF                                                            
         ICM   RF,3,APPARM+8                                                    
         BCTR  RF,0                SUBTRACT 1 DAY                               
*                                                                               
         GOTO1 VADDAY,APPARM,APWORK+7,APWORK+19,(RF)                            
*                                                                               
         GOTO1 VDATCON,APPARM,(0,APWORK+19),(2,BWDDATES+2)   COMPRESSED         
*                                                                               
MVIGNX   J     COPRXIT                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
EXTRAWKD DSECT                                                                  
XTRAFLG1 DS    XL1                 EXTRA FLAGS                                  
XF1NXESL EQU   X'80'                - SOME LINES NOT XFR CAUSE OF ESLN          
XAUTH    EQU   X'40'               IF ON, ALREADY WENT THROUGH CODE             
XSDE     EQU   X'20'               IF ON, SDESK AUTH OPEN FOR PRD OPTN          
*                                                                               
LSTDARKY DS    XL(L'DOKEY)         LAST DARE KEY (BY CLIENT)                    
STASNBKS DS    XL1998              STATONS (CL8) AND BOOKS (CL1)                
EXTRAWKL EQU   *-EXTRAWKD                                                       
         EJECT                                                                  
STABYD   DSECT                     DSECT FOR STATION/BUYLINE TABLE              
SBSTA    DS    CL8                 STATION                                      
SBLNLO   DS    PL2                 LOW BUY LINE                                 
SBLNHI   DS    PL2                 HIGH BUY LINE                                
STABYL   EQU   *-STABYD                                                         
*                                                                               
*                                                                               
LOCALD   DSECT                                                                  
LAFRDMEL DS    A                                                                
LAFRSPEL DS    A                                                                
LAFRUPEL DS    A                                                                
LAFROVEL DS    A                                                                
LAFRSHEL DS    A                                                                
LATOSPEL DS    A                                                                
LAMINBLK DS    A                                                                
LAMINBUF DS    A                                                                
LAMINRTB DS    A                                                                
LAMN2BLK DS    A                                                                
LAMN2BUF DS    A                                                                
LAMN2RTB DS    A                                                                
LACAMP   DS    A                                                                
*AMEDIA  DS    A                   SAME IDEA AS LACAMP                          
LSVBYRMK DS    XL1                                                              
         DS    XL3                 SPARE FOR NOW                                
AEXTRAWK DS    A                                                                
ASAVAREA DS    A                                                                
SAVERE   DS    A                   SAVE OFF THE REGISTER E                      
LKEYCOMP DS    XL1                                                              
LMED     DS    XL1                                                              
LBYR     DS    XL1                                                              
LCAM     DS    XL2                                                              
LMKT     DS    XL2                                                              
LMKTLKUP DS    XL2                                                              
LFRMKT   DS    XL2                                                              
LSTACD   DS    XL1                                                              
LSTA     DS    CL8                                                              
LBCLT    DS    XL2                 BINARY CLIENT FROM CAMPAIGN RECORD           
LQCLT    DS    CL3                 3 LETTER CLIENT CODE                         
LDPT     DS    CL1                                                              
LSLN     DS    XL1                                                              
LCMSEQ   DS    XL2                                                              
LFRDATES DS    XL(L'CMPDATSP)                                                   
LFRENDMN DS    XL2                                                              
LFREND   DS    XL3                                                              
LFRU2DAT DS    XL2                                                              
LFRCAMOP DS    XL1                                                              
LFRUF    DS    CL1                                                              
LFRUP    DS    XL8                                                              
LFRFB    DS    XL2                                                              
LFRFBTP  DS    XL1                 LFRFB BOOKTYPE                               
LFRFBLST DS    XL6                                                              
LFRUPIN  DS    CL32                                                             
LFRPUT   DS    CL1                                                              
LFRSHR   DS    CL1                                                              
LFRSLN   DS    XL1                                                              
LUF      DS    CL1                                                              
LUP      DS    XL8                                                              
LFB      DS    XL2                                                              
LFBTP    DS    XL1                 LFB BOOK TYPE (BINARY)                       
LFBLST   DS    XL6                                                              
LUPPUT   DS    CL1                                                              
LUPSHR   DS    CL1                                                              
LOVDAY   DS    XL1                                                              
LOVTIME  DS    XL4                                                              
LUP2DATE DS    XL2                                                              
LSVDATES DS    0XL4                                                             
LSVDATE1 DS    XL2                                                              
LSVDATE2 DS    XL2                                                              
SAVECABL DS    CL4                 SAVE CABLE SYSTEM FOR SUBROUTINE             
SAVESEQ  DS    XL2                                                              
SAVEKEY  DS    XL44                                                             
LDEMS    DS    XL(NDEMOS*3)                                                     
LOLDDEMS DS    XL(NDEMOS*3)                                                     
LADJDEMS DS    XL(NDEMOS*3)                                                     
LDMUPBLK DS    (SPDEMUP2)X                                                      
LDEMVALS DS    (NDEMOS)XL4                                                      
LADJNEW  DS    (NDEMOS)XL4                                                      
LADJVALS DS    (NDEMOS)XL4                                                      
LPKORNUM DS    XL1                                                              
LASTPKOR DS    XL1                                                              
LFSTKEY  DS    XL13                                                             
LASTKEY  DS    XL13                                                             
LSVUP1KY DS    XL13                                                             
*                                                                               
LFLAG    DS    XL1                                                              
LNOSKD   EQU   X'80'                                                            
LCPYDEM  EQU   X'40'                                                            
LFSTREC  EQU   X'20'                                                            
LUPGRAD1 EQU   X'10'                                                            
LUPGRAD2 EQU   X'08'                                                            
LNETCOST EQU   X'04'                                                            
LMKTCPY  EQU   X'02'               MARKET COPY!                                 
LCBLSYS  EQU   X'01'               CABLE SYSTEM COPY!!                          
*                                                                               
LFLAG2   DS    XL1                                                              
LFSTTIME EQU   X'80'               USED IN CABLEALL SUBROUTINE! 5/22/02         
LMKTDONE EQU   X'40'                 ''        ''         ''                    
LINVSTA  EQU   X'20'               INVALID STATION (CABLE SYSTEM COPY)          
LNOCST   EQU   X'10'               DON'T COPY THE COST                          
LDATESON EQU   X'08'               BWDDATES IS ON (ACTIVE WEEK)                 
LIGNDAT  EQU   X'04'               IGNORE DATES                                 
LHAVESPT EQU   X'02'               WE HAVE AT LEAST ONE SPOT                    
*                                                                               
SEQCOUNT DS    XL1                 COUNTER FOR DETAIL RECORD STATION            
SAVESC   DS    CL1                 SAVE THE STATION CODE                        
SAVESTA  DS    CL8                 SAVE THE STATION FOR COMPARE LATER           
*                                  ...IF STATION IS THE SAME,    MHC            
*                                  ...NO BUMPING FOR SEQCOUNT  06/13/02         
SVHDRSKD DS    XL(53)              SCHEDULE WEEK DISPLACEMENTS                  
YUNFLAG  DS    XL1                                                              
*                                                                               
LIND     DS    X                                                                
LPOL     EQU   X'80'                                                            
LNFPASS  EQU   X'40'               BORROWING THIS FOR VALKEY                    
*                                                                               
LMASPRD  DS    XL2                                                              
LHDRTAB  DS    (CPYMAX)XL(HDRTABL)                                              
*                                                                               
         ORG   LOCALD+4096                                                      
LOCALX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* HEADER TABLE DSECT                                                  *         
***********************************************************************         
         SPACE 1                                                                
HDRTABD  DSECT                                                                  
HDRAM    DS    XL1                 AGENCY/MEDIA                                 
HDRBYR   DS    XL1                 BUYER                                        
HDRCAM   DS    XL2                 CAMPAIGN NUMBER                              
HDRSEQ   DS    XL2                 CAMPAIGN/MARKET SEQUENCE NUMBER              
HDRSLN   DS    XL1                 CAMPAIGN SPOT LENGTH                         
HDRIND   DS    XL1                 CAMPAIGN/MARKET INDICATORS                   
HDRINEW  EQU   X'80'               NEW HEADER                                   
HDRINOSK EQU   X'40'               NO SCHEDULE COPY                             
HDRISAMD EQU   X'20'               SAME DATES AS FROM CAMPAIGN                  
HDRINSTA EQU   X'10'               NEW STATION(S) ADDED TO HEADER               
HDRIUPGR EQU   X'08'               USE TO CAMPAIGN UPGRADE                      
HDRIMADP EQU   X'04'               SUB-DPTS GROUPED UNDER MASTER DPT            
HDRISUDP EQU   X'02'               SCHEDULE SUB-DPTS SEPARATELY                 
HDRINEWD EQU   X'01'               NEW SET OF MINIO DETAIL RECORDS              
HDRIND2  DS    XL1                 CAMPAIGN INDICATORS                          
HDRINOSP EQU   X'80'               CAMPAIGN ORIGINALLY HAS NO SPOTS             
HDRISKD  EQU   X'40'               AT LEAST ONE SPOT SCHEDULED                  
HDRICOPU EQU   X'20'               COPY CAMPAIGN UPGRADE                        
HDRDEMAD DS    CL1                 AUTO DEMO ADJUST INDICATOR                   
HDRSLEQU DS    XL15                SPOT LENGTH EQUIVALENCES                     
HDRNXTST DS    XL1                 NEXT AVAILABLE STATION CODE                  
HDRSTRT  DS    XL2                 LAST DAY OF CAMPAIGN START WEEK              
HDREND   DS    XL3                 CAMPAIGN END DATE                            
HDRENDP  DS    XL2                 CAMPAIGN END DATE PACKED                     
HDRNWKS  DS    XL1                 NUMBER OF CAMPAIGN WEEKS                     
HDRUF    DS    CL1                 UPGRADE FILE                                 
HDRUP    DS    XL8                 UPGRADE EXPRESSION                           
HDRFB    DS    XL2                 OVERRIDE FROM BOOK                           
HDRFBTP  DS    XL1                 HDRFB BOOK TYPE                              
HDRFBLST DS    XL6                 OVERRIDE FROM BOOK LIST                      
HDRUPUT  DS    CL1                 UPGRADE PUT AVERAGING                        
HDRUSHR  DS    CL1                 UPGRADE SHR AVERAGING                        
HDRU2DAT DS    XL2                 2ND UPGARDE VALUES                           
HDRUF2   DS    CL1                                                              
HDRUP2   DS    XL8                                                              
HDRFB2   DS    XL2                                                              
HDRFB2TP DS    XL1                 HDRFB2 BOOKTYPE                              
HDRFB2L  DS    XL6                                                              
HDRIN2   DS    CL32                                                             
HDRUPUT2 DS    CL1                                                              
HDRUSHR2 DS    CL1                                                              
HDRCLT   DS    CL3                 CLIENT                                       
HDRSRC   DS    CL1                 A=ARB,N=NSI                                  
HDRBWPRO DS    XL16                CLIENT BW PROFILE                            
HDRDMENU DS    XL1                 ESTIMATE DEMO MENU                           
HDRBOOK  DS    XL2                 ESTIMATE DEFAULT BOOK                        
HDRBKTYP DS    XL1                 ESTIAMTE OVERRIDE BOOK TYPE                  
HDRDEMS  DS    XL60                ESTIMATE DEMOS                               
HDRSTA   DS    XL255               STATIONS                                     
****   CHANGED FROM 168 TO 255 BECAUSE OF RENSEQH    MHC  05/29/03              
****   SOURCE HEADER RECORD WITH MORE THAN 168 STATIONS IS SUPPORTED            
HDRSTMAX EQU   168                                                              
HDRSKDSP DS    XL(53+1)            SCHEDULE WEEK DISPLACEMENTS                  
HDRDATSP DS    XL216               CAMPAIGN LIST OF WEEKS (53+1WKS*4)           
HDRTABL  EQU   *-HDRTABD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT FOR SECOND MINIO AREAS                                        *         
***********************************************************************         
         SPACE 1                                                                
MIN2D    DSECT                                                                  
MIN2BLK  DS    XL(MINBLKL)               MINIO BLOCK                            
MIN2RTAB DS    (MINRTMAX)XL(6+L'BWDKEL)  MINIO RECORD TABLE                     
MIN2BUFF DS    (MINNBUFF)XL2000          MINIO BUFFERS                          
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSF6D                                                       
         SPACE 2                                                                
         ORG   SAVOVER                                                          
CABLETAB DS    CL255               TABLE TO STORE CABLE/NETWORK COMBO           
         EJECT                                                                  
* SPNWSCAM                                                                      
       ++INCLUDE SPNWSCAM                                                       
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STAHDRD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENAUTH                                                      
         EJECT                                                                  
       ++INCLUDE SPAUTHD                                                        
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'156SPNWS11   11/27/07'                                      
         END                                                                    
