*          DATA SET SEACS08    AT LEVEL 025 AS OF 03/19/20                      
*PHASE TA0D08A                                                                  
*INCLUDE SEC1RP                                                                 
*INCLUDE SCINKEY                                                                
*INCLUDE DATTIM                                                                 
*&&US                                                                           
*INCLUDE VEMAIL                                                                 
*&&                                                                             
*INCLUDE PWDVAL                                                                 
*INCLUDE PWDGEN                                                                 
                                                                                
         TITLE 'SEACS08 - SECURITY ACCESS - PERSON RECORDS'                     
ACS08    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS8**,RA,R9,R8,RR=RE                                        
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   AGENCYID,OPTAGY                                                  
         B     *+10                                                             
         MVC   AGENCYID,CUAALF                                                  
*                                                                               
         LARL  RF,DDLFTAB          SET ADDRESS OF DDLINK FIELD TABLE            
         ST    RF,APDDLTAB         (FOR GENERAL)                                
*                                                                               
         CLC   =C'SJ',AGENCYID                                                  
         BNE   MAIN10                                                           
         CLI   APACTN,ACTLST                                                    
         BE    MAIN10                                                           
         CLI   APACTN,ACTGRD                                                    
         BE    MAIN10                                                           
         CLI   APACTN,ACTREP                                                    
         BE    MAIN10                                                           
         NI    PERDAGH+FHATD,X'FF'-FHATLO                                       
         OI    PERDAGH+FHOID,FHOITR                                             
         NI    PERLAGH+FHATD,X'FF'-FHATLO                                       
         NI    PERLAGH+FHATD,X'FF'-FHATPR                                       
         OI    PERLAGH+FHOID,FHOITR                                             
         NI    PERLAGNH+FHATD,X'FF'-FHATLO                                      
         OI    PERLAGNH+FHOID,FHOITR                                            
*                                                                               
MAIN10   ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
                                                                                
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     DELREC              05 - APMDELR                                 
         B     RESREC              06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSELI             08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     EXIT                15 - APMVALQ                                 
         B     EXIT                16 - APMREPP                                 
         B     SETTWA              17 - APMSETT                                 
         B     PUTKEY              18 - APMPUTK                                 
         B     VALREC              19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXITNE   MVI   EXITC,0                                                          
         J     EXITCC                                                           
EXITLO   MVI   EXITC,0                                                          
         J     EXITCC                                                           
EXITHI   MVI   EXITC,2                                                          
         J     EXITCC                                                           
EXITEQ   MVI   EXITC,1                                                          
EXITCC   CLI   EXITC,1                                                          
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF PERSON AND ASSOCIATED PASSWORD RECORD    *         
***********************************************************************         
VALKEY   CLI   APACTN,ACTGRD       ACTION GRIDS?                                
         BNE   *+12                . NO                                         
         MVI   APINDS,APIOKDIS     DISPLAY ALLOWED                              
         B     EXIT                                                             
*                                  GET AGENCY ACCESS DETAILS                    
VK001    MVI   PIDREQD,C'N'                                                     
         XC    PWDMINLN,PWDMINLN                                                
         GOTO1 AGETAAD,AGENCYID                                                 
         TM    APWORK,CTAADPRQ     PERSONAL ID REQUIRED WITH PASSWORD?          
         BZ    *+8                                                              
         MVI   PIDREQD,C'Y'                                                     
         MVC   PWDINFO,APWORK      SAVE PASSWORD ATTRIBUTES                     
*                                                                               
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BNE   VKEY002                                                          
         CLI   APACTN,ACTCNV                                                    
         BE    VALKEYX                                                          
*                                                                               
VKEY002  LA    R2,IOKEY                                                         
*                                  INITIALISE STORAGE                           
         XC    OLDVALS(OLDVALSL),OLDVALS                                        
         XC    NEWVALS(NEWVALSL),NEWVALS                                        
         XC    OFFAPC,OFFAPC                                                    
         XC    DEPAPC,DEPAPC                                                    
         XC    APWORK,APWORK                                                    
         XC    APHALF,APHALF                                                    
         XC    LASTCDT,LASTCDT                                                  
         XC    LASTCTM,LASTCTM                                                  
         MVI   PIDIND,0                                                         
         MVI   PWDIND,0                                                         
*                                                                               
*CHECK FOR PID # PASSED FROM DDLINK IN DDLKPIN                                  
*                                                                               
         TM    CUSTAT,CUSDDL       DDLINK IN CONTROL?                           
         BZ    VKEY003                                                          
         CLC   DDLKPIN,SPACES      AND IS THERE A PIN?                          
         BE    VKEY003                                                          
         MVC   FVDDLERR,=AL2(0702) DDLINK FIELD CODE FOR PIN ERROR              
         MVC   APDUB(5),DDLKPIN    MAKE SURE NUMERIC                            
         NC    APDUB(5),=C'00000'                                               
         CLC   APDUB(5),=C'00000'                                               
         BNE   SAEIIF              NO                                           
         PACK  APDUB,DDLKPIN(5)                                                 
         CVB   R1,APDUB                                                         
         ST    R1,APDUB                                                         
         OC    APDUB(2),APDUB                                                   
         BNZ   SAEIIF              NOT IN RANGE                                 
         MVC   APHALF,APDUB+2                                                   
         B     VKEY003A                                                         
*                                                                               
* CHECK FOR PID # INPUT FOR DDS TERMINAL ONLY                                   
*                                                                               
VKEY003  CLC   PERPID(2),=C'P='    PID # ENTERED?                               
         BNE   VKEY008             NO                                           
*                                                                               
         TM    CUSTAT,CUSDDS       DDS TERMINAL?                                
         BZ    SAEIIF              NO - INVALID PID NAME                        
*                                                                               
         LA    R1,PERPIDH          SET CURSOR FOR ERROR MESSAGE                 
         ST    R1,FVADDR                                                        
*                                                                               
         GOTO1 VHEXIN,APPARM,PERPID+2,APHALF,4                                  
         CLC   =F'2',APPARM+12                                                  
         BNE   SAEIIF                                                           
*                                  USE PID# TO GET PID NAME                     
VKEY003A LA    R4,IOKEY                                                         
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,APHALF                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
*                                                                               
         USING SA0REC,R4                                                        
         L     R4,AIOAREA3         SET UP PASSWD RECORD                         
         LA    R3,SA0DATA                                                       
         DROP  R4                                                               
*                                                                               
VKEY004  CLI   0(R3),0             TEST E-O-R                                   
         BE    SAEIIF              CAN'T FIND THE PERSONAL ID NAME              
*                                                                               
         CLI   0(R3),SAPALELQ      PERSONAL-ID POINTER ELEMENT                  
         BNE   VKEY006                                                          
         USING SAPALD,R3                                                        
         MVC   PERPID(L'SAPALPID),SAPALPID                                      
         XC    FVDDLERR,FVDDLERR   CLEAR DDLKPID ERROR                          
         B     VKEY008                                                          
         DROP  R3                                                               
*                                                                               
VKEY006  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VKEY004                                                          
*                                                                               
VKEY008  GOTO1 =V(DATTIM),APPARM,(X'01',DATETIME),0,RR=APRELO                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATETIMC,DATETIME   SAVE CURRENT DATE AND TIME                   
         XC    DATETIMC,FFILL                                                   
*                                                                               
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   OLDDEF,TODAY                                                     
         MVC   OLDDEFC,FFILL       SET DATE IN PID RECORD KEY                   
         XC    OLDDEFC,OLDDEF                                                   
         MVI   COPYFLAG,0                                                       
         CLI   APACTN,ACTCPY       SET FLAG IF SECOND COPY KEY                  
         BNE   VKEY010                                                          
         OC    OPTAGY,OPTAGY       CAN'T COPY WITH AGENCY OPTION                
         BNZ   VALKEYX                                                          
         TM    ACLFMIND,ACLFMIFK   TEST FIRST TIME COPY FLAG                    
         BNZ   *+8                                                              
         MVI   COPYFLAG,1                                                       
*                                                                               
VKEY010  MVI   FVMINL,3                                                         
         GOTO1 AFVAL,PERPIDH       READ PERSONAL-ID                             
         BE    VKEY012                                                          
         CLI   APACTN,ACTADD       MUST BE INPUT FOR ADD,CONVERT,COPY           
         BE    VALKEYX                                                          
         CLI   APACTN,ACTCNV                                                    
         BE    VALKEYX                                                          
         CLI   COPYFLAG,0                                                       
         BNE   VALKEYX                                                          
         CLI   PIDREQD,C'Y'        MUST BE INPUT IF PID REQUIRED                
         BE    VALKEYX                                                          
         B     VKEY020                                                          
VKEY012  MVC   PIDIND,FVIIND       SAVE INPUT INDICATORS                        
         MVC   OLDPID,FVIFLD       SAVE PERSONAL-ID                             
         OC    OLDPID,SPACES                                                    
*                                                                               
VKEY020  TM    CUSTAT,CUSDDL       DDLINK ACTIVE?                               
         BZ    VKEY022                                                          
         CLI   APACTN,ACTADD       YES, ADD ACTION?                             
         BNE   VKEY022                                                          
         CLC   DDLKPWD,SPACES      YES, PASSWORD PASSED?                        
         BNE   VKEY021                                                          
         MVI   PWDRULE,0           NO, DISABLE PASSWORD RULE AND                
         GOTO1 =V(PWDGEN),APPARM,DDLKPWD,RR=APRELO GENERATE PASSWORD            
VKEY021  MVC   PERPWD,DDLKPWD                                                   
*                                                                               
VKEY022  MVI   FVMINL,3                                                         
         GOTO1 AFVAL,PERPWDH       READ PASSWORD FOR ADD,CONVERT,COPY           
         BE    VKEY030                                                          
         CLI   APACTN,ACTADD                                                    
         BE    VALKEYX                                                          
         CLI   APACTN,ACTCNV                                                    
         BE    VALKEYX                                                          
         CLI   COPYFLAG,0                                                       
         BNE   VALKEYX                                                          
         B     VKEY040                                                          
*                                                                               
VKEY030  LHI   R0,3                R0=VALIDATE PWD FOR ADD,CONVERT,COPY         
         CLI   APACTN,ACTADD                                                    
         BE    VKEY032                                                          
         CLI   APACTN,ACTCNV                                                    
         BE    VKEY032                                                          
         CLI   COPYFLAG,0                                                       
         BNE   VKEY032                                                          
         LHI   R0,2                R0=PROCESS PWD (CONVERT TO UPPER)            
         B     VKEY033                                                          
*                                                                               
VKEY032  CLI   PWDMINLN,3          CHECK MINIMUM LENGTH                         
         BNH   *+14                                                             
         CLC   FVILEN,PWDMINLN                                                  
         BL    SAEFTS                                                           
         LLC   R1,FVILEN                                                        
         LA    RE,FVIFLD                                                        
         CLI   0(RE),C' '          CANT HAVE SPACE IN PASSWORD                  
         BE    SAESPIP                                                          
         AHI   RE,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
VKEY033  MVC   PWDIND,FVIIND       SAVE INPUT INDICATORS                        
         MVC   OLDCODE,FVIFLD      SAVE PASSWORD                                
         XC    APPARM(20),APPARM                                                
         LA    RE,OLDCODE                                                       
         ST    RE,APPARM                                                        
         MVC   APPARM+0(1),FVILEN                                               
         LA    RE,APWORK           TEXT AREA TO RETURN UPPER CASE VALUE         
         ST    RE,APPARM+4                                                      
         XC    0(10,RE),0(RE)                                                   
         MVC   APPARM+4(1),PWDRULE SET PASSWORD RULE NUMBER                     
         L     RF,ASYS                                                          
         L     RF,VSSB-SYSFACD(RF) GET SSB                                      
         ST    RF,APPARM+8                                                      
         STC   R0,APPARM+8         R0=ACTION VALIDATE OR PROCESS                
         GOTO1 =V(PWDVAL),APPARM,RR=APRELO                                      
         MVC   OLDCODU,APWORK      SAVE UPPER CASE VERSION                      
*                                                                               
         SR    RF,RF               GET ERROR NUMBER                             
         ICM   RF,1,8(R1)                                                       
         BZ    VKEY040                                                          
         B     VKEY035                                                          
*                                                                               
VKEY034  AHI   RF,116              ERROR MESSAGES CON=342-355                   
         B     VKEY036                                                          
*                                                                               
VKEY035  CHI   RF,231              ERROR RETURN E2-E7=226-231                   
         BH    *+12                                                             
         AHI   RF,116              ERROR MESSAGES CON=342-347                   
         B     VKEY036                                                          
         ICM   RF,1,PWDRULE        GET PASSWORD RULE NUMBER                     
         BNZ   *+12                                                             
         ICM   RF,1,8(R1)          IF NO RULE USE ORIGINAL ERROR RETURN         
         B     VKEY034                                                          
         AHI   RF,360              RULE MESSAGES  CON=362-370                   
VKEY036  STH   RF,FVMSGNO                                                       
         B     NO                                                               
*                                                                               
VKEY040  CLI   APACTN,ACTADD       PROCESS ADD AND COPY ACTIONS                 
         BE    *+12                                                             
         CLI   COPYFLAG,0                                                       
         BE    VKEY050                                                          
         BRAS  RE,READPID          READ FILE                                    
         BNE   VALKEYX                                                          
         CLI   PIDIOERR,IOERNF     CHECK PERSONAL ID REC NOT FOUND              
         BE    VKEY042                                                          
         CLI   PIDIOERR,IOEDEL     CHECK PERSONAL ID REC DELETED                
         BE    VKEY042                                                          
         LA    R1,PERPIDH          SET CURSOR AND DISPLAY MESSAGE               
         ST    R1,FVADDR                                                        
         B     SAEPEA                                                           
VKEY042  BRAS  RE,READPWD                                                       
         BNE   VALKEYX                                                          
         CLI   PWDIOERR,IOERNF     CHECK PASSWORD REC NOT FOUND                 
         BE    VKEY044                                                          
         LA    R1,PERPWDH          SET CURSOR AND DISPLAY MESSAGE               
         ST    R1,FVADDR                                                        
         B     SAEPWA                                                           
VKEY044  BRAS  RE,INITDEFT         CLEAR DATE KEY SAVE TABLE                    
         MVI   APINDS,APIOKADD     INDICATE OK TO ADD                           
         CLI   APACTN,ACTCPY                                                    
         BNE   VKX200                                                           
*                                  REREAD RECORDS FOR COPY                      
         MVC   IOKEY(L'SAVPEKEY),SAVPEKEY                                       
         GOTO1 AIO,IOLOCK+IORDD+IOCONFIL+IO1                                    
         BNE   SAERNF                                                           
         MVC   IOKEY(L'SAV0KEY),SAV0KEY                                         
         GOTO1 AIO,IOLOCK+IORDD+IOCONFIL+IO3                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VKX200                                                           
*                                                                               
VKEY050  CLI   APACTN,ACTCNV       PROCESS CONVERT ACTION                       
         BNE   VKEY070                                                          
         BRAS  RE,READPID          READ FILE                                    
         BNE   VALKEYX                                                          
         CLI   PIDIOERR,IOERNF     CHECK PERSONAL ID REC NOT FOUND              
         BE    VKEY052                                                          
         LA    R1,PERPIDH          SET CURSOR AND DISPLAY MESSAGE               
         ST    R1,FVADDR                                                        
         B     SAEPEA                                                           
VKEY052  BRAS  RE,READPWD                                                       
         BNE   VALKEYX                                                          
         CLI   PWDIOERR,0          CHECK PASSWORD REC FOUND OK                  
         BE    VKEY054                                                          
         LA    R1,PERPWDH          SET CURSOR AND DISPLAY MESSAGE               
         ST    R1,FVADDR                                                        
         B     SAERNF                                                           
VKEY054  OC    APWORK,APWORK       CHECK PASSWORD RECORD OLD TYPE               
         BZ    VKEY056                                                          
         LA    R1,PERPWDH          SET CURSOR AND DISPLAY MESSAGE               
         ST    R1,FVADDR                                                        
         B     SAEPAC                                                           
VKEY056  MVC   OLDPWD,APHALF       SAVE PASSWORD NUMBER                         
         BRAS  RE,INITDEFT         CLEAR DATE KEY SAVE TABLE                    
         MVI   APINDS,APIOKCHA+APIOKDIS                                         
         B     VKX200                                                           
*                                  PROCESS OTHER ACTIONS                        
VKEY070  EQU   *                                                                
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    VKEY100                                                          
         TM    PIDIND,FVITHIS      RESOLVE NEW AND PREVIOUS INPUT               
         BNZ   VKEY090               OVERIDE KEY WITH THIS TIME INPUT           
         TM    PWDIND,FVITHIS        OF EITHER PERSONAL ID OR PASSWORD          
         BZ    VKEY090                                                          
         OC    OLDCODE,OLDCODE                                                  
         BZ    VKEY090                                                          
VKEY080  BRAS  RE,READPWD          GET KEY WITH NEW PASSWORD CODE               
         BNE   VALKEYX                                                          
         CLI   PWDIOERR,IOERNF     CHECK REC NOT FOUND                          
         BNE   *+14                                                             
         XC    APRECKEY,APRECKEY   FIXES LIST/CHANGE PASSWORD BUG               
         B     VKEY110                                                          
         MVC   OLDPID,APWORK                                                    
         B     VKEY100                                                          
VKEY090  OC    OLDPID,OLDPID                                                    
         BNZ   VKEY100                                                          
         OC    OLDCODE,OLDCODE                                                  
         BNZ   VKEY080                                                          
         B     VALKEYX                                                          
*                                  READ CURRENT PERSON RECORD AND ITS           
*                                    ASSOCIATED PASSWORD RECORD                 
VKEY100  MVC   PERPID,OLDPID                                                    
         OI    PERPIDH+(FVOIND-FVIHDR),FVOXMT                                   
         BRAS  RE,READPID                                                       
         BNE   VALKEYX                                                          
         CLI   PIDIOERR,IOERNF     CHECK REC NOT FOUND                          
         BE    VKEY110                                                          
         MVC   OLDCODE,APWORK                                                   
         MVC   OLDCODU,APWORK                                                   
         MVC   PERPWD,OLDCODE                                                   
         BRAS  RE,HIDEPWD                                                       
         OI    PERPWDH+(FVOIND-FVIHDR),FVOXMT                                   
         BRAS  RE,READPWD                                                       
         BNE   VALKEYX                                                          
         BRAS  RE,CRPIDPWD                                                      
         BNE   VALKEYX             OR RECORDS INCOMPATIBLE                      
*                                                                               
VKEY110  CLI   IOERR,0                                                          
         BE    VKEY120             RECORD FOUND OK                              
         TM    IOERR,IOEDEL                                                     
         BNZ   VKX100              PROCESS DELETED RECORD                       
         BRAS  RE,INITDEFT                                                      
         B     VKX200                                                           
*                                                                               
VKEY120  CLC   PDEFPID,OLDPID      IF PERSONAL ID CHANGED                       
         BE    VKDEF                                                            
         BRAS  RE,INITDEFT           BUILD EFFECTIVE DATE KEY TABLE             
         L     RF,=A(LOADDEFT)                                                  
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   VALKEYX                                                          
         MVC   PDEFPID,OLDPID                                                   
         B     VKDEF                                                            
         EJECT                                                                  
***********************************************************************         
* CONTINUE VALIDATE KEY: PROCESS EFFECTIVE DATE KEY FIELD             *         
* NOTE:                                                               *         
*     THE EFFECTIVE DATE OF THE PERSON RECORD IS HELD IN BINARY       *         
* 2'S COMPLEMENT FORM IN THE RECORD KEY.                              *         
*                                                                     *         
* ON ADD:                                                             *         
*     THE CURRENT DATE IS AUTOMATICALLY USED.                         *         
*                                                                     *         
* ON CHANGE:                                                          *         
*     IF NO VALUE IS ENTERED THE ADD DATE IS PRESERVED AND CHANGES    *         
* TO THE RECORD ARE MADE AS NORMAL, TAKING EFFECT IMMEDIALTELY.       *         
* IF A VALUE IS ENTERED THE RECORD IS SAVED WITH ITS OLD DATE AND     *         
* A NEW COPY IS MADE WHICH RECORDS THE NEW CHANGES TO RECORD DATA     *         
* TAKING EFFECT ON THE SPECIFIED DATE.                                *         
*     IN THIS WAY AN HISTORICAL RECORD IS MAINTAINED OF PAST CHANGES  *         
* AND CHANGES CAN BE PREPARED TO TAKE EFFECT NOW OR IN THE FUTURE     *         
* A DATE PREVIOUS TO THE PRESENT ONE CAN NOT BE SPECIFIED FOR A       *         
* CHANGE TO THE RECORD AND CERTAIN FIELDS CAN NOT BE UPDATED IN FUTURE*         
*                                                                     *         
* ON DISPLAY:                                                         *         
*     THE PAST, CURRENT AND FUTURE RECORDS FOR A PERSON CAN BE        *         
* DISPLAYED BY ENTERING THE NEAREST DATE AFTER OR ONE OF THE          *         
* FOLLOWING SPECIAL CODES:                                            *         
* '+'  ADVANCE TO NEXT RECORD IN TIME.                                *         
* '-'  STEP BACK TO LAST RECORD IN  TIME.                             *         
* '(F)IRST' DISPLAY OLDEST RECORD.                                    *         
* '(L)AST' DISPLAY LATEST RECORD.                                     *         
* '(N)OW', '(C)URRENT' AND 'T)ODAY' DISPLAY CURRENT RECORD.           *         
*                                                                     *         
***********************************************************************         
VKDEF    MVI   APINDS,0                                                         
         TM    PIDIND,FVITHIS                                                   
         BNZ   VKX100              IGNORE IF PERSONAL ID INPUT                  
         TM    PWDIND,FVITHIS                                                   
         BNZ   VKX100              IGNORE IF PASSWORD INPUT                     
         MVI   FVMINL,0            READ EFFECTIVE DATE FIELD                    
         GOTO1 AFVAL,PERDEFH                                                    
         BH    VALKEYX                                                          
         BE    VKACT                                                            
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,TODAY),(X'51',APWORK)                          
         MVC   PERDEF,APWORK                                                    
         OI    PERDEFH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 AFVAL,PERDEFH                                                    
         BH    VALKEYX                                                          
         B     VKACT                                                            
*                                  PROCESS SPECIAL ACTIONS                      
VKACT    LA    R4,PDEFTAB          EFFECTIVE DATE KEY LAST DISPLAY              
         CLI   FVIFLD,C'-'         GET PREVIOUS RECORD                          
         BE    VKMINUS                                                          
         CLI   FVIFLD,C'+'         GET NEXT RECORD                              
         BE    VKPLUS                                                           
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDLAST          GET LAST RECORD                              
         BE    VKLAST                                                           
         EX    RF,FLDFIRST         GET FIRST RECORD                             
         BE    VKFIRST                                                          
         EX    RF,FLDNOW           GET CURRENT RECORD                           
         BE    VKNOW                                                            
         EX    RF,FLDCURR                                                       
         BE    VKNOW                                                            
*                                  PROCESS VALID DATE INPUT                     
VKDATE   ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BNE   SAEED1                                                           
         CLI   APACTN,ACTCHA                                                    
         BE    VKD010                                                           
         CLC   OLDDEF,APWORK+PVALCSTA-PERVALD                                   
         BNE   VKD010              IF SAME AS DATE CURRENTLY DISPLAYED          
         MVC   ODISPDEF,OCURPDEF     SET KEY TABLE POINTER                      
         B     VKX100                PROCESS AS NORMAL                          
*                                  IF CHANGE SAVE NEW EFFECTIVE DATE            
VKD010   MVC   APHALF,FFILL          ELSE CONVERT TO COMPLEMENT                 
         XC    APHALF,APWORK+PVALCSTA-PERVALD                                   
         CLI   APACTN,ACTCHA       IF CHANGE ACTION SAVE NEW DATE               
         BNE   VKD020                                                           
         MVC   NEWDEF,APWORK+PVALCSTA-PERVALD                                   
         CLC   NEWDEF,TODAY          WHICH MUST BE TODAY OR FUTURE              
         BNL   VKD012                                                           
         MVC   NEWDEF,TODAY                                                     
*                                                                               
VKD012   XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,NEWDEF),(X'51',APWORK)                         
         MVC   PERDEF,APWORK                                                    
         OI    PERDEFH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   NEWDEFC,FFILL                                                    
         XC    NEWDEFC,NEWDEF                                                   
         XC    PDEFPID,PDEFPID     UPDATE KEY SAVE TABLE ON NEXT PASS           
*                                                                               
VKD020   OC    OFRSTDEF,OFRSTDEF   FIND CLOSEST VALUE IN KEY TABLE              
         BZ    VKUPDATE              ONLY ONE ENTRY SKIP SEARCH                 
         LR    RF,R4                 ELSE SEARCH TABLE                          
         A     RF,OFRSTDEF                                                      
         LA    RE,PDEFTLEN                                                      
         SR    RF,RE                                                            
         CLC   APHALF,0(R4)        LOOP DOWN TABLE                              
         BNH   VKUPDATE              TO FIND PREVIOUS VALUE                     
         BXLE  R4,RE,*-10                                                       
         B     VKUPDATE                                                         
*                                  UPDATE EFFECTIVE DATE FROM KEY TABLE         
VKNOW    A     R4,OCURPDEF         POINT TO CURRENT DATE KEY                    
         B     VKUPDATE                                                         
VKFIRST  A     R4,OFRSTDEF         POINT TO FIRST DATE KEY                      
         B     VKUPDATE                                                         
VKLAST   B     VKUPDATE            POINT TO LAST DATE KEY                       
VKPLUS   LA    RF,PDEFTAB          INCREMENT ALONG DATE KEYS                    
         MVI   DEFCUR,1            FLAG TO SET CURSOR ON DATE FIELD             
         A     R4,ODISPDEF                                                      
         CR    R4,RF                                                            
         BE    VKUPDATE                                                         
         LA    RE,PDEFTLEN                                                      
         SR    R4,RE                                                            
         B     VKUPDATE                                                         
VKMINUS  A     R4,ODISPDEF                                                      
         MVI   DEFCUR,1            FLAG TO SET CURSOR ON DATE FIELD             
         LA    R4,PDEFTLEN(R4)     DECREMENT ALONG DATE KEYS                    
         LA    RF,PDEFTAB                                                       
         A     RF,OFRSTDEF                                                      
         CR    R4,RF                                                            
         BNH   VKUPDATE                                                         
         LR    R4,RF                                                            
         B     VKUPDATE                                                         
*                                  UPDATE EFFECTIVE DATE KEY                    
VKUPDATE MVC   OLDDEFC,0(R4)         AND SAVE TABLE POINTERS                    
         MVC   OLDDEF,FFILL                                                     
         XC    OLDDEF,OLDDEFC                                                   
         LA    RF,PDEFTAB                                                       
         SR    R4,RF                                                            
         ST    R4,ODISPDEF         POINTER TO NEW KEY                           
         XC    OLDCODE,OLDCODE     CLEAR LAST PASSWORD                          
         XC    OLDCODU,OLDCODU                                                  
*                                                                               
         CLI   APACTN,ACTDIS       ACTION MUST BE DISPLAY,CHANGE DELETE         
         BE    VKU010                WITH UPDATED EFFECTIVE DATE                
         CLI   APACTN,ACTDEL                                                    
         BE    VKU010                                                           
         CLI   APACTN,ACTCHA                                                    
         BNE   SAEED2                                                           
         CLC   ODISPDEF,OCURPDEF   CANT CHANGE OLD RECORD                       
         BH    SAEED3                                                           
*                                  READ PERSON RECORD WITH UPDATED DATE         
VKU010   BRAS  RE,READPID                                                       
         BNE   VALKEYX                                                          
         CLI   PIDIOERR,IOERNF     CHECK REC NOT FOUND                          
         BE    VALKEYX                                                          
         MVC   OLDCODE,APWORK        AND ASSOCIATED PASSWORD RECORD             
         MVC   OLDCODU,APWORK                                                   
         BRAS  RE,READPWD                                                       
         BNE   VALKEYX             EXIT IF INVALID CONDITION                    
         CLI   PWDIOERR,IOERNF     CHECK REC NOT FOUND                          
         BE    VKU040                                                           
* RESET IOERR HERE ??                                                           
         BRAS  RE,CRPIDPWD                                                      
         BNE   VALKEYX             OR RECORDS INCOMPATIBLE                      
         MVC   IOERR,PIDIOERR                                                   
         CLI   IOERR,0               ELSE SET IO INDICATORS                     
         BE    VKU020                                                           
         TM    IOERR,IOEDEL                                                     
         BE    VKU030                                                           
         B     VALKEYX                                                          
VKU020   MVI   APINDS,APIOKCHA+APIOKDIS+APIOKDEL                                
         B     VKUPDX                                                           
VKU030   MVI   APINDS,APIOKDIS                                                  
         B     VKUPDX                                                           
VKU040   MVI   APINDS,APIOKDIS+APIOKDEL                                         
         B     VKUPDX                                                           
*                                                                               
VKUPDX   EQU   *                                                                
         B     VKX200                                                           
                                                                                
***********************************************************************         
* COMPLETE VALIDATE KEY PROCESSING                                    *         
***********************************************************************         
*                                  SET IO INDICATORS FOR CURRENT                
VKX100   CLI   IOERR,0               EFFECTIVE DATE PROCESSING                  
         BNE   VKX110                                                           
         MVI   APINDS,APIOKDIS+APIOKDEL+APIOKCHA                                
         B     VKX200                                                           
VKX110   TM    IOERR,IOEDEL                                                     
         BZ    VALKEYX                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VKX200                                                           
*                                                                               
VKX200   EQU   *                                                                
         OC    OLDOID,OLDOID       TEST OFFICE MANAGER ACCESS                   
         BZ    VKX210                                                           
*&&UK                                                                           
         GOTO1 ATSTOMAN,OLDOID                                                  
         BNE   VKX206                                                           
         B     VKX210                                                           
*&&                                                                             
*&&US                                                                           
         OC    OLDDID,OLDDID       TEST OFFICE/DEPT MANAGER ACCESS              
         BZ    VKX210                                                           
         GOTO1 ATSTDMAN,APPARM,OLDOID,OLDDID                                    
         BE    VKX210                                                           
*&&                                                                             
VKX206   XC    PERPWD,PERPWD                                                    
         NI    PERPWDH+(FVOIND-FVIHDR),X'FF'-FVOXMT                             
         LA    R1,PERPIDH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         B     VALKEYX                                                          
*                                                                               
VKX210   LA    R2,APRECKEY                                                      
         MVC   PERPID,SAPEPID                                                   
         CLI   COPYFLAG,0                                                       
         BNE   *+10                                                             
         MVC   SAVPEKEY,SAPEKEY    SAVE PID KEY FOR COPY                        
*                                  UPDATE SCREEN KEY FIELDS                     
         OI    PERPIDH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R1,SA0KEYSV                                                      
         USING SA0REC,R1                                                        
         MVC   PERPWD,OLDCODE                                                   
*                                                                               
         CLI   APACTN,ACTADD           ADD/COPY?                                
         BE    *+12                                                             
         CLI   APACTN,ACTCPY                                                    
         BNE   VKX220                                                           
         CLC   OLDCODU,=CL10'***'                                               
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)   *** CANNOT BE THE PASSWORD               
         B     VALKEYX                                                          
*                                                                               
VKX220   BRAS  RE,HIDEPWD                                                       
         MVC   SAV0KEY,SA0KEY      SAVE PWD KEY FOR COPY                        
         OI    PERPWDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                  CHECK 'DDS' OVERRIDE PASSWORD                
         TM    CUSTAT,CUSDDS       IGNORE IF DDS TERMINAL                       
         BNZ   VKX300                                                           
         CLC   OLDCODU,=CL10'DDS'                                               
         BNE   VKX300                                                           
         CLI   APACTN,ACTDIS       OK FOR DISPLAY ACTION                        
         BE    VKX300                                                           
         B     SAEDDS                                                           
*                                                                               
VKX300   TM    CUSTAT,CUSDDL       DDLINK IN CONTROL?                           
         BZ    VKX400                                                           
         XC    DDLKAEPN,DDLKAEPN   CLEAR AEP PIN                                
         CLC   DDLKAEP,SPACES      AND IS THERE AN ACCESS EQUIV PID?            
         BE    VKX400                                                           
         L     RF,=A(VALAEP)       MAKE SURE IT EXISTS                          
         A     RF,APRELO                                                        
         BASR  RE,RF               (SETS DDLKAEPN)                              
         BE    VKX400                                                           
         MVC   FVDDLERR,=AL2(0705) DDLINK FIELD CODE FOR AEP ERROR              
         B     SAERNF              RECORD NOT FOUND                             
*                                                                               
VKX400   MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALKEYX                                                          
*                                  EXIT KEY VALIDATION                          
VALKEYX  J     EXIT                                                             
         DROP  R1                                                               
                                                                                
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A PERSON AND PASSWORD RECORDS              *         
***********************************************************************         
VALREC   EQU   *                                                                
         XC    TERMCHG,TERMCHG                                                  
*                                                                               
         TM    CUSTAT,CUSDDL       DDLINK ACTIVE?                               
         BZ    VALREC0                                                          
         CLI   APACTN,ACTADD       YES, ADD ACTION?                             
         BE    VALREC0                                                          
         CLC   DDLKPWD,SPACES      NO, PASSWORD PASSED?                         
         BE    VALREC0                                                          
         MVC   PERPWDN,DDLKPWD     YES, MOVE TO NEW PASSWORD FIELD              
*                                                                               
VALREC0  CLI   PDEFXFLG,C'Y'                                                    
         BNE   VALREC1                                                          
         MVC   FVMSGNO,=AL2(CE#HPERR)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     VALRECX                                                          
*                                                                               
VALREC1  XC    PELLOSV,PELLOSV     CREATE/SAVE LAST LOGON DATE ELEMENT          
         XC    PELLODE,PELLODE                                                  
*                                                                               
         LA    R3,PELLODE                                                       
         USING SALLOD,R3                                                        
         MVI   SALLOEL,SALLOELQ                                                 
         MVI   SALLOLEN,SALLOLNQ                                                
         MVC   SALLODT,=X'640102'  DEFAULT DATE=01/02/00 ON ADD                 
         CLI   APACTN,ACTCPY                                                    
         BE    VRCPY                                                            
*                                                                               
         CLI   APACTN,ACTADD                                                    
         BNE   *+14                                                             
         MVC   PELLOSV,PELLODE     ADD NEW RECORDS/COPIES WITH DEFAULT          
         B     VALREC5                                                          
*                                                                               
         L     R2,AIOAREA1         POINT TO PERSON RECORD                       
         LA    R3,SAPEDATA                                                      
VALREC2  CLI   0(R3),0             SCAN FOR LAST LOGON DATE ELEMENT             
         BE    VALREC5                                                          
         CLI   0(R3),SALLOELQ                                                   
         BE    VALREC3                                                          
         LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VALREC2                                                          
*                                                                               
VALREC3  CLC   PERPWDN,SPACES      ARE WE CHANGING THE PASSWORD?                
         BNH   VALREC4             NO: DON'T TOUCH THIS ELEMENT                 
         CLI   SALLOLEN,SALLOCNT-SALLOEL IS LENGTH SUFFICIENT?                  
         BNH   VALREC4             NO: DON'T TOUCH THIS ELEMENT                 
         MVI   SALLOCNT,0          YES: RESET VIOLATION COUNT & FLAGS           
         MVI   SALLOFLG,0                                                       
*                                                                               
VALREC4  MVC   PELLOSV,0(R3)       SAVE LAST LOGON DATE ELEMENT                 
*                                                                               
VALREC5  L     R2,AIOAREA1         SET UP PERSON RECORD FOR ADD/CHANGE          
         XC    SAPEKEY(256),SAPEKEY                                             
         MVC   SAPEKEY(L'SAPEKEY),APRECKEY                                      
         LA    R0,SAPEDATA+1-SAPEREC                                            
         STCM  R0,3,SAPELEN                                                     
         OC    PELLOSV,PELLOSV     TEST IF SAVED LAST LOG ON ELEMENT            
         BZ    VALREC8                                                          
*                                                                               
         LLC   R1,PELLOSV+1         GET LENGTH OF SAVED ELEMENT                 
         AHI   R1,-1                                                            
         BNP   VALREC8                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAPEDATA(0),PELLOSV  MOVE SAVED ELEM TO EMPTY RECORD             
*                                                                               
         LLC   R1,PELLOSV+1        GET LENGTH OF SAVED ELEMENT                  
         AR    R0,R1                                                            
         STCM  R0,3,SAPELEN                                                     
*                                                                               
VALREC8  L     R2,AIOAREA3         SET UP PASSWD RECORD                         
         USING SA0REC,R2                                                        
         OI    SA0STAT,X'40'       FLAG NEW PERSON PASSWORD FORMAT              
         CLI   APACTN,ACTADD       CHECK IF ADD ACTION                          
         BNE   VRCNV                                                            
         XC    PERAGRN,PERAGRN     CLEAR LAST SCREEN DISPLAY FIELDS             
         OI    PERAGRNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERAPCN,PERAPCN                                                  
         OI    PERAPCNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERLAGN,PERLAGN                                                  
         OI    PERLAGNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PEROFFN,PEROFFN                                                  
         OI    PEROFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERDIDN,PERDIDN                                                  
         OI    PERDIDNH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         TM    PERPCNH+(FVATRB-FVIHDR),FVAPROT   IS THIS FIELD PROT'ED?         
         BZ    *+12                                                             
         MVI   PERPCN,C'Y'         DEFAULT TO C'Y'                              
         OI    PERPCNH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         XC    SA0KEY(256),SA0KEY                                               
         MVC   SA0KEY(L'SA0KEY),SA0KEYSV                                        
         OI    SA0STAT,X'40'       FLAG NEW PERSON PASSWORD FORMAT              
         LA    R0,SA0DATA+1-SA0REC                                              
         STCM  R0,3,SA0LEN                                                      
         MVC   PEFSTASV,OLDDEF                                                  
         MVC   PEFENDSV,FFILL                                                   
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BNE   VRPID                                                            
*                                  ADD FIRST PASSWORD HISTORY ELEMENT           
         GOTOR BLDPWD,OLDCODE                                                   
         GOTO1 AADDELS,SA0REC                                                   
         B     VRPID               GO AND VALIDATE DATA                         
*                                  SET UP PASSWORD RECORD FOR CONVERT           
VRCNV    CLI   APACTN,ACTCNV         UNLESS OTHER ACTION                        
         BNE   VRCHA                                                            
*                                  ??PP??                                       
         MVC   PEFSTASV,TODAY                                                   
         MVC   PEFENDSV,FFILL                                                   
         LA    R3,SA0DATA          SAVE ELEMENT DATA                            
VRCNV010 CLI   0(R3),0             TEST E-O-R                                   
         BE    VRCNVX                                                           
         USING SAPASD,R3                                                        
         CLI   SAPASEL,SAPASELQ    PASSWORD POINTER ELEMENT                     
         BE    VRCNV030                                                         
VRCNV020 SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRCNV010                                                         
VRCNV030 CLI   SAPASLEN,X'18'      SAVE OLD NAME POINTER ELEMENT                
         BNE   VRCNV020                                                         
         MVC   PNAMEKEY,SAPASDTA                                                
         B     VRCNV020                                                         
VRCNVX   B     VRCHA                                                            
*                                  SET UP PASSWORD RECORD FOR CHANGE            
*                                    ALSO HERE IF CONVERT ACTION                
VRCHA    LA    R3,SA0DATA                                                       
VRCHA010 CLI   0(R3),0             TEST E-O-R                                   
         BE    VRCHAX                                                           
         CLI   0(R3),SAACVELQ      ACTIVITY ELEMENT                             
         BE    VRCHA030                                                         
         CLI   0(R3),SAPASELQ      PASSWORD # POINTER ELEMENT                   
         BE    VRCHA030                                                         
         CLI   0(R3),SAPALELQ      PERSONAL-ID POINTER ELEMENT                  
         BE    VRCHA030                                                         
         CLI   0(R3),SAPEFELQ      EFFECTIVE DATES ELEMENT                      
         BE    VRCHA050                                                         
         B     VRCHA040                                                         
*                                                                               
VRCHA030 SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),SA0REC),0,0                    
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRCHA010                                                         
*                                  BUMP TO NEXT ELEMENT                         
VRCHA040 SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRCHA010                                                         
*                                  SAVE PASSWORD EFFECTIVE DATES                
         USING SAPEFD,R3                                                        
VRCHA050 MVC   PEFSTASV,SAPEFSTA                                                
         MVC   PEFENDSV,SAPEFEND                                                
         B     VRCHA030            DELETE ELEMENT                               
*                                                                               
VRCHAX   B     VRPID               GO AND VALIDATE DATA                         
         DROP  R2                                                               
*                                  PROCESS COPY ACTION                          
         USING SAPEREC,R2                                                       
VRCPY    NI    TWAMODE,X'FF'-TWAMDFR                                            
         L     R2,AIOAREA1         SET UP PERSON RECORD FOR COPY                
         MVC   SAPEKEY(L'SAPEKEY),APRECKEY                                      
         LA    R3,SAPEDATA                                                      
VRCPY010 CLI   0(R3),0             TEST E-O-R                                   
         BE    VRCPY100                                                         
         CLI   0(R3),SAACVELQ      ACTIVITY ELEMENT                             
         BE    VRCPY020                                                         
         CLI   0(R3),SALLOELQ      LAST LOGON ELEMENT                           
         BE    VRCPY020                                                         
         CLI   0(R3),SAPWDELQ      PASSWORD POINTER ELEMENT                     
         BE    VRCPY020                                                         
         CLI   0(R3),SANAMELQ      PERSON NAME ELEMENT                          
         BE    VRCPY020                                                         
         CLI   0(R3),SAADRELQ      PERSON ADDRESS ELEMENT                       
         BE    VRCPY020                                                         
         CLI   0(R3),SAPERELQ      PERSON DETAILS ELEMENT                       
         BE    VRCPY040                                                         
         CLI   0(R3),SAPEEELQ      PERSON EMAIL ELEMENT                         
         BE    VRCPY020                                                         
         CLI   0(R3),SAAGCELQ      ACCESS GROUP ELEMENT                         
         BE    VRCPY050                                                         
         CLI   0(R3),SALACELQ      LIMIT ACCESS GROUP ELEMENT                   
         BE    VRCPY060                                                         
         CLI   0(R3),SAAEPELQ      ACCESS EQUIVALENT PID ELEMENT                
         BE    VRCPY020                                                         
         B     VRCPY030                                                         
*                                                                               
VRCPY020 SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),SAPEREC),0,0                   
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRCPY010                                                         
*                                  BUMP TO NEXT ELEMENT                         
VRCPY030 SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRCPY010                                                         
*                                  SAVE PERSON DETAILS                          
         USING SAPERD,R3                                                        
VRCPY040 MVC   NEWOID,SAPEROFF                                                  
         MVC   NEWDID,SAPERDID                                                  
*                                                                               
         NI    SAPEROVF,X'FF'-COVFON  SET OVERRIDE FLAG OFF WHEN COPY           
         MVI   SAPERPCN,C'Y'          SET PWD CNTL FLAG OFF WHEN COPY           
         B     VRCPY030                                                         
*                                  SAVE ACCESS GROUP DATA                       
         USING SAAGCD,R3                                                        
VRCPY050 MVC   NEWAGC,SAAGCCOD                                                  
         MVC   NEWAGN,SAAGCNUM                                                  
         B     VRCPY030                                                         
*                                  SAVE LIMIT ACCESS GROUP DATA                 
         USING SALACD,R3                                                        
VRCPY060 MVC   NEWLAC,SALACCOD                                                  
         MVC   NEWLACN,SALACNUM                                                 
         B     VRCPY030                                                         
*                                  DEFAULT LAST LOG ON ELEMENT                  
VRCPY100 MVC   APELEM(SALLOLNQ),PELLODE                                         
         GOTO1 AADDELS,SAPEREC                                                  
*                                                                               
         USING SA0REC,R2                                                        
         L     R2,AIOAREA3         SET UP PASSWD RECORD FOR COPY                
         MVC   SA0KEY(L'SA0KEY),SA0KEYSV                                        
         MVC   PEFSTASV,TODAY                                                   
         MVC   PEFENDSV,FFILL                                                   
         LA    R3,SA0DATA                                                       
VRCPY110 CLI   0(R3),0             TEST E-O-R                                   
         BE    VRCPYX                                                           
         CLI   0(R3),SAACVELQ      ACTIVITY ELEMENT                             
         BE    VRCPY130                                                         
         CLI   0(R3),SALLOELQ      LAST LOGON ELEMENT                           
         BE    VRCPY130                                                         
         CLI   0(R3),SAPASELQ      PASSWORD # POINTER ELEMENT                   
         BE    VRCPY130                                                         
         CLI   0(R3),SAPALELQ      PERSONAL-ID POINTER ELEMENT                  
         BE    VRCPY130                                                         
         CLI   0(R3),SAPEFELQ      EFFECTIVE DATES ELEMENT                      
         BE    VRCPY130                                                         
         CLI   0(R3),SASYSELQ      SYSTEM ELEMENTS                              
         BE    VRCPY140                                                         
         B     VRCPY150                                                         
*                                                                               
VRCPY130 SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),SA0REC),0,0                    
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VRCPY110                                                         
*                                  BUMP TO NEXT ELEMENT                         
         USING SASYSD,R3                                                        
VRCPY140 EQU   *                                                                
         GOTO1 ATSTSYS,SASYSNUM    TEST USER CAN CONNECT TO SYSTEM              
         BNE   SAENAS                                                           
         B     VRCPY150                                                         
*                                  SYSTEM ELEMENTS                              
VRCPY150 SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VRCPY110                                                         
*                                                                               
VRCPYX   EQU   *                                                                
         L     R2,AIOAREA1         RESTORE SECURITY RECORD POINTER              
*                                  ADD PWD W/ CURRENT PWD ON SCREEN             
         CLI   PIDREQD,C'Y'        IF PERSONAL ID PASSWORD                      
         BNE   VRNAM               ADD FIRST PASSWORD HISTORY ELEMENT           
         GOTOR CHGPWD,OLDCODE                                                   
         B     VRNAM               CAN UPDATE NAME/ADDRESS ON COPY              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* VALIDATE ANY NEW PERSONAL ID                                        *         
***********************************************************************         
         USING SAPEREC,R2                                                       
VRPID    L     R2,AIOAREA1         RESTORE SECURITY RECORD POINTER              
*                                                                               
VRPID010 MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PERPIDNH      VALIDATE NEW PERSONAL ID                     
         BL    VRPID100                                                         
*                                                                               
         CLC   PERPIDN(2),=C'P='                                                
         BE    SAEIIF              P= CAN'T BE USED FOR PID NAME                
*                                                                               
         CLI   FVILEN,3                                                         
         BL    SAEFTS                                                           
         OC    NEWDEF,NEWDEF       CAN NOT UPDATE WITH NEW                      
         BZ    VRPID020              EFFECTIVE DATE NOT TODAY                   
         CLC   NEWDEF,TODAY                                                     
         BNE   SAEPED                                                           
*                                                                               
VRPID020 CLI   APACTN,ACTADD         OR ACTION ADD                              
         BE    SAEIIF                                                           
         CLI   APACTN,ACTCNV         OR ACTION CONVERT                          
         BE    SAEIIF                                                           
*                                                                               
         CLC   OLDPID,FVIFLD       CHANGE PERSONAL ID                           
         BE    VRPID100              UNLESS SAME ENTERED                        
         LA    R2,IOKEY            ENSURE NOT USED AND ACTIVE                   
         XC    SAPEKEY,SAPEKEY                                                  
         MVC   SAPEKEY,APRECKEY                                                 
         MVC   SAPEPID,FVIFLD                                                   
         XC    SAPEDEF,SAPEDEF                                                  
         GOTO1 AIO,IOHID+IOCONFIL+IO2                                           
         BL    SAEIIO                                                           
         BE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         BZ    VRPID030                                                         
         L     R2,AIOAREA2                                                      
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY                                   
         BE    SAERAE                                                           
         L     R2,AIOAREA1                                                      
*                                                                               
VRPID030 MVC   NEWPID,FVIFLD       SAVE NEW PID                                 
         B     VRPIDX                                                           
*                                                                               
VRPID100 EQU   *                                                                
         B     VRPIDX                                                           
*                                                                               
VRPIDX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE ANY NEW PASSWORD CODE                                      *         
***********************************************************************         
                                                                                
VRPWD    L     R2,AIOAREA1                                                      
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PERPWDNH      VALIDATE NEW PASSWORD                        
         BL    VRPWDX                                                           
         CLI   FVILEN,3            CHECK PASSWORD MINIMUM LENGTH                
         BL    SAEFTS                                                           
         CLI   PWDMINLN,3                                                       
         BNH   *+14                                                             
         CLC   FVILEN,PWDMINLN                                                  
         BL    SAEFTS                                                           
*                                                                               
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD?                   
         BNE   VRPW010             NO                                           
         CLC   NEWDEF,TODAY        FUTURE DATE?                                 
         BH    SAEFPE              YES - CAN'T CHANGE IT FOR PPS PID            
*                                                                               
         CLC   ODISPDEF,OCURPDEF   CHECK IF THIS IS CURRENT EFF DATE            
         BNE   SAEFPE                                                           
         B     *+14                                                             
VRPW010  OC    ODISPDEF,ODISPDEF   CHECK IF EFFECTIVE DATES PENDING             
         BNZ   SAEFPE                                                           
*                                                                               
         CLI   APACTN,ACTADD       CANT UPDATE WITH ACTION ADD,CONVERT          
         BE    SAEIIF                                                           
         CLI   APACTN,ACTCNV                                                    
         BE    SAEIIF                                                           
         CLI   PWDMINLN,3          CHECK MINIMUM LENGTH                         
         BNH   *+14                                                             
         CLC   FVILEN,PWDMINLN                                                  
         BL    SAEFTS                                                           
         LLC   R1,FVILEN                                                        
         LA    RE,FVIFLD                                                        
         CLI   0(RE),C' '          CANT HAVE SPACE IN PASSWORD                  
         BE    SAESPIP                                                          
         AHI   RE,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
VRPW020  MVC   NEWCODE,FVIFLD      VALIDATE NEW INPUT PASSWORD                  
         XC    APPARM(20),APPARM                                                
         LA    RE,NEWCODE                                                       
         ST    RE,APPARM                                                        
         MVC   APPARM+0(1),FVILEN                                               
         LA    RE,APWORK           TEXT AREA TO RETURN UPPER CASE VALUE         
         ST    RE,APPARM+4                                                      
         XC    0(10,RE),0(RE)                                                   
         MVC   APPARM+4(1),PWDRULE SET PASSWORD RULE NUMBER                     
         L     RF,ASYS                                                          
         L     RF,VSSB-SYSFACD(RF) GET SSB                                      
         ST    RF,APPARM+8                                                      
         MVI   APPARM+8,3          SET VALIDATE ACTION                          
         GOTO1 =V(PWDVAL),APPARM,RR=APRELO                                      
*                                                                               
         SR    RF,RF               GET ERROR NUMBER                             
         ICM   RF,1,8(R1)                                                       
         BZ    VRPW025                                                          
         CHI   RF,231              ERROR RETURN E2-E7=226-231                   
         BH    *+12                                                             
         AHI   RF,116              ERROR MESSAGES CON=342-347                   
         B     VRPW023                                                          
         IC    RF,PWDRULE          GET RULE NUMBER                              
         AHI   RF,360              ERROR MESSAGES CON=361-372                   
VRPW023  STH   RF,FVMSGNO                                                       
         B     NO                                                               
VRPW025  MVC   NEWCODU,APWORK      SAVE UPPER CASE VERSION                      
*                                                                               
VRPW030  CLC   OLDCODE,NEWCODE     CHANGE PASSWORD UNLESS SAME ENTERED          
         BNE   VRPW032                                                          
         XC    NEWCODE,NEWCODE     ZEROS MEANS NO NEW PASSWORD NEEDED           
         B     VRPWDX                                                           
VRPW032  CLC   OLDCODU,=CL10'DDS'  CANNOT CHANGE 'DDS' PASSWORD                 
         BE    SAEDDS                                                           
         CLC   NEWCODU,=CL10'***'  CANNOT USE '***' AS PASSWORD                 
         BE    SAEIIF                                                           
         CLC   NEWCODU,=CL10'DDS'  CANNOT SET PWD TO 'DDS'                      
         BE    SAEPWDRS                                                         
*                                                                               
VRPW040  CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    VRPW050                                                          
         L     R2,AIOAREA1         NO - THIS MUST BE UNIQUE                     
         LA    R1,IOKEY                                                         
         USING SA0REC,R1                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KCODE,NEWCODE                                                 
         GOTO1 AIO,IORDD+IOCONFIL+IO2                                           
         BL    SAEIIO                                                           
         BE    SAERAE                                                           
         TM    IOERR,IOEEOF                                                     
         BNZ   VRPWDX              EOF                                          
         TM    IOERR,IOERNF                                                     
         BNZ   VRPWDX              CONTINUE WITH EXISTING NUMBER                
         TM    IOERR,IOEDEL                                                     
         BNZ   VRPWDX                                                           
         B     SAEIIO                                                           
*                                                                               
VRPW050  GOTO1 =A(CHGPWD),NEWCODE,RR=APRELO                                     
         BNE   VALRECX                                                          
*                                                                               
VRPWDX   EQU   *                                                                
         DROP  R1                                                               
                                                                                
***********************************************************************         
* BUILD NAME ELEMENT IN PERSON RECORD                                 *         
***********************************************************************         
VRNAM    LA    R3,APELEM                                                        
         USING SANAMD,R3           BUILD NAME ELEMENT                           
         XC    SANAMEL(SANAMLNQ),SANAMEL                                        
         MVI   SANAMEL,SANAMELQ                                                 
         MVI   SANAMLN,SANAMLNQ                                                 
*                                                                               
         MVI   FVMINL,1            VALIDATE PERSON NAME                         
         GOTO1 AFVAL,PERFNAMH                                                   
         BNE   VALRECX                                                          
         OI    SANAMIND,SANAMIFN                                                
         ZIC   R1,SANAMLN                                                       
         LA    RE,SANAMD(R1)                                                    
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),FVIFLD                                                   
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)                                                         
         LA    RF,1(RF,R1)                                                      
         STC   RF,SANAMLN                                                       
*                                                                               
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,PERMNAMH      VALIDATE MIDDLE NAME                         
         BNE   VRNAM010                                                         
         OI    SANAMIND,SANAMIMN                                                
         ZIC   R1,SANAMLN                                                       
         LA    RE,SANAMD(R1)                                                    
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),FVIFLD                                                   
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)                                                         
         LA    RF,1(RF,R1)                                                      
         STC   RF,SANAMLN                                                       
*                                                                               
VRNAM010 MVI   FVMINL,1            VALIDATE LAST NAME                           
         GOTO1 AFVAL,PERLNAMH                                                   
         BNE   VALRECX                                                          
         OI    SANAMIND,SANAMILN                                                
         ZIC   R1,SANAMLN                                                       
         LA    RE,SANAMD(R1)                                                    
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),FVIFLD                                                   
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)                                                         
         LA    RF,1(RF,R1)                                                      
         STC   RF,SANAMLN                                                       
*                                                                               
         GOTO1 AADDELS,SAPEREC                                                  
VRNAMX   EQU   *                                                                
         CLI   APACTN,ACTCPY                                                    
*&&UK*&& BE    VRPDT               CAN UPDATE HIRE/TERM DATES ON COPY           
*&&US*&& BE    VRPEE               CAN UPDATE EMAIL ADDRESS ON COPY             
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD ACCESS GROUP CODE ELEMENT                                     *         
***********************************************************************         
         USING SAAGCD,R3                                                        
VRAGC    LA    R3,APELEM           INITIALISE ELEMENT                           
         XC    SAAGCEL(SAAGCLNQ),SAAGCEL                                        
         MVI   SAAGCEL,SAAGCELQ                                                 
         MVI   SAAGCLN,SAAGCLNQ                                                 
         MVC   SAAGCCOD,OLDAGC                                                  
         MVC   SAAGCNUM,OLDAGN                                                  
         MVI   FVMINL,1            VALIDATE ACCESS GROUP FIELD                  
         GOTO1 AFVAL,PERAGRH                                                    
         BE    VRAGC010                                                         
         OC    OLDAGC,OLDAGC       IGNORE IF NOT SET                            
         BZ    VRAGCX                                                           
         BRAS  RE,CHECKUP          CHECK OK TO REMOVE                           
         BNE   VALRECX                                                          
         B     VRAGCX                                                           
*                                                                               
VRAGC010 CLC   SAAGCCOD,FVIFLD     SAME AS PREVIOUS                             
         BE    VRAGC020                                                         
         BRAS  RE,CHECKUP          CHECK OK TO CHANGE                           
         BNE   VALRECX                                                          
         MVC   SAAGCCOD,FVIFLD                                                  
*                                                                               
         USING SAAGREC,R2                                                       
VRAGC020 LA    R2,IOKEY            CHECK IT EXISTS                              
         XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,SAAGCCOD                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         L     R1,AIOAREA2                                                      
         GOTO1 AGETGNAM            GET ACCESS GROUP NUMBER                      
         MVC   SAAGCNUM,APHALF                                                  
         MVC   NEWAGC,SAAGCCOD     SAVE NEW VALUE CODE                          
         MVC   NEWAGN,SAAGCNUM     SAVE NEW VALUE NUMBER                        
*                                                                               
         USING SAPEREC,R2                                                       
         L     R2,AIOAREA1         ADD ELEMENT                                  
         GOTO1 AADDELS,SAPEREC                                                  
VRAGCX   EQU   *                                                                
*&&US                                                                           
***********************************************************************         
* BUILD DATA ACCESS GROUP CODE ELEMENT                                          
***********************************************************************         
         USING SA0REC,R4                                                        
VRLAC    L     R4,AIOAREA3         CHECK PSWD RECORD FOR LIMIT ACCESS           
         LA    R3,SA0DATA                                                       
         DROP  R4                                                               
*                                                                               
         XC    APHALF,APHALF       NO LIMIT ACCESS                              
VRLAC010 CLI   0(R3),0             TEST E-O-R                                   
         BE    VRLAC040                                                         
         CLI   0(R3),X'21'         SYSTEM ELEMENT                               
         BE    VRLAC030                                                         
VRLAC020 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VRLAC010                                                         
*                                                                               
         USING SASYSD,R3                                                        
VRLAC030 OC    SASYSLMT,SASYSLMT                                                
         BZ    VRLAC020                                                         
         MVI   APHALF,C'Y'         YES, THERE IS LIMIT ACCESS                   
         MVC   APHALF+1(1),SASYSNUM   FOR THIS SYSTEM                           
         DROP  R3                                                               
*                                                                               
         USING SALACD,R3                                                        
VRLAC040 LA    R3,APELEM           INITIALISE ELEMENT                           
         XC    SALACEL(SALACLNQ),SALACEL                                        
         MVI   SALACEL,SALACELQ                                                 
         MVI   SALACLN,SALACLNQ                                                 
         MVC   SALACCOD,OLDLAC                                                  
         MVC   SALACNUM,OLDLACN                                                 
         MVI   FVMINL,1            VALIDATE LIMIT ACCESS GROUP FIELD            
         GOTO1 AFVAL,PERLAGH                                                    
         BE    VRLAC050                                                         
         OC    OLDLACN,OLDLACN     IGNORE IF NOT SET                            
         BZ    VRLACX                                                           
         BRAS  RE,CHECKUP          CHECK OK TO REMOVE                           
         BNE   VALRECX                                                          
         B     VRLACX                                                           
*                                                                               
VRLAC050 CLI   APHALF,C'Y'         ANY LIMIT ACCESS DEFINED?                    
         BE    SAELAMC             . YES, CLEAN THAT UP FIRST                   
         CLC   SALACCOD,FVIFLD     SAME AS PREVIOUS                             
         BE    VRLAC060                                                         
         BRAS  RE,CHECKUP          CHECK OK TO CHANGE                           
         BNE   VALRECX                                                          
         MVC   SALACCOD,FVIFLD                                                  
*                                                                               
         USING SALAREC,R2                                                       
VRLAC060 LA    R2,IOKEY            CHECK IT EXISTS                              
         XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         MVC   SALAAGY,AGENCYID                                                 
         MVC   SALAAGR,SALACCOD                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         L     R1,AIOAREA2                                                      
         GOTO1 AGETLNAM                                                         
         MVC   SALACNUM,APHALF                                                  
*                                                                               
         MVC   NEWLAC,SALACCOD     SAVE NEW VALUE                               
         MVC   NEWLACN,SALACNUM    SAVE NEW VALUE                               
*                                                                               
         USING SAPEREC,R2                                                       
         L     R2,AIOAREA1         ADD ELEMENT                                  
         GOTO1 AADDELS,SAPEREC                                                  
VRLACX   EQU   *                                                                
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* BUILD PERSONNEL DETAILS ELEMENT IN PERSON RECORD                    *         
***********************************************************************         
                                                                                
         USING SAPERD,R3                                                        
VRPDT    LA    R3,APELEM           INITIALISE ELEMENT                           
         XC    SAPEREL(SAPERLNQ),SAPEREL                                        
         MVI   SAPEREL,SAPERELQ                                                 
*&&UK                                                                           
         CLI   APACTN,ACTCPY                                                    
         BNE   VRPDT005                                                         
         GOTO1 AGETELS,SAPEREC     USE EXISTING ELEMENT ON COPY                 
         ICM   RF,15,APPARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                MUST BE THERE                                
         LLC   R1,1(,RF)                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAPEREL(0),0(RF)    COPY EXISTING ELEMENT                        
         LLC   R0,SAPERLN                                                       
         MVI   SAPERLN,0                                                        
         GOTO1 ADELELS,SAPEREC     MAKE SURE OLD ONE DELETED                    
         STC   R0,SAPERLN                                                       
         B     VRPDT090            ONLY HIRE/TERM DATES ON COPY                 
*&&                                                                             
VRPDT005 MVI   SAPERLN,SAPERLNQ                                                 
*                                                                               
         MVC   SAPEROFF,OLDOID     VALIDATE OFFICE CODE                         
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PEROFFH                                                    
         BNE   VALRECX                                                          
         CLC   SAPEROFF,FVIFLD                                                  
         BE    VRPDT010            UNLESS SAME AS BEFORE                        
         BRAS  RE,CHECKUP            CHECK OK TO CHANGE                         
         BNE   VALRECX                                                          
         MVC   SAPEROFF,FVIFLD                                                  
*                                                                               
VRPDT010 EQU   *                                                                
*&&UK                                                                           
*                                  TEST OFFICE MANAGER ACCESS                   
         GOTO1 ATSTOMAN,FVIFLD                                                  
         BNE   VALRECX                                                          
*&&                                                                             
         OC    SAPEROFF,SAPEROFF                                                
         BZ    SAEMIF                                                           
         LA    R2,IOKEY            CHECK OFFICE RECORD EXISTS                   
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY                                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,SAPEROFF                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   NEWOID,SAPEROFF     SAVE NEW CODE                                
         BRAS  RE,GETAPEL                                                       
         BNE   *+16                                                             
         MVC   OFFAPC,APWORK                                                    
         MVC   OFFAPN,APHALF                                                    
*                                                                               
         MVC   SAPERDID,OLDDID                                                  
         MVI   FVMINL,2            VALIDATE DEPARTMENT CODE                     
         GOTO1 AFVAL,PERDIDH                                                    
         BNE   VALRECX                                                          
         CLC   SAPERDID,FVIFLD                                                  
         BE    VRPDT020            UNLESS SAME AS BEFORE                        
         BRAS  RE,CHECKUP            CHECK OK TO CHANGE                         
         BNE   VALRECX                                                          
         MVC   SAPERDID,FVIFLD                                                  
*                                                                               
VRPDT020 EQU   *                                                                
*&&US                                                                           
*                                  TEST OFFICE/DEPT MANAGER ACCESS              
         GOTO1 ATSTDMAN,APPARM,SAPEROFF,SAPERDID                                
         BNE   VALRECX                                                          
*&&                                                                             
*                                                                               
         OC    SAPEROFF,SAPEROFF                                                
         BZ    SAEODF                                                           
         OC    SAPERDID,SAPERDID                                                
         BZ    SAEMIF                                                           
         LA    R2,IOKEY            CHECK DEPARTMENT CODE EXISTS                 
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY                                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,SAPEROFF                                                 
         MVC   SADPDID,SAPERDID                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   NEWDID,SAPERDID     SAVE NEW CODE                                
         BRAS  RE,GETAPEL                                                       
         BNE   *+16                                                             
         MVC   DEPAPC,APWORK                                                    
         MVC   DEPAPN,APHALF                                                    
*                                                                               
         USING SAPEREC,R2                                                       
         MVI   FVMINL,1            VALIDATE STAFF CODE                          
         GOTO1 AFVAL,PERSTAH                                                    
         BNE   VRPDT030                                                         
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAPERSTA(0),FVIFLD                                               
*                                                                               
VRPDT030 MVI   FVMINL,1            VALIDATE EXTENSION NUMBER                    
         GOTO1 AFVAL,PEREXTH                                                    
         BNE   VRPDT050                                                         
         MVC   SAPEREXT,SPACES                                                  
         ZIC   R0,FVILEN                                                        
         LA    RF,FVIFLD                                                        
         LA    RE,SAPEREXT                                                      
VRPDT040 CLI   0(RF),C' '          COMPRESS OUT SPACES                          
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VRPDT040                                                      
*                                                                               
VRPDT050 EQU   *                                                                
*&&UK                                                                           
         MVI   FVMINL,1            VALIDATE INSURANCE CODE                      
         GOTO1 AFVAL,PERINCH                                                    
         BNE   VRPDT060                                                         
*                                                                               
         CLI   CULANG,X'03'        IGNORE UK VALIDATION IN GERMANY              
         BE    VRPDT052                                                         
         CLI   FVILEN,9            SPECIAL UK VALIDATION 9 CHARACTERS           
         BH    SAEFTL                                                           
         L     RF,=A(VALNHI)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   EXIT                                                             
VRPDT052 EQU   *                                                                
*                                                                               
         CLI   CULANG,X'03'        GERMANY HAS EXTENDED INC                     
         BNE   VRPDT054                                                         
         MVC   SAPERINC,FVIFLD                                                  
         MVC   SAPERINX,FVIFLD+L'SAPERINC                                       
         B     VRPDT060                                                         
VRPDT054 ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAPERINC(0),FVIFLD                                               
*&&                                                                             
VRPDT060 EQU   *                                                                
*                                                                               
VRPDT090 MVI   FVMINL,1            VALIDATE DATE OF HIRE                        
         CLI   APACTN,ACTADD                                                    
         BNE   *+10                                                             
         MVC   SAPERDHI,TODAY                                                   
         GOTO1 AFVAL,PERDHIH                                                    
         BNE   VRPDT100                                                         
         GOTO1 VDATVAL,APPARM,(0,FVIFLD),APDUB                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    SAEIIF                                                           
         GOTO1 VDATCON,APPARM,(X'00',APDUB),(X'02',SAPERDHI)                    
*                                                                               
VRPDT100 MVI   FVMINL,1            VALIDATE DATE OF TERMINATION                 
         GOTO1 AFVAL,PERDTEH                                                    
         BNE   VRPDT130                                                         
         GOTO1 VDATVAL,APPARM,(0,FVIFLD),APDUB                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    SAEIIF                                                           
         GOTO1 VDATCON,APPARM,(X'00',APDUB),(X'02',SAPERDTE)                    
         MVC   TERMCHG,SAPERDTE                                                 
*                                                                               
VRPDT130 EQU   *                                                                
*&&UK*&& CLI   APACTN,ACTCPY                                                    
*&&UK*&& BE    VRPDT200            ONLY HIRE/TERM DATES ON COPY                 
         MVI   FVMINL,1            VALIDATE PERSON TITLE                        
         GOTO1 AFVAL,PERTITH                                                    
         BNE   VRPDT140                                                         
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAPERTIT(0),FVIFLD                                               
         AH    R1,=Y(SAPERLNQ+1)                                                
         STC   R1,SAPERLN                                                       
*                                  VALIDATE SEC. MANAGER OVERRIDE FLAG          
VRPDT140 MVC   SAPEROVF,OLDOVF     RESTORE CURRENT VALUE                        
         TM    PEROVFH+(FVIIND-FVIHDR),FVITHIS                                  
         BO    *+12                INPUT THIS TIME, USE INPUT VALUE             
         CLI   APACTN,ACTADD       NOT INPUT AND ACTION=ADD                     
         BE    VRPDT144            DEFAULT TO 'N'                               
*                                                                               
         TM    CUSTAT,CUSDDS       INPUT FOR DDS TERMINAL ONLY                  
         BNO   VRPDT150                                                         
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PEROVFH                                                    
         BNE   VRPDT144            DEFAULT TO 'N'                               
*                                                                               
         CLC   FVIFLD(1),CT@YES    'YES' KEYWORD                                
         BNE   VRPDT142                                                         
         OI    SAPEROVF,COVFON     SET OVERRIDE FLAG ON                         
         MVC   PEROVF,CT@YES                                                    
         B     VRPDT150                                                         
VRPDT142 CLC   FVIFLD(1),CT@NO     'NO' KEYWORD                                 
         BE    VRPDT144                                                         
         CLC   FVIFLD(1),SPACES    SPACE DEFAULT TO C'N'                        
         BNE   SAEIIF                                                           
VRPDT144 NI    SAPEROVF,X'FF'-COVFON  SET OVERRIDE FLAG OFF                     
         MVC   PEROVF,CT@NO                                                     
         B     VRPDT150            OVERRIDE FLAG OFF                            
*                                  VALIDATE PASSWORD CONTROL VALUE              
*RPDT150 MVC   SAPERPCN,OLDPCN     RESTORE CURRENT VALUE                        
VRPDT150 EQU   *                                                                
         TM    PERPCNH+(FVIIND-FVIHDR),FVITHIS                                  
         BO    *+16                INPUT THIS TIME, USE INPUT VALUE             
         CLI   APACTN,ACTADD       NOT INPUT AND ACTION=ADD                     
         BNE   *+8                 NO                                           
         MVI   PERPCN,SAPERPYQ     YES - DEFAULT TO 'Y'                         
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PERPCNH                                                    
         BNE   VRPDT200                                                         
         CLI   FVIFLD,0                                                         
         BE    VRPDT152                                                         
         CLI   FVIFLD,SAPERPNQ                                                  
         BE    VRPDT152                                                         
         CLI   FVIFLD,SAPERPYQ                                                  
         BE    VRPDT152                                                         
*"A", "G" OPTIONS ARE NO LONGER VALID, 4/27/04, YYUN                            
*        CLI   FVIFLD,SAPERPAQ                                                  
*        BE    VRPDT152                                                         
*        CLI   FVIFLD,SAPERPGQ                                                  
*        BE    VRPDT152                                                         
         B     SAEIIF                                                           
VRPDT152 MVC   SAPERPCN,FVIFLD                                                  
         MVC   PERPCN,FVIFLD                                                    
         OI    PERPCNH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         CLI   SAPERPCN,SAPERPAQ                                                
         BNE   VRPDT200                                                         
*                                  TEST PERSONID=PASSWORD                       
         OC    NEWPID,NEWPID                                                    
         BNZ   VRPDT156                                                         
         OC    NEWCODE,NEWCODE                                                  
         BNZ   VRPDT154                                                         
         CLC   OLDPID,OLDCODE                                                   
         BE    VRPDT200                                                         
         B     VRPDT158                                                         
*                                                                               
VRPDT154 CLC   OLDPID,NEWCODE                                                   
         BE    VRPDT200                                                         
         B     VRPDT158                                                         
*                                                                               
VRPDT156 OC    NEWCODE,NEWCODE                                                  
         BNZ   VRPDT157                                                         
         CLC   NEWPID,OLDCODE                                                   
         BE    VRPDT200                                                         
         B     VRPDT158                                                         
*                                                                               
VRPDT157 CLC   NEWPID,NEWCODE                                                   
         BE    VRPDT200                                                         
         B     VRPDT158                                                         
*                                                                               
VRPDT158 EQU   *                                                                
         MVC   FVMSGNO,=AL2(CE#PPSAC)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     VALRECX                                                          
*                                                                               
VRPDT200 L     R2,AIOAREA1         SAVE PERSONAL DETAILS ELEMENT                
         GOTO1 AADDELS,SAPEREC                                                  
*                                                                               
VRPDTX   EQU   *                                                                
                                                                                
***********************************************************************         
* BUILD PERSON EMAIL ID ELEMENT (REPLACES SAPERPRO)                   *         
***********************************************************************         
VRPEE    GOTO1 AFVAL,PERPROH                                                    
*&&US                                                                           
         CLC   =C'3N',AGENCYID     NO VALIDATION FOR TALENT PARTNERS            
         BE    VRPE30                                                           
*                                                                               
         MVI   FVMINL,6            MUST BE AT LEAST 6 CHAR A@B.TV               
         GOTO1 AFVAL,PERPROH                                                    
         BNE   SAEMAIL                                                          
*                                                                               
VRPE01   LA    R2,IOKEY            LOOK FOR ACTIVE DOMAIN RECORDS               
         USING DOMRECD,R2                                                       
         XC    DOMKEY,DOMKEY                                                    
         MVI   DOMKMIN,DOMKMIQ     C'D' DOMAIN RECORD                           
         MVC   DOMKSAG,AGENCYID    SECURITY AGENCY                              
         GOTO1 AIO,IOHIGH+IOGENDIR+IO2                                          
         BNE   SAEMDOM                                                          
         CLC   DOMKEY(DOMKGRP-DOMKEY),IOKEYSAV                                  
         BNE   SAEMDOM                                                          
*                                                                               
VRPE10   GOTO1 AIO,IOGET+IOGENFIL+IO2                                           
         BNE   SAEMAIL                                                          
*                                                                               
         GOTO1 =V(VEMAIL),APPARM,PERPROH,(CULANG,0),AIOAREA2,0,        +        
               RR=APRELO                                                        
         CLI   APPARM,0                EMAIL ADDRESS VALID?                     
         BE    VRPE30                  YES: ADD ELEMENT                         
*                                                                               
         LA    R2,IOKEY                                                         
         GOTO1 AIO,IOSEQ+IOGENDIR+IO2  TRY NEXT DOMAIN RECORD                   
         BNE   SAEMAIL                 NONE LEFT: EMAIL INVALID                 
         CLC   DOMKEY(DOMKGRP-DOMKEY),IOKEYSAV                                  
         BNE   SAEMAIL                 NONE LEFT: EMAIL INVALID                 
         B     VRPE10                                                           
*                                                                               
VRPE25   GOTO1 =V(VEMAIL),APPARM,PERPROH,(CULANG,0),0,0,RR=APRELO               
         CLI   APPARM,0                                                         
         BNE   SAEMAIL                                                          
         DROP  R2                                                               
*&&                                                                             
         USING SAPEED,R3                                                        
VRPE30   LA    R3,APELEM           INITIALISE ELEMENT                           
         XC    SAPEEEL(SAPEELNQ),SAPEEEL                                        
         MVI   SAPEEEL,SAPEEELQ                                                 
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VRPEEX                                                           
*&&UK                                                                           
         CLI   FVIFLD,C' '         DON'T ALLOW LEADING SPACES                   
         BE    SAEIIF                                                           
*&&                                                                             
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAPEEID(0),FVIFLD                                                
         AHI   R1,SAPEELNQ+1                                                    
         STC   R1,SAPEELN                                                       
*                                                                               
         L     R2,AIOAREA1                                                      
         USING SAPEREC,R2                                                       
         GOTO1 AADDELS,SAPEREC                                                  
*                                                                               
VRPEEX   EQU   *                                                                
         CLI   APACTN,ACTCPY                                                    
         BE    VRADR               CAN UPDATE ADDRESS ON COPY                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD TIME SHEET APPROVER GROUP CODE ELEMENT                        *         
***********************************************************************         
         USING SAAPCD,R3                                                        
VRAPC    LA    R3,APELEM           INITIALISE ELEMENT                           
         XC    SAAPCEL(SAAPCLNQ),SAAPCEL                                        
         MVI   SAAPCEL,SAAPCELQ                                                 
         MVI   SAAPCLN,SAAPCLNQ                                                 
         MVC   SAAPCCOD,OLDAPC                                                  
         MVC   SAAPCNUM,OLDAPN                                                  
         MVI   FVMINL,1            VALIDATE APPROVER GROUP FIELD                
         GOTO1 AFVAL,PERAPCH                                                    
         BE    VRAPC010                                                         
         OC    OLDAPC,OLDAPC       TEST IF NOT SET                              
         BZ    VRAPC030                                                         
         BRAS  RE,CHECKUP          CHECK OK TO REMOVE                           
         BNE   VALRECX                                                          
         B     VRAPC030                                                         
*                                                                               
VRAPC010 CLC   SAAPCCOD,FVIFLD     SAME AS PREVIOUS                             
         BE    VRAPC020                                                         
         BRAS  RE,CHECKUP          CHECK OK TO CHANGE                           
         BNE   VALRECX                                                          
         MVC   SAAPCCOD,FVIFLD                                                  
*                                                                               
VRAPC020 GOTO1 AGETAPG,SAAPCCOD    GET APPROVER GROUP RECORD DATA               
         BNE   VALRECX                                                          
         MVC   SAAPCNUM,APHALF                                                  
         B     VRAPC100                                                         
*                                                                               
VRAPC030 OC    DEPAPC,DEPAPC       GET APPROVER GROUP FROM DEPT RECORD          
         BZ    VRAPC040                                                         
         MVC   SAAPCCOD,DEPAPC                                                  
         MVC   SAAPCNUM,DEPAPN                                                  
         B     VRAPC100                                                         
*                                                                               
VRAPC040 OC    OFFAPC,OFFAPC       GET APPROVER GROUP FROM OFFICE REC.          
         BZ    VRAPCX                                                           
         MVC   SAAPCCOD,OFFAPC                                                  
         MVC   SAAPCNUM,OFFAPN                                                  
         B     VRAPC100                                                         
*                                                                               
         USING SAPEREC,R2                                                       
VRAPC100 MVC   NEWAPC,SAAPCCOD     SAVE NEW VALUE CODE                          
         MVC   NEWAPN,SAAPCNUM     SAVE NEW VALUE NUMBER                        
         L     R2,AIOAREA1         ADD ELEMENT                                  
         GOTO1 AADDELS,SAPEREC                                                  
VRAPCX   EQU   *                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD ADDRESS ELEMENTS IN PERSON RECORD                             *         
***********************************************************************         
VRADR    MVI   FVMINL,1            BUILD ADDRESS ELEMENTS                       
                                                                                
         LA    R0,SAADLINQ         ADDRESS LINE 1                               
         LA    R1,PERADR1H                                                      
         BRAS  RE,VALIDADR                                                      
                                                                                
         LA    R0,SAADLINQ+1       ADDRESS LINE 2                               
         LA    R1,PERADR2H                                                      
         BRAS  RE,VALIDADR                                                      
                                                                                
         LA    R0,SAADPHOQ         PHONE NUMBER                                 
         LA    R1,PERTELEH                                                      
         BRAS  RE,VALIDADR                                                      
*&&US                                                                           
         LA    R1,PERCTRYH                                                      
         GOTO1 AFVAL                                                            
         LARL  R1,CNTRYTAB                                                      
VRADR05  CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    SAEIIF              ** REQUIRED **                               
         CLC   PERCTRY(3),0(R1)                                                 
         BE    VRADR10                                                          
         LA    R1,L'CNTRYTAB(,R1)                                               
         B     VRADR05                                                          
*                                                                               
VRADR10  TM    3(R1),X'80'         ZIP CODE SUPPORTED COUNTRY                   
         BZ    VRADR20             NO:                                          
*                                                                               
         CLC   =C'3N',AGENCYID     NO VALIDATION FOR TALENT PARTNERS            
         BE    VRADR20                                                          
         CLI   CUCTRY,CTRYCAN      NO VALIDATION FOR CANADA                     
         BE    VRADR20                                                          
*                                                                               
         BRAS  RE,VALIDZIP         ZIP CODE                                     
         BNE   SAEMIF              INVALID ZIP CODE ENTERED                     
*                                                                               
VRADR20  LA    R0,SAADZIPQ         ZIP CODE                                     
         LA    R1,PERCODEH                                                      
         BRAS  RE,VALIDADR                                                      
*                                                                               
         LA    R0,SAADSTEQ         STATE                                        
         LA    R1,PERSTEH                                                       
         BRAS  RE,VALIDADR                                                      
*&&                                                                             
*&&UK                                                                           
         LA    R0,SAADCODQ         POSTAL CODE                                  
         LA    R1,PERCODEH                                                      
         BRAS  RE,VALIDADR                                                      
*&&                                                                             
         LA    R0,SAADCITQ         CITY                                         
         LA    R1,PERCITYH                                                      
         BRAS  RE,VALIDADR                                                      
*                                                                               
         LA    R0,SAADCTRQ         COUNTRY                                      
         LA    R1,PERCTRYH                                                      
         BRAS  RE,VALIDADR                                                      
*                                                                               
VRADRX   CLI   APACTN,ACTCPY                                                    
         BE    VRAEP               CAN UPDATE NAME/ADDRESS ONLY ON COPY         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* BUILD USER ID ELEMENTS IN PASSWORD RECORD                           *         
***********************************************************************         
VRUSR    MVI   APBYTE,0                                                         
         MVI   AEPUIOV,0           NO DDLINK OVERRIDES                          
         GOTO1 AFVAL,PERUSR1H                                                   
         OC    APBYTE,FVIIND                                                    
         GOTO1 AFVAL,PERUSR2H                                                   
         OC    APBYTE,FVIIND                                                    
*                                                                               
         GOTO1 AFVAL,PERURR1H                                                   
         OC    APBYTE,FVIIND                                                    
         GOTO1 AFVAL,PERURR2H                                                   
         OC    APBYTE,FVIIND                                                    
*                                                                               
         CLI   APACTN,ACTADD                                                    
         BE    *+12                FOR ADD ALWAYS VALIDATE                      
         TM    APBYTE,FVITHIS                                                   
         BZ    VRAEP               NO INPUT THIS TIME, SKIP CHECKING            
*                                                                               
         L     R2,AIOAREA3                                                      
         BRAS  RE,CHECKUP          CHECK VALID TO UPDATE ELEMENT                
         BNE   VALRECX                                                          
*                                                                               
*MUST HAVE INPUT FOR EITHER UPDATIVE OR READ-ONLY UNLESS COPYING FROM           
*ACCESS EQUIVALENT PID UNDER DDLINK                                             
*                                                                               
VRUSR030 EQU   *                                                                
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,PERUSR1H                                                   
         BE    VRUSR040                                                         
         OC    PERUSR2,PERUSR2                                                  
         BNZ   VRUSR040                                                         
*                                                                               
         OC    PERURR1,PERURR1                                                  
         BNZ   VRUSR040                                                         
         OC    PERURR2,PERURR2                                                  
         BNZ   VRUSR040                                                         
         TM    CUSTAT,CUSDDL       DDLINK?                                      
         BZ    SAEMIF              NO, MISSING INPUT                            
         CLC   DDLKAEP,SPACES      IS THERE AN ACCESS EQUIV PID                 
         BNE   VRURRX              YES, USER IDS WILL COME FROM THAT            
         B     SAEMIF              MISSING INPUT                                
*                                                                               
VRUSR040 EQU   *                   CHECK OVERRIDEIN ACCESS EQUIV PID            
         TM    CUSTAT,CUSDDL       DDLINK?                                      
         BZ    VRUSR050            NO                                           
         CLC   DDLKAEP,SPACES      IS THERE AN ACCESS EQUIV PID                 
         BE    VRUSR050            NO                                           
         TM    PERUSR1H+4,FVITHIS  IS CALLER OVERRIDING AEP PIDS?               
         BO    VRUSR042                                                         
         TM    PERUSR2H+4,FVITHIS                                               
         BO    VRUSR042                                                         
         TM    PERURR1H+4,FVITHIS                                               
         BO    VRUSR042                                                         
         TM    PERURR2H+4,FVITHIS                                               
         BZ    VRUSR050                                                         
VRUSR042 OI    AEPUIOV,X'80'       YES TELL COPYAEP ROUTINE                     
VRUSR050 EQU   *                   GET ALL 4 UID INPUT LINES                    
         GOTO1 VSCANNER,APPARM,PERUSR1H,(20,BLOCK1)                             
         MVC   BLKCNT1,4(R1)                                                    
         GOTO1 VSCANNER,APPARM,PERUSR2H,(20,BLOCK2)                             
         MVC   BLKCNT2,4(R1)                                                    
*                                                                               
         GOTO1 VSCANNER,APPARM,PERURR1H,(20,BLOCK3)                             
         MVC   BLKCNT3,4(R1)                                                    
         GOTO1 VSCANNER,APPARM,PERURR2H,(20,BLOCK4)                             
         MVC   BLKCNT4,4(R1)                                                    
*                                  CORRECT COUNTERS FOR EMPTY INPUT             
         CLC   PERUSR1,SPACES                                                   
         BH    *+8                                                              
         MVI   BLKCNT1,0                                                        
         CLC   PERUSR2,SPACES                                                   
         BH    *+8                                                              
         MVI   BLKCNT2,0                                                        
         CLC   PERURR1,SPACES                                                   
         BH    *+8                                                              
         MVI   BLKCNT3,0                                                        
         CLC   PERURR2,SPACES                                                   
         BH    *+8                                                              
         MVI   BLKCNT4,0                                                        
*                                                                               
         BRAS  RE,FIXSCAN          FIX SCANNER BLOCK W/ LEADING SPACES          
*                                                                               
         BRAS  RE,CHKDUP           CHECK FOR DUPLICATE ENTRY                    
         BNE   NO                                                               
*                                                                               
         MVI   RDOLYFST,C'Y'                                                    
         BRAS  RE,CHKWRUID         CHECK IF ADD READ-ONLY UIDS FIRST?           
         BE    *+8                 YES                                          
         MVI   RDOLYFST,C'N'       NO                                           
*                                  DELETE USER ID ELEMENTS                      
         L     R2,AIOAREA3                                                      
         LA    R0,SAIDELQ                                                       
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),(R2)),0,0                      
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   RDOLYFST,C'Y'       ADD READ-ONLY IDS FIRST?                     
         BE    VRURR               YES - WILL ADD UPD IDS AFTERWARD             
*                                                                               
VRUUR    EQU   *                                                                
         BRAS  RE,ADDUPID          ADD UPDATIVE USERIDS                         
         BNE   VALRECX                                                          
         CLI   RDOLYFST,C'Y'       READ-ONLY IDS WERE ADDED FIRST?              
         BE    VRURRX              YES-DON'T ADD AGAIN                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD READ ONLY USER ID ELEMENTS IN PASSWORD RECORD                 *         
***********************************************************************         
VRURR    BRAS  RE,ADDROID          ADD READ-ONLY USERIDS                        
         BNE   VALRECX                                                          
         CLI   RDOLYFST,C'Y'       ADD READ-ONLY IDS FIRST?                     
         BE    VRUUR               YES - NOW ADD UPD IDS                        
VRURRX   EQU   *                                                                
                                                                                
***********************************************************************         
* BUILD HIDDEN AEP ELEMENT IF REQUIRED (FOR DDLINK ONLY)              *         
***********************************************************************         
VRAEP    TM    CUSTAT,CUSDDL       DDLINK?                                      
         BZ    VRUPD               NO, don't touch AEPEL                        
         L     R2,AIOAREA1         Delete any existing AEPEL                    
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),('SAAEPELQ',(R2)),0,0                
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         CLC   DDLKAEP,SPACES      Was an Access Equiv Pid used?                
         BE    VRUPD                                                            
         USING SAAEPD,R3                                                        
         LA    R3,APELEM           Yes, add an AEPEL                            
         MVI   SAAEPEL,SAAEPELQ                                                 
         MVI   SAAEPLN,SAAEPLNQ                                                 
         MVC   SAAEPAEP,DDLKAEP                                                 
         GOTO1 AADDELS,(R2)                                                     
         DROP  R3                                                               
                                                                                
***********************************************************************         
* UPDATE RECORDS ON FILE                                              *         
***********************************************************************         
         USING SAPEREC,R2                                                       
VRUPD    L     R2,AIOAREA1                                                      
         CLI   APACTN,ACTCPY       OVERIDE COPY WITH ADD STATE                  
         BNE   *+8                                                              
         MVI   APACTN,ACTADD                                                    
         CLI   APACTN,ACTADD       CHECK ACTION STATE                           
         BNE   VRUPD007                                                         
*                                                                               
         LA    R1,IOKEY            IF ADD ALLOCATE NEW PASSWORD NUMBER          
         USING SA0REC,R1                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         GOTO1 AIO,IOHI+IOCONFIL+IO2                                            
         BL    VALRECX                                                          
*                                                                               
*&&UK*&& CLC   =C'#E',AGENCYID     DDS INTERNAL SECURITY FILE (UK)              
*&&US*&& CLC   =C'#N',AGENCYID     DDS INTERNAL SECURITY FILE (US)              
         BNE   VRUPD005                                                         
*                                  READ PASS THIS AGY'S LAST PASS REC           
VRUPD003 L     R1,AIOAREA2                                                      
         CLC   SA0KEY(SA0KNUM-SA0KEY),IOKEYSAV    STILL SAME AGY?               
         BNE   VRUPD004            NO  - GET NEXT PWD #                         
         MVC   NEWPWD,SA0KNUM      YES - SAVE THIS PASSWORD #                   
         GOTO1 AIO,IOSQ+IOCONFIL+IO2    READ SEQ                                
         B     VRUPD003                                                         
*                                                                               
VRUPD004 EQU   *                   +1 TO GET NEXT PASSWORD #                    
         SR    RE,RE                                                            
         ICM   RE,3,NEWPWD                                                      
         BNZ   *+8                 #1 IS RESERVE FOR OFFLINE USE                
         AHI   RE,1                                                             
         AHI   RE,1                                                             
         STCM  RE,3,NEWPWD                                                      
*                                                                               
         CLC   NEWPWD,=X'1000'                                                  
         BL    VRUPD100                                                         
         DC    H'0'                ALLOW ONLY 4095 DDS PERSONS                  
*                                                                               
VRUPD005 L     R1,AIOAREA2                                                      
         CLC   SA0KEY(SA0KNUM-SA0KEY),IOKEYSAV                                  
         BNE   *+10                                                             
         MVC   NEWPWD,SA0KNUM                                                   
         SR    RE,RE               NEXT NUMBER IS LAST MINUS ONE                
         ICM   RE,3,NEWPWD                                                      
         BCTR  RE,0                                                             
         STCM  RE,3,NEWPWD                                                      
*                                                                               
         CLC   NEWPWD,=X'1000'                                                  
         BNL   VRUPD100                                                         
         DC    H'0'                LOWER 4095 PWD# RESERVED FOR DDS USE         
         DROP  R1                                                               
*                                                                               
*                                  IF CONVERT ACTION DELETE REDUNDANT           
VRUPD007 CLI   APACTN,ACTCNV         NAME PASSWORD PASSIVE RECORD               
         BNE   VRUPD010                                                         
*                                  ??PP??                                       
         USING SA0REC,R1                                                        
         L     R1,AIOAREA2                                                      
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KEYS(L'PNAMEKEY),PNAMEKEY                                     
         MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R1,AIOAREA2                                                      
         GOTO1 ASETACT,SA0REC                                                   
         OI    SA0STAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                  UPDATE PID                                   
VRUPD010 OC    NEWPID,NEWPID       CHECK FOR NEW PERSONAL ID                    
         BZ    VRUPD020                                                         
         L     RF,=A(CHGEPID)      CHANGE ALL OTHER PID/PWD RECORDS             
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         L     R2,AIOAREA1         UPDATE NEW PERSON RECORD KEY                 
         MVC   SAPEPID,NEWPID                                                   
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY                                      
*                                                                               
VRUPD020 OC    NEWDEF,NEWDEF       IF NEW EFFECTIVE DATE                        
         BZ    VRUPD030                                                         
         MVC   SAPEDEF,NEWDEFC     UPDATE PERSON RECORD KEY                     
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY                                      
         DROP  R2                                                               
                                                                                
***********************************************************************         
* UPDATE EXISTING PASSWORD RECORDS                                    *         
***********************************************************************         
         USING SA0REC,R2                                                        
VRUPD030 OC    NEWCODE,NEWCODE     IF NEW PASSWORD CODE ENTERED                 
         BZ    VRUPD100                                                         
         CLI   PIDREQD,C'Y'        TEST IF PERSONAL ID PASSWORD                 
         BE    VRUPD100                                                         
         OC    NEWDEF,NEWDEF                                                    
         BNZ   *+14                                                             
         MVC   APHALF,TODAY                                                     
         B     *+10                                                             
         MVC   APHALF,NEWDEF                                                    
         BRAS  RE,UPDOLDPW         UPDATE OLD PASSWORD RECORDS                  
*                                                                               
         L     R2,AIOAREA3         UPDATE NEW PASSWORD RECORD KEY               
         MVC   SA0KCODE,NEWCODE                                                 
         MVC   SA0KEYSV(L'SA0KEY),SA0KEY                                        
         OC    NEWDEF,NEWDEF                                                    
         BNZ   *+14                                                             
         MVC   PEFSTASV,TODAY                                                   
         B     *+10                                                             
         MVC   PEFSTASV,NEWDEF                                                  
         B     VRUPD100                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* BUILD ACTIVITY AND POINTER ELEMENTS                                 *         
***********************************************************************         
         USING SAPEREC,R2                                                       
VRUPD100 L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SAPEREC     DEFINE ACTIVITY ELEMENTS                     
         LA    R3,APELEM                                                        
         USING SAPWDD,R3                                                        
         MVI   SAPWDEL,SAPWDELQ    BUILD PASSWORD POINTER ELEMENT               
         MVI   SAPWDLN,SAPWDLNQ                                                 
         OC    NEWPWD,NEWPWD                                                    
         BZ    *+14                                                             
         MVC   SAPWDNUM(L'NEWPWD),NEWPWD                                        
         B     *+10                                                             
         MVC   SAPWDNUM(L'OLDPWD),OLDPWD                                        
         OC    NEWCODE,NEWCODE                                                  
         BZ    *+14                                                             
         MVC   SAPWDCOD,NEWCODE                                                 
         B     *+10                                                             
         MVC   SAPWDCOD,OLDCODE                                                 
         GOTO1 AADDELS,SAPEREC                                                  
         DROP  R2,R3                                                            
*                                                                               
         L     R4,AIOAREA3         AND FOR PASSWORD RECORD                      
         USING SA0REC,R4                                                        
         GOTO1 ASETACT,SA0REC                                                   
         LA    R3,APELEM                                                        
         USING SAPALD,R3                                                        
         MVI   SAPALEL,SAPALELQ    BUILD PERSON POINTER ELEMENT                 
         MVI   SAPALLN,SAPALLNQ                                                 
         OC    NEWPID,NEWPID                                                    
         BZ    *+14                                                             
         MVC   SAPALPID,NEWPID                                                  
         B     *+10                                                             
         MVC   SAPALPID,OLDPID                                                  
         GOTO1 AADDELS,SA0REC                                                   
         CLI   PIDREQD,C'Y'        ??PP??                                       
         BE    VRUPD200                                                         
         USING SAPASD,R3                                                        
         MVI   SAPASEL,SAPASELQ    BUILD PASSWORD PASSIVE POINTER               
         MVI   SAPASLEN,X'04'                                                   
         OC    NEWPWD,NEWPWD                                                    
         BZ    *+14                                                             
         MVC   SAPASDTA(L'NEWPWD),NEWPWD                                        
         B     *+10                                                             
         MVC   SAPASDTA(L'OLDPWD),OLDPWD                                        
         GOTO1 AADDELS,SA0REC                                                   
         USING SAPEFD,R3                                                        
         XC    SAPEFEL(SAPEFLNQ),SAPEFEL                                        
         MVI   SAPEFEL,SAPEFELQ    BUILD PASSWORD EFFECTIVE DATE ELEM.          
         MVI   SAPEFLN,SAPEFLNQ                                                 
         MVC   SAPEFSTA,PEFSTASV                                                
         MVC   SAPEFEND,PEFENDSV                                                
         GOTO1 AADDELS,SA0REC                                                   
         DROP  R4,R3                                                            
*                                                                               
VRUPD200 EQU   *                                                                
                                                                                
***********************************************************************         
* CONTINUE RECORD UPDATE                                              *         
***********************************************************************         
         USING SAPEREC,R2                                                       
VRUPD300 MVC   IOKEY,SAPEKEY       UPDATE PERSON RECORD                         
         CLI   APACTN,ACTCNV       CHECK IF CONVERT ACTION                      
         BE    VRUPD306                                                         
         CLI   APACTN,ACTADD       CHECK IF ADD ACTION                          
         BNE   VRUPD302                                                         
         CLI   PIDIOERR,IOERNF       AND RECORD NOT FOUND                       
         BE    VRUPD306                                                         
VRUPD302 GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BNL   *+6                                                              
         DC    H'00'                                                            
         TM    IOERR,IOEDEL                                                     
         BNZ   VRUPD304                                                         
         TM    IOERR,IOERNF                                                     
         BNZ   VRUPD306                                                         
VRUPD304 LA    R1,IOWRITE+IOCONFIL+IO1                                          
         B     *+8                                                              
VRUPD306 LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    VRUPD310                                                         
         DC    H'0'                                                             
*                                                                               
VRUPD310 MVC   IOKEY,SA0KEYSV      UPDATE NEW PASSWORD RECORD                   
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    VRUPD320                                                         
         CLI   APACTN,ACTADD       CHECK IF ADD FUNCTION                        
         BE    VRUPD312                                                         
         OC    NEWCODE,NEWCODE       OR NEW PASSWORD SPECIFIED                  
         BZ    VRUPD314                                                         
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BH    *+6                                                              
         DC    H'00'                                                            
         TM    IOERR,IOEDEL                                                     
         BNZ   VRUPD314                                                         
         TM    IOERR,IOERNF                                                     
         BNZ   VRUPD312                                                         
         DC    H'00'                                                            
VRUPD314 LA    R1,IOWRITE+IOCONFIL+IO3                                          
         B     *+8                   ELSE OVERWRITE WITH CHANGE                 
VRUPD312 LA    R1,IOADD+IOCONFIL+IO3                                            
         GOTO1 AIO                                                              
         BE    VRUPD320                                                         
         DC    H'0'                                                             
*                                                                               
         USING SA0REC,R4                                                        
VRUPD320 L     R4,AIOAREA3         UPDATE NEW PASSWORD # RECORD                 
         MVC   SA0KEY,SA0KEYSV                                                  
         XC    SA0KEYS,SA0KEYS                                                  
         LA    R3,APELEM                                                        
         USING SAPEFD,R3                                                        
         MVI   SAPEFEL,SAPEFELQ    REMOVE EFFECTIVE DATES ELEMENT               
         MVI   SAPEFLN,0                                                        
         GOTO1 ADELELS,SA0REC                                                   
         LA    R3,APELEM                                                        
         USING SAPASD,R3                                                        
         MVI   SAPASEL,SAPASELQ    SHOULD THIS BE REMOVED ALSO?                 
         MVI   SAPASLEN,0                                                       
         GOTO1 ADELELS,SA0REC                                                   
         MVI   SAPASLEN,X'0C'                                                   
         OC    NEWCODE,NEWCODE                                                  
         BZ    *+14                                                             
         MVC   SAPASDTA(L'NEWCODE),NEWCODE                                      
         B     *+10                                                             
         MVC   SAPASDTA(L'OLDCODE),OLDCODE                                      
         GOTO1 AADDELS,SA0REC                                                   
*                                                                               
         TM    CUSTAT,CUSDDL       DDLINK IN CONTROL?                           
         BZ    VRUPD323                                                         
         CLC   DDLKAEP,SPACES      AND IS THERE AN ACCESS EQUIV PID?            
         BE    VRUPD323                                                         
         L     RF,=A(COPYAEP)      YES, COPY DETAILS FROM IT                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
VRUPD323 OC    NEWPWD,NEWPWD                                                    
         BZ    *+14                                                             
         MVC   SA0KNUM,NEWPWD                                                   
         B     *+10                                                             
         MVC   SA0KNUM,OLDPWD                                                   
*                                                                               
         MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         DROP  R4,R3                                                            
VRUPD321 LA    R1,IOWRITE+IOCONFIL+IO3                                          
         CLI   APACTN,ACTADD       CHECK IF ADD FUNCTION                        
         BE    VRUPD324                                                         
         OC    NEWPWD,NEWPWD       OR NEW PASSWORD SPECIFIED                    
         BZ    VRUPD322                                                         
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BE    VRUPD322            IT ALREADY EXISTS                            
         TM    IOERR,IOERNF                                                     
         BNZ   VRUPD324                                                         
VRUPD322 LA    R1,IOWRITE+IOCONFIL+IO3                                          
         B     *+8                 ELSE OVERWRITE WITH CHANGE                   
VRUPD324 LA    R1,IOADD+IOCONFIL+IO3                                            
         GOTO1 AIO                                                              
         BE    VRUPD330                                                         
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* UPDATE STAFF COUNT IN ASSOCIATED RECORDS                            *         
***********************************************************************         
VRUPD330 CLC   OLDAGC,NEWAGC       ACCESS GROUP RECORD                          
         BE    VRUPD334                                                         
         OC    NEWAGC,NEWAGC                                                    
         BZ    VRUPD332                                                         
         MVC   APWORK(L'NEWAGC),NEWAGC                                          
         BRAS  RE,INCAGRS                                                       
*                                                                               
VRUPD332 OC    OLDAGC,OLDAGC                                                    
         BZ    VRUPD334                                                         
         MVC   APWORK(L'OLDAGC),OLDAGC                                          
         BRAS  RE,DECAGRS                                                       
         B     VRUPD334                                                         
*                                                                               
VRUPD334 CLC   OLDAPC,NEWAPC       APPROVER GROUP RECORD                        
         BE    VRUPD340                                                         
         OC    NEWAPC,NEWAPC                                                    
         BZ    VRUPD336                                                         
         MVC   APWORK(L'NEWAPC),NEWAPC                                          
         BRAS  RE,INCAPRS                                                       
*                                                                               
VRUPD336 OC    OLDAPC,OLDAPC                                                    
         BZ    VRUPD340                                                         
         MVC   APWORK(L'OLDAPC),OLDAPC                                          
         BRAS  RE,DECAPRS                                                       
         B     VRUPD340                                                         
*                                                                               
VRUPD340 CLC   OLDOID,NEWOID       OFFICE AND DEPARTMENT RECORDS                
         BE    VRUPD342                                                         
         OC    NEWOID,NEWOID                                                    
         BZ    VRUPD350                                                         
         MVC   APWORK(L'NEWOID),NEWOID                                          
         BRAS  RE,INCOFFS                                                       
         B     VRUPD344                                                         
VRUPD342 CLC   OLDDID,NEWDID                                                    
         BE    VRUPD360                                                         
VRUPD344 OC    NEWDID,NEWDID                                                    
         BZ    VRUPD350                                                         
         MVC   APWORK(L'NEWOID),NEWOID                                          
         MVC   APWORK+L'NEWOID(L'NEWDID),NEWDID                                 
         BRAS  RE,INCDPTS                                                       
*                                                                               
VRUPD350 CLC   OLDOID,NEWOID                                                    
         BE    VRUPD352                                                         
         OC    OLDOID,OLDOID                                                    
         BZ    VRUPD360                                                         
         MVC   APWORK(L'OLDOID),OLDOID                                          
         BRAS  RE,DECOFFS                                                       
VRUPD352 OC    OLDDID,OLDDID                                                    
         BZ    VRUPD360                                                         
         MVC   APWORK(L'OLDOID),OLDOID                                          
         MVC   APWORK+L'OLDOID(L'OLDDID),OLDDID                                 
         BRAS  RE,DECDPTS                                                       
         B     VRUPD360                                                         
*                                                                               
VRUPD360 CLC   OLDLACN,NEWLACN     DATA ACCESS GROUP RECORD                     
         BE    VRUPD400                                                         
         OC    NEWLACN,NEWLACN                                                  
         BZ    VRUPD362                                                         
         MVC   APWORK(L'NEWLACN),NEWLACN                                        
         BRAS  RE,INCLAGS                                                       
*                                                                               
VRUPD362 OC    OLDLACN,OLDLACN                                                  
         BZ    VRUPD400                                                         
         MVC   APWORK(L'OLDLACN),OLDLACN                                        
         BRAS  RE,DECLAGS                                                       
         B     VRUPD400                                                         
                                                                                
***********************************************************************         
* UPDATE COMPLETED                                                    *         
***********************************************************************         
VRUPD400 OC    NEWPID,NEWPID                                                    
         BZ    VRUPD402                                                         
         MVC   PERPID,NEWPID       DISPLAY NEW PERSONAL ID                      
         OI    PERPIDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
VRUPD402 OC    NEWCODE,NEWCODE                                                  
         BZ    VRUPD404                                                         
         MVC   PERPWD,NEWCODE      DISPLAY NEW PASSWORD CODE                    
         BRAS  RE,HIDEPWD                                                       
         OI    PERPWDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         BRAS  RE,CHGFPWD          CHG ANY FUTURE PERSON RECORD' PWD            
*                                  EXIT UPDATE OK                               
VRUPD404 DS    0H                                                               
         OC    TERMCHG,TERMCHG                                                  
         BZ    VRUPD410                                                         
         CLC   TERMCHG,TODAY                                                    
         BH    VRUPD410            IGNORE FUTURE DATES                          
         GOTO1 =A(TERMNTFY),APPARM,RR=APRELO                                    
*                                                                               
VRUPD410 DS    0H                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   APACTN,ACTCNV       CHECK CONVERT ACTION SECOND PASS             
         BNE   DISREC                                                           
         MVI   APACTN,ACTCHA       OVERIDE ACTION CODE FOR DISPLAY              
         B     DISREC                                                           
*                                  EXIT UPDATE ERROR                            
VALRECX  J     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO CHECK OK FOR UPDATE OF ACTIVE DATA, RETURN .EQ. IF OK    *         
* E.G.FOR OFFICE, DEPARTENT, ACCESS GROUP CODES                       *         
***********************************************************************         
CHECKUP  NTR1                                                                   
         OC    OCURPDEF,OCURPDEF   CHECK IF EFFECTIVE DATES                     
         BNZ   SAEFPE                ARE PENDING                                
         OC    NEWDEF,NEWDEF       CHECK IF EFFECTIVE DATE UPDATE               
         BZ    YES                                                              
         CLC   NEWDEF,TODAY        OK IF TODAY                                  
         BNZ   SAEFED                                                           
         B     YES                                                              
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF RECORD                                    *         
***********************************************************************         
         USING SAPEREC,R2                                                       
DISKEY   LA    R2,APRECKEY         GET PERSON RECORD KEY DATA                   
         MVC   PERPID,SAPEPID                                                   
         OI    PERPIDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         MVC   APHALF,FFILL                                                     
         XC    APHALF,SAPEDEF                                                   
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,APHALF),(X'51',APWORK)                         
         MVC   PERDEF,APWORK                                                    
         OI    PERDEFH+(FVOIND-FVIHDR),FVOXMT                                   
         BRAS  RE,INITDEFT         CLEAR EFFECTIVE DATE KEY SAVE TABLE          
         L     R4,SAPEDATA                                                      
*                                                                               
DKEY010  CLI   0(R4),0                                                          
         BE    DISKEYX                                                          
         CLI   0(R4),SAPWDELQ                                                   
         BE    DKEY020                                                          
         SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     DKEY010                                                          
*                                                                               
         USING SAPWDD,R4                                                        
DKEY020  MVC   PERPWD,SAPWDCOD                                                  
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIDEPWD                                                       
         OI    PERPWDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
DISKEYX  J     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY RECORD                                                     
***********************************************************************         
         USING SAPEREC,R2                                                       
DISREC   CLI   APACTN,ACTGRD       ACTION GRIDS?                                
         BNE   *+12                                                             
         BRAS  RE,DISGRID                                                       
         B     EXIT                                                             
*                                                                               
         L     R2,AIOAREA1         DISPLAY PERSON RECORD DATA                   
         TWAXC PERFNAMH                                                         
*                                                                               
*&&UK*&& CLC   =C'#E',AGENCYID                                                  
*&&US*&& CLC   =C'#N',AGENCYID                                                  
         BE    DREC020                                                          
*&&US                                                                           
         L     RF,ASYS                                                          
         L     RF,VSSB-SYSFACD(RF) GET FACPAK SYSTEM ID INFO FROM SSB           
         CLI   SSBSYSID-SSBD(RF),11      CSC SYSTEM?                            
         BE    DREC010                   NO                                     
         TM    SSBSYSFL-SSBD(RF),X'80'   TEST SYSTEM                            
         BO    DREC020                                                          
*&&                                                                             
*                                                                               
DREC010  XC    PERUPID,PERUPID           DON'T SHOW READ-ONLY FIELD             
         MVCDD PERUPID,CT#CUSRS          COMPANY USER IDS                       
         OI    PERROIDH+(FVATRB-FVIHDR),FVALOWI                                 
         OI    PERURR1H+(FVATRB-FVIHDR),FVAPROT                                 
         OI    PERURR2H+(FVATRB-FVIHDR),FVAPROT                                 
         OI    PERUPIDH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERROIDH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERURR1H+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERURR2H+(FVOIND-FVIHDR),FVOXMT                                  
         B     DREC024                                                          
*                                                                               
DREC020  XC    PERUPID,PERUPID                                                  
         MVCDD PERUPID,CT#UPUID          UPDATIVE IDS                           
         NI    PERROIDH+(FVATRB-FVIHDR),X'FF'-FVALOWI                           
         OI    PERROIDH+(FVATRB-FVIHDR),FVAHIGH                                 
         NI    PERURR1H+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    PERURR2H+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         OI    PERUPIDH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERROIDH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERURR1H+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERURR2H+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
DREC024  OI    PERPCHH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PERPCNH+(FVATRB-FVIHDR),X'FF'                                    
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,NEWPWD         GET NEW PIN IF PRESENT                       
         BNZ   *+8                                                              
         ICM   R1,3,OLDPWD         ELSE OLD PIN                                 
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  DDLKPIN,APDUB+5(3)  RETURN TO DDLINK                             
*                                                                               
         TM    CUSTAT,CUSDDS                                                    
         BZ    DREC030                                                          
*                                  DDS ONLY OPTION TO EDIT EXPIRE?              
         MVI   PERPCHH+(FVATRB-FVIHDR),FVAPROT+FVAHIGH+FVALOWC                  
         OI    PERPCHH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   PERPCNH+(FVATRB-FVIHDR),FVAPROT+FVAXTND                          
         OI    PERPCNH+(FVOIND-FVIHDR),FVOXMT                                   
         CLI   OPTCTL,C'Y'         SPECIAL CONTROL OPTION ON?                   
         BNE   *+8                 NO - CAN'T EDIT                              
         NI    PERPCNH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
*                                                                               
         CLI   OPTPIN,C'Y'         DDS ONLY OPTION TO VIEW PIN ?                
         BNE   DREC030                                                          
         MVI   PERPIHH+(FVATRB-FVIHDR),FVAPROT                                  
         MVI   PERPINH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    PERPIHH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    PERPINH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    PERPIN,PERPIN                                                    
         SR    RF,RF                                                            
         ICM   RF,3,OLDPWD                                                      
         EDIT  (RF),(5,PERPIN),ALIGN=LEFT,WRK=APWORK,DUB=APDUB                  
*                                                                               
DREC030  BRAS  RE,REENTER          HANDLE DELETE/RESTORE PROMPT                 
         CLI   APACTN,ACTCNV       CHECK CONVERT ACTION FIRST PASS              
         BNE   DREC040                                                          
*                                  SET CURSOR FOR DATA INPUT                    
         LA    R1,PERFNAMH                                                      
         ST    R1,APCURSOR                                                      
         B     DREC250                                                          
*                                                                               
DREC040  CLI   DEFCUR,0            CHECK FOR EFFECTIVE DATE CURSOR              
         BE    DREC050                                                          
         LA    R1,PERDEFH                                                       
         ST    R1,APCURSOR                                                      
*                                                                               
DREC050  MVC   APHALF,FFILL        UPDATE EFFECTIVE DATE FROM KEY               
         XC    APHALF,SAPEDEF                                                   
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,APHALF),(X'51',APWORK)                         
         MVC   PERDEF,APWORK                                                    
         OI    PERDEFH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    PERDEFN,PERDEFN                                                  
*                                  DISPLAY EFFECTIVE DATE MESSAGE               
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         CLC   APHALF,TODAY                                                     
         BH    DREC070                                                          
         CLC   ODISPDEF,OCURPDEF                                                
         BNE   DREC060                                                          
         MVC   APHALF,=Y(CS#RECEF)                                              
         B     DREC080                                                          
DREC060  BL    DREC070                                                          
         MVC   APHALF,=Y(CS#RECOD)                                              
         B     DREC080                                                          
DREC070  MVC   APHALF,=Y(CS#RECPE)                                              
DREC080  GOTO1 ADISTXT,APPARM,APHALF                                            
         MVC   PERDEFN,APWORK                                                   
*                                  SET MESSAGE FIELDS FOR TRANSMIT              
         OI    PERDEFNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERPIDN,PERPIDN                                                  
         OI    PERPCNH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    PERPCN,PERPCN                                                    
         OI    PERPIDNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERPWDN,PERPWDN                                                  
         OI    PERPWDNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERAGRN,PERAGRN                                                  
         OI    PERAGRNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERAPCN,PERAPCN                                                  
         OI    PERAPCNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERLAGN,PERLAGN                                                  
         OI    PERLAGNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PEROFFN,PEROFFN                                                  
         OI    PEROFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERDIDN,PERDIDN                                                  
         OI    PERDIDNH+(FVOIND-FVIHDR),FVOXMT                                  
         TM    CUSTAT,CUSDDS                                                    
         BNO   *+14                                                             
         XC    PEROVF,PEROVF                                                    
         OI    PEROVFH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    PERLLOA,PERLLOA                                                  
         OI    PERLLOAH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PERLLO,PERLLO                                                    
         OI    PERLLOH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
*----------------------------------                                             
* GET DATA FROM ELEMENTS                                                        
*----------------------------------                                             
         LA    R3,SAPEDATA                                                      
DREC100  CLI   0(R3),0             TEST END-OF-RECORD                           
         BE    DREC250                                                          
         CLI   0(R3),SALLOELQ      LAST LOGON DATE ELEMENT X'04'                
         BE    DREC120                                                          
         CLI   0(R3),SANAMELQ      NAME ELEMENT X'C5'                           
         BE    DREC130                                                          
         CLI   0(R3),SAPERELQ      PERSONNEL ELEMENT X'C6'                      
         BE    DREC140                                                          
         CLI   0(R3),SAADRELQ      ADDRESS ELEMENT X'C7'                        
         BE    DREC150                                                          
         CLI   0(R3),SAPEEELQ      PERSON EMAIL ELEMENT X'E5'                   
         BE    DREC160                                                          
         CLI   0(R3),SAAGCELQ      ACCESS GROUP CODE ELEMENT X'C8'              
         BE    DREC170                                                          
         CLI   0(R3),SAAPCELQ      APPROVER GROUP CODE ELEMENT X'E0'            
         BE    DREC180                                                          
         CLI   0(R3),SALACELQ      DATA ACCESS GROUP ELEM X'CD'                 
         BE    DREC190                                                          
         CLI   0(R3),SAAEPELQ      ACCESS EQUIVALENT PID (DDLINK ONLY)          
         BE    DREC200                                                          
*                                                                               
DREC110  LLC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     DREC100                                                          
                                                                                
*--------------------                                                           
* LAST LOGON DATE ELEMENT                                                       
*--------------------                                                           
         USING SALLOD,R3                                                        
DREC120  DS    0H                                                               
         MVCDD PERLLOA,CT#LUSE                                                  
         MVCDD PERLLO,CT#UNKNW                                                  
         CLC   SALLODT,=X'640102'        JAN02/2000                             
         BL    DREC110                                                          
         BH    DREC128                                                          
         MVCDD PERLLO,CT#UNUSE                                                  
         B     DREC110                                                          
DREC128  GOTO1 VDATCON,APPARM,(3,SALLODT),(X'51',PERLLO)                        
         B     DREC110                                                          
                                                                                
*--------------------                                                           
* NAME ELEMENT                                                                  
*--------------------                                                           
         USING SANAMD,R3                                                        
DREC130  LA    RE,SANAMELN                                                      
         SR    R1,R1                                                            
         TM    SANAMIND,SANAMIFN                                                
         BZ    DREC132                                                          
         IC    R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERFNAM(0),1(RE)                                                 
         LA    RE,2(R1,RE)                                                      
DREC132  TM    SANAMIND,SANAMIMN                                                
         BZ    DREC134                                                          
         IC    R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERMNAM(0),1(RE)                                                 
         LA    RE,2(R1,RE)                                                      
DREC134  TM    SANAMIND,SANAMILN                                                
         BZ    DREC130                                                          
         IC    R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERLNAM(0),1(RE)                                                 
         LA    RE,2(R1,RE)                                                      
         B     DREC110                                                          
                                                                                
*--------------------                                                           
* PERSONNEL DETAILS ELEMENT                                                     
*--------------------                                                           
         USING SAPERD,R3                                                        
DREC140  OC    SAPEROFF,SAPEROFF   OFFICE ID                                    
         BZ    DREC142                                                          
         MVC   PEROFF,SAPEROFF                                                  
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY                                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,PEROFF                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   SAEONF                                                           
         L     R1,AIOAREA2                                                      
         GOTO1 AGETONAM                                                         
         MVC   PEROFFN,APWORK                                                   
*                                                                               
DREC142  L     R2,AIOAREA1                                                      
         OC    SAPERDID,SAPERDID   DEPARTMENT ID                                
         BZ    DREC144                                                          
         MVC   PERDID,SAPERDID                                                  
         LA    R2,IOKEY                                                         
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY                                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,PEROFF                                                   
         MVC   SADPDID,PERDID                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   SAEDNF                                                           
         L     R1,AIOAREA2                                                      
         GOTO1 AGETDNAM                                                         
         MVC   PERDIDN,APWORK                                                   
*                                                                               
         USING SAPEREC,R2                                                       
DREC144  L     R2,AIOAREA1         AND OTHER PERSONAL DATAILS                   
         MVC   PEREXT,SAPEREXT                                                  
         MVC   PERSTA,SAPERSTA                                                  
         MVC   PEREXT,SAPEREXT                                                  
         MVC   PERPRO(L'SAPERPRO),SAPERPRO                                      
         MVC   PEREXT,SAPEREXT                                                  
         MVC   PERPCN,SAPERPCN                                                  
         CLI   SAPERPCN,C' '                                                    
         BH    *+8                                                              
         MVI   PERPCN,C'Y'         DEFAULT DISPLAY C'Y'                         
*&&UK                                                                           
         MVC   PERINC(L'SAPERINC),SAPERINC                                      
         CLI   CULANG,X'03'        GERMANY HAS EXTENDED INC                     
         BNE   *+10                                                             
         MVC   PERINC+L'SAPERINC(L'SAPERINX),SAPERINX                           
*&&                                                                             
         GOTO1 VDATCON,APPARM,(2,SAPERDHI),(X'51',PERDHI)                       
         GOTO1 VDATCON,APPARM,(2,SAPERDTE),(X'51',PERDTE)                       
         L     R2,AIOAREA1                                                      
         SR    R1,R1               DISPLAY TITLE                                
         ICM   R1,1,SAPERLN                                                     
         SH    R1,=Y(SAPERLNQ+1)                                                
         BM    DREC146                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERTIT(0),SAPERTIT                                               
         B     DREC146                                                          
*                                                                               
DREC146  TM    CUSTAT,CUSDDS       DISPLAY SECURITY MANAGER OVERRIDE            
         BNO   DREC110               FLAG FOR DDS TERMINALS ONLY                
         MVC   PEROVF,CT@NO                                                     
         TM    SAPEROVF,COVFON                                                  
         BNO   DREC110                                                          
         MVC   PEROVF,CT@YES                                                    
         B     DREC110                                                          
                                                                                
*----------------------------------                                             
* PERSON ADDRESS ELEMENT                                                        
*----------------------------------                                             
         USING SAADRD,R3                                                        
DREC150  CLI   SAADRTYP,SAADLINQ                                                
         BNE   *+16                                                             
         LA    R1,PERADR1          ADDRESS FIRST LINE                           
         LHI   R0,L'PERADR1                                                     
         B     DREC154                                                          
*                                                                               
         CLI   SAADRTYP,SAADLINQ+1                                              
         BNE   *+16                                                             
         LA    R1,PERADR2          ADDRESS SECOND LINE                          
         LHI   R0,L'PERADR2                                                     
         B     DREC154                                                          
*                                                                               
         CLI   SAADRTYP,SAADCITQ                                                
         BNE   *+16                                                             
         LA    R1,PERCITY          ADDRESS CITY                                 
         LHI   R0,L'PERCITY                                                     
         B     DREC154                                                          
*&&US                                                                           
         CLI   SAADRTYP,SAADSTEQ                                                
         BNE   *+16                                                             
         LA    R1,PERSTE           ADDRESS STATE                                
         LHI   R0,L'PERSTE                                                      
         B     DREC154                                                          
*                                                                               
         CLI   SAADRTYP,SAADZIPQ                                                
         BNE   *+16                                                             
         LA    R1,PERCODE          ADDRESS ZIP                                  
         LHI   R0,L'PERCODE                                                     
         B     DREC154                                                          
*&&                                                                             
         CLI   SAADRTYP,SAADCODQ                                                
         BNE   *+16                                                             
         LA    R1,PERCODE          ADDRESS CODE                                 
         LHI   R0,L'PERCODE                                                     
         B     DREC154                                                          
*                                                                               
         CLI   SAADRTYP,SAADCTRQ                                                
         BNE   *+16                                                             
         LA    R1,PERCTRY          ADDRESS COUNTRY                              
         LHI   R0,L'PERCTRY                                                     
         B     DREC154                                                          
*                                                                               
         CLI   SAADRTYP,SAADPHOQ                                                
         BNE   *+16                                                             
         LA    R1,PERTELE          TELEPHONE                                    
         LHI   R0,L'PERTELE                                                     
         B     DREC154                                                          
*                                                                               
         B     DREC110                                                          
*                                                                               
DREC154  SR    RE,RE               DISPLAY FIELD                                
         IC    RE,SAADRDLN                                                      
         CR    RE,R0                                                            
         BNH   *+6                                                              
         LR    RE,R0                                                            
         AHI   RE,-1                                                            
         BM    DREC110                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SAADRDAT                                                 
         B     DREC110                                                          
                                                                                
*--------------------                                                           
* EMAIL ADDRESS ELEMENT                                                         
*--------------------                                                           
         USING SAPEED,R3                                                        
DREC160  SR    RE,RE               DISPLAY FIELD                                
         IC    RE,SAPEELN                                                       
         LA    RF,SAPEELNQ                                                      
         SR    RE,RF                                                            
         LTR   RE,RE                                                            
         BZ    DREC110                                                          
         BCTR  RE,0                                                             
         LA    R1,PERPRO                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SAPEEID                                                  
         B     DREC110                                                          
                                                                                
*--------------------                                                           
* ACCESS GROUP ELEMENT                                                          
*--------------------                                                           
         USING SAAGCD,R3                                                        
DREC170  OC    SAAGCCOD,SAAGCCOD   ACCESS GROUP                                 
         BZ    DREC110                                                          
         MVC   PERAGR,SAAGCCOD                                                  
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2                                                       
         XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,PERAGR                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   SAEANF                                                           
         L     R1,AIOAREA2                                                      
         GOTO1 AGETGNAM                                                         
         MVC   PERAGRN,APWORK                                                   
         TM    CUSTAT,CUSDDL       DDLINK ACTIVE                                
         BZ    DREC110                                                          
         XR    R1,R1                                                            
         ICM   R1,3,SAAGCNUM       YES, GET ACCESS GROUP #                      
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  DDLKAGR#,APDUB+5(3) RETURN TO DDLINK                             
         B     DREC110                                                          
*----------------------------------------------------------------------         
* APPROVER GROUP ELEMENT                                                        
*----------------------------------------------------------------------         
         USING SAAPCD,R3                                                        
DREC180  OC    SAAPCCOD,SAAPCCOD                                                
         BZ    DREC110                                                          
         MVC   PERAPC,SAAPCCOD                                                  
         GOTO1 AGETAPG,SAAPCCOD                                                 
         BNE   SAEAGCNF                                                         
         MVC   PERAPCN,APWORK                                                   
         B     DREC110                                                          
*----------------------------------------------------------------------         
* DATA ACCESS GROUP ELEMENT                                                     
*----------------------------------------------------------------------         
         USING SALACD,R3                                                        
DREC190  OC    SALACNUM,SALACNUM                                                
         BZ    DREC110                                                          
         LA    R2,IOKEY                                                         
         USING SALAREC,R2                                                       
         XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         MVC   SALAAGY,AGENCYID                                                 
         MVC   SALAAGN,SALACNUM                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   SAELNF                                                           
         L     R1,AIOAREA2                                                      
         GOTO1 AGETLNAM                                                         
         MVC   PERLAG,APWORK                                                    
         MVC   PERLAGN,APWORK+L'SALANCOD                                        
         B     DREC110                                                          
*----------------------------------------------------------------------         
* RETURN HIDDEN AEP ELEMENT IF DDLINK                                           
*----------------------------------------------------------------------         
         USING SAAEPD,R3                                                        
DREC200  TM    CUSTAT,CUSDDL       DDLINK ACTIVE?                               
         JZ    DREC110             NO, IGNORE                                   
         CLI   APACTN,ACTDIS       IS THIS A DISPLAY ACTION                     
         BE    DREC202             YES, SKIP                                    
         CLC   DDLKAEP,SPACES      ADD OR UPDATE, WAS NEW AEP PASSED            
         BNE   DREC110             YES, DO NOT REPLACE IT WITH OLD ONE          
DREC202  MVC   DDLKAEP,SAAEPAEP    RETURN WHAT'S IN THE RECORD                  
         B     DREC110                                                          
         DROP  R3                                                               
*----------------------------------------------------------------------         
* FINISHED READING THROUGH ELEMENTS                                             
*----------------------------------------------------------------------         
DREC250  CLI   APACTN,ACTADD       CHECK PASSWORD RECORD NOT REMOVED            
         BE    *+12                  UNLESS RECORD JUST ADDED                   
         CLI   PWDIOERR,IOERNF       AND DISPLAY MESSAGE                        
         BE    SAEPWR                                                           
         DROP  R2                                                               
*                                                                               
         L     R2,AIOAREA3         DISPLAY USER IDS FROM PASSWORD REC.          
         USING SA0REC,R2                                                        
         MVI   BLKCNT1,0                                                        
         LA    R4,BLOCK1                                                        
         LA    R3,SA0DATA                                                       
         SR    R0,R0                                                            
*                                                                               
DREC270  CLI   0(R3),0             TEST END-OF-RECORD                           
         BE    DREC300                                                          
         CLI   0(R3),SAIDELQ       TEST ID ELEMENT                              
         BNE   DREC280                                                          
         USING SAIDD,R3                                                         
         TM    SAIDTYP,SAIDNEQ     NEW TYPE?                                    
         BNZ   DREC290             NO                                           
         TM    SAIDTYP,SAIDROQ     READ-ONLY USERID?                            
         BNO   DREC290             NO - GET IT                                  
*                                                                               
DREC280  ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     DREC270                                                          
*                                  BUILD USER IDS IN BLOCK                      
DREC290  MVC   0(10,R4),SAID                                                    
         OC    0(2,R4),0(R4)       TEST ID LIST                                 
         BNZ   *+10                                                             
         MVC   0(2,R4),=C'L='                                                   
         CLC   0(2,R4),=X'0001'    TEST AGY=                                    
         BNE   *+16                                                             
         MVC   0(2,R4),=C'A='                                                   
         MVC   4(6,R4),SPACES                                                   
*                                                                               
         CLC   0(4,R4),=C'ALL '    CHECK FOR ALL AND TRANSLATE                  
         BNE   *+10                                                             
         MVC   0(L'CT@ALL,R4),CT@ALL                                            
*                                                                               
         LA    R4,10(R4)                                                        
         SR    R1,R1                                                            
         IC    R1,BLKCNT1                                                       
         LA    R1,1(R1)            BUMP COUNTER                                 
         STC   R1,BLKCNT1                                                       
         B     DREC280                                                          
*                                                                               
DREC300  MVI   ANYUPDID,C'Y'       ASSUME THERE ARE SOME UPDATIVE IDS           
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT1                                                     
         BNZ   DREC310                                                          
                                                                                
*----------------------------------                                             
* NEW RULE: NO UPD ELEMENT MEANS "ALL"                                          
* IF NO OTHER ID ELEMENT OTHERWISE IT                                           
* IS BLANK 7/15/04, YYUN                                                        
*----------------------------------                                             
         MVI   ANYUPDID,C'N'       NO UPDATIVE IDS                              
         B     DREC320                                                          
*                                                                               
DREC310  GOTO1 =V(SCINKEY),APPARM,(2,PERUSR1H),(10,BLOCK1),(R0),       *        
               RR=APRELO                                                        
                                                                                
*----------------------------------                                             
* DISPLAY READ ONLY USER IDS FROM PASSWORD REC.                                 
*----------------------------------                                             
DREC320  MVI   BLKCNT1,0                                                        
         LA    R4,BLOCK1                                                        
*                                                                               
         LA    R3,SA0DATA                                                       
DREC322  CLI   0(R3),0             TEST END-OF-RECORD                           
         BE    DREC370                                                          
         CLI   0(R3),SAIDELQ       TEST ID ELEMENT                              
         BNE   DREC330                                                          
*                                                                               
         TM    SAIDTYP,SAIDNEQ     NEW TYPE?                                    
         BNZ   DREC330             NO                                           
         TM    SAIDTYP,SAIDROQ     READ ONLY USERID?                            
         BO    DREC340             YES                                          
*                                                                               
DREC330  LLC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     DREC322                                                          
*                                  BUILD USER IDS IN BLOCK                      
DREC340  MVC   0(10,R4),SPACES                                                  
         LR    RE,R4                                                            
         TM    SAIDTYP,SAIDLIQ     ID LIST?                                     
         BZ    DREC350                                                          
         MVC   0(2,R4),=C'L='                                                   
         LA    RE,2(R4)                                                         
         B     DREC360                                                          
*                                                                               
DREC350  TM    SAIDTYP,SAIDAGQ     AGY NAME?                                    
         BZ    DREC360                                                          
         MVC   0(2,R4),=C'A='                                                   
         LA    RE,2(R4)                                                         
*                                                                               
DREC360  LLC   R1,SAIDLEN                                                       
         SHI   R1,SAIDNAM-SAIDD+1                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SAIDNAM                                                  
*                                                                               
         CLC   0(4,RE),=C'ALL '    CHECK FOR ALL AND TRANSLATE                  
         BNE   *+10                                                             
         MVC   0(L'CT@ALL,RE),CT@ALL                                            
*                                                                               
         LA    R4,10(R4)                                                        
         SR    R1,R1                                                            
         IC    R1,BLKCNT1                                                       
         LA    R1,1(R1)            BUMP COUNTER                                 
         STC   R1,BLKCNT1                                                       
         B     DREC330                                                          
         DROP  R3                                                               
*                                                                               
DREC370  SR    R0,R0                                                            
         ICM   R0,1,BLKCNT1        ANY READONLY?                                
         BZ    DREC380                                                          
         GOTO1 =V(SCINKEY),APPARM,(2,PERURR1H),(10,BLOCK1),(R0),       *        
               RR=APRELO                                                        
         B     DREC400                                                          
         DROP  R2                                                               
*                                                                               
DREC380  CLI   ANYUPDID,C'Y'       ANY UPDATIVE IDS?                            
         BE    DREC400             YES                                          
         LA    R1,PERUSR1H                                                      
         BRAS  RE,ECHOALL          NO ID ELEMENT, 'ALL' FOR UPD IDS             
*                                                                               
DREC400  MVC   IOKEY,APRECKEY                                                   
*&&UK                                                                           
         USING SAPEREC,R2                                                       
         L     R2,AIOAREA1                                                      
         GOTO1 ADISACT,SAPEREC     DISPLAY ACTIVITY DATE                        
*&&                                                                             
*&&US                                                                           
         USING SA0REC,R2                                                        
         L     R2,AIOAREA3                                                      
         GOTO1 ADISACT,SA0REC      DISPLAY ACTIVITY DATE                        
*&&                                                                             
         DROP  R2                                                               
*                                                                               
         TM    CUSTAT,CUSDDL       DDLINK ACTIVE?                               
         JZ    DISRECX                                                          
*                                                                               
         MVC   DDLKPWD,SPACES      ONLY SEND PASSWORD ON ADD ACTION             
         CLI   APACTN,ACTADD       YES, ADD ACTION?                             
         BNE   DREC412                                                          
         LA    R3,APELEM           YES, GET NEW (TEMPORARY) PASSWORD            
         USING SAPWDD,R3                                                        
         XC    SAPWDEL(SAPWDLNQ),SAPWDEL                                        
         MVI   SAPWDEL,SAPWDELQ                                                 
         L     R1,AIOAREA1                                                      
         GOTO1 AGETELS                                                          
         ICM   R3,15,APPARM                                                     
         BZ    DREC412                                                          
         MVC   DDLKPWD,SAPWDCOD                                                 
         DROP  R3                                                               
*                                                                               
DREC412  OC    LASTCDT,LASTCDT     YES, CHANGE DATE/TIME PRESENT?               
         JZ    DISRECX             NO, EXIT                                     
         GOTO1 VDATCON,APPARM,(3,LASTCDT),(23,DDLKACDT) YYYY-MM-DD              
         XR    R0,R0                                                            
         LR    R1,R0                                                            
         ICM   R1,7,LASTCTM        DDLKACTM IS TIME OF DAY AS SEC/100           
         D     R0,=F'100'          CONVERT TO SECONDS                           
         XR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,APDUB            R0=SS                                        
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(3),APDUB+6(2)                                             
         MVC   DDLKACTM+6(2),APWORK+1                                           
         MVI   DDLKACTM+5,C':'                                                  
         XR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,APDUB            R0=MM                                        
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(3),APDUB+6(2)                                             
         MVC   DDLKACTM+3(2),APWORK+1                                           
         MVI   DDLKACTM+2,C':'                                                  
         CVD   R1,APDUB            R1=HH                                        
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(3),APDUB+6(2)                                             
         MVC   DDLKACTM+0(2),APWORK+1                                           
*                                                                               
DISRECX  J     EXIT                                                             
                                                                                
***********************************************************************         
* HANDLE REENTER PROMPT                                                         
***********************************************************************         
REENTER  CLI   APACTN,ACTDEL       HANDLE DELETE REENTER PROMPT                 
         BE    *+12                                                             
         CLI   APACTN,ACTRES       HANDLE RESTORE REENTER PROMPT                
         BNE   *+8                                                              
*                                  AVOID NO DATA ENTERED SYSTEM MESSAGE         
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DELETE PERSON AND PASSWORD RECORDS                       *         
***********************************************************************         
DELREC   DS    0H                                                               
         MVC   APHALF,CUAALF       GET AGENCY APLHA ID                          
         OC    OPTAGY,OPTAGY         OR OPTION OVERRIDE                         
         BZ    *+10                                                             
         MVC   APHALF,OPTAGY                                                    
*&&US                                                                           
         GOTO1 VGETFACT,APPARM,0   GET A(SYSTEM INFO BLOCK)                     
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         CLI   FASYSID,X'04'       CHECK AND SEE IF YOU ARE CONNECTED           
         BE    DELR005                TO A REP FACPAC. (4, 9 & 14)              
         CLI   FASYSID,X'09'          FROM THE FACIDTAB.  IF FASYSID            
         BE    DELR005                IS ANY OF THESE NUMBERS SKIP              
         CLI   FASYSID,X'0E'          CHECKING THE ACC SIDES FOR NOW            
         BE    DELR005                                                          
         DROP  R1                                                               
*                                                                               
*&&                                                                             
         TM    CUSTAT,CUSDDS       CHECK PASSWORD IF DDS TERMINAL               
         BNO   *+12                                                             
         CLI   OPTPWS,C'Y'         IF PASSWORD OPTION IS SET TO 'Y'             
         BE    DELR005             DO NOT BOTHER CHECKING PERSON-JHAY           
*                                                                               
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL        GET TODAYS DATE                              
         XC    TODAYC,TODAY                                                     
*                                                                               
         USING SAPEREC,R1                                                       
         L     R1,AIOAREA1                                                      
         CLC   SAPEDEF,TODAYC      IS THIS A FUTURE RECORD?                     
         BL    DELR005             YES - NO NEED TO CHECK ACC-JUST DEL          
         DROP  R1                                                               
*                                                                               
         USING SAPEREC,R2                                                       
         LA    R2,IOKEY            READ FOR HISTORIC RECORDS.                   
         XC    SAPEKEY,SAPEKEY                                                  
         MVC   SAPEKEY,0(R1)       SET KEY FROM CURRENT ONE                     
         XC    SAPEDEF,SAPEDEF     SET TO ZERO FOR READ HIGN                    
         GOTO1 AIO,IOHI+IOCONFIL+IO3                                            
         B     DELR002                                                          
DELR001  GOTO1 AIO,IOSQ+IOCONFIL+IO3    READ SEQ                                
DELR002  L     R1,AIOAREA1         R1=CURRENT RECORD TRYING TO DELETE           
         L     R2,AIOAREA3         R2=REC READ FOR EFFECTIVE DATE COMP          
         CLC   SAPEKEY,0(R1)       DID WE GET THE SAME KEY?                     
         BE    DELR004             YES-MUST BE CURRENT REC - READ ACC           
         CLC   SAPEDEF,TODAYC      IS THIS A FUTURE RECORD?                     
         BL    DELR001             YES - READ SEQ UNTIL TO GET CURRENT          
*                                                                               
         USING SA0REC,R2                                                        
         MVC   IOKEY,SA0KEYSV      ELSE - RESTORE IO3 AND DELETE REC.           
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         B     DELR005                                                          
*                                                                               
DELR004  L     RF,=A(CHKPER)       CHECK ACC RECORDS FOR PID                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   NO                                                               
*                                                                               
         USING SAPEREC,R2                                                       
DELR005  CLC   ODISPDEF,OCURPDEF   CHECK IF CURRENT RECORD                      
         BNE   DELR100               IF NOT ONLY DELETE THIS ONE                
         L     R2,AIOAREA1           ELSE DELETE ALL OTHERS                     
         LA    R3,SAPEDATA         GET PERSON RECORD ELEMENT DATA               
DELR010  CLI   0(R3),0             E-O-R                                        
         BE    DELR020                                                          
         CLI   0(R3),SAPERELQ                                                   
         BE    DELRPEE                                                          
         CLI   0(R3),SAAGCELQ                                                   
         BE    DELRAGE                                                          
         CLI   0(R3),SAAPCELQ                                                   
         BE    DELRAPC                                                          
         CLI   0(R3),SALACELQ                                                   
         BE    DELRLAE                                                          
DELR012  ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DELR010                                                          
*                                  PERSONAL DATAILS ELEMENT                     
         USING SAPERD,R3                                                        
DELRPEE  OC    SAPEROFF,SAPEROFF   UPDATE OFFICE RECORD STAFF COUNT             
         BZ    DELR012                                                          
         MVC   APWORK(L'SAPEROFF),SAPEROFF                                      
         BRAS  RE,DECOFFS          BY DECREMENTING IT                           
DELRP010 OC    SAPERDID,SAPERDID   UPDATE DEPARTMENT RECORD STAFF COUNT         
         BZ    DELR012                                                          
         MVC   APWORK(L'SAPEROFF),SAPEROFF                                      
         MVC   APWORK+L'SAPEROFF(L'SAPERDID),SAPERDID                           
         BRAS  RE,DECDPTS          BY DECREMENTING IT                           
         B     DELR012                                                          
*                                  ACCESS GROUP ELEMENT                         
         USING SAAGCD,R3                                                        
DELRAGE  OC    SAAGCCOD,SAAGCCOD   UPDATE ACCESS GROUP REC STAFF COUNT          
         BZ    DELR012                                                          
         MVC   APWORK(L'SAAGCCOD),SAAGCCOD                                      
         BRAS  RE,DECAGRS          BY DECREMENTING IT                           
         B     DELR012                                                          
*                                  APPROVER GROUP ELEMENT                       
         USING SAAPCD,R3                                                        
DELRAPC  OC    SAAPCCOD,SAAPCCOD   UPDATE APPROVER GROUP REC STAFF CNT          
         BZ    DELR012                                                          
         MVC   APWORK(L'SAAPCCOD),SAAPCCOD                                      
         BRAS  RE,DECAPRS          BY DECREMENTING IT                           
         B     DELR012                                                          
*                                  LIMIT ACCESS GROUP ELEMENT                   
         USING SALACD,R3                                                        
DELRLAE  OC    SALACNUM,SALACNUM   UPDATE ACCESS GROUP REC STAFF COUNT          
         BZ    DELR012                                                          
         MVC   APWORK(L'SALACNUM),SALACNUM                                      
         BRAS  RE,DECLAGS          BY DECREMENTING IT                           
         B     DELR012                                                          
*                                                                               
DELR020  L     RF,=A(CLEARPID)     DELETE ALL PERSON/PASSWORD RECORDS           
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     DELRECX                                                          
*                                                                               
         USING SAPEREC,R2                                                       
DELR100  L     R2,AIOAREA1         DELETE SINGLE PERSON RECORD                  
         MVC   IOKEY(L'SAPEKEY),APRECKEY                                        
         GOTO1 ASETACT,SAPEREC                                                  
         OI    SAPESTAT,X'80'                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                  CHECK IF ASSOCIATED PASSWORD RECORD          
*                                    SHOULD BE DELETED AS WELL                  
         CLI   PIDREQD,C'Y'        CHECK PERSONAL ID PASSWORD                   
         BE    DELRECX                                                          
         CLI   PWDIOERR,IOERNF                                                  
         BE    DELRECX                                                          
         LA    R4,PDEFTAB          LOOK AT PERSON HISTORY SAVE TABLE            
         A     R4,ODISPDEF                                                      
         CLC   ODISPDEF,OFRSTDEF   POINT TO THIS PERSON/PASSWORD                
         BE    DELRECX                                                          
         LR    RF,R4                                                            
         LA    RE,PDEFTLEN                                                      
         AR    R4,RE                                                            
*                                  CHECK IF PASSWORD SAME AS PREVIOUS           
         CLC   L'SAPEDEF(L'SA0KCODE,R4),L'SAPEDEF(RF)                           
         BE    DELRECX             IF SO EXIT WITHOUT CHANGE                    
         SR    R4,RE                                                            
         SR    RF,RE                                                            
*                                  CHECK IF PASSWORD SAME AS NEXT               
         CLC   L'SAPEDEF(L'SA0KCODE,R4),L'SAPEDEF(RF)                           
         BNE   DELR110                                                          
*                                  HERE IF PASSWORD SAME AS NEXT                
*                                  UPDATE PASSWORD RECORD EFF. DATES            
*                                  TO INDICATE REMOVAL OF PERSON REC            
         BRAS  RE,RDPWDDEF         GET PASSWORD EFFECTIVE DATES                 
         SR    RF,RF                                                            
         ICM   RF,3,PEFENDSV                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,APHALF                                                      
         MVC   PEFSTASV,FFILL                                                   
         LA    RE,PDEFTLEN                                                      
         LR    RF,R4                                                            
         SR    RF,RE                                                            
         XC    PEFSTASV,0(RF)                                                   
         BRAS  RE,UPDOLDPW         UPDATE THIS PASSWORD RECORD                  
*                                                                               
         LA    RE,PDEFTLEN         UPDATE PREVIOUS PASSWORD RECORD              
         AR    R4,RE                                                            
         MVC   OLDDEFC,0(R4)                                                    
         BRAS  RE,READPID          READ PERSONAL ID RECORD                      
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   PIDIOERR,IOERNF                                                  
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   OLDCODE,APWORK                                                   
         MVC   OLDCODU,APWORK                                                   
         BRAS  RE,READPWD          READ ASSOCIATED PASSWORD RECORD              
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   PWDIOERR,IOERNF     CHECK PASSWORD NOT REMOVED                   
         BE    DELRECX                                                          
*                                                                               
         BRAS  RE,CRPIDPWD                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         SR    RF,RF                                                            
         ICM   RF,3,PEFSTASV                                                    
         STCM  RF,3,APHALF                                                      
         BRAS  RE,RDPWDDEF                                                      
         BRAS  RE,UPDOLDPW         UPDATE PASSWORD RECORD                       
         B     DELRECX                                                          
         EJECT                                                                  
*                                  HERE IF PASSWORD NOT SAME AS NEXT            
*                                  UPDATE PASSWORD RECORD EFF. DATES            
*                                  TO INDICATE REMOVAL OF PERSON REC            
DELR110  BRAS  RE,RDPWDDEF         GET PASSWORD EFFECTIVE DATES                 
         MVC   APHALF,PEFSTASV                                                  
         BRAS  RE,UPDOLDPW         UPDATE THIS PASSWORD RECORD                  
*                                                                               
         LA    RE,PDEFTLEN         UPDATE PREVIOUS PASSWORD RECORD              
         AR    R4,RE                                                            
         MVC   OLDDEFC,0(R4)                                                    
         BRAS  RE,READPID          READ PERSONAL ID RECORD                      
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   PIDIOERR,IOERNF                                                  
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   OLDCODE,APWORK                                                   
         MVC   OLDCODU,APWORK                                                   
         BRAS  RE,READPWD          READ ASSOCIATED PASSWORD RECORD              
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   PWDIOERR,IOERNF     CHECK PASSWORD NOT REMOVED                   
         BE    DELRECX                                                          
*                                                                               
         BRAS  RE,CRPIDPWD                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         SR    RF,RF                                                            
         ICM   RF,3,PEFENDSV                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,APHALF                                                      
         BRAS  RE,RDPWDDEF                                                      
         BRAS  RE,UPDOLDPW         UPDATE PASSWORD RECORD                       
         B     DELRECX                                                          
*                                                                               
DELRECX  BRAS  RE,INITDEFT         CLEAR EFFECTIVE DATE KEY SAVE TABLE          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO SAVE PASSWORD EFF. DATES                                           
***********************************************************************         
         USING SA0REC,R2                                                        
RDPWDDEF NTR1                                                                   
         L     R2,AIOAREA3                                                      
         LA    R3,SA0DATA          GET PASSWORD ELEMENT DATA                    
RPD010   CLI   0(R3),0             E-O-R                                        
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R3),SAPEFELQ                                                   
         BE    RPD014                                                           
RPD012   ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RPD010                                                           
*                                  PASSWORD EFFECTIVE DATES ELEMENT             
         USING SAPEFD,R3                                                        
RPD014   MVC   PEFSTASV,SAPEFSTA   SAVE ELEMENT DATA                            
         MVC   PEFENDSV,SAPEFEND                                                
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO RESTORE DELETED PERSON AND PASSWORD RECORDS              *         
***********************************************************************         
         USING SAPEREC,R2                                                       
RESREC   EQU   *                                                                
         CLI   PDEFXFLG,C'Y'                                                    
         BNE   *+20                                                             
         MVC   FVMSGNO,=AL2(CE#HPERR)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     EXIT                                                             
         L     R2,AIOAREA1         RESTORE PERSON RECORD                        
         LA    R3,SAPEDATA         GET PERSON RECORD ELEMENT DATA               
RESR010  CLI   0(R3),0             E-O-R                                        
         BE    RESR020                                                          
         CLI   0(R3),SAPERELQ                                                   
         BE    RESRPEE                                                          
         CLI   0(R3),SAAGCELQ                                                   
         BE    RESRAGE                                                          
         CLI   0(R3),SAAPCELQ                                                   
         BE    RESRAPC                                                          
         CLI   0(R3),SALACELQ                                                   
         BE    RESRLAE                                                          
RESR012  ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RESR010                                                          
*                                  PERSONAL DATIALS ELEMENT                     
         USING SAPERD,R3                                                        
RESRPEE  OC    SAPEROFF,SAPEROFF   UPDATE OFFICE STAFF COUNT                    
         BZ    RESR012                                                          
         MVC   APWORK(L'SAPEROFF),SAPEROFF                                      
         BRAS  RE,INCOFFS          BY INCREMENTING IT                           
RESRP010 OC    SAPERDID,SAPERDID   UPDATE DEPARTMENT RECORD STAFF COUNT         
         BZ    RESR012                                                          
         MVC   APWORK(L'SAPEROFF),SAPEROFF                                      
         MVC   APWORK+L'SAPEROFF(L'SAPERDID),SAPERDID                           
         BRAS  RE,INCDPTS          BY DECREMENTING IT                           
         B     RESR012                                                          
*                                  ACCESS GROUP ELEMENT                         
         USING SAAGCD,R3                                                        
RESRAGE  OC    SAAGCCOD,SAAGCCOD   UPDATE ACCESS GROUP STAFF COUNT              
         BZ    RESR012                                                          
         MVC   APWORK(L'SAAGCCOD),SAAGCCOD                                      
         BRAS  RE,INCAGRS          BY INCREMENTING IT                           
         B     RESR012                                                          
*                                  APPROVER GROUP CODE ELEMENT                  
         USING SAAPCD,R3                                                        
RESRAPC  OC    SAAPCCOD,SAAPCCOD   UPDATE APPROVER GROUP STAFF COUNT            
         BZ    RESR012                                                          
         MVC   APWORK(L'SAAPCCOD),SAAPCCOD                                      
         BRAS  RE,INCAPRS          BY INCREMENTING IT                           
         B     RESR012                                                          
*                                  LIMIT ACCESS GROUP ELEMENT                   
         USING SALACD,R3                                                        
RESRLAE  OC    SALACNUM,SALACNUM   UPDATE ACCESS GROUP STAFF COUNT              
         BZ    RESR012                                                          
         MVC   APWORK(L'SALACNUM),SALACNUM                                      
         BRAS  RE,INCLAGS          BY INCREMENTING IT                           
         B     RESR012                                                          
*                                                                               
RESR020  L     R2,AIOAREA1         RESTORE PERSON RECORD                        
         MVC   IOKEY(L'SAPEKEY),APRECKEY                                        
         GOTO1 ASETACT,SAPEREC                                                  
         NI    SAPESTAT,X'FF'-X'80'                                             
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                  RESTORE PASSWORD RECORDS                     
         DROP  R2                                                               
         USING SA0REC,R2                                                        
         LA    R2,IOKEY                                                         
         MVC   SA0KEY,SA0KEYSV                                                  
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         L     R2,AIOAREA2         UNLOCK PASSWORD RECORD                       
         BRAS  RE,UNLKPWD                                                       
         CLI   PIDREQD,C'Y'        CHECK PERSONAL ID PASSWORD                   
         BE    RESRECX                                                          
*                                  RESTORE PASSWORD NUMBER RECORD               
         LA    R2,IOKEY                                                         
         XC    SA0KEYS,SA0KEYS                                                  
         MVC   SA0KNUM,OLDPWD                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         L     R2,AIOAREA2         UNLOCK PASSWORD RECORD                       
         BRAS  RE,UNLKPWD                                                       
*                                                                               
RESRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET EFFECTIVE DATE END, RESTORE PERSONAL ID POINTER                           
* AND UNLOCK PASSWORD RECORD                                                    
***********************************************************************         
UNLKPWD  NTR1                                                                   
         LA    R3,SA0DATA                                                       
*                                                                               
UPWD010  CLI   0(R3),0             TEST END OF RECORD                           
         BE    UPWD100                                                          
         CLI   0(R3),SAPEFELQ                                                   
         BE    UPWD030                                                          
         CLI   0(R3),SAPALELQ                                                   
         BE    UPWD040                                                          
UPWD020  SR    R0,R0                                                            
         IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     UPWD010                                                          
*                                  SET END DATE TO FFS                          
         USING SAPEFD,R3                                                        
UPWD030  MVC   SAPEFEND,FFILL                                                   
         B     UPWD020                                                          
*                                  RESTORE PID                                  
         USING SAPALD,R3                                                        
UPWD040  MVC   SAPALPID,OLDPID                                                  
         B     UPWD020                                                          
*                                  UNLOCK PASSWORD RECORD                       
UPWD100  MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         GOTO1 ASETACT,SA0REC                                                   
         NI    SA0STAT,X'FF'-X'A0'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)    *         
***********************************************************************         
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         J     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
                                                                                
         USING SAPEREC,R2                                                       
VALSEL   MVI   PIDREQD,C'N'        GET AGENCY ACCESS DETAILS                    
         GOTO1 AGETAAD,AGENCYID                                                 
         TM    APWORK,CTAADPRQ     PERSONAL ID REQUIRED WITH PASSWORD?          
         BZ    *+8                                                              
         MVI   PIDREQD,C'Y'                                                     
         MVC   PWDINFO,APWORK      SAVE PASSWORD ATTRIBUTES                     
*                                                                               
         LA    R2,APRECKEY                                                      
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    SAPEKEY,SAPEKEY     INITIALISE PERSON RECORD KEY                 
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAVPARM(32),APPARM  APPARM USED BY LIST PROCESS                  
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL        GET TODAYS DATE                              
         XC    TODAYC,TODAY                                                     
         XC    SAVPID,SAVPID       INIT. LAST PID READ                          
         XC    SELOPT(SELOPTL),SELOPT  INIT. SELECT FILTERS                     
*                                                                               
VSPID    GOTO1 AFVAL,LSTPIDH       VALIDATE PERSON ID FILTER                    
         BNE   VSPIDX                                                           
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VSPID1   CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VSPID2              FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VSPID1                                                        
VSPID2   STC   RE,SELKEYCL                                                      
         MVC   SELPID,FVIFLD                                                    
         MVC   SELPIDL,FVILEN                                                   
         MVC   SELPIDSP,0(RF)                                                   
VSPIDX   EQU   *                                                                
         MVC   SAPEPID,FVIFLD                                                   
*                                                                               
VS002    GOTO1 AFVAL,LSTUSRH      VALIDATE USER ID FILTER                       
         BNE   VS010                                                            
         GOTO1 AVALUID,LSTUSRH                                                  
         BNE   VALSELX                                                          
         MVC   SELUSR,FVIFLD                                                    
*                                                                               
VS010    GOTO1 AFVAL,LSTNAMEH      VALIDATE LAST NAME FILTER                    
         BNE   VS020                                                            
         MVC   SELNAM,FVIFLD                                                    
         MVC   SELNAML,FVILEN                                                   
*                                                                               
VS020    GOTO1 AFVAL,LSTDEFH       VALIDATE EFFECTIVE DATE FILTER               
         BNE   VS030                                                            
         MVC   SELDEFC,FFILL                                                    
         ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BE    VS026                                                            
         SR    RE,RE                                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDALL           'ALL' KEYWORD                                
         BNE   SAEIIF                                                           
         LA    R1,LSTDEFH                                                       
         BRAS  RE,ECHOALL                                                       
         XC    SELDEFC,FFILL                                                    
         B     VS028                                                            
VS026    XC    SELDEFC,APWORK+PVALCSTA-PERVALD                                  
VS028    MVC   SAPEDEF,SELDEFC                                                  
         MVI   SELDEFF,1           FLAG FILTER SET                              
*                                                                               
VS030    GOTO1 AFVAL,LSTAGRH       VALIDATE ACCESS GROUP FILTER                 
         BNE   VS032                                                            
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2                                                       
         XC    SAAGKEY,SAAGKEY     CHECK ACCESS GROUP RECORD EXISTS             
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELAGR,FVIFLD                                                    
*                                                                               
VS032    GOTO1 AFVAL,LSTAPCH       VALIDATE APPROVER GROUP CODE FILTER          
         BNE   VS040                                                            
         LA    R2,IOKEY                                                         
         USING SAAPREC,R2                                                       
         XC    SAAPKEY,SAAPKEY     CHECK APPROVER GROUP RDCORD EXISTS           
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID                                                 
         MVC   SAAPAGR,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELAPC,FVIFLD                                                    
*                                                                               
VS040    GOTO1 AFVAL,LSTOFFH       VALIDATE OFFICE CODE FILTER                  
         BNE   VS050                                                            
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY     CHECK OFFICE RECORD EXISTS                   
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELOFF,FVIFLD                                                    
*                                                                               
VS050    GOTO1 AFVAL,LSTDIDH       VALIDATE DEPARTMENT CODE FILTER              
         BNE   VS060                                                            
         OC    SELOFF,SELOFF                                                    
         BZ    SAEODF                                                           
         LA    R2,IOKEY                                                         
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY     CHECK DEPARTMENT RECORD EXISTS               
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,SELOFF                                                   
         MVC   SADPDID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELDID,FVIFLD                                                    
*                                                                               
VS060    GOTO1 AFVAL,LSTOPIH      VALIDATE OLD PERSON ID FILTER                 
         BNE   VALSELY                                                          
         MVC   SELOPI,FVIFLD                                                    
*                                                                               
VALSELY  MVC   APPARM(32),SAVPARM  RESTORE APPARM                               
         MVI   GETSEQF,0           INTERNAL READ SEQUENCE FLAG                  
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  J     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
* INDIRECT BRANCH OUTSIDE MAOIN ADDRESSABLE AREA                      *         
***********************************************************************         
         USING SAPEREC,R2                                                       
GETSELI  L     RF,=A(GETSELA)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         USING SAPEREC,R2                                                       
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                  GET DATA FROM RECORD KEY                     
         MVC   LISTPID,SAPEPID       DISPLAY PERSONAL-ID                        
         MVC   APHALF,FFILL                                                     
         XC    APHALF,SAPEDEF                                                   
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,APHALF),(X'08',APWORK)                         
         MVC   LISTDEF,APWORK      DISPLAY EFFECTIVE DATE                       
         CLI   EXPFLAG,C'Y'                                                     
         BNE   DSEL010                                                          
         MVI   LISTTERM,C'*'                                                    
*                                  GET DATA FROM ELEMENTS                       
DSEL010  LA    R3,SAPEDATA                                                      
*                                                                               
DSLP1    CLI   0(R3),0             E-O-R                                        
         BE    DISSELX                                                          
         CLI   0(R3),SANAMELQ                                                   
         BE    DSNAME                                                           
         CLI   0(R3),SAPERELQ                                                   
         BE    DSPER                                                            
         CLI   0(R3),SAAGCELQ                                                   
         BE    DSAGC                                                            
         CLI   0(R3),SALACELQ                                                   
         BE    DSLAC                                                            
*                                                                               
DSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSLP1                                                            
*                                                                               
         USING SANAMD,R3                                                        
DSNAME   LR    R1,R3               DISPLAY PERSON NAME                          
         GOTO1 ADISPNAM                                                         
         MVC   LISTNAME,APWORK                                                  
         B     DSLP1A                                                           
*                                                                               
         USING SAPERD,R3                                                        
DSPER    CLI   SAPERLN,SAPERLNQ    DISPLAY PERSONNEL DETAILS                    
         BL    DSLP1A                                                           
         MVC   LISTOFF,SAPEROFF                                                 
         MVC   LISTDID,SAPERDID                                                 
         MVC   LISTEXT,SAPEREXT                                                 
         B     DSLP1A                                                           
*                                                                               
         USING SAPWDD,R3                                                        
DSPWD    CLI   SAPWDLN,SAPWDLNQ    DISPLAY PASSWORD DETAILS                     
         BL    DSLP1A                                                           
         MVC   LISTPWD,SAPWDCOD                                                 
         B     DSLP1A                                                           
*                                                                               
         USING SAAGCD,R3                                                        
DSAGC    CLI   SAAGCLN,SAAGCLNQ    DISPLAY ACCESS GROUP CODE                    
         BL    DSLP1A                                                           
         MVC   LISTAGR,SAAGCCOD                                                 
         B     DSLP1A                                                           
*                                                                               
         USING SALACD,R3                                                        
DSLAC    CLI   SALACLN,SALACLNQ    DISPLAY LIMIT ACCESS GROUP CODE              
         BL    DSLP1A                                                           
         MVC   LISTLAG,SALACCOD                                                 
         B     DSLP1A                                                           
*                                                                               
DISSELX  J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
                                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE INITIALISATION OF TWA                             *         
***********************************************************************         
                                                                                
SETTWA   EQU   *                                                                
         CLI   APACTN,ACTGRD       ACTION GRIDS?                                
         JE    EXIT                                                             
*                                                                               
*&&UK*&& CLC   =C'#E',AGENCYID                                                  
*&&US*&& CLC   =C'#N',AGENCYID                                                  
         BE    STWA040                                                          
*&&US                                                                           
         L     RF,ASYS                                                          
         L     RF,VSSB-SYSFACD(RF) GET FACPAK SYSTEM ID INFO FROM SSB           
         CLI   SSBSYSID-SSBD(RF),11      CSC SYSTEM?                            
         BE    STWA020                                                          
         TM    SSBSYSFL-SSBD(RF),X'80'   TEST SYSTEM                            
         BO    STWA040                   YES                                    
*&&                                                                             
STWA020  EQU   *                         DON'T SHOW READ-ONLY FIELD             
         XC    PERUPID,PERUPID                                                  
         MVCDD PERUPID,CT#CUSRS          COMPANY USER IDS                       
         OI    PERROIDH+(FVATRB-FVIHDR),FVALOWI                                 
         OI    PERURR1H+(FVATRB-FVIHDR),FVAPROT                                 
         OI    PERURR2H+(FVATRB-FVIHDR),FVAPROT                                 
         OI    PERUPIDH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERROIDH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERURR1H+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERURR2H+(FVOIND-FVIHDR),FVOXMT                                  
         B     STWA050                                                          
*                                                                               
STWA040  EQU   *                                                                
         XC    PERUPID,PERUPID                                                  
         MVCDD PERUPID,CT#UPUID          UPDATIVE IDS                           
         NI    PERROIDH+(FVATRB-FVIHDR),X'FF'-FVALOWI                           
         OI    PERROIDH+(FVATRB-FVIHDR),FVAHIGH                                 
         NI    PERURR1H+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    PERURR2H+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         OI    PERUPIDH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERROIDH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERURR1H+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PERURR2H+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
STWA050  EQU   *                                                                
         TM    CUSTAT,CUSDDS                                                    
         BO    STWA060                                                          
*                                  DDS ONLY ACCESS TO OVERIDE FLAG              
         OI    PEROVHH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PEROVFH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PERPIHH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PERPINH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PERPCHH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PERPCNH+(FVATRB-FVIHDR),X'FF'                                    
         B     STWA100                                                          
*                                  DDS ONLY OPTION TO VIEW PIN ?                
STWA060  CLI   OPTPIN,C'Y'                                                      
         BE    STWA070                                                          
         OI    PERPIHH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PERPINH+(FVATRB-FVIHDR),X'FF'                                    
         B     STWA100                                                          
STWA070  NI    PERPIHH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    PERPINH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         OI    PERPIHH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    PERPINH+(FVOIND-FVIHDR),FVOXMT                                   
         B     SETTWAX                                                          
*                                                                               
*                                  CHECK PASSWORD FIELD SECURITY                
STWA100  OC    ACASEC,ACASEC                                                    
         BZ    SETTWAX                                                          
         CLI   ASONOFF,ASOFF                                                    
         BE    SETTWAX                                                          
         MVI   APBYTE,FLDPWD                                                    
         LA    RF,APBYTE                                                        
         GOTO1 VSECRET,SCPARM,('SECPFLDP',ACASEC),(RF)                          
         BL    STWA110                                                          
         BH    STWA120                                                          
         B     SETTWAX             FULL READ/WRITE ACCESS                       
*                                  NO ACCESS                                    
STWA110  OI    PERPWHH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PERPNHH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PERPWDH+(FVATRB-FVIHDR),X'FF'                                    
         OI    PERPWDNH+(FVATRB-FVIHDR),X'FF'                                   
         B     SETTWAX                                                          
*                                  READ ACCESS ONLY                             
STWA120  OI    PERPWDH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    PERPWDNH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    PERPWDH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    PERPWDNH+(FVOIND-FVIHDR),FVOXMT                                  
         B     SETTWAX                                                          
*                                                                               
SETTWAX  J     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
         USING SAPEREC,R2                                                       
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         SR    R0,R0                                                            
*                                                                               
         MVI   0(R3),KEYPID                                                     
         MVI   1(R3),10                                                         
         MVC   2(8,R3),SAPEPID                                                  
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),0                                                          
         GOTO1 APUTKEY                                                          
*                                                                               
PUTKEYX  J     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO READ PASSWORD RECORD                                     *         
* ON ENTRY:                                                           *         
*        PASSWORD CODE IN OLDCODE                                     *         
*        PASSWORD # IN OLDPWD                                         *         
* ON EXIT:                                                            *         
*        ASSOCIATED PERSONAL ID IN APWORK                             *         
*        PASSWORD # IN APHALF                                         *         
*        RECORD KEY IN SA0KEYSV                                       *         
*        RECORD IN AIOAREA3                                           *         
*        IO ERROR STATE IN PWDIOERR                                   *         
***********************************************************************         
READPWD  NTR1                                                                   
         MVI   PWDIOERR,IOERNF                                                  
         CLI   PIDREQD,C'Y'        TEST FOR PERSONAL ID PASSWORD                
         BE    RPWD002                                                          
*                                  READ PASSWORD RECORD                         
         LA    R4,IOKEY            BUILD KEY                                    
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KCODE,OLDCODE                                                 
         MVC   SA0KEYSV(L'SA0KEY),SA0KEY                                        
         LA    R1,IORDD+IOCONFIL+IO3                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    SAEIIO                                                           
         MVC   PWDIOERR,IOERR      SAVE IO ERROR INDICATORS                     
         CLI   PWDIOERR,0                                                       
         BE    RPWD010                                                          
         TM    PWDIOERR,IOEDEL                                                  
         BZ    RPWDYES             RECORD NOT FOUND                             
         B     RPWD010                                                          
*                                                                               
RPWD002  EQU   *                   READ PWD # RECORD                            
         MVC   APHALF,OLDPWD                                                    
         MVI   PWDIOERR,IOERNF                                                  
         LA    R4,IOKEY            BUILD KEY                                    
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,OLDPWD                                                   
         MVC   SA0KEYSV(L'SA0KEY),SA0KEY                                        
         OC    OLDPWD,OLDPWD       ?? CHECK THIS                                
         BZ    RPWDYES                                                          
         LA    R1,IORDD+IOCONFIL+IO3                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    SAEIIO                                                           
         MVC   PWDIOERR,IOERR      SAVE IO ERROR INDICATORS                     
         CLI   PWDIOERR,0                                                       
         BE    RPWD010                                                          
         TM    PWDIOERR,IOEDEL                                                  
         BZ    RPWDYES             RECORD NOT FOUND                             
*                                                                               
RPWD010  L     R4,AIOAREA3         GET SAVE DATA FROM ELEMENTS                  
         TM    SA0STAT,X'20'       CHECK IF LOCKED                              
         BNO   RPWD012                                                          
         MVI   IOERR,IOEDEL        IF SO FLAG IN DELETED STATE                  
         MVC   PWDIOERR,IOERR                                                   
RPWD012  LA    R3,SA0DATA          GET PERSON-ID POINTER ELEMENT                
RPWD020  CLI   0(R3),0             TEST END OF RECORD                           
         BE    RPWD100             PROCESS PERSON ID                            
         CLI   0(R3),SAPALELQ                                                   
         BE    RPWD040                                                          
         CLI   0(R3),SAPASELQ                                                   
         BE    RPWD050                                                          
RPWD030  SR    R0,R0                                                            
         IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     RPWD020                                                          
         USING SAPALD,R3                                                        
RPWD040  MVC   APWORK,SAPALPID     SAVE PERSONAL-ID                             
         B     RPWD030                                                          
         USING SAPASD,R3                                                        
RPWD050  CLI   SAPASLEN,X'04'                                                   
         BNE   RPWD030                                                          
         MVC   APHALF,SAPASDTA     SAVE PASSWORD #                              
         B     RPWD030                                                          
*                                                                               
RPWD100  EQU   *                                                                
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    RPWDYES                                                          
         CLI   APACTN,ACTDIS                                                    
         BE    RPWDYES                                                          
         MVC   IOERRSV,IOERR       READ AND LOCK PASSWORD NUMBER                
         USING SA0REC,R4                                                        
         LA    R4,IOKEY            BUILD KEY                                    
         XC    SA0KEY,SA0KEY       RECORD IF UPDATING                           
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,APHALF                                                   
         LA    R1,IORDD+IOCONFIL+IO2                                            
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         MVC   IOERR,IOERRSV                                                    
         B     RPWDYES                                                          
*                                  EXIT OK                                      
RPWDYES  B     YES                                                              
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
* ROUTINE TO TEST COMPATABILITY OF PERSONAL ID AND PASSWORD RECORDS   *         
***********************************************************************         
CRPIDPWD NTR1                                                                   
         CLC   PIDIOERR,PWDIOERR   IF PERSON/PASSWORD IOERR SAME                
         BE    CRPYES                CONTINUE OK                                
         CLI   PWDIOERR,IOERNF     ELSE EITHER PASSWORD REC CANCELLED           
         BE    CRP010                                                           
         CLI   PWDIOERR,IOEDEL                                                  
         BE    CRP010                                                           
         LA    R1,PERPIDH            OR PERSON RECORD CANCELLED                 
         ST    R1,FVADDR                                                        
         B     SAEPEC                                                           
CRP010   LA    R1,PERPWDH                                                       
         ST    R1,FVADDR                                                        
         B     SAEPWC                                                           
CRPYES   B     YES                                                              
                                                                                
***********************************************************************         
* ROUTINE TO INITIALISE PERSON RECORDS EFFECTIVE DATES SAVE TABLE     *         
***********************************************************************         
INITDEFT XC    PDEFPID,PDEFPID                                                  
         XC    OCURPDEF,OCURPDEF                                                
         XC    ODISPDEF,ODISPDEF                                                
         XC    OFRSTDEF,OFRSTDEF                                                
         BR    RE                                                               
*                                  GETTXT MESSAGE # ERROR EXITS                 
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     NO                  INPUT FIELD ERROR                            
SAEFTL   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         J     NO                  INPUT FIELD TOO LONG                         
SAEFNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         J     NO                  INPUT FIELD NOT NUMERIC                      
SAEFTS   MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         J     NO                  INPUT FIELD TOO SHORT                        
SAEFNH   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     NO                  INPUT FIELD ERROR                            
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         J     NO                  MISSING FIELD                                
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         J     NO                  I/O ERROR                                    
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     NO                  RECORD NOT FOUND                             
SAEDIF   MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         J     NO                  DUPLICATE                                    
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         J     NO                  ALREADY EXISTS                               
SAERTB   MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  RECORD TOO BIG                               
SAEODF   MVC   FVMSGNO,=AL2(CE#OFFDP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  MUST ENTER OFFICE CODE FOR DEPT              
SAEPWR   MVC   FVMSGNO,=AL2(CE#PWDRM)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  PASSWORD HAS BEEN REMOVED                    
SAEPED   MVC   FVMSGNO,=AL2(CE#PIDED)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  CAN NOT CHANGE PID AND EFF. DATE             
SAEPEA   MVC   FVMSGNO,=AL2(CE#PIDAE)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  PERSONAL-ID RECORD ALREADY EXISTS            
SAEPWA   MVC   FVMSGNO,=AL2(CE#PWDAE)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  PASSWORD RECORD ALREADY EXISTS               
SAEFED   MVC   FVMSGNO,=AL2(CE#FLDED)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  CAN NOT CHANGE FIELD WITH FUTURE ED.         
SAEFPE   MVC   FVMSGNO,=AL2(CE#FLDPC)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  CAN NOT CHANGE FIELD/PENDING RECORD          
SAEPWD   MVC   FVMSGNO,=AL2(CE#PWDEL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  PASSWORD RECORD DELETED                      
SAEPEC   MVC   FVMSGNO,=AL2(CE#PIDCH)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  PERSONAL ID HAS INCOMPATIBLE PWD             
SAEPWC   MVC   FVMSGNO,=AL2(CE#PWDCH)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  PASSWORD HAS INCOMPATIBLE PID                
SAEPAC   MVC   FVMSGNO,=AL2(CE#PWDAC)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  PASSWORD RECORD ALREADY CONVERTED            
SAETST   MVC   FVMSGNO,=AL2(CE#TESTM)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  TEST MESSAGE                                 
SAEED1   MVC   FVMSGNO,=AL2(CE#DEFIN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  INVALID EFFECTIVE DATE                       
SAEED2   MVC   FVMSGNO,=AL2(CE#DEFAC)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  INVALID EFFECTIVE DATE FOR ACTION            
SAEED3   MVC   FVMSGNO,=AL2(CE#DEFUP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  EFFECTIVE DATE FUTURE FOR CHANGE             
SAELST   MVC   FVMSGNO,=AL2(CE#IDLST)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  WRONG LIST=NAME FORMAT                       
SAEINF   MVC   FVMSGNO,=AL2(CE#IDRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  USER ID RECORD NOT FOUND                     
SAEWNF   MVC   FVMSGNO,=AL2(CE#WLRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  USER ID LIST RECORD NOT FOUND                
SAEAYNF  MVC   FVMSGNO,=AL2(CE#ALRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  AGENCY (X'9B01') RECORD NOT FOUND            
SAEFPI   MVC   FVMSGNO,=AL2(CE#FEPID)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  FIRST ENTRY MUST BE PID                      
SAEACT   MVC   FVMSGNO,=AL2(CE#AGICT)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  USER ID AGENCY NOT SAME AS CONNECT           
*&&US                                                                           
SAEMAIL  MVC   FVMSGNO,=AL2(CE#IMAIL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  INVALID EMAIL                                
SAEMDOM  MVC   FVMSGNO,=AL2(CE#MIDOM)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  MISSING DOMAIN RECORD                        
SAEIDOM  MVC   FVMSGNO,=AL2(CE#INDOM)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  INVALID DOMAIN                               
*&&                                                                             
SAELAMC  MVC   FVMSGNO,=AL2(CE#LAFMC)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  LIMIT ACCESS MUST BE REMOVED                 
SAEGI0   MVC   FVMSGNO,=AL2(CE#GID00)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  GETID EXIT CODE 00                           
SAEGIF   MVC   FVMSGNO,=AL2(CE#GIDFF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  GETID EXIT CODE FF                           
SAEINC   MVC   FVMSGNO,=AL2(CE#IDNPL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  USER ID NOT IN PID COMPATIBLE LIST           
SAELNC   MVC   FVMSGNO,=AL2(CE#LINPL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  LIST ID NOT IN PID COMPATIBLE LIST           
SAEANC   MVC   FVMSGNO,=AL2(CE#AGNPL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         J     NO                  AGENCY NOT IN PID COMPATIBLE LIST            
SAEONF   MVC   FVMSGNO,=AL2(CE#OFRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,PERPIDH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         J     NO                  OFFICE RECORD NOT FOUND                      
SAEDNF   MVC   FVMSGNO,=AL2(CE#DERNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,PERPIDH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         J     NO                  DEPARTMENT RECORD NOT FOUND                  
SAEANF   MVC   FVMSGNO,=AL2(CE#AGRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,PERPIDH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         J     NO                  ACCESS GROUP RECORD NOT FOUND                
SAEAGCNF MVC   FVMSGNO,=AL2(CE#AGCNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,PERPIDH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         J     NO                  APPROVER GROUP CODE RECORD NOT FOUND         
SAELNF   MVC   FVMSGNO,=AL2(CE#LGRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,PERPIDH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         J     NO                  LIMIT ACCESS GROUP RECORD NOT FOUND          
SAEDDS   MVC   FVMSGNO,=AL2(CE#UPDDS)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,PERPWDH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         J     NO                  CANNOT UPDATE DDS PASSWORD                   
SAEPWDRS MVC   FVMSGNO,=AL2(CE#PWDRS)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,PERPWDNH         SET CURSOR                                   
         ST    R1,FVADDR                                                        
         J     NO                  CANNOT UPDATE DDS PASSWORD                   
SAENAS   MVC   FVMSGNO,=AL2(CE#NASYS)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,PERPIDH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         J     NO                  NOT AUTHORISED FOR SYSTEM(S)                 
SAESPIP  MVC   FVMSGNO,=AL2(346)                                                
         J     NO                  CANT HAVE SPACE IN PASSWORD                  
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         J     EXIT                                                             
*                                                                               
ECHOALL  MVC   FVIFLD-FVIHDR(L'CT@ALL,R1),CT@ALL                                
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BR    RE                                                               
*                                  EXECUTE TYPE INSTRUCTIONS                    
*                                    INPUT FIELD KEYWORD COMPARISONS            
FLDLAST  CLC   FVIFLD(0),CT@LAST                                                
FLDFIRST CLC   FVIFLD(0),CT@FIRST                                               
FLDNOW   CLC   FVIFLD(0),CT@NOW                                                 
FLDCURR  CLC   FVIFLD(0),CT@CURNT                                               
FLDALL   CLC   FVIFLD(0),CT@ALL                                                 
FLDDPT   CLC   FVIFLD(0),CT@DPT                                                 
FLDACCS  CLC   FVIFLD(0),CT@ACCS                                                
         LTORG                                                                  
                                                                                
CTFILE   DC    C'CTFILE '                                                       
FFILL    DC    4X'FF'                                                           
SPACES   DC    80C' '                                                           
*                                                                               
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
*&&US                                                                           
*----------------------------------------------------------------------         
* COUNTRY TABLE =  CL3=CODE                                                     
*                  XL1=FLAG          . X'80' = ZIP CODE SUPPORTED               
*                  CL40=COUNTRY NAME                                            
*----------------------------------------------------------------------         
         DS    0H                                                               
CNTRYTAB DS    0CL44                                                            
         DC    XL3'000',X'80',CL40'UNITED STATES OF AMERICA'                    
         DC    CL3'   ',X'80',CL40'UNITED STATES OF AMERICA'                    
         DC    CL3'USA',X'80',CL40'UNITED STATES OF AMERICA'                    
         DC    CL3'CAN',X'00',CL40'CANADA'                                      
         DC    CL3'CRI',X'00',CL40'COSTA RICA'                                  
         DC    CL3'IND',X'00',CL40'INDIA'                                       
         DC    CL3'JPN',X'00',CL40'JAPAN'                                       
         DC    CL3'PRI',X'80',CL40'PUERTO RICO'                                 
         DC    CL3'MEX',X'00',CL40'MEXICO'                                      
         DC    CL3'VIR',X'80',CL40'VIRGIN ISLANDS (USA)'                        
         DC    CL3'GUM',X'80',CL40'GUAM'                                        
         DC    CL3'TBD',X'00',CL40'TO BE DETERMINED'                            
         DC    X'FF'                                                            
*&&                                                                             
*----------------------------------------------------------------------         
* DATA DICTIONARY = SEACSDICT                                                   
*----------------------------------------------------------------------         
       ++INCLUDE SEACSDICT                                                      
                                                                                
***********************************************************************         
* GRIDS COLUMN TABLE - COVERED BY GRIDFD AND GRIDCD                             
***********************************************************************         
GCTBL    DC    C'080'                         GRIDS FORMAT ID                   
         DC    AL1(6)                         DOR ADJUSTMENT NUMBER             
         DC    AL2(GRDLIN1-TWAD)              DISP TO FIRST LINE                
         DC    AL2(GRDLINL-TWAD)              DISP TO LAST LINE                 
         DC    AL1(L'GRDLIN1)                 LENGTH OF LINE                    
         DC    AL1(GRDLIN2-GRDLIN1)           DISP BETWEEN LINES                
         DC    AL1(0)                         # OF GRIDS DESC LINES             
         DC    AL1(0)                         N/D                               
*                                                                               
       ++INCLUDE SEACS08G                                                       
*                                                                               
GVPGMNQ  EQU   (GVPGMN-GVPGMSD)+(GVPGMS-GVALSD)                                 
GVPGMVQ  EQU   (GVPGMV-GVPGMSD)+(GVPGMS-GVALSD)                                 
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE BEYOND THE MAIN BASE REGISTERS               
* R9 AND R8 WILL STILL BE ACTIVE AND COVER COMMON TABLES AND STORAGE            
***********************************************************************         
         DROP  RB,RA                                                            
                                                                                
***********************************************************************         
* ROUTINE TO READ PERSONAL ID RECORD                                  *         
* ON ENTRY:                                                           *         
*        PERSONAL-ID IN OLDPID                                        *         
*        EFFECTIVE DATE COMPLEMENT IN OLDDEFC                         *         
* ON EXIT:                                                            *         
*        ASSOCIATED PASSWORD IN APWORK                                *         
*        PASSWORD # IN APHALF/OLDPWD                                  *         
*        OFFICE ID IN OLDOID                                          *         
*        DEPARTMENT ID IN OLDDID                                      *         
*        ACCESS GROUP  IN OLDDID                                      *         
*        SECURITY MANAGER OVERRIDE FLAG IN OLDOVF                     *         
*        RECORD KEY IN APRECKEY                                       *         
*        RECORD IN AIOAREA1                                           *         
*        IO ERROR STATE IN PIDIOERR                                   *         
***********************************************************************         
READPID  NTR1  BASE=*,LABEL=*                                                   
         MVI   PIDIOERR,IOERNF                                                  
*                                  READ PERSON RECORD                           
         LA    R2,IOKEY            BUILD PERSON KEY                             
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,OLDPID                                                   
         MVC   SAPEDEF,OLDDEFC                                                  
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY                                      
*                                  READ ANY NON DELETED RECORD                  
         LA    R1,IOHI+IOCONFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         JL    SAEIIO                                                           
         BNE   RPID002                                                          
         L     R2,AIOAREA1                                                      
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),APRECKEY                                
         BE    RPID010             RECORD FOUND                                 
*                                    ELSE READ DELETED RECORD                   
RPID002  LA    R1,IOHID+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         JL    SAEIIO                                                           
*                                                                               
RPID010  L     R2,AIOAREA1                                                      
         MVC   PIDIOERR,IOERR      SAVE IO ERROR INDICATORS                     
         CLI   PIDIOERR,0                                                       
         BE    RPID012                                                          
         TM    PIDIOERR,IOEDEL     TEST RECORD DELETED                          
         BZ    RPIDYES                                                          
*                                                                               
RPID012  CLC   SAPEKEY(SAPEDEF-SAPEKEY),APRECKEY                                
         BE    RPID020             READ HIGH RECORD FOUND                       
         MVI   PIDIOERR,IOERNF                                                  
         MVC   IOERR,PIDIOERR                                                   
         B     RPIDYES             RECORD NOT FOUND                             
*                                  GET SAVE DATA FROM ELEMENTS                  
RPID020  CLI   APACTN,ACTADD                                                    
         BE    RPIDYES             EXIT IF ADD ACTION                           
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY                                      
         MVC   OLDDEFC,SAPEDEF     UPDATE EFFECTIVE DATE SAVE                   
         MVC   OLDDEF,FFILL          FROM READ KEY                              
         XC    OLDDEF,OLDDEFC                                                   
*                                  GET SAVE DATA FROM ELEMENTS                  
         LA    R3,SAPEDATA                                                      
         SR    R0,R0                                                            
RPID022  CLI   0(R3),0             TEST END OF RECORD                           
         BE    RPIDYES             PROCESS PASSWORD RECORD                      
         CLI   0(R3),GACTELQ                                                    
         BE    RPID040                                                          
         CLI   0(R3),SAPWDELQ                                                   
         BE    RPID050                                                          
         CLI   0(R3),SAPERELQ                                                   
         BE    RPID060                                                          
         CLI   0(R3),SAAGCELQ                                                   
         BE    RPID070                                                          
         CLI   0(R3),SALACELQ                                                   
         BE    RPID080                                                          
         CLI   0(R3),SAAPCELQ                                                   
         BE    RPID090                                                          
RPID030  IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     RPID022                                                          
*                                  ACTIVITY ELEMENT (RECENT)                    
         USING GACTELD,R3                                                       
RPID040  MVC   LASTCDT,GACTCDT     SAVE CHANGE DATE                             
         MVC   LASTCTM,GACTTIM     AND CHANGE TIME                              
         B     RPID030                                                          
*                                  PASSWORD POINTER ELEMENT                     
         USING SAPWDD,R3                                                        
RPID050  MVC   OLDPWD,SAPWDNUM     SAVE PASSWORD NUMBER                         
         MVC   APWORK,SAPWDCOD     AND PASSWORD CODE                            
         B     RPID030                                                          
*                                  PERSONEL DATAILS ELEMENT                     
         USING SAPERD,R3                                                        
RPID060  MVC   OLDOID,SAPEROFF     SAVE OLD OFFICE ID                           
         MVC   OLDDID,SAPERDID     SAVE OLD DEPARTMENT ID                       
         MVC   OLDOVF,SAPEROVF     SAVE OLD SEC. MANAGER OVERRIDE FLAG          
         MVC   OLDPCN,SAPERPCN     SAVE OLD PASSWORD CONTROL VALUE              
         B     RPID030                                                          
*                                  ACCESS GROUP ELEMENT                         
         USING SAAGCD,R3                                                        
RPID070  MVC   OLDAGC,SAAGCCOD     SAVE OLD ACCESS GROUP CODE                   
         MVC   OLDAGN,SAAGCNUM     SAVE OLD ACCESS GROUP NUMBER                 
         B     RPID030                                                          
*                                  LIMIT ACCESS GROUP ELEMENT                   
         USING SALACD,R3                                                        
RPID080  MVC   OLDLAC,SALACCOD     SAVE OLD LIMIT ACCESS GROUP CODE             
         MVC   OLDLACN,SALACNUM    SAVE OLD LIMIT ACCESS GROUP #                
         B     RPID030                                                          
*                                  ACCESS GROUP ELEMENT                         
         USING SAAPCD,R3                                                        
RPID090  MVC   OLDAPC,SAAPCCOD     SAVE OLD APPROVER GROUP CODE                 
         MVC   OLDAPN,SAAPCNUM     SAVE OLD APPROVER GROUP NUMBER               
         B     RPID030                                                          
*                                  EXIT OK                                      
RPIDYES  J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
* CHANGE ANY FUTURE PERSON RECORD'S PASSWORD                                    
***********************************************************************         
CHGFPWD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PIDREQD,C'Y'        PPS?                                         
         BNE   CFPX                NO - EXIT                                    
*                                                                               
         OC    OCURPDEF,OCURPDEF   ANY FUTURE PERSON RECORD PENDING             
         BZ    CFPX                NO - EXIT                                    
*                                                                               
         SR    R4,R4               SET PDEFTAB (OFFSET) POINTER                 
CFP10    C     R4,OCURPDEF         FUTURE EFFECTIVE DATE (1S COMP)?             
         BNL   CFPX                NO - EXIT                                    
         LA    RE,PDEFTAB(R4)                                                   
         MVC   APHALF,0(RE)        GET THE EFF DATE (1S COMP)                   
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY     READ NEXT PERSON RECORD                      
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,APRECKEY+(SAPEPID-SAPEKEY)                               
         MVC   SAPEDEF,APHALF      EFFECTIVE DATE FOR READ                      
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2                                                      
         GOTO1 ASETACT,SAPEREC     MARK ACTIVITY                                
         LA    R3,APELEM                                                        
         USING SAPWDD,R3                                                        
         MVI   SAPWDEL,SAPWDELQ    BUILD PASSWORD ELEMENT                       
         MVI   SAPWDLN,0                                                        
         GOTO1 ADELELS,SAPEREC                                                  
         MVI   SAPWDLN,SAPWDLNQ                                                 
         MVC   SAPWDNUM,OLDPWD     OLD PID#                                     
         MVC   SAPWDCOD,NEWCODE    NEW PASSWORD                                 
         GOTO1 AADDELS,SAPEREC                                                  
         DROP  R3                                                               
*                                                                               
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         AHI   R4,L'PDEFTAB        NEXT EFF DATE                                
         B     CFP10                                                            
*                                                                               
CFPX     J     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO CHECK FOR WILD CARD STYLE USERID IN COMPATIBLE ID LIST             
* R4=A(SCANNER CONTROL BLOCK)                                                   
* IF WILDCARD (I.E. STRING ENDS WITH '*') RETURN IN LIDSAVE                     
* AND RETURN CC .EQ.                                                            
***********************************************************************         
WILDCARD NTR1  BASE=*,LABEL=*                                                   
         ZIC   RF,0(R4)                                                         
         LA    R1,11(RF,R4)                                                     
WCAR010  CLI   0(R1),C'*'                                                       
         BE    WCAR020                                                          
         CLI   0(R1),C' '                                                       
         BE    WCAR012                                                          
         B     WCARNO                                                           
WCAR012  BCTR  R1,0                                                             
         BCT   RF,WCAR010                                                       
         B     WCARNO                                                           
WCAR020  BCTR  RF,0                                                             
         STC   RF,WILDCLEN                                                      
         B     WCAROK                                                           
WCAROK   J     EXITEQ                                                           
WCARNO   J     EXITNE                                                           
                                                                                
***********************************************************************         
* GRIDS PROCESSING                                                              
***********************************************************************         
DISGRID  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,=A(GCTBL)              GRIDS COLUMN TABLE                     
         A     RF,APRELO                                                        
         ST    RF,AGCTBL                                                        
*                                                                               
         LHI   R3,PCDRIVEN-TWAD                                                 
         A     R3,ATWA                                                          
         USING PCDRIVEN,R3                                                      
*                                                                               
         TM    PCGRIDS,PCGPAPQ           GRIDS IN PROCESS FOR APP?              
         BO    DG010                     . YES                                  
         OI    PCGRIDS,PCGPAPQ           IT IS NOW                              
         GOTO1 AGRIDS,APPARM,(C'C',AGCTBL),GCOSEL  CLEAR GRIDS SCREEN           
         XC    SVSELOPT,SVSELOPT                                                
         XC    SAVPEKEY,SAVPEKEY                                                
         XC    MAXIOPID,MAXIOPID                                                
         MVC   FVMSGNO,=AL2(FVFEKEY)     ENTER KEY                              
         B     DG090                     CHANCE TO ENTER FILTERS                
*                                                                               
DG010    BRAS  RE,VALGSEL                VALIDATE SELECT FIELDS                 
         BNE   DGX                                                              
*                                                                               
         OC    MAXIOPID,MAXIOPID         IF NOT ZERO,                           
         BZ    DG030                                                            
         MVC   SAVPID,MAXIOPID           WE HAD MAXIO LAST TIME                 
         XC    MAXIOPID,MAXIOPID                                                
DG030    BRAS  RE,GETGSEL                GET THE RECORD                         
         BL    DG050                     LIST FINISHED                          
         BH    DG060                     TOO MANY I/OS                          
*                                                                               
         BRAS  RE,PREGRID                PRE-PROCESS GRIDS DATA                 
*                                                                               
         GOTO1 AGRIDS,APPARM,AGCTBL,GCOSEL                                      
         BNE   DG080                                                            
         B     DG030                                                            
*                                                                               
DG050    TM    PCGRIDS,PCGCOFQ           OUTPUT COLUMN HEADINGS?                
         BO    DG056                                                            
         MVC   FVMSGNO,=AL2(CE#NATLS)    NOTHING AVAILABLE TO LIST              
         B     DG090                                                            
DG056    GOTO1 AGRIDS,APPARM,(C'E',AGCTBL),GCOSEL    END GRIDS DISPLAY          
         B     DG080                                                            
*                                                                               
DG060    GOTO1 AGRIDS,APPARM,(C'F',AGCTBL),GCOSEL    FLUSH THE SCREEN           
         MVC   MAXIOPID,SAVPID                                                  
*                                                                               
DG080    NI    GETSEQF,X'FF'-APILNSEQ    NO SEQ WHEN WE COME BACK               
         MVC   ACSMSG,FVOMSG                                                    
         MVC   FVMSGNO,=AL2(FVFSET)                                             
DG090    LA    R1,GRDPIDH                SET CURSOR                             
         ST    R1,FVADDR                                                        
DGX      J     EXIT                                                             
         DROP  R2,R3                                                            
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE GRIDS SELECT PARAMETERS                                   
***********************************************************************         
         USING SAPEREC,R2                                                       
VALGSEL  NTR1  BASE=*,LABEL=*                                                   
         MVI   GCOSEL,0            GENERIC GRIDS COLUMNS                        
*                                                                               
         MVI   PIDREQD,C'N'                                                     
         GOTO1 AGETAAD,AGENCYID    GET AGENCY ACCESS DETAILS                    
         TM    APWORK,CTAADPRQ     PERSONAL ID REQUIRED WITH PASSWORD?          
         BZ    *+8                                                              
         MVI   PIDREQD,C'Y'                                                     
         MVC   PWDINFO,APWORK      SAVE PASSWORD ATTRIBUTES                     
*                                                                               
         LA    R2,APRECKEY                                                      
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         XC    SAPEKEY,SAPEKEY     INITIALISE PERSON RECORD KEY                 
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID    AGENCY                                       
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL        GET TODAYS DATE                              
         XC    TODAYC,TODAY                                                     
         XC    SAVPID,SAVPID       INIT. LAST PID READ                          
         XC    SELOPT(SELOPTL),SELOPT  INIT. SELECT FILTERS                     
*                                                                               
VGSPID   GOTO1 AFVAL,GRDPIDH       VALIDATE PERSON ID FILTER                    
         BNE   VGSPIDX                                                          
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VGSPID1  CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VGSPID2             FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VGSPID1                                                       
VGSPID2  STC   RE,SELKEYCL                                                      
         MVC   SELPID,FVIFLD                                                    
         MVC   SELPIDL,FVILEN                                                   
         MVC   SELPIDSP,0(RF)                                                   
VGSPIDX  MVC   SAPEPID,FVIFLD                                                   
*                                                                               
VGS002   GOTO1 AFVAL,GRDUSRH      VALIDATE USER ID FILTER                       
         BNE   VGS010                                                           
         GOTO1 AVALUID,GRDUSRH                                                  
         BNE   VGSERX                                                           
         MVC   SELUSR,FVIFLD                                                    
*                                                                               
VGS010   GOTO1 AFVAL,GRDNAMEH      VALIDATE LAST NAME FILTER                    
         BNE   VGS020                                                           
         MVC   SELNAM,FVIFLD                                                    
         MVC   SELNAML,FVILEN                                                   
*                                                                               
VGS020   GOTO1 AFVAL,GRDDEFH       VALIDATE EFFECTIVE DATE FILTER               
         BNE   VGS030                                                           
         MVC   SELDEFC,FFILL                                                    
         ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BE    VGS026                                                           
         SR    RE,RE                                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDALL           'ALL' KEYWORD                                
         BNE   SAEIIF                                                           
         LA    R1,GRDDEFH                                                       
         BRAS  RE,ECHOALL                                                       
         XC    SELDEFC,FFILL                                                    
         B     VGS028                                                           
VGS026   XC    SELDEFC,APWORK+PVALCSTA-PERVALD                                  
VGS028   MVC   SAPEDEF,SELDEFC                                                  
         MVI   SELDEFF,1           FLAG FILTER SET                              
*                                                                               
VGS030   GOTO1 AFVAL,GRDAGRH       VALIDATE ACCESS GROUP FILTER                 
         BNE   VGS032                                                           
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2                                                       
         XC    SAAGKEY,SAAGKEY     CHECK ACCESS GROUP RECORD EXISTS             
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELAGR,FVIFLD                                                    
*                                                                               
VGS032   GOTO1 AFVAL,GRDAPCH       VALIDATE APPROVER GROUP CODE FILTER          
         BNE   VGS040                                                           
         LA    R2,IOKEY                                                         
         USING SAAPREC,R2                                                       
         XC    SAAPKEY,SAAPKEY     CHECK APPROVER GROUP RDCORD EXISTS           
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID                                                 
         MVC   SAAPAGR,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELAPC,FVIFLD                                                    
*                                                                               
VGS040   GOTO1 AFVAL,GRDOFFH       VALIDATE OFFICE CODE FILTER                  
         BNE   VGS050                                                           
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY     CHECK OFFICE RECORD EXISTS                   
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELOFF,FVIFLD                                                    
*                                                                               
VGS050   GOTO1 AFVAL,GRDDIDH       VALIDATE DEPARTMENT CODE FILTER              
         BNE   VGS060                                                           
         OC    SELOFF,SELOFF                                                    
         BZ    SAEODF                                                           
         LA    R2,IOKEY                                                         
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY     CHECK DEPARTMENT RECORD EXISTS               
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,SELOFF                                                   
         MVC   SADPDID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELDID,FVIFLD                                                    
*                                                                               
VGS060   MVI   FVMINL,1            READ SYSTEM FIELD                            
         GOTO1 AFVAL,GRDSYSH                                                    
         BNE   VGS070                                                           
         GOTO1 AVALSYS,GRDSYSH     VALIDATE SYSTEM                              
         BNE   VGSERX                                                           
         MVC   SELSYS,APWORK       HOLD FOR LATER USE                           
         GOTO1 VHEXOUT,APPARM,SELSYS,APWORK,L'SELSYS,=C'TOG'                    
         MVC   GCOSEL,APWORK+1                                                  
*                                                                               
VGS070   MVI   FVMINL,1                  PROGRAM FILTER                         
         GOTO1 AFVAL,GRDPGMH                                                    
         BNE   VGS080                                                           
         MVC   FVMSGNO,=AL2(CE#PFNVS)    PROGRAM NOT VALID W/O SYSTEM           
         OC    SELSYS,SELSYS                                                    
         BZ    VGSERX                                                           
         GOTO1 AVALPGM,APPARM,(SELSYS,GRDPGMH)                                  
         BNE   VGSERX                                                           
         MVC   SELPGM,APWORK                                                    
*                                                                               
VGS080   LHI   R3,PCDRIVEN-TWAD                                                 
         A     R3,ATWA                                                          
         USING PCDRIVEN,R3                                                      
*                                                                               
         TM    PCGRIDS,PCGBEGQ                                                  
         BNO   VGS082                                                           
         CLC   SVSELOPT,SELOPT                                                  
         BE    VGS090                                                           
*                                                                               
VGS082   NI    PCGRIDS,X'FF'-PCGBEGQ-PCGCOFQ                                    
         MVC   SVSELOPT,SELOPT                                                  
         B     VGS092                                                           
         DROP  R3                                                               
*                                                                               
VGS090   OC    SAVPEKEY,SAVPEKEY                                                
         BNZ   VGSOKX                                                           
VGS092   MVC   SAVPEKEY,APRECKEY                                                
         BRAS  RE,GETASF                                                        
         BRAS  RE,INITPGMT               INITIALIZE PROGRAM TABLE               
         NI    GETSEQF,X'FF'-APILNSEQ                                           
*                                                                               
VGSOKX   CR    RB,RB                                                            
         J     VGSX                                                             
VGSERX   LTR   RB,RB                                                            
VGSX     J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
                                                                                
*************************************************************                   
* GET NEXT RECORD FOR GRIDS                                                     
*************************************************************                   
GETGSEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
GSG000   L     R2,AIOAREA1                                                      
         USING SAPEREC,R2                                                       
         MVC   IOKEY(L'SAPEKEY),SAVPEKEY                                        
*                                                                               
         TM    GETSEQF,APILRERD           TEST SEQUENTIAL READ BROKEN           
         BZ    GSG006                                                           
         NI    GETSEQF,X'FF'-APILRERD                                           
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GSGLOX                                                           
*                                                                               
GSG006   TM    GETSEQF,APILNSEQ                                                 
         BO    GSG008                                                           
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GSG008   LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         MVC   SAVPEKEY(L'SAPEKEY),SAPEKEY                                      
         BNE   GSGLOX                                                           
*                                                                               
         CLI   SAPETYP,SAPETYPQ          PERSON RECORD?                         
         BNE   GSGLOX                                                           
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   GSGLOX                                                           
         CLC   SAPEAGY,AGENCYID          PROPER AGENCY?                         
         BNE   GSGLOX                                                           
         OI    GETSEQF,APILNSEQ                                                 
*                                                                               
         GOTO1 VGETFACT,APPARM,0   GET A(SYSTEM INFO BLOCK)                     
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MH    RF,=H'9'                                                         
         D     RE,=F'10'           90 PERCENT OF MAX IOS IN RF                  
         CLM   RF,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BNH   GSGHIX                                                           
         DROP  R1                                                               
*                                                                               
         CLI   SELDEFF,0           FILTER ON EFFECTIVE DATE IN KEY              
         BE    GSG020                                                           
         CLC   SAPEDEF,SELDEFC                                                  
         BL    GSG008                                                           
         B     GSG026                                                           
*                                                                               
GSG020   CLC   SAPEDEF,TODAYC      CHECK CURRENT EFFECTIVE DATE                 
         BL    GSG008                                                           
         CLC   SAPEPID,SAVPID                                                   
         BE    GSG008              GET NEXT PERSON ID                           
         MVC   SAVPID,SAPEPID                                                   
         MVI   CURRPID,C'Y'                                                     
         MVI   EXPFLAG,C'N'        INITIALISE EXPIRED FLAG                      
         B     GSG030                                                           
*                                                                               
GSG026   MVI   CURRPID,C'N'        SET CURRENT PERSONAL ID FLAG                 
         CLC   SAPEDEF,TODAYC      CHECK CURRENT EFFECTIVE DATE                 
         BL    GSG030                                                           
         CLC   SAPEPID,SAVPID                                                   
         BE    GSG030                                                           
         MVC   SAVPID,SAPEPID                                                   
         MVI   CURRPID,C'Y'                                                     
         MVI   EXPFLAG,C'N'        INITIALISE EXPIRED FLAG                      
         B     GSG030                                                           
*                                  FILTER ON PERSONAL ID                        
GSG030   CLI   SELPIDSP,C' '       OFFICE CODE - FILTER ONLY IF IT              
         BNH   GSG040                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GSG038                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAPEPID(0),SELPID                                                
         BH    GSGLOX              (NO MORE RELEVENT RECORDS)                   
GSG038   GOTO1 ATXTFLT,APPARM,(SELPIDL,SELPID),(L'SAPEPID,SAPEPID)              
         BNE   GSG000              READ NEXT RECORD                             
*                                  SAVE LAST RECORD KEY                         
GSG040   LA    R3,SAPEDATA         FILTER ON DATA IN ELEMENTS                   
         MVI   SELAGRF,0                                                        
         MVI   SELAPCF,0                                                        
         MVI   SELPWDF,0                                                        
*                                                                               
GSG050   CLI   0(R3),0             E-O-R                                        
         BE    GSG160                                                           
         CLI   0(R3),SANAMELQ                                                   
         BE    GSG070                                                           
         CLI   0(R3),SAPERELQ                                                   
         BE    GSG100                                                           
         CLI   0(R3),SAAGCELQ                                                   
         BE    GSG140                                                           
         CLI   0(R3),SAAPCELQ                                                   
         BE    GSG150                                                           
         CLI   0(R3),SAPWDELQ                                                   
         BE    GSG130                                                           
*                                                                               
GSG060   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GSG050                                                           
*                                                                               
         USING SANAMD,R3                                                        
GSG070   OC    SELNAM,SELNAM       FILTER ON PERSON SURNAME                     
         BZ    GSG060                                                           
         CLI   SANAMLN,SANAMLNQ                                                 
         BL    GSG000                                                           
         LA    RE,SANAMELN                                                      
         SR    R1,R1                                                            
         TM    SANAMIND,SANAMIFN                                                
         BZ    GSG080                                                           
         IC    R1,0(RE)                                                         
         LA    RE,1(R1,RE)                                                      
*                                                                               
GSG080   TM    SANAMIND,SANAMIMN                                                
         BZ    GSG090                                                           
         IC    R1,0(RE)                                                         
         LA    RE,1(R1,RE)                                                      
*                                                                               
GSG090   TM    SANAMIND,SANAMILN                                                
         BZ    GSG000                                                           
         IC    R1,SELNAML                                                       
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         CR    R1,RF                                                            
         BH    GSG000                                                           
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'APWORK-1),APWORK                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    APWORK(0),1(RE)                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELNAM(0),APWORK                                                 
         BNE   GSG000                                                           
         B     GSG060                                                           
*                                                                               
         USING SAPERD,R3                                                        
GSG100   EQU   *                   FILTER ON PERSONNEL DETAILS                  
         CLI   CURRPID,C'Y'        TEST IF CURRENT PID                          
         BNE   GSG104                                                           
         MVI   EXPFLAG,C'N'        SET EXPIRED FLAG                             
         OC    SAPERDTE,SAPERDTE                                                
         BZ    GSG104                                                           
         CLC   SAPERDTE,TODAY                                                   
         BNL   GSG104                                                           
         MVI   EXPFLAG,C'Y'                                                     
*                                                                               
GSG104   CLI   OPTTER,C'Y'         OPTION TERM=Y                                
         BE    GSG110                                                           
         CLI   OPTTER,C'O'         OPTION TERM=O                                
         BE    GSG108                                                           
*                                  DEFAULT TERM=N                               
         CLI   EXPFLAG,C'N'                                                     
         BE    GSG110                                                           
         B     GSG000                                                           
*                                                                               
GSG108   CLI   EXPFLAG,C'Y'                                                     
         BE    GSG110                                                           
         B     GSG000                                                           
*                                                                               
GSG110   EQU     *                                                              
*&&UK                                                                           
         GOTO1 ATSTOMAN,SAPEROFF   TEST OFFICE MANAGER ACCESS                   
         BNE   GSG114                                                           
         B     GSG116                                                           
*&&                                                                             
*&&US                                                                           
         GOTO1 ATSTDMAN,APPARM,SAPEROFF,SAPERDID                                
         BE    GSG116              TEST OFFICE/DEPT MANAGER ACCESS              
*&&                                                                             
GSG114   OC    GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR ERROR MESSAGE TO CONTINUE LIST         
         B     GSG000                                                           
*                                                                               
GSG116   OC    GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         OC    SELOFF,SELOFF                                                    
         BZ    GSG118                                                           
         CLC   SELOFF,SAPEROFF                                                  
         BNE   GSG000                                                           
GSG118   OC    SELDID,SELDID                                                    
         BZ    GSG120                                                           
         CLC   SELDID,SAPERDID                                                  
         BNE   GSG000                                                           
GSG120   CLI   SAPERLN,SAPERLNQ                                                 
         BL    GSG000                                                           
         B     GSG060                                                           
*                                                                               
         USING SAPWDD,R3                                                        
GSG130   OC    SELUSR,SELUSR       EXTRACT PASSWORD RECORD                      
         BNZ   *+12                                                             
         CLI   SELSYS,0                                                         
         BE    GSG060                                                           
         CLI   SAPWDLN,SAPWDLNQ                                                 
         BL    GSG000                                                           
         MVI   SELPWDF,1                                                        
         MVC   SAVPWD,SAPWDCOD                                                  
         MVC   SAVPWD#,SAPWDNUM                                                 
         B     GSG060                                                           
*                                                                               
         USING SAAGCD,R3                                                        
GSG140   MVI   SELAGRF,1                                                        
         OC    SELAGR,SELAGR       FILTER ON ACCESS GROUP CODE                  
         BZ    GSG060                                                           
         CLI   SAAGCLN,SAAGCLNQ                                                 
         BL    GSG000                                                           
         CLC   SELAGR,SAAGCCOD                                                  
         BNE   GSG000                                                           
         B     GSG060                                                           
*                                                                               
         USING SAAPCD,R3                                                        
GSG150   MVI   SELAPCF,1                                                        
         OC    SELAPC,SELAPC       FILTER ON APPROVER GROUP CODE                
         BZ    GSG060                                                           
         CLI   SAAPCLN,SAAPCLNQ                                                 
         BL    GSG000                                                           
         CLC   SELAPC,SAAPCCOD                                                  
         BNE   GSG000                                                           
         B     GSG060                                                           
*                                                                               
GSG160   CLI   SELAGRF,0           CHECK ACCESS GROUP FOUND                     
         BNE   *+14                                                             
         OC    SELAGR,SELAGR                                                    
         BNZ   GSG000                                                           
         CLI   SELAPCF,0           CHECK APPROVER GROUP FOUND                   
         BNE   *+14                                                             
         OC    SELAPC,SELAPC                                                    
         BNZ   GSG000                                                           
*                                                                               
         OC    SELUSR,SELUSR                                                    
         BNZ   GSG170                                                           
         CLI   SELSYS,0                                                         
         BE    GSGOKX                                                           
GSG170   CLI   SELPWDF,0           CHECK PASSWORD RECORD FILTER                 
         BE    GSG000                                                           
         DROP  R2                                                               
*                                                                               
         LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY       READ PASSWORD RECORD                         
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,SAVPWD#                                                  
         OI    GETSEQF,APILRERD    FLAG GRID  READ SEQUENCE BROKEN              
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   GSG000              IGNORE IF RECORD NOT FOUND                   
         L     R2,AIOAREA3                                                      
*                                                                               
         CLI   SELSYS,0                                                         
         BE    GSG200                                                           
         LA    R3,SA0DATA                                                       
GSG180   CLI   0(R3),0                   TEST END-OF-RECORD                     
         BE    GSG000                                                           
         CLI   0(R3),SASYSELQ            SYSTEM ELEMENT                         
         BE    GSG190                                                           
GSG184   SR    R0,R0                                                            
         ZIC   R0,1(R3)                  NEXT ELEMENT                           
         AR    R3,R0                                                            
         B     GSG180                                                           
*                                                                               
         USING SASYSD,R3                 SYSTEM ELEMENT X'21'                   
GSG190   CLC   SELSYS,SASYSNUM                                                  
         BNE   GSG184                                                           
         DROP  R3                                                               
*                                  FILTER USER ID COMPATIBLE                    
GSG200   OC    SELUSR,SELUSR                                                    
         BZ    GSGOKX                                                           
         MVC   APWORK,SELUSR                                                    
         L     RF,=A(FILTUSER)                                                  
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    GSGOKX                OK                                         
         B     GSG000                ELSE NEXT RECORD RESTART SEQUENCE          
*                                                                               
GSGHIX   MVI   APBYTE,2                                                         
         B     *+8                                                              
GSGLOX   MVI   APBYTE,0                                                         
         B     *+8                                                              
GSGOKX   MVI   APBYTE,1                                                         
         CLI   APBYTE,1                                                         
         J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* PRE-PROCESS GRIDS DATA                                                        
***********************************************************************         
         USING SAPEREC,R2                                                       
PREGRID  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,WORKD                                                         
         AHI   R4,GVALS-WORKD                                                   
         USING GVALSD,R4                                                        
         LR    R2,R4                                                            
         LHI   R3,GVALSL                                                        
         SR    RE,RE                                                            
         LHI   RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R2,RE                     CLEAR GVALS                            
*                                                                               
         XC    ASYSEL,ASYSEL                                                    
         XC    DATAGRP,DATAGRP                                                  
         XC    GVDACGN,GVDACGN                                                  
         MVC   GVLACC(3),=C'N/A'         LIM ACC NOT AVAIL IF NO SYS            
*&&UK*&& MVC   GVCLAC(3),=C'N/A'         CLI LIM ACC NEITHER                    
*&&UK*&& XC    GVOACCG,GVOACCG           PRE EXPIRY GROUP                       
         XC    GVPWHDTE,GVPWHDTE         PASSWORD HISTORY CHANGE DATE           
         XC    GVPLODTE,GVPLODTE         LAST LOGON DATE                        
*                                                                               
PG008    L     R2,AIOAREA1                                                      
         LA    R3,SAPEDATA                                                      
PG010    CLI   0(R3),0                                                          
         BE    PG100                                                            
         CLI   0(R3),SANAMELQ            NAME ELEMENT                           
         BE    PG030                                                            
         CLI   0(R3),SAPERELQ            PERSONNEL ELEMENT                      
         BE    PG050                                                            
         CLI   0(R3),SAAPCELQ            APPROVER GROUP ELEMENT                 
         BE    PG060                                                            
         CLI   0(R3),SAAGCELQ            ACCESS GROUP CODE ELEMENT              
         BE    PG070                                                            
         CLI   0(R3),SALACELQ            LIMIT ACCESS GROUP CODE ELEM           
         BE    PG080                                                            
         CLI   0(R3),SAPWDELQ            PASSWORD POINTER ELEMENT               
         BE    PG090                                                            
         CLI   0(R3),SALLOELQ            LAST LOGON DETAILS ELEM                
         BE    PG095                                                            
*                                                                               
PG020    SR    R0,R0                     BUMP TO NEXT ELEMENT                   
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PG010                                                            
                                                                                
*----------------------------------------                                       
* FIRST, MIDDLE, AND LAST NAME                                                  
*----------------------------------------                                       
         USING SANAMD,R3                                                        
PG030    LA    RF,SANAMELN                                                      
         SR    R1,R1                                                            
         TM    SANAMIND,SANAMIFN         FIRST NAME                             
         BZ    PG035                                                            
         ICM   R1,1,0(RF)                                                       
         BNP   PGX                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GVFNAME(0),1(RF)                                                 
         LA    RF,2(R1,RF)                                                      
*                                                                               
PG035    TM    SANAMIND,SANAMIMN         MIDDLE NAME                            
         BZ    PG040                                                            
         ICM   R1,1,0(RF)                                                       
         BNP   PGX                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GVMNAME(0),1(RF)                                                 
         LA    RF,2(R1,RF)                                                      
*                                                                               
PG040    TM    SANAMIND,SANAMILN         LAST NAME                              
         BZ    PG020                                                            
         ICM   R1,1,0(RF)                                                       
         BNP   PGX                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GVLNAME(0),1(RF)                                                 
         B     PG020                                                            
                                                                                
*----------------------------------------                                       
* PERSONNEL ELEMENT                                                             
*----------------------------------------                                       
         USING SAPERD,R3                                                        
PG050    MVC   GVPCN,SAPERPCN                                                   
         OC    SAPEROFF,SAPEROFF         OFFICE ID                              
         BZ    PG055                                                            
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY                                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,SAPEROFF                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   PG055                                                            
         L     R1,AIOAREA2                                                      
         GOTO1 AGETONAM                                                         
         MVC   GVOFFN,APWORK                                                    
*                                                                               
PG055    OC    SAPERDID,SAPERDID         DEPARTMENT ID                          
         BZ    PG020                                                            
         LA    R2,IOKEY                                                         
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY                                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,SAPEROFF                                                 
         MVC   SADPDID,SAPERDID                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   PG020                                                            
         L     R1,AIOAREA2                                                      
         GOTO1 AGETDNAM                                                         
         MVC   GVDEPN,APWORK                                                    
         B     PG020                                                            
                                                                                
*----------------------------------------                                       
* APPROVER GROUP ELEMENT                                                        
*----------------------------------------                                       
         USING SAAPCD,R3                                                        
PG060    OC    SAAPCCOD,SAAPCCOD         APPROVER GROUP                         
         BZ    PG020                                                            
*                                                                               
         GOTO1 AGETAPG,SAAPCCOD                                                 
         BNE   PG020                                                            
         MVC   GVAPPGN,APWORK                                                   
         B     PG020                                                            
                                                                                
*----------------------------------------                                       
* ACCESS GROUP ELEMENT                                                          
*----------------------------------------                                       
         USING SAAGCD,R3                                                        
PG070    OC    SAAGCCOD,SAAGCCOD          ACCESS GROUP                          
         BZ    PG020                                                            
*&&UK*&& MVC   GVOACCG,SAAGCCOD           SAVE TEMPORARILY                      
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2                                                       
         XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,SAAGCCOD                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   PG020                                                            
         L     R1,AIOAREA2                                                      
         GOTO1 AGETGNAM                                                         
         MVC   GVACCGN,APWORK                                                   
         B     PG020                                                            
                                                                                
*----------------------------------------                                       
* LIMIT ACCESS ELEMENT                                                          
*----------------------------------------                                       
         USING SALACD,R3                                                        
PG080    OC    SALACNUM,SALACNUM         DATA ACCESS GROUP                      
         BZ    PG020                                                            
         MVC   DATAGRP,SALACNUM                                                 
*                                                                               
         LA    R2,IOKEY                                                         
         USING SALAREC,R2                                                       
         XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         MVC   SALAAGY,AGENCYID                                                 
         MVC   SALAAGN,SALACNUM                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   PG020                                                            
         L     R1,AIOAREA2                                                      
         GOTO1 AGETLNAM                                                         
         MVC   GVDACGN,APWORK+L'SALACCOD                                        
         BRAS  RE,DISDHD                                                        
         B     PG020                                                            
                                                                                
*----------------------------------------                                       
* PASSWORD POINTER RECORD                                                       
*----------------------------------------                                       
         USING SAPWDD,R3                                                        
PG090    MVC   GVPSWD,SAPWDCOD                                                  
         MVC   SAVPWD,SAPWDCOD                                                  
         MVC   SAVPWD#,SAPWDNUM          SAVE PASSWORD NUMBER                   
         BRAS  RE,HIDEPWD                                                       
         B     PG020                                                            
                                                                                
*----------------------------------------                                       
* LAST LOGON DATE                                                               
*----------------------------------------                                       
         USING SALLOD,R3                                                        
PG095    CLC   SALLODT,=X'640102'                                               
         BL    PG096                                                            
         BH    PG097                                                            
         XC    GVPLODTE,GVPLODTE                                                
         MVC   GVPLODTE(6),CT@UNUSE                                             
         B     PG020                                                            
PG096    XC    GVPLODTE,GVPLODTE                                                
         MVC   GVPLODTE(7),CT@UNKNW                                             
         B     PG020                                                            
PG097    XC    GVPLODTE,GVPLODTE                                                
         GOTO1 VDATCON,APPARM,(3,SALLODT),(21,GVPLODTE)                         
         B     PG020                                                            
                                                                                
*----------------------------------------                                       
* PASSWORD RECORD DETAILS                                                       
*----------------------------------------                                       
         USING SA0REC,R2                                                        
PG100    LA    R2,IOKEY                  BUILD KEY                              
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,SAVPWD#                                                  
         GOTO1 AIO,IORDD+IOCONFIL+IO3                                           
         BL    PGX                                                              
*                                                                               
         LA    R1,GVVSYS                                                        
         ST    R1,SAVPARM                A(VALID SYSTEM LIST)                   
         LA    R1,GVUIDS                                                        
         ST    R1,SAVPARM+4              A(UPDATIVE IDS)                        
         LA    R1,GVRIDS                                                        
         ST    R1,SAVPARM+8              A(READ-ONLY IDS)                       
*                                                                               
         L     R2,AIOAREA3               USER IDS FROM PASSWORD REC.            
         LA    R3,SA0DATA                                                       
PG110    CLI   0(R3),0                   TEST END-OF-RECORD                     
         BE    PG200                                                            
         CLI   0(R3),SAIDELQ             ID ELEMENT                             
         BE    PG130                                                            
         CLI   0(R3),SASYSELQ            SYSTEM ELEMENT                         
         BE    PG180                                                            
         CLI   0(R3),SAPWHELQ            PASSWORD HISTORY                       
         BE    PG190                                                            
PG120    SR    R0,R0                                                            
         ZIC   R0,1(R3)                  NEXT ELEMENT                           
         AR    R3,R0                                                            
         B     PG110                                                            
                                                                                
*-----------------------------------                                            
* ID ELEMENT  X'20'                                                             
*-----------------------------------                                            
         USING SAIDD,R3                                                         
PG130    TM    SAIDTYP,SAIDNEQ           NEW STYLE ELEMENT?                     
         BO    PG132                     . NO, ZERO MEANS NEW                   
         TM    SAIDTYP,SAIDROQ           READ-ONLY                              
         BO    PG150                     . YES, NEW STYLE                       
*                                                                               
PG132    L     R1,SAVPARM+4              UPDATIVE IDS                           
         OC    SAID(2),SAID              TEST ID LIST                           
         BNZ   PG134                                                            
         MVC   0(2,R1),=C'L='                                                   
         MVC   2(L'SAIDNAM,R1),SAID+2                                           
         B     PG140                                                            
PG134    CLC   SAID(2),=X'0001'          TEST AGY=                              
         BNE   PG136                                                            
         MVC   0(2,R1),=C'A='                                                   
         MVC   2(2,R1),SAID+2                                                   
         B     PG140                                                            
PG136    MVC   0(L'SAIDNAM,R1),SAID                                             
PG140    CLI   0(R1),C' '                FIND END OF ID                         
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         B     PG140                                                            
         MVI   0(R1),C','                SEPERATE ID FROM NEXT ONE              
         LA    R1,1(R1)                                                         
         ST    R1,SAVPARM+4              UPDATIVE IDS                           
         B     PG120                                                            
*                                                                               
PG150    L     R1,SAVPARM+8              READ-ONLY IDS                          
         TM    SAIDTYP,SAIDLIQ           TEST ID LIST                           
         BZ    PG154                                                            
         MVC   0(2,R1),=C'L='                                                   
         LA    R1,2(R1)                                                         
         B     PG156                                                            
PG154    TM    SAIDTYP,SAIDAGQ           TEST AGY=                              
         BZ    PG156                                                            
         MVC   0(2,R1),=C'A='                                                   
         LA    R1,2(R1)                                                         
PG156    SR    RF,RF                                                            
         IC    RF,SAIDLEN                                                       
         SHI   RF,1                                                             
         TM    SAIDTYP,SAIDNUQ           LAST TWO BYTES THE ID NUMBER?          
         BZ    *+8                       . NO                                   
         SHI   RF,2                      . YES, REDUCE THE LENGTH               
         SHI   RF,SAIDNAM-SAIDD                                                 
         BNP   PG160                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SAIDNAM                                                  
PG160    CLI   0(R1),C' '                FIND END OF ID                         
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         B     PG160                                                            
         MVI   0(R1),C','                SEPERATE ID FROM NEXT ONE              
         LA    R1,1(R1)                                                         
         ST    R1,SAVPARM+8              READ-ONLY IDS                          
         B     PG120                                                            
                                                                                
*-----------------------------------                                            
* SYSTEM ELEMENT X'21'                                                          
*-----------------------------------                                            
         USING SASYSD,R3                                                        
PG180    GOTO1 ADISSYS,SASYSNUM                                                 
         L     R1,SAVPARM                                                       
         MVC   0(7,R1),APWORK      INSERT NAME                                  
         LA    R1,6(R1)                                                         
         CLI   0(R1),C' '          SQUASH OUT SPACES                            
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C','                                                       
         LA    R1,2(R1)                                                         
         ST    R1,SAVPARM                                                       
*                                                                               
         CLC   SELSYS,SASYSNUM                                                  
         BNE   PG120                                                            
         ST    R3,ASYSEL           SAVE SYSTEM ELEMENT                          
         GOTO1 ASETSEL,SASYSNUM                                                 
*&&US                                                                           
         XC    CLSTACC,CLSTACC     CLIENT LIST LIMIT ACCESS SET                 
         CLI   SELSYS,X'07'        TALENT                                       
         BE    PG184                                                            
         CLI   SELSYS,X'08'        REP                                          
         BE    PG184                                                            
         CLI   SELSYS,X'0A'        CONTROL                                      
         BE    PG184                                                            
         CLI   SELSYS,X'0C'        CPP                                          
         BE    PG184                                                            
         BRAS  RE,DISCLA                                                        
         OC    CLSTACC,CLSTACC     CLIENT LIST LIMIT ACCESS SET                 
         BNZ   PG120                                                            
*&&                                                                             
*&&UK*&& BRAS  RE,DISCLA                                                        
*                                                                               
PG184    GOTO1 ADISLACC,APPARM,(SASYSNUM,SASYSLMT)                              
         CLC   APWORK(15),SPACES   AVIOD OVERWRITING PREVIOUSLY                 
         BNH   PG120               DEFINED DATA ACCESS                          
         MVC   GVLACC(15),APWORK                                                
         B     PG120                                                            
                                                                                
*-----------------------------------                                            
* PASSWORD HISTORY X'E4'                                                        
*-----------------------------------                                            
         USING SAPWHD,R3                                                        
PG190    SR    R1,R1                                                            
         IC    R1,SAPWHLN                                                       
         SHI   R1,SAPWHLNQ+1                                                    
         BNP   PG120                                                            
         MVC   APWORK(L'SAPWHPWD),SPACES                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SAPWHPWD                                               
         CLC   SAVPWD,APWORK             PASSWORD WE'RE LOOKING FOR?            
         BNE   PG120                     . YES                                  
*                                                                               
         CLI   PIDREQD,C'Y'              PPS?                                   
         BNE   PG120                     NO - EXIT                              
*                                                                               
         CLC   SAPWHDTE,=X'FFFF'         TEMPORARY ELEMENT                      
         BE    PG120                     . YES, IGNORE                          
         MVC   GVPWHDTE,SAPWHDTE                                                
*                                                                               
         MVC   GVPWHPSB,CT@USER          INIT PASSWORD SET BY TO USER           
         MVC   GVPWSTAT,CT@ACTIV                                                
*                                                                               
         TM    SAPWHFLG,SAPWHPPS         SET BY PPS CONVERSION                  
         BZ    *+10                                                             
         MVC   GVPWHPSB,=CL8'CONV'                                              
*                                                                               
         TM    SAPWHFLG,SAPWHEXP         IMMEDIATE EXPIRATION?                  
         BO    *+14                                                             
         CLC   SAPWHDTE,=X'CA21'         OLD ADMIN ELEM (JAN01/2001)            
         BNE   *+10                                                             
         MVC   GVPWHPSB,CT@ADMIN                                                
*                                                                               
         CLI   PWDTOUT,0                 NO TIMEOUT, THEN ACTIVE                
         BE    PG120                                                            
         CLI   GVPCN,C'N'                DOES NOT EXPIRE                        
         BE    PG120                                                            
         TM    SAPWHFLG,SAPWHEXP         IMMEDIATE EXPIRATION?                  
         BO    PG198                                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,PWDTOUT                                                       
         GOTO1 VDATCON,APPARM,(2,SAPWHDTE),(0,APDUB)                            
         GOTO1 VADDAY,APPARM,(C'D',APDUB),(0,APWORK),(R0)                       
         GOTO1 VDATCON,APPARM,(0,APWORK),(2,APWORK)                             
         CLC   TODAY,APWORK                                                     
         BNH   PG120                                                            
*                                                                               
PG198    MVC   GVPWSTAT,CT@EXPR                                                 
         B     PG120                                                            
         DROP  R3                                                               
                                                                                
*----------------------------------------                                       
* TIDY UP THE OUTPUT FIELDS                                                     
*----------------------------------------                                       
PG200    L     R1,SAVPARM                VALID SYSTEMS                          
         BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
         LA    RF,GVRIDS                                                        
         L     R1,SAVPARM+8              READ-ONLY IDS                          
         BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CR    R1,RF                     ANY READ-ONLY IDS?                     
         BH    PG204                     . YES                                  
*                                                                               
         L     R1,SAVPARM+4              UPDATIVE IDS?                          
         LA    RF,GVUIDS                                                        
         CR    R1,RF                     ANY UPDATIVE IDS?                      
         BH    PG204                     . YES                                  
         MVC   0(L'CT@ALL,R1),CT@ALL                                            
         B     PG210                                                            
*                                                                               
PG204    L     R1,SAVPARM+4              UPDATIVE IDS                           
         BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
PG210    BRAS  RE,BPGMS                  BUILD PROGRAMS VALUES                  
*&&UK                                                                           
*----------------------------------------                                       
* OLD SECURITY GROUP IF EXPIRED                                                 
*----------------------------------------                                       
PG300    CLI   EXPFLAG,C'N'               HAS THE PID EXPIRED?                  
         BE    PG310                      NO, SKIP                              
*                                                                               
         USING SAPEREC,R2                                                       
         L     R2,AIOAREA1                                                      
PG302    MVC   IOKEY(L'SAPEKEY),SAPEKEY   COPY KEY                              
         LA    R2,IOKEY                                                         
         XR    R1,R1                                                            
         ICM   R1,3,SAPEDEF                                                     
         LA    R1,1(,R1)                  BUMP DATE                             
         STCM  R1,3,SAPEDEF                                                     
         GOTO1 AIO,IOHI+IOCONFIL+IO2                                            
         BNE   PG310                                                            
         L     R2,AIOAREA2                                                      
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY  SAME PERSON?                     
         BNE   PG310                                                            
         LA    R3,SAPEDATA               YES, GET OLD GROUP                     
         SR    R0,R0                     BUMP TO NEXT ELEMENT                   
PG304    CLI   0(R3),0                                                          
         BE    PG310                                                            
         CLI   0(R3),SAAGCELQ            ACCESS GROUP CODE ELEM                 
         BE    PG306                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PG304                                                            
         USING SAAGCD,R3                                                        
PG306    OC    SAAGCCOD,SAAGCCOD         IS THERE AN ACCESS GROUP               
         BZ    PG310                     NO, SKIP                               
         CLC   GVOACCG,SAAGCCOD          IS IT SAME AS CURRENT                  
         MVC   GVOACCG,SAAGCCOD                                                 
         BNE   PGX                       NO, SO IT'S PREVIOUS VERSION           
         B     PG302                     ELSE TRY OLDER COPY                    
         DROP  R3                                                               
                                                                                
PG310    XC    GVOACCG,GVOACCG           NO OLD GROUP                           
*&&                                                                             
PGX      OI    GETSEQF,APILRERD          FLAG READ SEQUENCE BROKEN              
         J     EXIT                                                             
         DROP  R2,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET AGENCY SYSTEM SECURITY FLAGS AND SAVE IN SAVASF                           
***********************************************************************         
GETASF   NTR1  BASE=*,LABEL=*                                                   
         XC    SAVASF(L'SAVASF),SAVASF                                          
*                                                                               
         CLI   SELSYS,0                                                         
         BE    GASFX                                                            
*                                                                               
         L     R4,AIOAREA2         READ ID RECORD FROM CONTROL FILE             
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCYID                                                
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GASFX                                                            
*                                                                               
         LA    R3,CT5DATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
         USING CTSYSEL,R3                                                       
GASF010  CLI   0(R3),0                                                          
         BE    GASFX                                                            
         CLI   CTSYSEL,CTSYSELQ                                                 
         BE    GASF030                                                          
*                                                                               
GASF020  IC    R0,1(R3)            DO NEXT ELEMENT                              
         AR    R3,R0                                                            
         B     GASF010                                                          
*                                                                               
GASF030  CLC   SELSYS,CTSYSNUM                                                  
         BNE   GASF020                                                          
         CLI   CTSYSLEN,X'18'                                                   
         BNE   GASFX                                                            
         MVC   SAVASF,CTSYSPGM                                                  
         B     GASFX                                                            
*                                                                               
GASFX    MVC   FVMSGNO,=AL2(FVFOK) IGNORE IO ERROR MESSAGE                      
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R3,R4                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO BUILD PROGRAM TABLE (NAME AND CODE SAVE TABLE)                     
***********************************************************************         
INITPGMT NTR1  BASE=*,LABEL=*                                                   
         XC    PGMSTCNT,PGMSTCNT         CLEAR PROGRAM COUNT                    
*                                                                               
         CLI   SELSYS,0                                                         
         BE    IPLX                                                             
*                                                                               
         BAS   RE,GSELST                 GET SELIST ENTRY FOR SYSTEM            
*                                                                               
         L     R1,ASE                    ADDRESS IN ASE                         
         L     R1,SEPGMS-SELISTD(R1)                                            
         LH    RE,0(R1)                                                         
         STCM  RE,3,PLSTLEN              SAVE LENGTH PGMLST ENTRY               
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         ST    R1,APGMLST                                                       
         USING PGMLSTD,R1                R1=A(PROGRAMS LIST)                    
         LA    R4,PGMSTAB                SORT PROGRAM NAMES ALPHA               
         SR    R8,R8                     COUNT NUMBER OF PROGRAMS               
         XC    PROGRAM,PROGRAM                                                  
*                                        SORT PROGRAM NAMES ALPHABTCLLY         
IPL010   EQU   *                                                                
*&&UK                                                                           
*        TM    PGMIND3,PGMIPC            IGNORE PC PROGRAM                      
*        BO    IPL060                                                           
*&&                                                                             
         TM    CUSTAT,CUSDDS                                                    
         BO    *+12                      IF NOT DDS TERMINAL                    
         TM    PGMIND,PGMINOP            IGNORE NOP PROGRAM                     
         BO    IPL060                                                           
*                                                                               
         TM    CUSTAT,CUSDDS                                                    
         BO    *+12                      IF NOT DDS TERMINAL                    
         TM    PGMIND,PGMIACC            IGNORE RESTRICTED ACCESS PGM           
         BO    IPL060                                                           
*                                                                               
         TM    CUSTAT,CUSDDS                                                    
         NOP   *+12                      IF NOT DDS TERMINAL                    
         TM    PGMIND4,PGMINAUT          IGNORE NO AUTH REQUIRED PGM            
         BO    IPL060                                                           
*&&UK                                                                           
         CLI   PGMCTRY,0                 TEST COUNTRY CODE (NOT IN US)          
         BE    *+14                      KEEP ENGLISH DEFAULT                   
         CLC   PGMCTRY,CUCTRY            AND CONNECT COUNTRY                    
         BNE   IPL060                                                           
*&&                                                                             
         OC    SELPGM,SELPGM                                                    
         BZ    *+14                                                             
         CLC   SELPGM,PGMNUM                                                    
         BNE   IPL060                                                           
                                                                                
         MVC   L'PGMNAME(1,R4),PGMNUM                                           
         MVC   0(L'PGMNAME,R4),PGMNAME                                          
         LA    R4,L'PGMNAME+L'PGMNUM(R4)                                        
         LA    R8,1(R8)                                                         
IPL060   BXLE  R1,RE,IPL070                                                     
         B     IPL100                                                           
*                                                                               
IPL070   MVC   PROGRAM,PGMNUM                                                   
         MVC   PROGCTRY,PGMCTRY                                                 
         LR    R0,RF                                                            
         LR    RF,R1                                                            
         SR    RF,RE                                                            
         L     R3,APGMLST                                                       
IPL080   EQU   *                                                                
*&&UK                                                                           
         CLC   CUCTRY,PGMCTRY-PGMLSTD(R3)                                       
         BE    *+14                      NO SYNONYM CHECK ACROSS CNTRYS         
         CLC   PROGCTRY,PGMCTRY-PGMLSTD(R3)                                     
         BNE   IPL090                                                           
*&&                                                                             
         CLC   PROGRAM,PGMNUM-PGMLSTD(R3)                                       
         BNE   IPL090                    AVOID SYNONOMOUS PROGRAMS              
         LR    RF,R0                                                            
         B     IPL060                                                           
IPL090   BXLE  R3,RE,IPL080                                                     
         LR    RF,R0                                                            
         B     IPL010                                                           
*                                                                               
IPL100   LTR   R8,R8                     ANY PROGRAMS?                          
         BZ    IPLX                      . NO, EXIT                             
*                                                                               
         ST    R1,APGMLSTX                                                      
         LA    R4,PGMSTAB                                                       
         LA    R0,L'PGMNAME                                                     
         LA    R3,L'PGMNAME+L'PGMNUM                                            
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CXSORT-COMFACSD(RF)                                           
         GOTO1 (RF),APPARM,(X'00',(R4)),(R8),(R3),(R0),0                        
         STCM  R8,3,PGMSTCNT                                                    
*                                                                               
IPLX     J     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* LOCATE SELIST ENTRY FOR SYSTEM AND SAVE AT ASE                                
***********************************************************************         
GSELST   NTR1                                                                   
         L     R3,ASYS                                                          
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
*                                                                               
         CLC   SELSYS,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         ST    R3,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
*                                                                               
         J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* ROUTINE TO DISPLAY CLIENT CODE LIST IN SYSTEM LIMIT ACCESS                    
*         ON ENTRY R2=A(PASSWORD RECORD)                                        
*                  R4=A(GRIDS VALUES AREA)                                      
***********************************************************************         
         USING SA0REC,R2           R2=A(RECORD KEY)                             
         USING GVALSD,R4                                                        
DISCLA   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,GVLACC           R4=A(OUTPUT LINE)                            
         DROP  R4                                                               
*                                                                               
         LA    R3,SA0DATA          R3=A(ELEMENT)                                
DCLA010  CLI   0(R3),0             TEST END OF RECORD                           
         BE    DCLA100                                                          
         CLI   0(R3),SACLAELQ      X'E1' LIMIT ACCESS CLIENTS                   
         BE    DCLA030                                                          
DCLA020  LLC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     DCLA010                                                          
*                                                                               
         USING SACLAD,R3                                                        
DCLA030  CLC   SACLASYS,SELSYS                                                  
         BNE   DCLA020                                                          
         LA    R8,SACLACOD                                                      
         B     DCLA050                                                          
*                                                                               
DCLA040  LA    R8,L'SACLACOD(,R8)  BUMP TO NEXT CLIENT IN ELEMENT               
         LLC   RF,1(R3)                                                         
         AR    RF,R3                                                            
         CR    RF,R8               ARE WE FINISHED WITH CLIENTS?                
         BNH   DCLA020             YES                                          
*                                                                               
DCLA050  OC    CLSTACC,CLSTACC                                                  
         BNZ   DCLA060                                                          
         MVC   CLSTACC(3),0(R8)                                                 
*                                                                               
DCLA060  MVC   APWORK(4),SPACES                                                 
         CLI   SELSYS,X'04'        PRINT                                        
         BNE   DCLA070                                                          
         MVC   APWORK(3),0(R8)                                                  
         B     DCLA080                                                          
*                                                                               
DCLA070  MVC   APPARM+4(4),=X'D9000A15' CLUNPK                                  
         L     RF,VCOLY                                                         
         GOTO1 (RF),APPARM,0                                                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R8),APWORK                                            
*                                                                               
DCLA080  MVC   0(3,R4),APWORK                                                   
         CLI   2(R4),C' '                                                       
         BE    DCLA090                                                          
         MVI   3(R4),C','                                                       
         LA    R4,4(R4)                                                         
         B     DCLA040                                                          
*                                                                               
DCLA090  MVI   2(R4),C','                                                       
         LA    R4,3(R4)                                                         
         B     DCLA040                                                          
*                                                                               
DCLA100  OC    CLSTACC,CLSTACC                                                  
         BZ    DCLA110                                                          
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
*                                                                               
DCLA110  J     EXIT                                                             
         LTORG                                                                  
         DROP  R2,R3                                                            
*&&                                                                             
*&&UK                                                                           
***********************************************************************         
* ROUTINE TO DISPLAY CLIENT LIMIT ACCESS (UK VERSION)                           
*         ON ENTRY R2=A(PASSWORD RECORD)                                        
*                  R4=A(GRIDS VALUES AREA)                                      
***********************************************************************         
         USING SA0REC,R2           R2=A(RECORD KEY)                             
         USING GVALSD,R4                                                        
DISCLA   NTR1  BASE=*,LABEL=*                                                   
         XC    GVCLAC,GVCLAC                                                    
         XC    APPARM(12),APPARM   COUNT +VE AND -VE CLIENTS AND LALS           
*                                                                               
         LA    R3,SA0DATA          R3=A(ELEMENT)                                
         USING SACLAD,R3                                                        
DCLA010  CLI   0(R3),0             TEST END OF RECORD                           
         BE    DCLA100                                                          
         CLI   0(R3),SACLAELQ                                                   
         BNE   DCLA020                                                          
         CLC   SACLASYS,SELSYS                                                  
         BE    DCLA030                                                          
*                                                                               
DCLA020  LLC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     DCLA010                                                          
*                                                                               
DCLA030  XR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         SHI   RF,SACLACOD-SACLAEL RF=LENGTH LEFT IN ELEMENT                    
         BNP   DCLA020             EMPTY ELEMENT!                               
         LA    RE,SACLACOD         RE=FIRST CODE                                
*                                                                               
         CLI   0(RE),0             TEST ELEMENT IS LIMIT ACCESS LIST            
         BNE   DCLA040             NO, SKIP, IT'S A LIST OF CLIENTS             
*                                                                               
         L     R0,APPARM+8         INCREMENT LAL COUNTER                        
         AHI   R0,1                                                             
         ST    R0,APPARM+8                                                      
         B     DCLA020             YES, IGNORE THIS PASS                        
*                                                                               
DCLA040  LA    R1,APPARM           +VE CLIENT COUNTER                           
         TM    0(RE),X'80'         TEST -VE CLIENT                              
         BO    *+8                 NO, SKIP                                     
         AHI   R1,4                -VE CLIENT COUNTER                           
         L     R0,0(,R1)           INCREMENT + OR - COUNTER                     
         AHI   R0,1                                                             
         ST    R0,0(,R1)                                                        
         AHI   RE,L'SACLACOD       NEXT CLIENT                                  
         SHI   RF,L'SACLACOD                                                    
         BP    DCLA040             COUNT ALL CLIENTS IN ELEMENT                 
*                                                                               
         B     DCLA020             NO MORE, NEXT ELEMENT                        
         DROP  R3                                                               
*                                                                               
DCLA100  OC    APPARM(12),APPARM   TEST ANY CLIENT LIMIT ACCESS                 
         JZ    EXIT                NO, LEAVE BLANK                              
         LA    R4,GVCLAC           *** REUSING R4 ***                           
         DROP  R4                  GVALSD NO LONGER ADDRESSABLE                 
         MVI   0(R4),C'+'                                                       
         LA    R4,1(,R4)                                                        
         L     R1,APPARM                                                        
         EDIT  (R1),(3,(R4)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB,          X        
               ZERO=NOBLANK                                                     
         AR    R4,R0                                                            
         MVC   0(2,R4),=C'/-'                                                   
         LA    R4,2(,R4)                                                        
         L     R1,APPARM+4                                                      
         EDIT  (R1),(3,(R4)),ALIGN=LEFT,WRK=APWORK,DUB=APDUB,          X        
               ZERO=NOBLANK                                                     
         AR    R4,R0                                                            
*                                                                               
         LA    R3,SA0DATA          R3=A(ELEMENT)                                
         LA    R0,10+1             MAX 10 LALS +1: BCT BEFORE NOT AFTER         
         USING SACLAD,R3                                                        
DCLA110  CLI   0(R3),0             TEST END OF RECORD                           
         JE    EXIT                                                             
         CLI   0(R3),SACLAELQ                                                   
         BNE   DCLA120                                                          
         CLC   SACLASYS,SELSYS                                                  
         BE    DCLA130                                                          
DCLA120  ZIC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     DCLA110                                                          
*                                                                               
DCLA130  LLC   RF,1(R3)                                                         
         SHI   RF,SACLACOD-SACLAEL RF=LENGTH LEFT IN ELEMENT                    
         BNP   DCLA120             EMPTY ELEMENT!                               
         CLI   SACLACOD,0          TEST ELEMENT IS LIMIT ACCESS LIST            
         BNE   DCLA120             NO, SKIP, IT'S A LIST OF CLIENTS             
*                                                                               
         BCT   R0,DCLA140                                                       
         BCTR  R4,0                IF MORE THAN 10,                             
         CLI   0(R4),C'/'          REPLACE LAST ONE WITH ELLIPSIS               
         BNE   *-6                                                              
         MVC   1(3,R4),=C'...'     SO ENDS LIKE XXX/YYY/...                     
         J     EXIT                                                             
DCLA140  MVI   0(R4),C'/'          LAL SHOULD BE ONLY THING IN ELEMENT!         
         MVC   1(3,R4),SACLALAL                                                 
         CLI   3(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,4(,R4)                                                        
*                                                                               
         B     DCLA120             NEXT ELEMENT                                 
         DROP  R2,R3                                                            
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PROGRAM NAMES AND ACCESS CODES                               
***********************************************************************         
BPGMS    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SELSYS,0                  ANY SYSTEM ENTERED?                    
         BE    BPGMX                     . NO, EXIT                             
         SR    R2,R2                     R2=NUMBER OF PROGRAMS IN TABLE         
         ICM   R2,3,PGMSTCNT             ANY PROGRAMS FOR SYSTEM?               
         BZ    BPGMX                     . NO, EXIT                             
*                                                                               
         LA    R3,WORKD                                                         
         AHI   R3,GVALS-WORKD                                                   
         USING GVALSD,R3                                                        
         LA    R3,GVPGMS                 R3=A(PROGRAM GRIDS VALUES)             
         USING GVPGMSD,R3                                                       
*                                                                               
         LA    R4,PGMSTAB                R4=A(SORTED PROGRAMS TABLE)            
*                                                                               
BPGM020  ICM   R1,15,APGMLST             GET A(PGMLST ENTRY)                    
BPGM030  CLC   L'PGMNAME(1,R4),PGMNUM-PGMLSTD(R1)                               
         BE    BPGM040                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PLSTLEN                                                     
         LA    R1,0(RF,R1)                                                      
         CLM   R1,15,APGMLSTX            GET A(END PGMLST)                      
         BL    BPGM030                                                          
         DC    H'0'                                                             
*                                                                               
BPGM040  GOTO1 TSTAGYLA,(R1)             RESTRICTED AGENCY ACCESS LIST          
         BNE   BPGM080                                                          
*                                                                               
         MVC   APBYTE,L'PGMNAME(R4)                                             
         GOTO1 ATSTPGM,APBYTE            CHECK USER PROGRAM AUTH<>NO            
         BE    BPGM070                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)       CLEAR RETURN MESSAGE                   
*&&UK*&& B     BPGM080                   BRANCH OUT HERE IN UK ONLY             
*                                                                               
         OC    ASYSEL,ASYSEL                                                    
         BZ    BPGM068                                                          
*                                                                               
         MVC   PROGRAM,APBYTE            CHECK AUTH VALUE PRESENT               
         BAS   RE,GETPAVAL               IN SYSTEM ELEMENT                      
         BNE   BPGM050                                                          
         CLC   PACCVAL,NAUTH                                                    
         BNE   BPGM070                   IF AUTH<>N DISPLAY PROGRAM             
         B     BPGM068                                                          
*                                                                               
BPGM050  L     RE,ASYSEL                 CHECK AUTH TAKEN FROM DEFAULT          
         USING SASYSD,RE                                                        
         CLC   SASYSALL,NAUTH                                                   
         BNE   BPGM070                   IF AUTH<>N DISPLAY PROGRAM             
         DROP  RE                                                               
*                                                                               
BPGM068  OI    GVPGMF,GVPANOQ            AUTH=NO OVERRIDE                       
         B     *+8                                                              
BPGM070  NI    GVPGMF,X'FF'-GVPANOQ                                             
         MVC   GVPGMN,0(R4)              SAVE PROGRAM NAME                      
         MVC   GVPGMC,L'PGMNAME(R4)      SAVE PROGRAM CODE                      
         MVC   GVPGMV,SPACES                                                    
         MVI   GVPGMV,C'N'               DEFAULT TO NO AUTH                     
         MVC   PROGRAM,GVPGMC                                                   
         BAS   RE,DISPACC                                                       
*                                                                               
         LA    R3,GVPGMLQ(R3)                                                   
BPGM080  LA    R4,L'PGMNAME+L'PGMNUM(R4)                                        
         BCT   R2,BPGM020                                                       
*                                                                               
BPGMX    J     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------                   
* TEST RESTRICTED AGENCY ACCESS LIST IN PGMAGYLA                                
*------------------------------------------------------------                   
TSTAGYLA NTR1                                                                   
         USING PGMLSTD,R1                R1=A(PROGRAMS LIST)                    
         SR    RF,RF                                                            
         ICM   RF,7,PGMAGYLA                                                    
         BZ    TALAOK                    ALL AGENCIES VALID                     
*                                                                               
TALA010  CLC   0(2,RF),SPACES            END OF LIST, AGENCY INVALID            
         BE    TALANO                                                           
         CLC   0(2,RF),AGENCYID          AGENCY FOUND IN LIST                   
         BE    TALAOK                                                           
         LA    RF,2(RF)                                                         
         B     TALA010                                                          
*                                                                               
TALANO   J     NO                                                               
TALAOK   J     YES                                                              
         DROP  R1                                                               
         EJECT                                                                  
*------------------------------------------------------------                   
*   GET PROGRAM ACCESS CODE VALUE FROM SYSTEM ELEMENT                           
*   ON INPUT PROGRAM 1 BYTE PROGRAM CODE, R3 POINTS TO SYSTEM ELEM.             
*   ON OUTPUT PACCVAL 2 BYTE ACCESS CODE IF FOUND ELSE CC .NE.                  
*------------------------------------------------------------                   
         USING SASYSD,R3                                                        
GETPAVAL NTR1                                                                   
         L     R3,ASYSEL                                                        
         XC    PACCVAL,PACCVAL                                                  
         LA    R1,SASYSPGM               POINT TO SYSTEM ELEMENT                
         ZIC   RE,SASYSLN                                                       
*                                        FIND PROGRAM IN ELEMENT                
GPAV010  CHI   RE,SASYSLNQ                                                      
         BNH   GPAVNO                    END OF ELEMENT                         
         CLC   PROGRAM,0(R1)                                                    
         BE    GPAVYES                   PROGRAM FOUND                          
         LA    R1,L'SASYSPGM(R1)         GET NEXT PROGRAM                       
         SHI   RE,L'SASYSPGM                                                    
         B     GPAV010                                                          
*                                                                               
GPAVNO   J     NO                        PROGRAM NOT FOUND                      
GPAVYES  MVC   PACCVAL,1(R1)             SAVE ACCESS CODE VALUE                 
         J     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
*------------------------------------------------------------                   
*   DISPLAY PROGRAM ACCESS CODES                                                
*------------------------------------------------------------                   
         USING GVPGMSD,R3                                                       
DISPACC  NTR1                                                                   
*                                                                               
         TM    GVPGMF,GVPANOQ            AUTH=NO OVERRIDE?                      
         BZ    DPAC020                   . NO, CONTINUE                         
         MVC   PACCVAL,NAUTH                                                    
         B     DPAC040                                                          
*                                                                               
DPAC020  L     R2,ASYSEL                 CHECK AUTH TAKEN FROM DEFAULT          
         USING SASYSD,R2                                                        
         BAS   RE,GETPAVAL               GET ACCESS FROM SYSTEM ELEM            
         BE    *+10                      NOT FOUND USE DEFAULT                  
         MVC   PACCVAL,SASYSALL          DEFAULT TO ALL VALUE                   
         DROP  R2                                                               
*                                                                               
         MVI   ACCASF,C'N'                                                      
         BAS   RE,TSTASF                 TEST FOR 'C' CONVERTED PROGRAM         
         BNE   DPAC040                                                          
         CLI   SELSYS,X'06'              SPECIAL PROCESSING FOR ACC             
         BNE   *+12                                                             
         MVI   ACCASF,C'Y'                                                      
         B     *+12                                                             
         MVI   GVPGMV,C'C'                                                      
         B     DPACX                                                            
*                                                                               
DPAC040  BAS   RE,DISPAVAL               DISPLAY ACCESS VALUE IN TABLE          
         MVC   GVPGMV,APWORK                                                    
*                                                                               
DPACX    J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*------------------------------------------------------------                   
*   FORMAT PROGRAM ACCESS CODE VALUE FOR DISPLAY                                
*   PACCVAL 2 BYTE INPUT CODE, OUTPUT TEXT IN APWORK                            
*------------------------------------------------------------                   
DISPAVAL NTR1                                                                   
         XC    APWORK,APWORK                                                    
         CLI   ACCASF,C'Y'         TEST FOR CONVERTED ACC PROGRAM               
         BE    DPAV030                                                          
         CLC   YAUTH,PACCVAL                                                    
         BNE   DPAV010                                                          
         MVC   APWORK(1),CT@YES    PROG=Y                                       
         B     DPAVX                                                            
*                                                                               
DPAV010  EQU   *                                                                
         CLC   NAUTH,PACCVAL                                                    
         BNE   DPAV020                                                          
         MVC   APWORK(1),CT@NO     PROG=N                                       
         B     DPAVX                                                            
*                                                                               
DPAV020  EQU   *                                                                
*                                  OUTPUT HEX 4 CHAR VALUE                      
         GOTO1 VHEXOUT,APPARM,PACCVAL,APWORK,2,=C'TOG'                          
         B     DPAVX                                                            
*                                                                               
DPAV030  EQU   *                                                                
         CLC   YAUTH,PACCVAL                                                    
         BNE   DPAV032                                                          
         MVI   APWORK,C'C'                                                      
         B     DPAVX                                                            
*                                                                               
DPAV032  EQU   *                                                                
         CLC   NAUTH,PACCVAL                                                    
         BNE   DPAV040                                                          
         MVI   APWORK,C'N'                                                      
         B     DPAVX                                                            
*                                  OUTPUT CONVERTED + HEX VALUE                 
DPAV040  EQU   *                                                                
         MVI   APWORK,C'C'                                                      
         GOTO1 VHEXOUT,APPARM,PACCVAL+1,APWORK+1,1,=C'TOG'                      
*                                                                               
DPAVX    J     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------                   
* TEST IF PROGRAM AUTH IS TO BE DISPLAYED AS 'C' CONVERTED TO NEW SEC           
*------------------------------------------------------------                   
TSTASF   NTR1                                                                   
*&&US*&& B     TASFNO              UK ONLY                                      
         CLI   SELSYS,X'04'        MEDIA/MPL/FEE/ACC SYSTEM ONLY                
         BE    TASF008                                                          
         CLI   SELSYS,X'05'                                                     
         BE    TASF008                                                          
         CLI   SELSYS,X'07'                                                     
         BE    TASF008                                                          
         CLI   SELSYS,X'06'                                                     
         BE    TASF008                                                          
         B     TASFNO                                                           
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
TASF008  ICM   R1,15,APGMLST                                                    
         BZ    TASFNO                                                           
TASF010  CLC   PROGRAM,PGMNUM                                                   
         BE    TASF020                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PLSTLEN                                                     
         LA    R1,0(RF,R1)                                                      
         CLM   R1,15,APGMLSTX      GET A(END PGMLST)                            
         BL    TASF010                                                          
         DC    H'0'                                                             
*                                                                               
TASF020  TM    PGMIND2,PGMISECA    TEST PROGRAM SUPPORTS NEW SECURITY           
         BZ    TASFNO                                                           
         TM    PGMIND2,PGMISECB    TEST PROGRAM SUPPORTS OLD SECURITY           
         BZ    TASFOK              MUST BE CONVERTED                            
         DROP  R1                                                               
*                                                                               
         LA    RE,1                TEST AGENCY SYSTEM SECURITY FLAGS            
         SLL   RE,31               FOR PROGRAM IN 64 BIT MASK                   
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         IC    R1,PROGRAM                                                       
         BCTR  R1,0                                                             
         SRDL  RE,0(R1)                                                         
         LTR   RE,RE                                                            
         BZ    TASF030                                                          
         ICM   RF,15,SAVASF        FIRST GROUP OF FOUR BYTES                    
         NR    RE,RF                                                            
         BZ    TASFNO                                                           
         B     TASFOK                                                           
*                                                                               
TASF030  ICM   RE,15,SAVASF+4      SECOND GROUP OF FOUR BYTES                   
         NR    RF,RE                                                            
         BZ    TASFNO                                                           
         B     TASFOK                                                           
*                                                                               
TASFNO   J     NO                  EXIT NO NOT 'C' TYPE                         
TASFOK   J     YES                 EXIT OK 'C' TYPE                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HIDE THE PASSWORD                                                   *         
***********************************************************************         
HIDEPWD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
**NOP    MVC   DDLKPWD,PERPWD      COPY PASSWORD FOR DDLINK                     
*                                                                               
         CLI   APACTN,ACTGRD       ACTION GRIDS?                                
         BE    *+12                                                             
         TM    PERPWDH+(FVATRB-FVIHDR),FVAPROT+FVALOWI PROT'D & LOW INT         
         BO    HPWDX               YES - DON'T HIDE IT                          
*                                                                               
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    HPWD60                                                           
*                                                                               
         CLC   AGENCYID,XIAGYPER   AGY = PID AGY?                               
         BNE   HPWD60              NO - HIDE PWD                                
         DROP  RE,RF                                                            
*                                                                               
         OC    ACASEC,ACASEC                                                    
         BZ    *+16                                                             
         L     RF,ACASEC           'DDS' PASSWORD                               
         TM    SECINDS-SECD(RF),SECIDDS                                         
         BO    HPWD60              YES - HIDE PWD                               
*                                                                               
         CLI   PIDREQD,C'Y'        PPS?                                         
         BNE   HPWDX               NO - DON'T HIDE PWD                          
*                                                                               
HPWD60   CLI   APACTN,ACTGRD       ACTION GRIDS?                                
         BE    *+14                                                             
         MVC   PERPWD,=CL10'***'                                                
         B     HPWDX                                                            
*                                                                               
         LA    R1,WORKD                                                         
         AHI   R1,GVALS-WORKD                                                   
         USING GVALSD,R1                                                        
         MVC   GVPSWD,=CL10'***'                                                
         DROP  R1                                                               
*                                                                               
HPWDX    J     EXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO UPDATE OLD PASSWORD RECORD WITH NEW EFFECTIVE DATE END   *         
* DATE IN APHALF                                                      *         
***********************************************************************         
         USING SA0REC,R2                                                        
UPDOLDPW NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOAREA2         UPDATE EXISTING PASSWORD RECORD              
         MVC   IOKEY(L'SA0KEY),SA0KEYSV                                         
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 ASETACT,SA0REC                                                   
*                                                                               
         USING SAPEFD,R3                                                        
         LA    R3,APELEM           UPDATE EFFECTIVE DATES ELEMENT               
         MVI   SAPEFEL,SAPEFELQ       WITH NEW DATE                             
         MVI   SAPEFLN,0                                                        
         GOTO1 ADELELS,SA0REC                                                   
         MVI   SAPEFLN,SAPEFLNQ                                                 
         MVC   SAPEFSTA,PEFSTASV                                                
         SR    RF,RF                                                            
         ICM   RF,3,APHALF         DECREMENT DATE VALUE                         
         BZ    UOLDP010                                                         
         GOTO1 VDATCON,APPARM,(2,APHALF),(0,ADDATE)                             
         GOTO1 VADDAY,APPARM,ADDATE,ADDATE,F'-1'                                
         GOTO1 VDATCON,APPARM,(0,ADDATE),(2,SAPEFEND)                           
         B     UOLDP020                                                         
UOLDP010 BCTR  RF,0                                                             
         STCM  RF,3,SAPEFEND                                                    
UOLDP020 GOTO1 AADDELS,SA0REC                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE PERSON ADDRESS ELEMENT FIELD AND ADD IN THE     *         
* FORMAT OF SAADR TO SAPEREC IN AIOAREA1                              *         
* R0 - ELEMENT SUBCODE  R1 - FIELD HEADER                             *         
***********************************************************************         
         USING SAPEREC,R2                                                       
VALIDADR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,APELEM                                                        
         USING SAADRD,R3                                                        
         XC    SAADREL(SAADRLNQ),SAADREL                                        
         MVI   SAADREL,SAADRELQ                                                 
         MVI   SAADRLN,SAADRLNQ                                                 
         STC   R0,SAADRTYP                                                      
         GOTO1 AFVAL                                                            
         BNE   VAEXIT                                                           
         ZIC   R1,SAADRLN                                                       
         LA    RE,SAADRD(R1)                                                    
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),FVIFLD                                                   
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)                                                         
         LA    RF,1(RF,R1)                                                      
         STC   RF,SAADRLN                                                       
*                                                                               
         GOTO1 AADDELS,SAPEREC                                                  
*                                                                               
VAEXIT   J     EXIT                                                             
         DROP  R3                                                               
*&&US                                                                           
***********************************************************************         
* ROUTINE TO VALIDATE PERSON ADDRESS ELEMENT STATE AND ZIP CODE                 
* FORMAT OF SAADR TO SAPEREC IN AIOAREA1                                        
***********************************************************************         
VALIDZIP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,PERCODEH                                                      
         GOTO1 AFVAL                                                            
         JNE   EXITLO              ZIP CODE REQUIRED                            
         CLI   FVILEN,PPOSUSAQ     ZIP CODE LENGTH                              
         JNE   EXITHI                                                           
*                                                                               
         LA    R2,IOKEY                                                         
         USING PPOSD,R2                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   PPOSKMAJ,PPOSKMAQ                                                
         MVI   PPOSKMIN,PPOSKMIQ                                                
         MVI   PPOSKREC,PPOSKREQ                                                
         LA    R1,PPOSKCDE+L'PPOSKCDE-PPOSUSAQ                                  
         MVC   0(PPOSUSAQ,R1),FVIFLD                                            
         GOTO1 AIO,IORD+IOGENDIR+IO2                                            
         JNE   EXITHI                                                           
         GOTO1 AIO,IOGET+IOGENFIL+IO2                                           
         JNE   EXITHI                                                           
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R4,PPFIRST(R2)                                                   
VZ010    CLI   0(R4),0                                                          
         JE    EXITHI                                                           
         CLI   0(R4),PCITYELQ                                                   
         BE    VZ020                                                            
VZ015    LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VZ010                                                            
*                                                                               
         USING PCITYD,R4                                                        
VZ020    MVI   PERCODEH+FHILD,PPOSUSAQ                                          
         OI    PERCODEH+FHOID,FHOITR                                            
         MVC   PERCODE(5),PPOSKCDE+L'PPOSKCDE-PPOSUSAQ                          
*                                                                               
         MVI   PERCTRYH+FHILD,2                                                 
         OI    PERCTRYH+FHOID,FHOITR                                            
         MVC   PERSTE,PSTATE                                                    
*                                                                               
         MVI   PERCTRYH+FHILD,3                                                 
         OI    PERCTRYH+FHOID,FHOITR                                            
         MVC   PERCTRY,SPACES                                                   
         MVC   PERCTRY(3),PCOUNTRY                                              
*                                                                               
         TM    PERCODEH+FHIID,FHIITH                                            
         BO    *+12                                                             
         CLI   PERCITY,C' '                                                     
         BH    VZ050                                                            
         MVC   PERCITY,SPACES                                                   
         LLC   R1,PCITYL                                                        
         CHI   R1,L'PERCITY                                                     
         BNH   *+8                                                              
         LHI   R1,L'PERCITY                                                     
         AHI   R1,-1                                                            
         BNP   VZ050                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERCITY(0),PCITY                                                 
         AHI   R1,1                                                             
         STC   R1,PERCITYH+FHILD                                                
         OI    PERCITYH++FHOID,FHOITR                                           
*                                                                               
VZ050    J     EXITEQ                                                           
*&&                                                                             
***********************************************************************         
* INCREMENT ACCESS GROUP RECORD STAFF COUNT                           *         
* ACCESS GROUP CODE IN APWORK                                         *         
***********************************************************************         
                                                                                
         USING SAAGREC,R2                                                       
INCAGRS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SAAGKEY,SAAGKEY     READ RECORD                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,APWORK                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R4,SAAGDATA         GET STAFF COUNT ELEMENT                      
         SR    R0,R0                                                            
IAS010   CLI   0(R4),0                                                          
         BE    IASX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    IAS020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     IAS010                                                           
         USING SAPCTD,R4                                                        
IAS020   SR    RE,RE               INCREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    IASX                                                             
         DC    H'00'                                                            
IASX     J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INCREMENT APPROVER GROUP RECORD STAFF COUNT                         *         
* APPROVER GROUP CODE IN APWORK                                       *         
***********************************************************************         
                                                                                
         USING SAAPREC,R2                                                       
INCAPRS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SAAPKEY,SAAPKEY     READ RECORD                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID                                                 
         MVC   SAAPAGR,APWORK                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R4,SAAPDATA         GET STAFF COUNT ELEMENT                      
         SR    R0,R0                                                            
IAP010   CLI   0(R4),0                                                          
         BE    IAPX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    IAP020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     IAP010                                                           
         USING SAPCTD,R4                                                        
IAP020   SR    RE,RE               INCREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    IAPX                                                             
         DC    H'00'                                                            
IAPX     J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INCREMENT LIMIT ACCESS GROUP RECORD STAFF COUNT                     *         
* ACCESS GROUP CODE IN APWORK                                         *         
***********************************************************************         
                                                                                
         USING SALAREC,R2                                                       
INCLAGS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SALAKEY,SALAKEY     READ RECORD                                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         MVC   SALAAGY,AGENCYID                                                 
         MVC   SALAAGN,APWORK                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R4,SALADATA         GET STAFF COUNT ELEMENT                      
         SR    R0,R0                                                            
ILS010   CLI   0(R4),0                                                          
         BE    ILSX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    ILS020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ILS010                                                           
         USING SAPCTD,R4                                                        
ILS020   SR    RE,RE               INCREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    ILSX                                                             
         DC    H'00'                                                            
ILSX     J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INCREMENT OFFICE RECORD STAFF COUNT                                 *         
* OFFICE CODE IN APWORK                                               *         
***********************************************************************         
                                                                                
         USING SAOFREC,R2                                                       
INCOFFS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SAOFKEY,SAOFKEY     READ RECORD                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,APWORK                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2         GET STAFF COUNT ELEMENT                      
         LA    R4,SAOFDATA                                                      
         SR    R0,R0                                                            
IOS010   CLI   0(R4),0                                                          
         BE    IOSX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    IOS020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     IOS010                                                           
         USING SAPCTD,R4                                                        
IOS020   SR    RE,RE               INCREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    IOSX                                                             
         DC    H'00'                                                            
IOSX     J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INCREMENT DEPARTMENT RECORD STAFF COUNT                             *         
* DEPARTMENT CODE IN APWORK                                           *         
***********************************************************************         
                                                                                
         USING SADPREC,R2                                                       
INCDPTS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SADPKEY,SADPKEY     READ RECORD                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,APWORK                                                   
         MVC   SADPDID,APWORK+L'SADPOID                                         
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2         GET STAFF COUNT ELEMENT                      
         LA    R4,SADPDATA                                                      
         SR    R0,R0                                                            
IDS010   CLI   0(R4),0                                                          
         BE    IDSX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    IDS020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     IDS010                                                           
         USING SAPCTD,R4                                                        
IDS020   SR    RE,RE               INCREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    IDSX                                                             
         DC    H'00'                                                            
IDSX     J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DECREMENT ACCESS GROUP RECORD STAFF COUNT                           *         
* ACCESS GROUP CODE IN APWORK                                         *         
***********************************************************************         
                                                                                
         USING SAAGREC,R2                                                       
DECAGRS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SAAGKEY,SAAGKEY     READ RECORD                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,APWORK                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2         GET STAFF COUNT ELEMENT                      
         LA    R4,SAAGDATA                                                      
         SR    R0,R0                                                            
DAS010   CLI   0(R4),0                                                          
         BE    DASX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    DAS020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DAS010                                                           
         USING SAPCTD,R4                                                        
DAS020   SR    RE,RE               DECREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    DASX                                                             
         DC    H'00'                                                            
*                                                                               
DASX     J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DECREMENT APPROVER GROUP RECORD STAFF COUNT                         *         
* APPROVER GROUP CODE IN APWORK                                       *         
***********************************************************************         
                                                                                
         USING SAAPREC,R2                                                       
DECAPRS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SAAPKEY,SAAPKEY     READ RECORD                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID                                                 
         MVC   SAAPAGR,APWORK                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2         GET STAFF COUNT ELEMENT                      
         LA    R4,SAAPDATA                                                      
         SR    R0,R0                                                            
DAP010   CLI   0(R4),0                                                          
         BE    DAPX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    DAP020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DAP010                                                           
         USING SAPCTD,R4                                                        
DAP020   SR    RE,RE               DECREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    DAPX                                                             
         DC    H'00'                                                            
*                                                                               
DAPX     J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DECREMENT LIMIT ACCESS GROUP RECORD STAFF COUNT                     *         
* ACCESS GROUP CODE IN APWORK                                         *         
***********************************************************************         
                                                                                
         USING SALAREC,R2                                                       
DECLAGS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SALAKEY,SALAKEY     READ RECORD                                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         MVC   SALAAGY,AGENCYID                                                 
         MVC   SALAAGN,APWORK                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2         GET STAFF COUNT ELEMENT                      
         LA    R4,SALADATA                                                      
         SR    R0,R0                                                            
DLS010   CLI   0(R4),0                                                          
         BE    DLSX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    DLS020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DLS010                                                           
         USING SAPCTD,R4                                                        
DLS020   SR    RE,RE               DECREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    DLSX                                                             
         DC    H'00'                                                            
*                                                                               
DLSX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* DECREMENT OFFICE RECORD STAFF COUNT                                 *         
* OFFICE CODE IN APWORK                                               *         
***********************************************************************         
         USING SAOFREC,R2                                                       
DECOFFS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SAOFKEY,SAOFKEY     READ RECORD                                  
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,APWORK                                                   
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2         GET STAFF COUNT ELEMENT                      
         LA    R4,SAOFDATA                                                      
         SR    R0,R0                                                            
DOS010   CLI   0(R4),0                                                          
         BE    DOSX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    DOS020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DOS010                                                           
         USING SAPCTD,R4                                                        
DOS020   SR    RE,RE               DECREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    DOSX                                                             
         DC    H'00'                                                            
DOSX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* DECREMENT DEPARTMENT RECORD STAFF COUNT                             *         
* DEPARTMENT CODE IN APWORK                                           *         
***********************************************************************         
         USING SADPREC,R2                                                       
DECDPTS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         XC    SADPKEY,SADPKEY     READ RECORD                                  
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,APWORK                                                   
         MVC   SADPDID,APWORK+L'SADPOID                                         
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2         GET STAFF COUNT ELEMENT                      
         LA    R4,SADPDATA                                                      
         SR    R0,R0                                                            
DDS010   CLI   0(R4),0                                                          
         BE    DDSX                                                             
         CLI   0(R4),SAPCTELQ                                                   
         BE    DDS020                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DDS010                                                           
         USING SAPCTD,R4                                                        
DDS020   SR    RE,RE               DECREMENT STAFF COUNT                        
         ICM   RE,3,SAPCTVAL                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STCM  RE,3,SAPCTVAL                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    DDSX                                                             
         DC    H'00'                                                            
*                                                                               
DDSX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO GET APPROVER GROUP DATA FROM OFFICE OR DEPRTMENT RECORD  *         
* AIOAREA2=A(OFFICE OR DEPARTMENT RECORD)                             *         
***********************************************************************         
         USING SAOFREC,R3                                                       
GETAPEL  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOAREA2                                                      
         LA    R3,SAOFDATA                                                      
         SR    RF,RF                                                            
         USING SAAPCD,R3                                                        
GAPE010  CLI   0(R3),0             SEARCH FOR APPROVER GROUP ELEMENT            
         BE    GAPENO                                                           
         CLI   SAAPCEL,SAAPCELQ                                                 
         BE    *+14                                                             
         IC    RF,SAAPCLN                                                       
         AR    R3,RF                                                            
         B     GAPE010                                                          
         MVC   APWORK(L'SAAPCCOD),SAAPCCOD                                      
         MVC   APHALF(L'SAAPCNUM),SAAPCNUM                                      
         B     GAPEOK                                                           
GAPENO   J     NO                                                               
GAPEOK   J     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CHECK READ-ONLY AND WRITE USER IDS FIELDS TO SEE WHICH             
* ONE WE SHOULD ADD FIRST.                                                      
* RETURN .EQ. IF ADD READ-ONLY FIRST, OTHERWISE .NE.                            
* NOTE: 'APELEM' IS USED FOR TEMPORARY WORK AREA                                
***********************************************************************         
CHKWRUID NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R0,R0                                                            
         MVC   APELEM(L'PERURR1),PERURR1                                        
         MVC   APELEM+L'PERURR1(L'PERURR2),PERURR2                              
*                                                                               
         LA    RE,APELEM                                                        
         LHI   RF,L'PERURR1+L'PERURR2                                           
*                                                                               
         CLC   CT@ALL,0(RE)        LANGUAGE SOFT 'ALL'                          
         BE    CWR108                                                           
         CLI   3(RE),C' '          'ALL' (THEN SPACE OR ZERO)                   
         BH    CWR110                                                           
         CLC   CT@ALL(3),0(RE)                                                  
         BNE   CWR110                                                           
*                                                                               
CWR108   LA    R0,3                ALL USERS IS SET                             
         B     CWR190                                                           
*                                                                               
CWR110   CLC   =C'A=',0(RE)                                                     
         BNE   *+12                                                             
         LA    R0,2                A= IS SET                                    
         B     CWR190                                                           
*                                                                               
         CLC   =C'L=',0(RE)                                                     
         BNE   CWR120                                                           
         LA    R0,1                L= IS SET                                    
*                                                                               
CWR120   AHI   RE,1                                                             
         BCT   RF,CWR110                                                        
*                                                                               
CWR190   LTR   R0,R0               ANY ALL, A=, OR L=?                          
         BZ    CWROK               NO - READ READ-ONLY UIDS FIRST               
         STH   R0,APHALF           ELSE CONTINUE TO CHECK UPD IDS               
*                                                                               
CWR200   EQU   *                                                                
         SR    R0,R0                                                            
         MVC   APELEM(L'PERUSR1),PERUSR1                                        
         MVC   APELEM+L'PERUSR1(L'PERUSR2),PERUSR2                              
*                                                                               
         LA    RE,APELEM                                                        
         LHI   RF,L'PERUSR1+L'PERUSR2                                           
*                                                                               
         CLC   CT@ALL,0(RE)        LANGUAGE SOFT 'ALL'                          
         BE    CWR208                                                           
         CLI   3(RE),C' '          'ALL' (THEN SPACE OR ZERO)                   
         BH    CWR210                                                           
         CLC   CT@ALL(3),0(RE)                                                  
         BNE   CWR210                                                           
*                                                                               
CWR208   LA    R0,3                ALL USERS IS SET                             
         B     CWR290                                                           
*                                                                               
CWR210   CLC   =C'A=',0(RE)                                                     
         BNE   *+12                                                             
         LA    R0,2                A= IS SET                                    
         B     CWR290                                                           
*                                                                               
         CLC   =C'L=',0(RE)                                                     
         BNE   CWR220                                                           
         LA    R0,1                L= IS SET                                    
*                                                                               
CWR220   AHI   RE,1                                                             
         BCT   RF,CWR210                                                        
*                                                                               
* IF {UPD UIDS >= READ-ONLY UIDS}, READ THE READ-ONLY UIDS FIRST                
CWR290   CH    R0,APHALF           UPD UIDS >= READ-ONLY UIDS?                  
         BL    CWRNO               NO - .NE., READ UPD IDS FIRST                
*                                                                               
CWROK    SR    RC,RC                                                            
CWRNO    LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO FIX SCANNER BLOCK AND ELIMINATE LEAD IN SPACES                     
***********************************************************************         
FIXSCAN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,BLOCK1                                                        
         LHI   R3,BLKNTYQ                TOTAL # OF SCANNER ENTRIES             
*                                                                               
FXS010   CLC   12(10,R2),SPACES          SCANNER ENTRY ALL SPACES?              
         BNH   FXS030                    . YES, THEN SKIP                       
         SR    R4,R4                                                            
         IC    R4,0(R2)                  NUMBER OF CHARACTERS IN ENTRY          
*                                                                               
FXS020   STC   R4,0(R2)                  ADJUST NUMBER OF CHARACTERS            
         CLI   12(R2),C' '               FIND THE NON-SPACE                     
         BH    FXS030                    THEN MOVE ON                           
         MVC   12(9,R2),13(R2)           SHIFT DATA                             
         MVI   21(R2),C' '               SPACE OUT LAST BYTE                    
         BCT   R4,FXS020                                                        
*                                                                               
FXS030   LA    R2,L'BLOCK1(R2)           NEXT SCANNER BLOCK ENTRY               
         BCT   R3,FXS010                                                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE ENTRY IN UPDATIVE AND READ-ONLY FIELDS                    
*      (CHECK FOR DUPLICATION AND SOME BASIC VALIDATION)                        
* RETURN .EQ.  OKAY                                                             
*        .NE.  ERROR FOUND                                                      
*              SET FVADDR & FVINDX FOR ERROR DISPLAY                            
***********************************************************************         
CHKDUP   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,BLOCK1                                                        
         LHI   R0,BLKNTYQ-1                                                     
*                                                                               
CHKD20   LR    R4,R1                                                            
         AHI   R4,L'BLOCK1                                                      
         LR    R3,R0                                                            
*                                                                               
         CLC   CT@ALL,12(R1)       LANGUAGE SOFT 'ALL'                          
         BE    CHKD28                                                           
         CLI   15(R1),C' '          'ALL' (THEN SPACE OR ZERO)                  
         BH    CHKD30                                                           
         CLC   CT@ALL(3),12(R1)                                                 
         BNE   CHKD30                                                           
CHKD28   MVC   12(10,R1),=CL10'ALL' STORE ENGLISH 'ALL' FOR CONNECT             
         BRAS  RE,CHKUALL                                                       
         BE    CHKD130                                                          
         LR    R4,R1               POSITION THE ERROR                           
         B     CHKD160                                                          
*                                                                               
CHKD30   EQU   *                                                                
         SR    RE,RE                                                            
         IC    RE,0(R1)            LENGTH                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R1),=C'AGY'    CHECK FOR KEYWORD 'AGY'                      
         BNE   CHKD40                                                           
         BRAS  RE,CHKAGY                                                        
         BE    CHKD130                                                          
         LR    R4,R1               POSITION THE ERROR                           
         B     CHKD160                                                          
*                                                                               
CHKD40   EQU   *                                                                
         SR    RE,RE                                                            
         IC    RE,0(R1)            LENGTH                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R1),=C'LIST'   CHECK FOR KEYWORD 'LIST'                     
         BNE   CHKD50                                                           
         BRAS  RE,CHKLIST                                                       
         BE    CHKD130                                                          
         L     R4,APFULL           POSITION THE ERROR                           
         B     CHKD160                                                          
*                                                                               
CHKD50   CLI   12(R1),C' '                                                      
         BNH   CHKD140                                                          
         BRAS  RE,CHKUID                                                        
         BE    CHKD130                                                          
         LR    R4,R1               POSITION THE ERROR                           
         B     CHKD160                                                          
*                                                                               
CHKD130  CLI   12(R4),C' '                                                      
         BNH   CHKD140                                                          
*                                                                               
         CLC   12(L'BLOCK1-12,R1),12(R4)                                        
         BE    CHKD150                                                          
CHKD140  AHI   R4,L'BLOCK1                                                      
         BCT   R3,CHKD130                                                       
*                                                                               
         AHI   R1,L'BLOCK1                                                      
         BCT   R0,CHKD20                                                        
         B     CHKDOKX                                                          
*                                                                               
CHKD150  EQU   *                                                                
         MVC   FVMSGNO,=AL2(CE#DUPNT)                                           
         MVC   FVOSYS,ASSYSE                                                    
*                                                                               
CHKD160  EQU   *                   R4=CURRENT POSITION                          
         LR    R1,R4                                                            
         LA    RE,BLOCK4                                                        
         SR    R1,RE               IN THE BLOCK?                                
         BM    CHKD170             NO - CHECK NEXT BLOCK                        
         LA    RE,PERURR2H                                                      
         ST    RE,FVADDR           SET CURSOR                                   
         B     CHKD200                                                          
*                                                                               
CHKD170  EQU   *                   R4=CURRENT POSITION                          
         LR    R1,R4                                                            
         LA    RE,BLOCK3                                                        
         SR    R1,RE               IN THE BLOCK?                                
         BM    CHKD180             NO - CHECK NEXT BLOCK                        
         LA    RE,PERURR1H                                                      
         ST    RE,FVADDR           SET CURSOR                                   
         B     CHKD200                                                          
*                                                                               
CHKD180  EQU   *                   R4=CURRENT POSITION                          
         LR    R1,R4                                                            
         LA    RE,BLOCK2                                                        
         SR    R1,RE               IN THE BLOCK?                                
         BM    CHKD190             NO - CHECK NEXT BLOCK                        
         LA    RE,PERUSR2H                                                      
         ST    RE,FVADDR           SET CURSOR                                   
         B     CHKD200                                                          
*                                                                               
CHKD190  EQU   *                   R4=CURRENT POSITION                          
         LR    R1,R4                                                            
         LA    RE,BLOCK1                                                        
         SR    R1,RE               IN THE BLOCK?                                
         BNM   *+6                                                              
         DC    H'0'                MUST BE IN THIS BLOCK                        
         LA    RE,PERUSR1H                                                      
         ST    RE,FVADDR           SET CURSOR                                   
*                                                                               
CHKD200  EQU   *                                                                
         SRL   R1,5                /32, (L'BLOCK=32)                            
         AHI   R1,1                +1                                           
         STC   R1,FVINDX           RETURN ENTRY INDEX                           
         B     CHKDBADX                                                         
*                                                                               
*                                                                               
CHKDOKX  SR    RC,RC               OKAY                                         
CHKDBADX LTR   RC,RC               DUPLICATE ENTRY FOUND                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK "ALL" IS ALLOWED AND IF YES, IT CAN'T COMBINE W/ OTHER UIDS             
*                                                                               
* INPUT:  R1 = CURRENT POSITION                                                 
***********************************************************************         
CHKUALL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R4,R1               SAVE THE CURRENT POSITION POINTER            
*&&US                                                                           
         LH    RF,=Y(CONNECT-TWAD)                                              
         A     RF,ATWA                                                          
         USING CONNECT,RF                                                       
         CLI   C#UID,C#UIDALL      TEST ALL USER-IDS ARE ALLOWED                
         BE    CKUA20                                                           
         DROP  RF                                                               
*                                                                               
         MVC   FVMSGNO,=AL2(CE#NCUID)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     CKUABADX                                                         
*&&                                                                             
* CHECK IF 'ALL' IS COMBINED OTHER USER, IF SO, SET ERROR.                      
CKUA20   EQU   *                                                                
         LA    RE,BLOCK3                                                        
         SR    R4,RE               IN BLK1/2(UPD) OR BLK3/4(READ-ONLY)?         
         BNM   CKUA40              +, SO IT IS IN BLK3/4(READ-ONLY)             
*                                                                               
         ZIC   R1,BLKCNT1          ADD BLKCNT1 & BLKCNT2                        
         ZIC   R0,BLKCNT2                                                       
         AR    R1,R0                                                            
         CHI   R1,1                ALL IS THE ONLY INPUT?                       
         BE    CKUAOKX             YES - OKAY                                   
         B     CKUA90              ERROR                                        
*                                                                               
CKUA40   EQU   *                                                                
         ZIC   R1,BLKCNT3          ADD BLKCNT3 & BLKCNT4                        
         ZIC   R0,BLKCNT4                                                       
         AR    R1,R0                                                            
         CHI   R1,1                ALL IS THE ONLY INPUT?                       
         BE    CKUAOKX             YES - OKAY                                   
*                                                                               
CKUA90   EQU   *                                                                
         MVC   FVMSGNO,=AL2(CE#CCALL)    CAN'T HAVE ALL W/ ANY UIDS             
         MVC   FVOSYS,ASSYSE                                                    
         B     CKUABADX                                                         
*                                                                               
CKUAOKX  SR    RC,RC               OKAY                                         
CKUABADX LTR   RC,RC               BAD ENTRY FOUND                              
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK "A="                                                                    
*                                                                               
* INPUT:  R1 = CURRENT POSITION                                                 
***********************************************************************         
CHKAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R4,R1               SAVE THE CURRENT POSITION POINTER            
*                                                                               
*&&UK*&& B     *+12                *TEMPORY-UK HAS NO SUPPORT FOR A=*           
         CLI   1(R4),2             AGENCY MUST BE 2 CHARS                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     CKAY90                                                           
*                                                                               
         L     R3,AIOAREA2         READ AGENCY ID ACCESS RECORD                 
         USING CT5REC,R3                                                        
         XC    CT5KEY,CT5KEY       BUILD ID KEY                                 
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,22(R4)                                                  
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2   CHECK RECORD FOUND/OK                  
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)     IO ERROR                               
         B     CKAY90                                                           
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)     RECORD NOT FOUND                       
         B     CKAY90                                                           
*                                                                               
* FOR DDS {#N,#E AND SJ(ANY TEST SYSTEM)}, SKIP THE FOLLOWING CHECK             
*                                                                               
*&&UK*&& CLC   =C'#E',AGENCYID                                                  
*&&US*&& CLC   =C'#N',AGENCYID                                                  
         BE    CKAY35              CONTINUE CHECK VS CONNECTED PID              
*&&US                                                                           
         L     RF,ASYS                                                          
         L     RF,VSSB-SYSFACD(RF) GET FACPAK SYSTEM ID INFO FROM SSB           
         TM    SSBSYSFL-SSBD(RF),X'80'   TEST SYSTEM                            
         BNO   *+14                      NO                                     
         CLC   =C'SJ',AGENCYID                                                  
         BE    CKAY35              CONTINUE CHECK VS CONNECTED PID              
*&&                                                                             
*                                                                               
         LA    R1,CT5DATA                                                       
         SR    R0,R0                                                            
*                                                                               
CKAY20   CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    RF,CT5KALPH                                                      
         B     CKAY30                                                           
         CLI   0(R1),CTSEAELQ      TEST SECURITY AGENCY ID ELEMENT              
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CKAY20                                                           
         LA    RF,CTSEAAID-CTSEAD(R1)                                           
         DROP  R3                                                               
*                                                                               
*A=??, ??'S SECURITY AGENCY IS SAME AS THIS AGENCY (OPTAGY/CUAALF)              
CKAY30   CLC   0(L'AGENCYID,RF),AGENCYID                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#AGICT)                                           
         B     CKAY90                                                           
*                                                                               
*CHECK IF THE CONNECTED PID CAN CONNECT TO THIS AGENCY                          
*                                                                               
CKAY35   EQU   *                                                                
         LH    RF,=Y(CONNECT-TWAD)                                              
         A     RF,ATWA                                                          
         USING CONNECT,RF                                                       
         CLI   C#UID,C#UIDALL      TEST ALL USER-IDS ARE ALLOWED                
         BE    CKAYOKX             YES - NO NEED TO CHECK ANY FURTHER           
         DROP  RF                                                               
*                                                                               
         L     R3,AIOAREA2                                                      
         USING SA0REC,R3           R3=A(PASSWORD RECORD)                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CUAALF      AGENCY                                       
         MVC   SA0KNUM,CUPASS      PID#                                         
         MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2   CHECK RECORD FOUND/OK                  
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)     IO ERROR                               
         B     CKAY90                                                           
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)     RECORD NOT FOUND                       
         B     CKAY90                                                           
*                                                                               
         LA    R1,SA0DATA                                                       
         SR    R0,R0                                                            
*                                                                               
CKAY40   CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(CE#NCAGY)                                           
         B     CKAY90                                                           
         CLI   0(R1),SAIDELQ       TEST FOR ID ELEMENT                          
         BNE   CKAY60                                                           
*                                                                               
         USING SAIDD,R1                                                         
         TM    SAIDTYP,SAIDNEQ     NEW TYPE?                                    
         BNZ   CKAY50              NO                                           
         TM    SAIDTYP,SAIDROQ     READ-ONLY?                                   
         BNO   CKAY50              NO                                           
         TM    SAIDTYP,SAIDAGQ     NAME IS AGENCY?                              
         BNO   CKAY60              NO - SKIP THIS ELEMENT                       
*                                                                               
         CLC   22(L'AGENCYID,R4),SAIDNAM                                        
         BE    CKAYOKX             MATCH - EXIT OK                              
         B     CKAY60                                                           
*                                                                               
CKAY50   CLC   =X'0001',SAID       AGENCY ELEMENT?                              
         BNE   CKAY60              NO - SKIP THIS ELEMENT                       
*                                                                               
         CLC   22(L'AGENCYID,R4),SAID+2                                         
         BE    CKAYOKX             MATCH - EXIT OK                              
         DROP  R1                                                               
*                                                                               
CKAY60   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CKAY40                                                           
         DROP  R3                                                               
*                                                                               
CKAY90   MVC   FVOSYS,ASSYSE                                                    
         B     CKAYBADX                                                         
*                                                                               
CKAYOKX  SR    RC,RC               OKAY                                         
CKAYBADX LTR   RC,RC               BAD ENTRY FOUND                              
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK "L="                                                                    
* INPUT:  R1 = CURRENT POSITION                                                 
* OUTPUT: APFULL = A(ERROR ENTRY)                                               
***********************************************************************         
CHKLIST  NTR1  BASE=*,LABEL=*,WORK=(R2,CLWORKX-CLWORKD)                         
*                                                                               
         USING CLWORKD,R2                                                       
         LR    R4,R1               SAVE THE CURRENT POSITION POINTER            
         ST    R4,APFULL                                                        
*                                                                               
         CLI   1(R4),6                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     CKLT190                                                          
*                                                                               
         L     R3,AIOAREA2                                                      
         USING SAWREC,R3                                                        
         XC    SAWKEY,SAWKEY       BUILD LIST KEY                               
         MVI   SAWKTYP,C'W'                                                     
         MVI   SAWKREC,C'I'                                                     
         MVC   SAWKID,22(R4)                                                    
         MVC   IOKEY(L'SAWKEY),SAWKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)     IO ERROR                               
         B     CKLT190                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)     RECORD NOT FOUND                       
         B     CKLT190                                                          
*                                  SAVE ID-LIST REC                             
         LA    R0,CLIDLST                                                       
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,SAWLEN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         DROP  R2,R3                                                            
*                                                                               
         AHI   R2,SAWDATA-SAWREC                                                
*                                                                               
CKLT20   CLI   0(R2),0                                                          
         BE    CKLTOKX             OKAY - END OF RECORD                         
         CLI   0(R2),CTLSTELQ      USERID ELEMENT                               
         BE    CKLT40                                                           
*                                                                               
CKLT30   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     CKLT20                                                           
*                                                                               
CKLT40   EQU   *                                                                
         USING SAIREC,R3                                                        
         XC    SAIKEY,SAIKEY       BUILD ID KEY                                 
         MVI   SAIKTYP,C'I'                                                     
         MVC   SAIKID,CTLSTDTA-CTLSTD(R2)                                       
         MVC   IOKEY(L'SAIKEY),SAIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2   CHECK RECORD FOUND/OK                  
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)     IO ERROR                               
         B     CKLT190                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)     REC NOT FOUND(NEED BETTER MSG)         
         B     CKLT190                                                          
*                                                                               
         LA    R1,SAIDATA          GET AGENCY ALPHA FOR THIS USERID             
CKLT50   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                THIS USERID RECORD IS BAD!                   
         CLI   0(R1),SAAGYELQ                                                   
         BE    CKLT60                                                           
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CKLT50                                                           
*                                                                               
         USING SAAGYD,R1                                                        
CKLT60   MVC   APHALF,SAAGYID      SAVE AGENCY ALPHA                            
         DROP  R1,R3                                                            
*                                                                               
* FOR DDS {#N,#E AND SJ(ANY TEST SYSTEM)}, SKIP THE FOLLOWING CHECK             
*                                                                               
*&&UK*&& CLC   =C'#E',AGENCYID                                                  
*&&US*&& CLC   =C'#N',AGENCYID                                                  
         BE    CKLT100             CONTINUE CHECK VS CONNECTED PID              
*&&US                                                                           
         L     RF,ASYS                                                          
         L     RF,VSSB-SYSFACD(RF) GET FACPAK SYSTEM ID INFO FROM SSB           
         TM    SSBSYSFL-SSBD(RF),X'80'   TEST SYSTEM                            
         BNO   *+14                      NO                                     
         CLC   =C'SJ',AGENCYID                                                  
         BE    CKLT100             CONTINUE CHECK VS CONNECTED PID              
*&&                                                                             
*                                                                               
         L     R3,AIOAREA2                                                      
         USING CT5REC,R3                                                        
         XC    CT5KEY,CT5KEY       BUILD ACCESS REC KEY                         
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,APHALF                                                  
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2   CHECK RECORD FOUND/OK                  
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)     IO ERROR                               
         B     CKLT190                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)     RECORD NOT FOUND                       
         B     CKLT190                                                          
*                                                                               
         LA    R1,CT5DATA                                                       
         SR    R0,R0                                                            
*                                                                               
CKLT70   CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    RF,CT5KALPH                                                      
         B     CKLT80                                                           
         CLI   0(R1),CTSEAELQ      TEST SECURITY AGENCY ID ELEMENT              
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CKLT70                                                           
         LA    RF,CTSEAAID-CTSEAD(R1)                                           
         DROP  R3                                                               
*                                                                               
*CHECK IF THIS USERID'S SEC AGY IS SAME AS THE SIGN-ON AGENCY                   
CKLT80   CLC   0(L'AGENCYID,RF),AGENCYID                                        
         BE    CKLT100                                                          
         MVC   FVMSGNO,=AL2(CE#AGICT)                                           
         B     CKLT190                                                          
*                                                                               
CKLT100  EQU   *                                                                
         LH    RF,=Y(CONNECT-TWAD)                                              
         A     RF,ATWA                                                          
         USING CONNECT,RF                                                       
         CLI   C#UID,C#UIDALL      TEST ALL USER-IDS ARE ALLOWED                
         BE    CKLT120             YES - CHECK NEXT USERID IN LIST              
         DROP  RF                                                               
*                                                                               
         L     R3,AIOAREA2                                                      
         USING SA0REC,R3           R3=A(PASSWORD RECORD)                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CUAALF      AGENCY                                       
         MVC   SA0KNUM,CUPASS      PID#                                         
         MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2   CHECK RECORD FOUND/OK                  
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)     IO ERROR                               
         B     CKLT190                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)     RECORD NOT FOUND                       
         B     CKLT190                                                          
*                                                                               
         GOTO1 VGETIDS,APPARM,(C'L',(R3)),ATIA,(C'A',VDMGR),3(R2)               
         TM    12(R1),X'01'        MATCH?                                       
         BO    CKLT120             YES - CHECK NEXT USERID IN LIST              
         MVC   FVMSGNO,=AL2(CE#NCULT)                                           
         MVC   FVXTRA(10),3(R2)                                                 
         B     CKLT190                                                          
*                                                                               
CKLT120  EQU   *                   CHECK IF USERID IS DUPLICATED                
         LA    R1,BLOCK3                                                        
         LR    RF,R4                                                            
         SR    RF,R1               IN BLK1/2(UPD) OR BLK3/4(READ-ONLY)?         
         BNM   CKLT160             +, SO IT IS IN BLK3/4(READ-ONLY)             
*                                                                               
         LA    R1,BLOCK1                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT1                                                     
         BZ    CKLT130                                                          
*                                                                               
         CLC   3(10,R2),12(R1)                                                  
         BE    CKLT179                                                          
         AHI   R1,L'BLOCK1                                                      
         BCT   R0,*-14                                                          
*                                                                               
CKLT130  LA    R1,BLOCK2                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT2                                                     
         BZ    CKLT160                                                          
*                                                                               
         CLC   3(10,R2),12(R1)                                                  
         BE    CKLT179                                                          
         AHI   R1,L'BLOCK2                                                      
         BCT   R0,*-14                                                          
         B     CKLT30                                                           
*                                                                               
CKLT160  LA    R1,BLOCK3                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT3                                                     
         BZ    CKLT170                                                          
*                                                                               
         CLC   3(10,R2),12(R1)                                                  
         BE    CKLT179                                                          
         AHI   R1,L'BLOCK3                                                      
         BCT   R0,*-14                                                          
*                                                                               
CKLT170  LA    R1,BLOCK4                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT4                                                     
         BZ    CKLT30                                                           
*                                                                               
         CLC   3(10,R2),12(R1)                                                  
         BE    CKLT179                                                          
         AHI   R1,L'BLOCK4                                                      
         BCT   R0,*-14                                                          
         B     CKLT30                                                           
*                                                                               
CKLT179  ST    R1,APFULL                                                        
         MVC   FVMSGNO,=AL2(CE#IDILT)   ID ALREADY IN L=                        
         MVC   FVXTRA(10),22(R4)                                                
         B     CKLT190                                                          
*                                                                               
CKLT190  MVC   FVOSYS,ASSYSE                                                    
         B     CKLTBADX                                                         
*                                                                               
CKLTOKX  SR    RC,RC               OKAY                                         
CKLTBADX LTR   RC,RC               BAD ENTRY FOUND                              
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK USER IDS                                                                
* INPUT:  R1 = CURRENT POSITION                                                 
***********************************************************************         
CHKUID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R4,R1               SAVE THE CURRENT POSITION POINTER            
*                                                                               
         CLI   0(R4),3             VALIDATE USER-ID                             
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)    TOO SHORT                               
         B     CKID190                                                          
         CLI   0(R4),10                                                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)    TOO LONG                                
         B     CKID190                                                          
*                                                                               
         L     R3,AIOAREA2         SWITCH I/O AREAS                             
         USING SAIREC,R3                                                        
         XC    SAIKEY,SAIKEY       BUILD ID KEY                                 
         MVI   SAIKTYP,C'I'                                                     
         MVC   SAIKID,12(R4)                                                    
         MVC   IOKEY(L'SAIKEY),SAIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2   CHECK RECORD FOUND/OK                  
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)     IO ERROR                               
         B     CKID190                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)     RECORD NOT FOUND                       
         B     CKID190                                                          
*                                                                               
         LA    R1,SAIDATA          GET AGENCY ALPHA FOR THIS USERID             
CKID20   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                THIS USERID RECORD IS BAD!                   
         CLI   0(R1),SAAGYELQ                                                   
         BE    CKID30                                                           
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CKID20                                                           
*                                                                               
         USING SAAGYD,R1                                                        
CKID30   MVC   APHALF,SAAGYID      SAVE AGENCY ALPHA                            
         DROP  R1                                                               
*                                                                               
* CHECK IF THIS UID IS ALREADY DEFINED IN ONE OF THE A=                         
*                                                                               
         LA    R1,BLOCK3                                                        
         LR    RF,R4                                                            
         SR    RF,R1               IN BLK1/2(UPD) OR BLK3/4(READ-ONLY)?         
         BNM   CKID50              +, SO IT IS IN BLK3/4(READ-ONLY)             
*                                                                               
         LA    R1,BLOCK1                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT1                                                     
         BZ    CKID42                                                           
*                                                                               
         BAS   RE,CKIDAL                                                        
         BE    CKID60                                                           
         AHI   R1,L'BLOCK1                                                      
         BCT   R0,*-12                                                          
*                                                                               
CKID42   LA    R1,BLOCK2                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT2                                                     
         BZ    CKID50                                                           
*                                                                               
         BAS   RE,CKIDAL                                                        
         BE    CKID60                                                           
         AHI   R1,L'BLOCK2                                                      
         BCT   R0,*-12                                                          
         B     CKID90                                                           
*                                                                               
CKID50   LA    R1,BLOCK3                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT3                                                     
         BZ    CKID52                                                           
*                                                                               
         BAS   RE,CKIDAL                                                        
         BE    CKID60                                                           
         AHI   R1,L'BLOCK3                                                      
         BCT   R0,*-12                                                          
*                                                                               
CKID52   LA    R1,BLOCK4                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT4                                                     
         BZ    CKID90                                                           
*                                                                               
         BAS   RE,CKIDAL                                                        
         BE    CKID60                                                           
         AHI   R1,L'BLOCK4                                                      
         BCT   R0,*-12                                                          
         B     CKID90                                                           
*                                                                               
CKID60   EQU   *                                                                
         MVC   FVMSGNO,=AL2(CE#IDIAG)   ID ALREADY IN A=                        
         MVC   FVXTRA(2),22(R1)                                                 
         B     CKID190                                                          
*                                                                               
CKID90   EQU   *                                                                
* CHECK IF THIS UID IS ALREADY DEFINED IN ONE OF THE L=                         
*                                                                               
*INSTEAD OF READING THE ID-LIST RECORD AND CHECK IF THIS UID IS IN IT,          
*I WILL DO THE CHECKING IN CHKLIST BECAUSE IT SAVES IO#.                        
*                                                                               
*                                                                               
* FOR DDS {#N,#E AND SJ(ANY TEST SYSTEM)}, SKIP THE FOLLOWING CHECK             
*                                                                               
*&&UK*&& CLC   =C'#E',AGENCYID                                                  
*&&US*&& CLC   =C'#N',AGENCYID                                                  
         BE    CKID100             CONTINUE CHECK VS CONNECTED PID              
*&&US                                                                           
         L     RF,ASYS                                                          
         L     RF,VSSB-SYSFACD(RF) GET FACPAK SYSTEM ID INFO FROM SSB           
         TM    SSBSYSFL-SSBD(RF),X'80'   TEST SYSTEM                            
         BNO   *+14                      NO                                     
         CLC   =C'SJ',AGENCYID                                                  
         BE    CKID100             CONTINUE CHECK VS CONNECTED PID              
*&&                                                                             
*                                                                               
         USING CT5REC,R3                                                        
         XC    CT5KEY,CT5KEY       BUILD ACCESS REC KEY                         
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,APHALF                                                  
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2   CHECK RECORD FOUND/OK                  
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)     IO ERROR                               
         B     CKID190                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)     RECORD NOT FOUND                       
         B     CKID190                                                          
*                                                                               
         LA    R1,CT5DATA                                                       
         SR    R0,R0                                                            
*                                                                               
CKID92   CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    RF,CT5KALPH                                                      
         B     CKID95                                                           
         CLI   0(R1),CTSEAELQ      TEST SECURITY AGENCY ID ELEMENT              
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CKID92                                                           
         LA    RF,CTSEAAID-CTSEAD(R1)                                           
         DROP  R3                                                               
*                                                                               
*CHECK IF THIS USERID'S SEC AGY IS SAME AS THE SIGN-ON AGENCY                   
CKID95   CLC   0(L'AGENCYID,RF),AGENCYID                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#AGICT)                                           
         B     CKID190                                                          
*                                                                               
*CHECK IF THE CONNECTED PID CAN CONNECT TO THIS USERID                          
*                                                                               
CKID100  EQU   *                                                                
         LH    RF,=Y(CONNECT-TWAD)                                              
         A     RF,ATWA                                                          
         USING CONNECT,RF                                                       
         CLI   C#UID,C#UIDALL      TEST ALL USER-IDS ARE ALLOWED                
         BE    CKIDOKX             YES - NO NEED TO CHECK ANY FURTHER           
         DROP  RF                                                               
*                                                                               
         L     R3,AIOAREA2                                                      
         USING SA0REC,R3           R3=A(PASSWORD RECORD)                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CUAALF      AGENCY                                       
         MVC   SA0KNUM,CUPASS      PID#                                         
         MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2   CHECK RECORD FOUND/OK                  
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)     IO ERROR                               
         B     CKID190                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)     RECORD NOT FOUND                       
         B     CKID190                                                          
*                                                                               
         GOTO1 VGETIDS,APPARM,(C'L',(R3)),ATIA,(C'A',VDMGR),12(R4)              
         TM    12(R1),X'01'        MATCH?                                       
         BO    CKIDOKX             YES                                          
         MVC   FVMSGNO,=AL2(CE#NCUID)                                           
         B     CKID190                                                          
         DROP  R3                                                               
*                                                                               
CKID190  MVC   FVOSYS,ASSYSE                                                    
         B     CKIDBADX                                                         
*                                                                               
CKIDOKX  SR    RC,RC               OKAY                                         
CKIDBADX LTR   RC,RC               BAD ENTRY FOUND                              
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK IF THIS USERID IS ALREADY IN A=                                         
* INPUT:  R1 = CURRENT POSITION                                                 
***********************************************************************         
CKIDAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R4,R1               SAVE THE CURRENT POSITION POINTER            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(R4)            LENGTH                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'AGY'    CHECK FOR KEYWORD 'AGY'                      
         BNE   CKIALNO                                                          
*                                                                               
         CLC   APHALF,22(R4)       SAME AGENY?                                  
         BE    CKIALYES            YES                                          
         B     CKIALNO                                                          
*                                                                               
*                                                                               
CKIALYES SR    RC,RC               YES - USERID IN A=                           
CKIALNO  LTR   RC,RC               NO  - USERID NOT IN A=                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ADD UPDATIVE USERIDS, A= AND L=                                               
* INPUT:  AIOAREA3 - PASSWORD RECORD                                            
* OUTPUT: AIOAREA3 - PASSWORD RECORD WILL BE UPDATED                            
***********************************************************************         
ADDUPID  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,BLOCK1                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT1                                                     
         BZ    AUID50                                                           
*                                                                               
AUID10   BAS   RE,ADDUPID2         ADD THIS ENTRY                               
         BNE   AUIDNOX                                                          
         AHI   R1,L'BLOCK1                                                      
         BCT   R0,AUID10                                                        
*                                                                               
AUID50   LA    R1,BLOCK2                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT2                                                     
         BZ    AUIDOKX                                                          
*                                                                               
AUID60   BAS   RE,ADDUPID2         ADD THIS ENTRY                               
         BNE   AUIDNOX                                                          
         AHI   R1,L'BLOCK2                                                      
         BCT   R0,AUID60                                                        
*                                                                               
AUIDOKX  SR    RC,RC               YES - ADDED OKAY                             
AUIDNOX  LTR   RC,RC               NO  - ERROR, CAN'T ADD                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ADD UPDATIVE USERIDS, A= AND L=                                               
* INPUT:  AIOAREA3 - PASSWORD RECORD, R1 - (SCANNER ENTRY)                      
* OUTPUT: AIOAREA3 - PASSWORD RECORD WILL BE UPDATED                            
***********************************************************************         
ADDUPID2 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   12(L'SAIDNAM,R1),SPACES  Anything actually entered?              
         BNH   AUID2OKX                 No: Just skip it                        
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R1)            LENGTH                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R1),=C'AGY'    CHECK FOR KEYWORD 'AGY'                      
         BNE   AUID220                                                          
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SAIDD,R3                                                         
         MVI   SAIDEL,SAIDELQ      BUILD THE ELEMENT                            
         MVI   SAIDLEN,X'06'                                                    
         MVC   SAID(2),=X'0001'    AGY= SUBCODE                                 
         MVC   SAID+2(8),22(R1)                                                 
         B     AUID250                                                          
         DROP  R3                                                               
*                                                                               
AUID220  EQU   *                                                                
         SR    RE,RE                                                            
         IC    RE,0(R1)            LENGTH                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R1),=C'LIST'   CHECK FOR KEYWORD 'LIST'                     
         BNE   AUID230                                                          
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SAIDD,R3                                                         
         MVI   SAIDEL,SAIDELQ      BUILD THE ELEMENT                            
         MVI   SAIDLEN,X'0C'                                                    
         XC    SAID(2),SAID                                                     
         MVC   SAID+2(8),22(R1)    LIST CODE                                    
         B     AUID250                                                          
         DROP  R3                                                               
*                                                                               
AUID230  EQU   *                                                                
*&&UK*&& CLC   12(4,R1),=C'ALL '   ALL INDICATED BY NO ELEMENT                  
*&&UK*&& BE    AUID2OKX                                                         
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SAIDD,R3                                                         
         MVI   SAIDEL,SAIDELQ      BUILD ID ELEMENT                             
         MVI   SAIDLEN,X'0C'                                                    
         MVC   SAID,12(R1)         USERID OR ALL                                
         B     AUID250                                                          
         DROP  R3                                                               
*                                                                               
AUID250  EQU   *                   ADD ID ELEMENT TO PWD REC                    
         GOTO1 VHELLO,APPARM,(C'P',CTFILE),(R2),(R3),=C'ADD=CODE'               
         CLI   12(R1),0                                                         
         BE    AUID2OKX                                                         
         MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     AUID2NOX                                                         
*                                                                               
AUID2OKX SR    RC,RC               YES - ADDED OKAY                             
AUID2NOX LTR   RC,RC               NO  - ERROR, CAN'T ADD                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ADD READ-ONLY USERIDS, A= AND L=                                              
* INPUT:  AIOAREA3 - PASSWORD RECORD                                            
* OUTPUT: AIOAREA3 - PASSWORD RECORD WILL BE UPDATED                            
***********************************************************************         
ADDROID  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,BLOCK3                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT3                                                     
         BZ    ARID50                                                           
*                                                                               
ARID10   BAS   RE,ADDROID2         ADD THIS ENTRY                               
         BNE   ARIDNOX                                                          
         AHI   R1,L'BLOCK3                                                      
         BCT   R0,ARID10                                                        
*                                                                               
ARID50   LA    R1,BLOCK4                                                        
         SR    R0,R0                                                            
         ICM   R0,1,BLKCNT4                                                     
         BZ    ARIDOKX                                                          
*                                                                               
ARID60   BAS   RE,ADDROID2         ADD THIS ENTRY                               
         BNE   ARIDNOX                                                          
         AHI   R1,L'BLOCK4                                                      
         BCT   R0,ARID60                                                        
*                                                                               
ARIDOKX  SR    RC,RC               YES - ADDED OKAY                             
ARIDNOX  LTR   RC,RC               NO  - ERROR, CAN'T ADD                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ADD READ-ONLY USERIDS, A= AND L=                                              
* INPUT:  AIOAREA3 - PASSWORD RECORD, R1 - (SCANNER ENTRY)                      
* OUTPUT: AIOAREA3 - PASSWORD RECORD WILL BE UPDATED                            
***********************************************************************         
ADDROID2 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   12(L'SAIDNAM,R1),SPACES  Anything actually entered?              
         BNH   ARID2OKX                 No: Just skip it                        
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R1)            LENGTH                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R1),=C'AGY'    CHECK FOR KEYWORD 'AGY'                      
         BNE   ARID220                                                          
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SAIDD,R3                                                         
         MVI   SAIDEL,SAIDELQ      BUILD THE ELEMENT                            
         OI    SAIDTYP,SAIDAGQ                                                  
         MVC   SAIDNAM,22(R1)                                                   
         B     ARID250                                                          
         DROP  R3                                                               
*                                                                               
ARID220  EQU   *                                                                
         SR    RE,RE                                                            
         IC    RE,0(R1)            LENGTH                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R1),=C'LIST'   CHECK FOR KEYWORD 'LIST'                     
         BNE   ARID230                                                          
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SAIDD,R3                                                         
         MVI   SAIDEL,SAIDELQ      BUILD THE ELEMENT                            
         OI    SAIDTYP,SAIDLIQ     ID LIST                                      
         MVC   SAIDNAM,22(R1)      LIST CODE                                    
         B     ARID250                                                          
         DROP  R3                                                               
*                                                                               
ARID230  EQU   *                                                                
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SAIDD,R3                                                         
         MVI   SAIDEL,SAIDELQ      BUILD ID ELEMENT                             
         MVC   SAIDNAM,12(R1)      USERID OR ALL                                
         B     ARID250                                                          
         DROP  R3                                                               
*                                                                               
ARID250  EQU   *                   ADD ID ELEMENT TO PWD REC                    
         LA    R3,APELEM                                                        
         USING SAIDD,R3                                                         
         OI    SAIDTYP,SAIDROQ     MARK IT AS READ ONLY                         
         LA    RE,SAIDNAM+L'SAIDNAM-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         SR    RE,R3                                                            
         AHI   RE,1                                                             
         STC   RE,SAIDLEN                                                       
         DROP  R3                                                               
*                                                                               
         GOTO1 VHELLO,APPARM,(C'P',CTFILE),(R2),(R3),=C'ADD=CODE'               
         CLI   12(R1),0                                                         
         BE    ARID2OKX                                                         
         MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     ARID2NOX                                                         
*                                                                               
ARID2OKX SR    RC,RC               YES - ADDED OKAY                             
ARID2NOX LTR   RC,RC               NO  - ERROR, CAN'T ADD                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHANGE PASSWORD HISTORY ELEMENTS                                              
* R1=A(NEW PASSWORD)                                                            
***********************************************************************         
         USING SA0REC,R2                                                        
CHGPWD   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOAREA3                                                      
*                                                                               
         MVC   APWORK(10),0(R1)                                                 
*                                                                               
         CLI   APACTN,ACTCPY                                                    
         BNE   CHGPWD10                                                         
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),('SAPWHELQ',SA0REC),0,0              
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
CHGPWD10 GOTOR BLDPWD,APWORK                                                    
*                                                                               
         LA    R3,APELEM           NEED TO MAKE SURE NEW ELEMENT                
         USING SAPWHD,R3           IS LAST BECAUSE FLAGS ARE STORED             
         MVC   SAPWHDTE,=X'FFFF'   BETWEEN DATE AND TIME                        
*                                                                               
         GOTO1 VHELLO,APPARM,(C'P',CTFILE),('SAPWHELQ',SA0REC),APELEM,0         
         CLI   APPARM+12,0                                                      
         JNE   *+2                                                              
         L     R3,APPARM+16                                                     
         MVC   SAPWHDTE,TODAY      CORRECT TO TODAY'S DATE                      
*                                                                               
CPWDOK   SR    RC,RC               RETURN CC EQUAL                              
CPWDNO   LTR   RC,RC               RETURN CC NOT EQUAL                          
         J     EXIT                                                             
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD PASSWORD HISTORY ELEMENT IN APELEM                                      
* R1=A(PASSWORD)                                                                
***********************************************************************         
BLDPWD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RE,R1                                                            
         LR    R2,R1                                                            
         SR    R4,R4                                                            
         LA    R0,10                                                            
BPWD010  CLI   0(RE),C' '                                                       
         BE    BPWD020                                                          
         CLI   0(RE),0                                                          
         BE    BPWD020                                                          
         LA    R4,1(R4)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BPWD010                                                       
BPWD020  EQU   *                                                                
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SAPWHD,R3                                                        
         MVI   SAPWHEL,SAPWHELQ    BUILD PERSON POINTER ELEMENT                 
         MVI   SAPWHLN,SAPWHLNQ                                                 
         MVC   SAPWHDTE,TODAY      TODAY'S DATE                                 
         OI    SAPWHFLG,SAPWHEXP   FORCE IMMEDIATE EXPIRE                       
         TIME  BIN                                                              
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  R0,7,SAPWHTME                                                    
         LR    RE,R4                                                            
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SAPWHPWD(0),0(R2)                                                
         SR    RF,RF                                                            
         IC    RF,SAPWHLN                                                       
         AR    RF,RE                                                            
         STC   RF,SAPWHLN                                                       
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DETERMINE DATA ACCESS SETUP (CALLED FROM PREGRID)                  
***********************************************************************         
         USING GVALSD,R4                                                        
DISDHD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SELSYS,X'07'        TALENT SYSTEM                                
         BE    DSDOK                                                            
         CLI   SELSYS,X'08'        REP SYSTEM                                   
         BE    DSDOK                                                            
         CLI   SELSYS,X'0A'        CONTROL SYSTEM                               
         BE    DSDOK                                                            
         CLI   SELSYS,X'0C'        DEMO SYSTEM                                  
         BE    DSDOK                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         USING SALAREC,R2                                                       
         XC    SALAKEY,SALAKEY     CHECK DATA GROUP RECORD EXISTS               
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         MVC   SALAAGY,CUAALF                                                   
         MVC   SALAAGN,DATAGRP                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   DSDNO                                                            
         L     R2,AIOAREA2                                                      
*                                                                               
         MVI   APBYTE,C' '                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SALASELQ     FIND SYSTEM ELEMENTS                         
         GOTO1 AGETELS,SALAREC                                                  
         ICM   R3,15,APPARM        APPARM =A(ELEMENT) IF FOUND                  
         BZ    DSD060                                                           
*                                                                               
         USING SALASD,R3                                                        
DSD030   CLI   0(R3),0                                                          
         BE    DSD060                                                           
         CLI   0(R3),SALASELQ                                                   
         BNE   DSD060                                                           
         CLC   SALASNUM,SELSYS                                                  
         BE    DSD035                                                           
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSD030                                                           
*                                                                               
DSD035   TM    SALASIND,SALASIDF   DEFERRING TO COMPANY ID?                     
         BZ    DSD038              . NO, LOOK FOR DATA                          
         MVC   GVLACC(35),=CL35'ACCESS DEFERRED TO COMPANY ID'                  
         B     DSDOK                                                            
*                                                                               
DSD038   LA    R3,APELEM                                                        
         USING LISTIOD,R3                                                       
         XC    LISTIOB(LISTIOBL),LISTIOB                                        
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,DATAGRP                                                 
         MVC   LISTISYS,SELSYS                                                  
DSD040   MVI   LISTACTN,LISTANLG         NEXT LIST FOR THIS GROUP               
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSD060                                                           
         CLC   LISTISYS,SELSYS                                                  
         BNE   DSD060                                                           
*                                                                               
         MVI   LISTACTN,LISTATYP         GET LIST TYPE                          
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSD040                                                           
         MVC   APWORK(L'LISTITNM),LISTITNM                                      
*                                                                               
         XC    LISTDVAL,LISTDVAL         FIRST ITEM IN LIST                     
         MVI   LISTACTN,LISTANXT         ANY DATA IN LIST                       
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSD040                                                           
         MVI   APBYTE,C'D'                                                      
         MVC   GVLACC(12),=CL12'DATA TYPE :'                                    
         MVC   GVLACC+12(L'SALANCOD),APWORK                                     
         B     DSD040                                                           
         DROP  R3                                                               
*                                                                               
DSD060   CLI   APBYTE,C'D'                                                      
         BE    DSDOK                                                            
         MVC   GVLACC(12),=CL12'NO ACCESS'                                      
*                                                                               
DSDOK    SR    RC,RC                                                            
DSDNO    LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*        DDLINK field table for GENERAL controller.                             
*                                                                               
DDLFTAB  DS    0D                  DDLINK FIELD TABLE.                          
*                                  SEE DDLFTD IN GEGENWRK                       
* KEY FIELDS                                                                    
 DC AL2(0701,PERPIDH-TWAD),AL1(L'PERPID,DDLFTIKQ)                               
 DC AL2(0702,DDLKPIN-WORKD),AL1(L'DDLKPIN,DDLFTIKQ+DDLFTIWQ)                    
 DC AL2(0703,PERDEFH-TWAD),AL1(L'PERDEF,DDLFTIKQ)                               
 DC AL2(0705,DDLKAEP-WORKD),AL1(L'DDLKAEP,DDLFTIKQ+DDLFTIWQ)                    
 DC AL2(0713,DDLKPWD-WORKD),AL1(L'DDLKPWD,DDLFTIKQ+DDLFTIWQ)                    
* DATA FIELDS                                                                   
 DC AL2(0711,PERPIDNH-TWAD),AL1(L'PERPIDN,0)                                    
 DC AL2(0717,PEROVFH-TWAD),AL1(L'PEROVF,0)                                      
 DC AL2(0719,PERPCNH-TWAD),AL1(L'PERPCN,0)                                      
 DC AL2(0721,PEROFFH-TWAD),AL1(L'PEROFF,0)                                      
 DC AL2(0723,PERDIDH-TWAD),AL1(L'PERDID,0)                                      
 DC AL2(0725,PERAGRH-TWAD),AL1(L'PERAGR,0)                                      
 DC AL2(0727,PERAPCH-TWAD),AL1(L'PERAPC,0)                                      
*&&US*&& DC AL2(0729,PERLAGH-TWAD),AL1(L'PERLAG,0)                              
 DC AL2(0731,PERUSR1H-TWAD),AL1(L'PERUSR1,0)                                    
 DC AL2(0731,PERUSR2H-TWAD),AL1(L'PERUSR2,0)                                    
 DC AL2(0733,PERURR1H-TWAD),AL1(L'PERURR1,0)                                    
 DC AL2(0733,PERURR2H-TWAD),AL1(L'PERURR2,0)                                    
 DC AL2(0741,PERDHIH-TWAD),AL1(L'PERDHI,0)                                      
 DC AL2(0743,PERDTEH-TWAD),AL1(L'PERDTE,0)                                      
 DC AL2(0751,PERSTAH-TWAD),AL1(L'PERSTA,0)                                      
 DC AL2(0753,PERTITH-TWAD),AL1(L'PERTIT,0)                                      
 DC AL2(0755,PERFNAMH-TWAD),AL1(L'PERFNAM,0)                                    
 DC AL2(0757,PERMNAMH-TWAD),AL1(L'PERMNAM,0)                                    
 DC AL2(0759,PERLNAMH-TWAD),AL1(L'PERLNAM,0)                                    
 DC AL2(0761,PEREXTH-TWAD),AL1(L'PEREXT,0)                                      
*&&UK*&& DC AL2(0763,PERHTELH-TWAD),AL1(L'PERHTEL,0)                            
*&&US*&& DC AL2(0763,PERTELEH-TWAD),AL1(L'PERTELE,0)                            
*&&UK*&& DC AL2(0765,PERINCH-TWAD),AL1(L'PERINC,0)                              
 DC AL2(0767,PERPROH-TWAD),AL1(L'PERPRO,0)                                      
 DC AL2(0771,PERADR1H-TWAD),AL1(L'PERADR1,0)                                    
 DC AL2(0773,PERADR2H-TWAD),AL1(L'PERADR2,0)                                    
 DC AL2(0777,PERCITYH-TWAD),AL1(L'PERCITY,0)                                    
*&&US*&& DC AL2(0779,PERSTEH-TWAD),AL1(L'PERSTE,0)                              
 DC AL2(0781,PERCODEH-TWAD),AL1(L'PERCODE,0)                                    
 DC AL2(0783,PERCTRYH-TWAD),AL1(L'PERCTRY,0)                                    
* DATA FIELDS, OUTPUT ONLY                                                      
 DC AL2(0722,PEROFFNH-TWAD),AL1(L'PEROFFN,DDLFTIOQ)                             
 DC AL2(0724,PERDIDNH-TWAD),AL1(L'PERDIDN,DDLFTIOQ)                             
 DC AL2(0726,PERAGRNH-TWAD),AL1(L'PERAGRN,DDLFTIOQ)                             
 DC AL2(0794,DDLKAGR#-WORKD),AL1(L'DDLKAGR#,DDLFTIOQ+DDLFTIWQ)                  
 DC AL2(0728,PERAPCNH-TWAD),AL1(L'PERAPCN,DDLFTIOQ)                             
*&&US*&& DC AL2(0730,PERLAGNH-TWAD),AL1(L'PERLAGN,DDLFTIOQ)                     
 DC AL2(0790,DDLKACDT-WORKD),AL1(L'DDLKACDT,DDLFTIOQ+DDLFTIWQ)                  
 DC AL2(0791,DDLKACTM-WORKD),AL1(L'DDLKACTM,DDLFTIOQ+DDLFTIWQ)                  
 DC AL2(0792,PERLLOH-TWAD),AL1(L'PERLLO,DDLFTIOQ)                               
*                                                                               
 DC AL2(0000) EOT                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ACCESS EQUIVALENT PID (AEP)                     *         
* DDLKAEP CONTAINS REQUIRED PID. ROUTINE READS IT INTO AIOARE2 AND    *         
* COPIES ITS PIN INTO DDLKAEPN FOR USE BY THE COPYAEP ROUTINE.        *         
* EXIT CC EQ IF FOUND, CC NEQ IF NOT FOUND OR NO PIN IN RECORD        *         
***********************************************************************         
                                                                                
VALAEP   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VALAEP,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+VAE'    INSERT NAME                                  
*                                                                               
**NOP    XC    DDLKAEPN,DDLKAEPN   CLEAR PIN                                    
*                                                                               
         MVC   APWORK(L'SAPEKEY),IOKEY SAVE IOKEY                               
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          R2=A(RECORD KEY)                             
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,DDLKAEP                                                  
         GOTO1 AIO,IOHI+IOCONFIL+IO2                                            
         BNE   VALAEPNO            RECORD NOT FOUND                             
         L     R2,AIOAREA2                                                      
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY SAME PERSON?                      
         BNE   VALAEPNO            NO                                           
*                                                                               
         LA    R3,SAPEDATA         LOOK FOR PIN                                 
         SR    RF,RF                                                            
VALAEP10 CLI   0(R3),0             TEST E-O-R                                   
         BE    VALAEPNO            NO PIN FOUND                                 
         CLI   0(R3),SAPWDELQ      PASSWORD POINTER ELEMENT                     
         BE    VALAEP20                                                         
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VALAEP10                                                         
*                                                                               
         USING SAPWDD,R3                                                        
VALAEP20 MVC   DDLKAEPN,SAPWDNUM   SAVE PIN                                     
         DROP  R2,R3                                                            
*                                                                               
VALAEPOK SR    RC,RC                                                            
VALAEPNO LTR   RC,RC                                                            
         MVC   IOKEY(L'SAPEKEY),APWORK RESTORE IOKEY                            
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ACTION ACCESS EQUIVALENT PID (AEP)                       *         
* DDLKAEPN CONTAINS THE PIN OF THE REQUIRED PID AS SAVED BY VALAEP.   *         
* ROUTINE FIRST READS THE AEP SA0 RECORD INTO IOAREA2.                *         
* IT THEN DELETES RELEVANT ELEMENTS FROM OUR SA0 RECORD IN IOAREA3.   *         
* FINALLY IT COPIES RELEVANT ELEMENTS TO THE SA0 RECORD IN IOAREA3    *         
* FROM THE ACCESS EQUIVALENT PID'S SA0 RECORD IN IOAREA2.             *         
* THE LIST OF RELEVANT ELEMENTS IS IN FIELD COPAEPLS.                 *         
***********************************************************************         
                                                                                
COPYAEP  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING COPYAEP,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+CAE'    INSERT NAME                                  
*                                                                               
         MVC   APWORK(L'SA0KEY),IOKEY SAVE IOKEY                                
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           R2=A(RECORD KEY)                             
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,DDLKAEPN                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO2 AEP SA0 REC INTO IOAREA2 (NOT 3!)          
         BNE   COPAEPNO            RECORD NOT FOUND                             
                                                                                
         LA    R4,COPAEPLS         POINT TO RELEVANT ELEMENT CODES              
         TM    AEPUIOV,X'80'                                                    
         BZ    *+8                                                              
         LA    R4,COPAEPL2         USE THIS IF USER IDS OVERRIDEN               
                                                                                
*        DELETE ALL RELEVANT ELEMENTS FROM CURRENT RECORD IN IOAREA3            
*                                                                               
         L     R2,AIOAREA3         POINT TO CURRENT SYSTEM RECORD               
         LA    R3,SA0DATA                                                       
COPAEP10 CLI   0(R3),0             TEST E-O-R                                   
         BE    COPAEP18            GO DELETE OLD ELEMENTS                       
         LR    RE,R4               POINT TO RELEVANT ELEMENT CODES              
COPAEP12 CLC   0(1,R3),0(RE)       IS THIS ELEMENT RELEVANT?                    
         BNE   COPAEP14                                                         
         MVI   0(R3),X'FF'         YES, MARK IT FOR DELETION                    
         B     COPAEP16                                                         
COPAEP14 LA    RE,1(,RE)           NEXT RELEVANT ELEMENT CODE                   
         CLI   0(RE),0             END OF LIST                                  
         BNE   COPAEP12            NO                                           
COPAEP16 LLC   RF,1(R3)            NEXT ELEMENT                                 
         AR    R3,RF                                                            
         B     COPAEP10                                                         
COPAEP18 GOTO1 VHELLO,APPARM,(C'D',CTFILE),(X'FF',AIOAREA3),0,0                 
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
*        COPY ALL RELEVANT ELEMENTS FROM AEP SYSTEM RECORD WE JUST READ         
*        INTO IOAREA2 INTO CURRENT SYSTEM RECORD IN IOAREA3                     
*                                                                               
         L     R2,AIOAREA2         POINT TO AEP SYSTEM RECORD JUST READ         
         LA    R3,SA0DATA                                                       
COPAEP20 CLI   0(R3),0             TEST E-O-R                                   
         BE    COPAEPOK            YES, ALL DONE                                
         LR    RE,R4               POINT TO RELEVANT ELEMENT CODES              
COPAEP22 CLC   0(1,R3),0(RE)       IS THIS ELEMENT RELEVANT?                    
         BE    COPAEP24                                                         
         LA    RE,1(,RE)           NEXT RELEVANT ELEMENT CODE                   
         CLI   0(RE),0             END OF LIST                                  
         BNE   COPAEP22            NO                                           
         B     COPAEP26                                                         
*                                  YES, COPY TO CURRENT SYSTEM RECORD           
COPAEP24 GOTO1 VHELLO,APPARM,(C'P',CTFILE),AIOAREA3,(R3),=C'ADD=CODE'           
         CLI   APPARM+12,0                                                      
         JNE   *+2                                                              
COPAEP26 LLC   RF,1(R3)            NEXT ELEMENT TOP COPY                        
         AR    R3,RF                                                            
         B     COPAEP20                                                         
*                                                                               
COPAEPOK SR    RC,RC                                                            
COPAEPNO LTR   RC,RC                                                            
         MVC   IOKEY(L'SAPEKEY),APWORK RESTORE IOKEY                            
         XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
*        LIST OF ELEMENT CODES TO COPY FROM AEP SA0 RECORD                      
*                                                                               
COPAEPLS DC    AL1(SAIDELQ,SASYSELQ,SACLAELQ,0)                                 
COPAEPL2 DC    AL1(SASYSELQ,SACLAELQ,0) IF USER IDS OVERRIDEN                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOAD PERSON RECORDS EFFECTIVE DATES INTO SAVE TABLE      *         
* FOR THE PERSONAL ID DEFINED IN APRECKEY                             *         
* CURRENT EFFECTIVE DATE KEY (COMPLEMENT) IN OLDDEFC                  *         
***********************************************************************         
         USING SAPEREC,R2                                                       
LOADDEFT CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING LOADDEFT,RB                                                      
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+LDE'    INSERT NAME                                  
*                                                                               
         MVI   PDEFXFLG,C'N'                                                    
         XC    APHALF,APHALF       USE TO HOLD EFFECTIVE DATE KEY               
         LA    R4,PDEFTAB          SAVE TABLE POINTER                           
LDT010   LA    R2,IOKEY                                                         
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY     READ NEXT PERSON RECORD                      
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,APRECKEY+(SAPEPID-SAPEKEY)                               
         MVC   SAPEDEF,APHALF                                                   
         GOTO1 AIO,IOHI+IOCONFIL+IO2                                            
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   LDTX                EXIT IF NOT FOUND                            
         L     R2,AIOAREA2                                                      
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY                                   
         BNE   LDTX                EXIT IF NOT CURRENT PERSONAL ID              
         CLC   SAPEDEF,OLDDEFC                                                  
         BNE   LDT012              NOT CURRENT RECORD DATE                      
         LA    RF,PDEFTAB                                                       
         LR    RE,R4                                                            
         SR    RE,RF                                                            
         ST    RE,OCURPDEF         SAVE OFFSET TO CURRENT RECORD                
         ST    RE,ODISPDEF           WHICH IS ALSO DISPLAY RECORD               
LDT012   MVC   APHALF,SAPEDEF      SAVE LAST EFFECTIVE DATE KEY                 
         MVC   0(L'SAPEDEF,R4),SAPEDEF  SAVE DATE IN TABLE                      
         LA    R1,SAPEDATA                                                      
*                                  GET PASSWORD POINTER ELEMENT                 
LDT020   CLI   0(R1),0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R1),SAPWDELQ                                                   
         BE    LDT022                                                           
         SR    R0,R0                                                            
         IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     LDT020                                                           
         USING SAPWDD,R1                                                        
*                                  SAVE PASSWORD CODE IN TABLE                  
LDT022   MVC   L'SAPEDEF(L'SAPWDCOD,R4),SAPWDCOD                                
         SR    RE,RE               UPDATE EFFECTIVE DATE KEY                    
         ICM   RE,3,APHALF                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,APHALF                                                      
         LA    R4,PDEFTLEN(R4)     BUMP SAVE TABLE POINTER                      
         LA    RF,PDEFTEND         CHECK OVERFLOW                               
         CR    R4,RF                                                            
         BL    LDT010              READ NEXT RECORD                             
         MVI   PDEFXFLG,C'Y'                                                    
         B     LDT010                                                           
*                                  LAST RECORD READ                             
LDTX     LA    RF,PDEFTAB                                                       
         LA    RF,PDEFTLEN(RF)                                                  
         SR    R4,RF                                                            
         ST    R4,OFRSTDEF         SAVE OFFSET TO FIRST ENTRY IN TABLE          
         B     LDTOK                                                            
*                                  EXIT EROR TOO MANY HISTORICAL RECS.          
LDTERR   EQU   *                                                                
         MVC   FVMSGNO,=AL2(CE#HPERR)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     LDTNO                                                            
         DROP  R1                                                               
*                                                                               
LDTOK    SR    RC,RC                                                            
LDTNO    LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CHECK PASSIVE PERSON RECORDS IN =COST                    *         
*         BY READING THE CT5 RECORDS AND MAKING A CALL TO SEC1RP.     *         
***********************************************************************         
CHKPER   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING CHKPER,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+CHK'    INSERT NAME                                  
*                                                                               
         LA    R1,BLOCK2                                                        
         MVI   0(R1),X'FF'         INITIALIZE BLOCK TO START WITH               
*                                                                               
* GET ID NUMBER FROM CT5RECS - BUILD TABLE OF ID #'S IN BLOCK2                  
*                                                                               
         USING CT5REC,R2           R2=A(ID RECORD)                              
         LA    R2,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    C'5' - SYSTEM ACCESS RECORDS                 
         MVC   CT5KALPH,APHALF     CURRENT IS DEFAULT                           
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   CHKP40              NO REC FOUND READ THE REST                   
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R3,CT5DATA          POINT TO FIRST ELEM                          
CHKP10   CLI   0(R3),X'00'         END OF RECORD                                
         BE    CHKP40              READ SEQ                                     
         CLI   0(R3),X'21'         SYSTEM AUTH ELEMENT                          
         BE    CHKP30                                                           
CHKP20   SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CHKP10                                                           
*                                                                               
         USING CTSYSD,R3                                                        
CHKP30   CLI   CTSYSNUM,X'06'      CHECK IF ACC SYSTEMS                         
         BNE   CHKP35                                                           
*                                                                               
         USING SECATBD,R1                                                       
         LA    R1,BLOCK2                                                        
         MVC   SECASEID,CTSYSSE    SAVE OFF SYSTEM SE#/BIN ID                   
         MVC   SVLIDSE,CTSYSSE     SAVE OFF CURRENT ACCT SE#                    
         LA    R1,SECALNQ(R1)                                                   
         MVI   0(R1),X'FF'         SET EOF MARKER IN TABLE                      
         DROP  R1                                                               
         B     CHKP20                                                           
*                                                                               
CHKP35   CLI   CTSYSNUM,X'0A'      CHECK IF CONTROL                             
         BNE   CHKP20                                                           
         MVC   SVCNTLSE,CTSYSSE    SAVE OFF CURRENT SE#                         
         B     CHKP20                                                           
         DROP  R3                                                               
*                                                                               
         USING CT5REC,R2           R2=A(ID RECORD)                              
CHKP40   LA    R2,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    C'5' - SYSTEM ACCESS RECORDS                 
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         B     CHKP60                                                           
*                                                                               
CHKP50   GOTO1 AIO,IOSQ+IOCONFIL+IO2                                            
CHKP60   L     R2,AIOAREA2                                                      
         CLI   CT5KTYP,CT5KTYPQ    MAKE SURE WE ARE STILL DOING                 
         BNE   CHKP130                  ACCESS RECORDS                          
*                                                                               
         XC    SVSEBID,SVSEBID     CLEAR SE NUM/ID FIELD                        
         LA    R3,CT5DATA          POINT TO FIRST ELEM                          
CHKP70   CLI   0(R3),X'00'         END OF RECORD                                
         BE    CHKP50              READ SEQ                                     
         CLI   0(R3),X'21'         SYSTEM AUTH ELEMENT                          
         BE    CHKP90                                                           
         CLI   0(R3),X'B8'         SECURITY ALPHA ID ELM                        
         BE    CHKP100                                                          
CHKP80   SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CHKP70                                                           
*                                                                               
         USING CTSYSD,R3                                                        
CHKP90   CLI   CTSYSNUM,X'06'      ONLY WANT ACC SYSTEMS                        
         BNE   CHKP80                                                           
         MVC   SVSEBID,CTSYSSE     SAVE OFF SYSTEM SE#/BIN ID                   
         B     CHKP80                                                           
*                                                                               
         USING CTSEAD,R3                                                        
CHKP100  CLC   CTSEAAID,APHALF     COMPARE ON SECURITY ID                       
         BNE   CHKP50              READ SEQ                                     
         DROP  R3                                                               
*                                                                               
         OC    SVSEBID,SVSEBID     ANY BINARY ID                                
         BZ    CHKP50              NO READ SEQ                                  
*                                                                               
         USING SECATBD,R1                                                       
         LA    R1,BLOCK2                                                        
         LA    R0,BLKSECQ-1        LIMIT BINARY IDS TO FIT BLK                  
CHKP110  CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    CHKP120                                                          
         CLC   SVBINID,SECABID     DON'T ADD IF IN TABLE ALREADY                
         BE    CHKP50                                                           
         LA    R1,SECALNQ(R1)                                                   
         BCT   R0,CHKP110                                                       
         DC    H'0'                TOO MANY ID'S                                
*                                                                               
CHKP120  MVC   SECASEID,SVSEBID                                                 
         LA    R1,SECALNQ(R1)                                                   
         MVI   0(R1),X'FF'         SET EOF MARKER IN TABLE                      
         B     CHKP50                                                           
         DROP  R1                                                               
*                                                                               
* READ TABLE IN BLOCK2 AND CALL SEC1RP WITH EACH SE/ID BUT FIRST MAKE           
*      CALL TO SEC1RP TO DELETE SWITCH LIST SO THAT IT WILL NOT DIE             
*      IN FASWITCH IF THE AGENCY'S SWITCH LIST IS FULL ON ENTRY                 
*                                                                               
CHKP130  LA    R4,BLOCK2           ANYTHING IN TABLE?                           
         CLI   0(R4),X'FF'         NO - SKIP IT                                 
         BE    CHKPOK                                                           
         GOTO1 =V(SEC1RP),APPARM,(R7),(3,0),RR=APRELO   DEL SWTCH LIST          
         CLI   4(R1),0             WERE WE ABLE TO DELETE THE LIST?             
         BNE   CHKPNO                                                           
         LA    R4,BLOCK2           ANYTHING IN TABLE?                           
CHKP140  CLI   0(R4),X'FF'                                                      
         BE    CHKPOK                                                           
         MVC   SVSEBID,0(R4)       CHECK TIME AGAINST BINARY ID                 
*                                                                               
         GOTO1 =V(SEC1RP),APPARM,(R7),(2,OLDPWD),SVSEBLK,RR=APRELO              
         CLI   4(R1),0                                                          
         BE    CHKP142             NO TIME EXISTS - OK TO DELETE                
         LA    RF,PERPIDH                                                       
         ST    RF,FVADDR                                                        
         MVC   FVMSGNO,=AL2(CE#PDINV)                                           
         MVC   FVOSYS,ASSYSE                                                    
         CLI   4(R1),1                                                          
         BE    CHKPNO              NOT ABLE TO SWITCH?                          
         MVC   FVMSGNO,=AL2(CE#PIDTM)                                           
         B     CHKPNO                                                           
*                                                                               
CHKP142  LA    R4,SECALNQ(R4)                                                   
         B     CHKP140                                                          
*                                                                               
CHKPOK   SR    RC,RC                                                            
CHKPNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* LITERAL                                                             *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO FIND IF USER ID IN APWORK IS COMPATIBLE WITH RECORD      *         
* R2 = A(RECORD)                                                      *         
***********************************************************************         
FILTUSER CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING FILTUSER,RB                                                      
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+FIL'    INSERT NAME                                  
         GOTO1 VGETIDS,APPARM,(C'C',(R2)),ATIA,VDMGR                            
         TM    4(R1),X'01'                                                      
         BZ    *+6                  GETID PARAMETER ERROR                       
         DC    H'00'                                                            
         CLI   8(R1),0                                                          
         BNE   *+12                                                             
         CLI   0(R1),0                                                          
         BE    FUSEROK             NULL TABLE SO CAN ACCESS ANY                 
         L     R1,4(R1)                                                         
FUSER010 CLI   0(R1),X'FF'         TEST E-O-L                                   
         BE    FUSERNO                                                          
         CLC   CT@ALL,0(R1)                                                     
         BNE   FUSER020                                                         
         CLC   FUSESPCS(10-L'CT@ALL),L'CT@ALL(R1)                               
         BNE   FUSER020                                                         
         TM    CUSTAT,CUSDDS       ONLY VALID FOR DDS TERMINALS                 
         BZ    FUSER030                                                         
         B     FUSEROK                                                          
FUSER020 CLC   0(10,R1),APWORK     MATCH ID WITH TABLE                          
         BE    FUSEROK                                                          
FUSER030 LA    R1,12(R1)           GET NEXT TABLE ENETRY                        
         B     FUSER010                                                         
FUSEROK  SR    RC,RC                                                            
FUSERNO  LTR   RC,RC                                                            
         XIT1                                                                   
FUSESPCS DC    CL10' '                                                          
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         USING SAPEREC,R2                                                       
GETSELA  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETSELA,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GSL'    INSERT NAME                                  
GETSEL   LA    R2,IOKEY            READ NEXT RECORD                             
         MVC   SAPEKEY,APRECKEY      FROM LAST KEY                              
         TM    GETSEQF,APILRERD    TEST GETSEL SEQUENCE BROKEN                  
         BZ    GS002                                                            
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     GS004                                                            
GS002    TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    GS010                                                            
         NI    APINDS,X'FF'-APILRERD                                            
GS004    GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GS020                                                            
         B     GSEND                                                            
GS010    TM    APINDS,APILNSEQ     TEST FIRST LINE IN LIST SEQUENCE             
         BNZ   GS020                                                            
         OI    APINDS,APILNSEQ                                                  
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOCONFIL+IOSQ+IO1  ELSE NEXT LIST LINE                        
         GOTO1 AIO                                                              
         BNE   GSEND                                                            
*                                                                               
         GOTO1 VGETFACT,APPARM,0   GET A(SYSTEM INFO BLOCK)                     
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MH    RF,=H'9'                                                         
         D     RE,=F'10'           90 PERCENT OF MAX IOS IN RF                  
         CLM   RF,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BH    GS021                                                            
         DROP  R1                                                               
         MVC   FVMSGNO,=AL2(GI$IOLIM)                                           
         MVI   FVOSYS,GTGENSYS     GENERAL MESSAGE                              
         MVI   FVOMTYP,C'I'                                                     
         OI    ACLSMIND,ACLSMIMP   END OF LIST MESSAGE PROVIDED                 
         L     R1,ALSM                                                          
         USING LSMD,R1                                                          
         OI    LSMINDS,LSMIEOS     END OF SCREEN                                
         DROP  R1                                                               
         B     GETSELX                                                          
*                                                                               
GS021    L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY  SAVE LAST KEY READ                  
         CLI   SAPETYP,SAPETYPQ    VALIDATE KEY                                 
         BNE   GSEND               EXIT IF NOT PERSON RECORD TYPE               
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   GSEND                                                            
         CLC   SAPEAGY,AGENCYID                                                 
         BNE   GSEND                                                            
         CLI   SELDEFF,0           FILTER ON EFFECTIVE DATE IN KEY              
         BE    GS022                                                            
         CLC   SAPEDEF,SELDEFC                                                  
         BL    GS020                                                            
         B     GS026                                                            
*                                                                               
GS022    OC    SELOPI,SELOPI       UNLESS OLD PERSON ID FILTER SET              
         BNZ   GS026                                                            
         CLC   SAPEDEF,TODAYC      CHECK CURRENT EFFECTIVE DATE                 
         BL    GS020                                                            
         CLC   SAPEPID,SAVPID                                                   
         BE    GS020               GET NEXT PERSON ID                           
         MVC   SAVPID,SAPEPID                                                   
         MVI   CURRPID,C'Y'                                                     
         MVI   EXPFLAG,C'N'        INITIALISE EXPIRED FLAG                      
         B     GSPID                                                            
*                                                                               
GS026    MVI   CURRPID,C'N'        SET CURRENT PERSONAL ID FLAG                 
         CLC   SAPEDEF,TODAYC      CHECK CURRENT EFFECTIVE DATE                 
         BL    GSPID                                                            
         CLC   SAPEPID,SAVPID                                                   
         BE    GSPID                                                            
         MVC   SAVPID,SAPEPID                                                   
         MVI   CURRPID,C'Y'                                                     
         MVI   EXPFLAG,C'N'        INITIALISE EXPIRED FLAG                      
         B     GSPID                                                            
*                                  FILTER ON PERSONAL ID                        
GSPID    CLI   SELPIDSP,C' '       OFFICE CODE - FILTER ONLY IF IT              
         BNH   GSPIDX                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GSPID1                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAPEPID(0),SELPID                                                
         BH    GSEND               (NO MORE RELEVENT RECORDS)                   
GSPID1   GOTO1 ATXTFLT,APPARM,(SELPIDL,SELPID),(L'SAPEPID,SAPEPID)              
         BNE   GETSEL              READ NEXT RECORD                             
GSPIDX   EQU   *                                                                
*                                  SAVE LAST RECORD KEY                         
GS030    LA    R3,SAPEDATA         FILTER ON DATA IN ELEMENTS                   
         MVI   SELAGRF,0                                                        
         MVI   SELAPCF,0                                                        
         MVI   SELPWDF,0                                                        
         MVI   SELOPIF,0                                                        
*                                                                               
GS032    CLI   0(R3),0             E-O-R                                        
         BE    GS036                                                            
         CLI   0(R3),SANAMELQ                                                   
         BE    GSNAME                                                           
         CLI   0(R3),SAPERELQ                                                   
         BE    GSPER                                                            
         CLI   0(R3),SAAGCELQ                                                   
         BE    GSAGC                                                            
         CLI   0(R3),SAAPCELQ                                                   
         BE    GSAPC                                                            
         CLI   0(R3),SAPWDELQ                                                   
         BE    GSPWD                                                            
         CLI   0(R3),SAOPIELQ                                                   
         BE    GSOPI                                                            
*                                                                               
GS034    SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GS032                                                            
*                                                                               
         USING SANAMD,R3                                                        
GSNAME   OC    SELNAM,SELNAM       FILTER ON PERSON SURNAME                     
         BZ    GS034                                                            
         CLI   SANAMLN,SANAMLNQ                                                 
         BL    GETSEL                                                           
         LA    RE,SANAMELN                                                      
         SR    R1,R1                                                            
         TM    SANAMIND,SANAMIFN                                                
         BZ    GSNM                                                             
         IC    R1,0(RE)                                                         
         LA    RE,1(R1,RE)                                                      
*                                                                               
GSNM     TM    SANAMIND,SANAMIMN                                                
         BZ    GSNL                                                             
         IC    R1,0(RE)                                                         
         LA    RE,1(R1,RE)                                                      
*                                                                               
GSNL     TM    SANAMIND,SANAMILN                                                
         BZ    GETSEL                                                           
         IC    R1,SELNAML                                                       
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         CR    R1,RF                                                            
         BH    GETSEL                                                           
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'APWORK-1),APWORK                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    APWORK(0),1(RE)                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELNAM(0),APWORK                                                 
         BNE   GETSEL                                                           
         B     GS034                                                            
*                                                                               
         USING SAPERD,R3                                                        
GSPER    EQU   *                   FILTER ON PERSONNEL DETAILS                  
         CLI   CURRPID,C'Y'        TEST IF CURRENT PID                          
         BNE   GSPE010                                                          
         MVI   EXPFLAG,C'N'        SET EXPIRED FLAG                             
         OC    SAPERDTE,SAPERDTE                                                
         BZ    GSPE010                                                          
         CLC   SAPERDTE,TODAY                                                   
         BNL   GSPE010                                                          
         MVI   EXPFLAG,C'Y'                                                     
*                                                                               
GSPE010  CLI   OPTTER,C'Y'         OPTION TERM=Y                                
         BE    GSPE100                                                          
         CLI   OPTTER,C'O'         OPTION TERM=O                                
         BE    GSPE020                                                          
*                                  DEFAULT TERM=N                               
         CLI   EXPFLAG,C'N'                                                     
         BE    GSPE100                                                          
         B     GETSEL                                                           
*                                                                               
GSPE020  CLI   EXPFLAG,C'Y'                                                     
         BE    GSPE100                                                          
         B     GETSEL                                                           
*                                  TEST OFFICE MANAGER ACCESS                   
GSPE100  EQU   *                                                                
*&&US                                                                           
*                                  TEST OFFICE/DEPT MANAGER ACCESS              
         GOTO1 ATSTDMAN,APPARM,SAPEROFF,SAPERDID                                
         BE    GSPE106                                                          
*                                                                               
GSPE104  MVC   GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR ERROR MESSAGE TO CONTINUE LIST         
         B     GETSEL                                                           
*&&                                                                             
*&&UK*&& MVC   OLDOID,SAPEROFF     SAVE OFFICE ID FOR LATER ATSTOMAN            
*                                                                               
GSPE106  EQU   *                                                                
         MVC   GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         OC    SELOFF,SELOFF                                                    
         BZ    GSPE110                                                          
         CLC   SELOFF,SAPEROFF                                                  
         BNE   GETSEL                                                           
GSPE110  OC    SELDID,SELDID                                                    
         BZ    GSPE120                                                          
         CLC   SELDID,SAPERDID                                                  
         BNE   GETSEL                                                           
GSPE120  CLI   SAPERLN,SAPERLNQ                                                 
         BL    GETSEL                                                           
         B     GS034                                                            
*                                                                               
         USING SAPWDD,R3                                                        
GSPWD    OC    SELUSR,SELUSR       EXTRACT PASSWORD RECORD                      
         BZ    GS034                                                            
         CLI   SAPWDLN,SAPWDLNQ                                                 
         BL    GETSEL                                                           
         MVI   SELPWDF,1                                                        
         MVC   SAVPWD,SAPWDCOD                                                  
         MVC   SAVPWD#,SAPWDNUM                                                 
         B     GS034                                                            
*                                                                               
         USING SAAGCD,R3                                                        
GSAGC    MVI   SELAGRF,1                                                        
         OC    SELAGR,SELAGR       FILTER ON ACCESS GROUP CODE                  
         BZ    GS034                                                            
         CLI   SAAGCLN,SAAGCLNQ                                                 
         BL    GETSEL                                                           
         CLC   SELAGR,SAAGCCOD                                                  
         BNE   GETSEL                                                           
         B     GS034                                                            
*                                                                               
         USING SAAPCD,R3                                                        
GSAPC    MVI   SELAPCF,1                                                        
         OC    SELAPC,SELAPC       FILTER ON APPROVER GROUP CODE                
         BZ    GS034                                                            
         CLI   SAAPCLN,SAAPCLNQ                                                 
         BL    GETSEL                                                           
         CLC   SELAPC,SAAPCCOD                                                  
         BNE   GETSEL                                                           
         B     GS034                                                            
*                                                                               
         USING SAOPID,R3                                                        
GSOPI    MVI   SELOPIF,1           FLAG OPI FOUND                               
         OC    SELOPI,SELOPI       FILTER ON OLD PERSON ID                      
         BZ    GS034                                                            
         CLI   SAOPILN,SAOPILNQ                                                 
         BL    GETSEL                                                           
         CLC   SELOPI,SAOPIPID                                                  
         BNE   GETSEL                                                           
         B     GS034                                                            
*                                                                               
GS036    CLI   SELAGRF,0           CHECK ACCESS GROUP FOUND                     
         BNE   *+14                                                             
         OC    SELAGR,SELAGR                                                    
         BNZ   GETSEL                                                           
         CLI   SELAPCF,0           CHECK APPROVER GROUP FOUND                   
         BNE   *+14                                                             
         OC    SELAPC,SELAPC                                                    
         BNZ   GETSEL                                                           
         CLI   SELOPIF,0           CHECK OLD PERSONID FOUND                     
         BNE   *+14                                                             
         OC    SELOPI,SELOPI                                                    
         BNZ   GETSEL                                                           
         OC    SELUSR,SELUSR                                                    
         BZ    GS040                                                            
         CLI   SELPWDF,0           CHECK PASSWORD RECORD FILTER                 
         BE    GETSEL                                                           
         LA    R2,IOKEY                                                         
         DROP  R2                                                               
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY       READ PASSWORD RECORD                         
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
*        MVC   SA0KCODE,SAVPWD                                                  
         MVC   SA0KNUM,SAVPWD#                                                  
         OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   GETSEL              IGNORE IF RECORD NOT FOUND                   
         L     R2,AIOAREA3                                                      
         MVC   APWORK,SELUSR                                                    
*                                  FILTER USER ID COMPATIBLE                    
         L     RF,=A(FILTUSER)                                                  
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BE    GS040                 OK                                         
         B     GETSEL                ELSE NEXT RECORD RESTART SEQUENCE          
*&&UK                                                                           
*                                  TEST OFFICE MANAGER ACCESS                   
GS040    GOTO1 ATSTOMAN,OLDOID                                                  
         BE    GS042                                                            
*                                                                               
         OC    GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR ERROR MESSAGE TO CONTINUE LIST         
         B     GETSEL                                                           
*                                                                               
GS042    OC    GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         MVC   FVMSGNO,=AL2(FVFOK) EXIT OK                                      
         B     GETSELX                                                          
*&&                                                                             
*&&US                                                                           
GS040    MVC   FVMSGNO,=AL2(FVFOK) EXIT OK                                      
         B     GETSELX                                                          
*&&                                                                             
GSEND    MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
GETSELX  XIT1                                                                   
         DROP  R2,R3,R4                                                         
         LTORG                                                                  
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* TEST THAT NHI NUMBER IS OF THE FORM ABNNNNNNX WHERE:                *         
*  - A AND B MUST EACH BE A THRU' Z AND A VALID COMBINATION           *         
*    (SEE NHICOMBS)                                                   *         
*  - NNNNNN MUST BE NUMERIC                                           *         
*  - X MUST BE A THRU' D                                              *         
*                                                                     *         
* ENTRY - FIELD IN FVAREA                                             *         
* EXIT  - SET CC NOT EQUAL ON ANY ERROR. FVMSGNO SET                  *         
***********************************************************************         
                                                                                
VALNHI   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VALNHI,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+VNH'    INSERT NAME                                  
         LA    R3,FVIFLD                                                        
         USING NHIDS,R3                                                         
         LA    R1,NHIDSP           TEST PREFIX IS ALPHA                         
         LA    R0,L'NHIDSP                                                      
VNHI010  CLI   0(R1),C'A'                                                       
         BL    SAENIC                                                           
         CLI   0(R1),C'Z'                                                       
         BH    SAENIC                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VNHI010                                                       
*                                                                               
         LA    R1,NHIDSN           MUST BE 6 DIGITS IN MIDDLE                   
         LA    R0,L'NHIDSN                                                      
VNHI020  CLI   0(R1),C'0'                                                       
         BL    SAENIC                                                           
         CLI   0(R1),C'9'                                                       
         BH    SAENIC                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VNHI020                                                       
*                                                                               
         CLI   NHIDSS,C'A'         TEST FOR ALPHA SUFFIX                        
         BL    SAENIC                                                           
         CLI   NHIDSS,C'Z'                                                      
         BH    SAENIC                                                           
*                                                                               
         LA    RE,NHICOMBT                                                      
         USING NHICOMBD,RE                                                      
         SR    R0,R0                                                            
VNHI030  CLI   NHICELEN,EOTQ                                                    
         BE    SAENIP              NOT AN ALLOWED PREFIX                        
         CLC   NHICFRST,NHIDSP                                                  
         BE    VNHI040                                                          
         IC    R0,NHICELEN                                                      
         AR    RE,R0                                                            
         B     VNHI030                                                          
*                                                                               
VNHI040  IC    R0,NHICELEN                                                      
         SH    R0,=Y(L'NHICELEN+L'NHICFRST)                                     
         SRDL  R0,32                                                            
         D     R0,=AL4(L'NHICSCND)                                              
VNHI050  CLC   NHICSCND,NHIDSP+1                                                
         BE    VNHI060                                                          
         LA    RE,L'NHICSCND(RE)                                                
         BCT   R1,VNHI050                                                       
         B     SAENIP              NOT VALID FOR THIS FIRST CHAR                
*                                                                               
VNHI060  CLI   NHIDSS,C'A'         TEST FOR SPECIFIC SUFFIXES                   
         BL    SAENIS                                                           
         CLI   NHIDSS,C'D'                                                      
         BH    SAENIS                                                           
         B     VNHIOK                                                           
*                                                                               
SAENIC   MVC   FVMSGNO,=Y(CE#NHIIC)                                             
         MVC   FVOSYS,ASSYSE                                                    
         B     VNHINO              INVALID NHI CODE FORMAT                      
SAENIP   MVC   FVMSGNO,=Y(CE#NHIIP)                                             
         MVC   FVOSYS,ASSYSE                                                    
         B     VNHINO              INVALID NHI PREFIX                           
SAENIS   MVC   FVMSGNO,=Y(CE#NHIIS)                                             
         MVC   FVOSYS,ASSYSE                                                    
         B     VNHINO              INVALID NHI SUFFIX                           
*                                                                               
VNHIOK   SR    RC,RC                                                            
VNHINO   LTR   RC,RC                                                            
         XIT1  1                                                                
                                                                                
         LTORG                                                                  
                                                                                
* AFNHICOMBS                                                                    
* NHI PREFIX/SUFFIX VAID COMBINATIONS AS DEFINED FOR FEE SYSTEM                 
EOTQ     EQU   X'FF'                                                            
NHICOMBT DS    0C                                                               
       ++INCLUDE AFNHICOMBS                                                     
         DROP  R3,RE                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ROUTINE TO DELETE ALL PERSON AND ASSOCIATED PASSWORD RECORDS        *         
* USING PID IN APRECKEY                                               *         
***********************************************************************         
         USING SAPEREC,R2                                                       
CLEARPID CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING CLEARPID,RB                                                      
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+CLP'    INSERT NAME                                  
         XC    APHALF,APHALF       USE TO HOLD NEXT EFFECTIVE DATE KEY          
         XC    PNUMSAVE,PNUMSAVE                                                
*                                                                               
CLP010   LA    R2,IOKEY                                                         
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY     READ NEXT PERSON RECORD                      
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,APRECKEY+(SAPEPID-SAPEKEY)                               
         MVC   SAPEDEF,APHALF      EFFECTIVE DATE FOR READ HIGH                 
         GOTO1 AIO,IOHIUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   CLP200                                                           
         L     R2,AIOAREA2                                                      
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY                                   
         BNE   CLP200              IF NOT CURRENT PERSON THEN EXIT              
         MVC   APHALF,SAPEDEF      SAVE LAST EFFECTIVE DATE                     
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
         GOTO1 ASETACT,SAPEREC     MARK ACTIVITY                                
         OI    SAPESTAT,X'80'                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R4,SAPEDATA         GET PASSWORD POINTER ELEMENT                 
CLP012   CLI   0(R4),0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R4),SAPWDELQ                                                   
         BE    CLP014                                                           
         SR    R0,R0                                                            
         IC    R0,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     CLP012                                                           
         USING SAPWDD,R4                                                        
*                                  UPDATE PASSWORD RECORDS                      
CLP014   MVC   PNUMSAVE,SAPWDNUM                                                
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    CLP020                                                           
         LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KCODE,SAPWDCOD                                                
         DROP  R4                                                               
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   CLP020                                                           
         L     R2,AIOAREA2                                                      
         TM    SA0STAT,X'20'       BYPASS IF ALREADY LOCKED                     
         BO    CLP020                                                           
         LA    R1,SA0DATA          UPDATE ELEMENT DATA                          
*                                                                               
CLP015   CLI   0(R1),0                                                          
         BE    CLP018                                                           
         CLI   0(R1),SAPALELQ                                                   
         BE    CLP017                                                           
CLP016   SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CLP015                                                           
*                                                                               
         USING SAPALD,R1                                                        
CLP017   CLC   SAPALPID,OLDPID     CHECK PASSWORD CODE NOT REUSED               
         BNE   CLP019                                                           
         B     CLP016                                                           
         DROP  R1                                                               
*                                  LOCK THIS PASSWORD CODE RECORD               
CLP018   EQU   *                                                                
         BRAS  RE,LOCKPWD                                                       
*                                  UPDATE PASSWORD NUMBER RECORD                
CLP019   EQU   *                                                                
         LA    R2,IOKEY                                                         
         XC    SA0KEYS,SA0KEYS                                                  
         MVC   SA0KNUM,PNUMSAVE                                                 
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   CLP020                                                           
         L     R2,AIOAREA2         LOCK PASSWORD RECORD                         
         TM    SA0STAT,X'20'       BYPASS IF ALREADY LOCKED                     
         BO    CLP020                                                           
         BRAS  RE,LOCKPWD                                                       
*                                  UPDATE FOR NEXT EFFECTIVE DATE               
CLP020   SR    RE,RE                                                            
         ICM   RE,3,APHALF                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,APHALF                                                      
         B     CLP010                                                           
*                                  HERE AT END OF PERSONAL ID LOOP              
CLP200   EQU   *                                                                
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BNE   CLPX                                                             
         OC    PNUMSAVE,PNUMSAVE                                                
         BZ    CLPX                                                             
*                                  UPDATE PASSWORD NUMBER RECORD                
         LA    R2,IOKEY                                                         
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,PNUMSAVE                                                 
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   CLPX                                                             
         L     R2,AIOAREA2         LOCK PASSWORD RECORD                         
         TM    SA0STAT,X'20'       BYPASS IF ALREADY LOCKED                     
         BO    CLPX                                                             
         BRAS  RE,LOCKPWD                                                       
*                                                                               
CLPX     XIT1                                                                   
                                                                                
***********************************************************************         
* SET EFFECTIVE DATE END AND LOCK PASSWORD RECORD                               
***********************************************************************         
LOCKPWD  NTR1                                                                   
         LA    R3,SA0DATA                                                       
*                                                                               
LPWD010  CLI   0(R3),0             TEST END OF RECORD                           
         BE    LPWD030                                                          
         CLI   0(R3),SAPEFELQ                                                   
         BE    LPWD020                                                          
         SR    R0,R0                                                            
         IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     LPWD010                                                          
*                                  SET END DATE TO YESTERDAY                    
         USING SAPEFD,R3                                                        
LPWD020  GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         GOTO1 VDATCON,APPARM,(2,TODAY),(0,ADDATE)                              
         GOTO1 VADDAY,APPARM,ADDATE,ADDATE,F'-1'                                
         GOTO1 VDATCON,APPARM,(0,ADDATE),(2,SAPEFEND)                           
*                                  LOCK PASSWORD RECORD                         
LPWD030  MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         GOTO1 ASETACT,SA0REC                                                   
         OI    SA0STAT,X'20'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE ALL PERSON AND ASSOCIATED PASSWORD RECORDS        *         
* THAT ARE NOT CURRENTLY EFFECTIVE, WITH A NEW PERSONAL ID IN         *         
* NEWPID AND OLD PID IN APRECKEY                                      *         
* SAVE OLD PERSONAL ID IN CURRENT PERSON RECORD                       *         
***********************************************************************         
         USING SAPEREC,R2                                                       
CHGEPID  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING CHGEPID,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+CPI'    INSERT NAME                                  
         XC    APHALF,APHALF       USE TO HOLD NEXT EFFECTIVE DATE KEY          
         XC    PNUMSAVE,PNUMSAVE                                                
*                                                                               
CHP010   LA    R2,IOKEY                                                         
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY     READ NEXT PERSON RECORD                      
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,APRECKEY+(SAPEPID-SAPEKEY)                               
         MVC   SAPEDEF,APHALF      EFFECTIVE DATE FOR READ HIGH                 
         GOTO1 AIO,IOHIUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   CHP400                                                           
         L     R2,AIOAREA2                                                      
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEKEY                                   
         BNE   CHP400              IF NOT CURRENT PERSON THEN EXIT LOOP         
*                                                                               
         MVC   APHALF,SAPEDEF      SAVE LAST EFFECTIVE DATE                     
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
         GOTO1 ASETACT,SAPEREC     MARK ACTIVITY                                
         OI    SAPESTAT,X'80'      DELETE OLD PID RECORD                        
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         CLC   APHALF,TODAYC       IF PERSON RECORD EFFECTIVE DATE IS           
         BNH   CHP011                BEFORE TO TODAY THEN BUILD OLD             
         LA    R3,APELEM             PERSONAL ID ELEMENT                        
         USING SAOPID,R3                                                        
         XC    SAOPIEL(SAOPILNQ),SAOPIEL                                        
         MVI   SAOPIEL,SAOPIELQ                                                 
         GOTO1 AGETELS,SAPEREC     CHECK ELEMENT NOT ALREADY PRESENT            
         OC    APPARM,APPARM                                                    
         BNZ   CHP011                                                           
         MVI   SAOPILN,SAOPILNQ                                                 
         MVC   SAOPIPID,OLDPID                                                  
         MVC   SAOPIDAT,TODAY                                                   
         GOTO1 AADDELS,SAPEREC     UPDATE THIS PERSON RECORD                    
         DROP  R3                                                               
*                                                                               
CHP011   NI    SAPESTAT,X'FF'-X'80'                                             
         MVC   SAPEPID,NEWPID      ADD NEW PID RECORD                           
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
         GOTO1 AIO,IOADD+IOCONFIL+IO2                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R4,SAPEDATA         GET PASSWORD POINTER ELEMENT                 
CHP012   CLI   0(R4),0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R4),SAPWDELQ                                                   
         BE    CHP100                                                           
         SR    R0,R0                                                            
         IC    R0,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     CHP012                                                           
         USING SAPWDD,R4                                                        
*                                  UPDATE PASSWORD NAME RECORD WITH NEW         
*                                    PID POINTER IF NOT CURRENT                 
CHP100   MVC   PNUMSAVE,SAPWDNUM                                                
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    CHP300                                                           
         CLC   SAPWDCOD,SA0KEYSV+(SA0KCODE-SA0KEY)                              
         BE    CHP300                                                           
         LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KCODE,SAPWDCOD                                                
         DROP  R4                                                               
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         BZ    CHP300                                                           
         L     R2,AIOAREA2                                                      
         LA    R3,SA0DATA          UPDATE ELEMENT DATA                          
*                                                                               
CHP110   CLI   0(R3),0                                                          
         BE    CHP150                                                           
         CLI   0(R3),SAPALELQ                                                   
         BE    CHP130                                                           
         CLI   0(R3),SAPASELQ                                                   
         BE    CHP140                                                           
CHP120   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CHP110                                                           
*                                                                               
         USING SAPALD,R3                                                        
CHP130   MVC   SAPALPID,NEWPID       CHANGE TO NEW PID                          
         B     CHP120                                                           
         DROP  R3                                                               
*                                                                               
         USING SAPASD,R3                                                        
CHP140   CLI   SAPASLEN,X'04'                                                   
         BNE   CHP120                                                           
         MVC   PNUMSAVE,SAPASDTA    SAVE PASSWORD #                             
         B     CHP120                                                           
         DROP  R3                                                               
*                                  SAVE UPDATED PASSWORD NAME RECORD            
CHP150   MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         GOTO1 ASETACT,SA0REC                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                  UPDATE PASSWORD # RECORD WITH NEW            
*                                    PID POINTER IF NOT CURRENT                 
CHP160   LA    R2,IOKEY                                                         
         XC    SA0KCODE,SA0KCODE                                                
         MVC   SA0KNUM,PNUMSAVE                                                 
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         BZ    CHP300                                                           
         L     R2,AIOAREA2         UPDATE ELEMENT DATA                          
         LA    R3,SA0DATA                                                       
*                                                                               
CHP170   CLI   0(R3),0                                                          
         BE    CHP200                                                           
         CLI   0(R3),SAPALELQ                                                   
         BE    CHP190                                                           
CHP180   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CHP170                                                           
*                                                                               
         USING SAPALD,R3                                                        
CHP190   MVC   SAPALPID,NEWPID       CHANGE TO NEW PID                          
         B     CHP180                                                           
         DROP  R3                                                               
*                                  SAVE UPDATED PASSWORD # RECORD               
CHP200   MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         GOTO1 ASETACT,SA0REC                                                   
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                  UPDATE FOR NEXT EFFECTIVE DATE               
CHP300   SR    RE,RE                                                            
         ICM   RE,3,APHALF                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,APHALF                                                      
         B     CHP010              EXIT AT END PF PERSONAL ID LOOP              
*                                                                               
CHP400   CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BNE   CHPX                                                             
         OC    PNUMSAVE,PNUMSAVE                                                
         BZ    CHPX                                                             
*                                                                               
         LA    R2,IOKEY            UPDATE PASSWORD # RECORD WITH NEW            
         XC    SA0KEY,SA0KEY       PID POINTER IF NOT CURRENT                   
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KNUM,PNUMSAVE                                                 
         GOTO1 AIO,IORDUP+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         BZ    CHPX                                                             
         L     R2,AIOAREA2         UPDATE ELEMENT DATA                          
         LA    R3,SA0DATA                                                       
*                                                                               
CHP410   CLI   0(R3),0                                                          
         BE    CHPX                                                             
         CLI   0(R3),SAPALELQ                                                   
         BE    CHP430                                                           
CHP420   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CHP410                                                           
*                                                                               
         USING SAPALD,R3                                                        
CHP430   MVC   SAPALPID,NEWPID       CHANGE TO NEW PID                          
         B     CHPX                                                             
         DROP  R3                                                               
*                                                                               
CHPX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
TERMNTFY NTR1  BASE=*,LABEL=*                                                   
         MVC   DATADISP,=AL2(28)                                                
*                                                                               
         USING SAPEREC,R6                                                       
         L     R6,AIOAREA1         POINT TO PERSON RECORD                       
*                                                                               
* LABEL                                                                         
         L     R4,AIOAREA2                                                      
         MVC   0(16,R4),=CL16'SECINFO*********'                                 
         LA    R4,16(R4)                                                        
*                                                                               
* <TERMINATION> TAG                                                             
         MVC   0(L'TAG_TERM,R4),TAG_TERM                                        
         LA    R4,L'TAG_TERM(R4)                                                
*                                                                               
* <CONTINENT> TAG                                                               
         MVC   0(L'TAG_CONT,R4),TAG_CONT                                        
         LA    R4,L'TAG_CONT(R4)                                                
*                                                                               
*&&US*&& MVC   0(2,R4),=C'NA'                                                   
*&&UK*&& MVC   0(2,R4),=C'EU'                                                   
         LA    R4,2(R4)                                                         
*                                                                               
* <\CONTINENT> TAG                                                              
         MVC   0(L'TAG_CONTX,R4),TAG_CONTX                                      
         LA    R4,L'TAG_CONTX(R4)                                               
*                                                                               
* <PID> TAG                                                                     
         MVC   0(L'TAG_PID,R4),TAG_PID                                          
         LA    R4,L'TAG_PID(R4)                                                 
*                                                                               
* PID FROM THE KEY                                                              
         MVC   0(L'SAPEPID,R4),SAPEPID                                          
         LA    R4,L'SAPEPID(R4)                                                 
*                                                                               
* NOW NUMERIC PID FROM THE PASSWORD ELEMENT                                     
         L     R6,AIOAREA1                                                      
         MVI   ELCODE,SAPWDELQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   TERMN10                                                          
*                                                                               
         MVI   0(R4),C'('                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         USING SAPWDD,R6                                                        
         XR    R0,R0                                                            
         ICM   R0,3,SAPWDNUM                                                    
         EDIT  (R0),(5,0(R4)),WRK=APWORK,DUB=APDUB,FILL=0                       
         LA    R4,5(R4)                                                         
         DROP  R6                                                               
*                                                                               
         MVI   0(R4),C')'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
TERMN10  DS    0H                                                               
*                                                                               
* </PID> TAG                                                                    
         MVC   0(L'TAG_PIDX,R4),TAG_PIDX                                        
         LA    R4,L'TAG_PIDX(R4)                                                
*                                                                               
* <SECAGENCY> TAG                                                               
         MVC   0(L'TAG_SAGY,R4),TAG_SAGY                                        
         LA    R4,L'TAG_SAGY(R4)                                                
*                                                                               
* SECURITY AGENCY FROM RECORD'SKEY                                              
         L     R6,AIOAREA1         POINT TO PERSON RECORD                       
         MVC   0(L'SAPEAGY,R4),SAPEAGY-SAPEREC(R6)                              
         LA    R4,L'SAPEAGY(R4)                                                 
*                                                                               
* </SECAGENCY> TAG                                                              
         MVC   0(L'TAG_SAGYX,R4),TAG_SAGYX                                      
         LA    R4,L'TAG_SAGYX(R4)                                               
*                                                                               
* <FIRSTNAME> TAG                                                               
         MVC   0(L'TAG_FNAM,R4),TAG_FNAM                                        
         LA    R4,L'TAG_FNAM(R4)                                                
*                                                                               
         L     R6,AIOAREA1                                                      
         MVI   ELCODE,SANAMELQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   TERMN20                                                          
*                                                                               
         USING SANAMD,R6                                                        
*                                                                               
         LA    R3,SANAMES          START OF NAME SUBELEMENTS                    
*                                                                               
         TM    SANAMIND,SANAMIFN                                                
         BZ    TERMN20                                                          
*                                                                               
         LLC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),1(R3)       FIRST NAME                                   
*                                                                               
         LA    R3,2(R3,RF)         ADVANCE SUB-ELEMENT POINTER                  
         LA    R4,1(RF,R4)                                                      
*                                                                               
TERMN20  DS    0H                                                               
*                                                                               
* </FIRSTNAME> TAG                                                              
         MVC   0(L'TAG_FNAMX,R4),TAG_FNAMX                                      
         LA    R4,L'TAG_FNAMX(R4)                                               
*                                                                               
* <LASTNAME> TAG                                                                
         MVC   0(L'TAG_LNAM,R4),TAG_LNAM                                        
         LA    R4,L'TAG_LNAM(R4)                                                
*                                                                               
         TM    SANAMIND,SANAMIMN                                                
         BZ    TERMN25                                                          
         LLC   RF,0(R3)            SKIP MIDDLE NAME                             
         LA    R3,1(R3,RF)         ADVANCE SUB-ELEMENT POINTER                  
*                                                                               
TERMN25  DS    0H                                                               
         TM    SANAMIND,SANAMILN                                                
         BZ    TERMN30                                                          
*                                                                               
         LLC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),1(R3)       FIRST NAME                                   
*                                                                               
         LA    R4,1(RF,R4)                                                      
*                                                                               
         DROP  R6                                                               
*                                                                               
TERMN30  DS    0H                                                               
*                                                                               
* </LASTNAME> TAG                                                               
         MVC   0(L'TAG_LNAMX,R4),TAG_LNAMX                                      
         LA    R4,L'TAG_LNAMX(R4)                                               
*                                                                               
* <EMAIL> TAG                                                                   
         MVC   0(L'TAG_EML,R4),TAG_EML                                          
         LA    R4,L'TAG_EML(R4)                                                 
*                                                                               
         L     R6,AIOAREA1                                                      
         MVI   ELCODE,SAPEEELQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   TERMN40                                                          
*                                                                               
         USING SAPEED,R6                                                        
*                                                                               
         LLC   RF,1(R6)                                                         
         SHI   RF,3                MINUS ELCODE,LENGTH, BCTR RF,0               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SAPEEID     EMAIL ADDRESS                                
         LA    R4,1(RF,R4)                                                      
*                                                                               
         DROP  R6                                                               
*                                                                               
TERMN40  DS    0H                                                               
*                                                                               
* </EMAIL> TAG                                                                  
         MVC   0(L'TAG_EMLX,R4),TAG_EMLX                                        
         LA    R4,L'TAG_EMLX(R4)                                                
*                                                                               
* <TIMESTAMP> TAG                                                               
         MVC   0(L'TAG_TIME,R4),TAG_TIME                                        
         LA    R4,L'TAG_TIME(R4)                                                
*                                                                               
         GOTO1 =V(DATTIM),APPARM,(X'02',0),APWORK,RR=APRELO                     
* TIME RETURNED AS YYYYMMDDHHMMSS                                               
         MVC   0(4,R4),APWORK      YYYY                                         
         MVI   4(R4),C'-'                                                       
         LA    R4,5(R4)                                                         
         MVC   0(2,R4),APWORK+4    MM                                           
         MVI   2(R4),C'-'                                                       
         LA    R4,3(R4)                                                         
         MVC   0(2,R4),APWORK+6    DD                                           
         MVI   2(R4),C' '                                                       
         LA    R4,3(R4)                                                         
         MVC   0(2,R4),APWORK+8    HH                                           
         MVI   2(R4),C' '                                                       
         LA    R4,3(R4)                                                         
         MVC   0(2,R4),APWORK+10   MM                                           
         MVI   2(R4),C' '                                                       
         LA    R4,3(R4)                                                         
         MVC   0(2,R4),APWORK+12   SS                                           
         LA    R4,2(R4)                                                         
*                                                                               
* </TIMESTAMP> TAG                                                              
         MVC   0(L'TAG_TIMEX,R4),TAG_TIMEX                                      
         LA    R4,L'TAG_TIMEX(R4)                                               
*                                                                               
* </TERMINATION> TAG                                                            
         MVC   0(L'TAG_TERMX,R4),TAG_TERMX                                      
         LA    R4,L'TAG_TERMX(R4)                                               
*                                                                               
         XC    APDUB,APDUB                                                      
         L     R6,AIOAREA2                                                      
         SR    R4,R6                                                            
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CMQIO-COMFACSD(RF)                                            
         GOTO1 (RF),APPARM,=CL8'PUT',(R6),(R4),0,0,APDUB,C'UNIT',0              
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TAG_CONT  DC   C'<Continent>'                                                   
TAG_CONTX DC   C'</Continent>'                                                  
TAG_TERM  DC   C'<Termination>'                                                 
TAG_TERMX DC   C'</Termination>'                                                
TAG_PID   DC   C'<Pid>'                                                         
TAG_PIDX  DC   C'</Pid>'                                                        
TAG_SAGY  DC   C'<SecAgency>'                                                   
TAG_SAGYX DC   C'</SecAgency>'                                                  
TAG_FNAM  DC   C'<FirstName>'                                                   
TAG_FNAMX DC   C'</FirstName>'                                                  
TAG_LNAM  DC   C'<LastName>'                                                    
TAG_LNAMX DC   C'</LastName>'                                                   
TAG_EML   DC   C'<eMail>'                                                       
TAG_EMLX  DC   C'</eMail>'                                                      
TAG_TIME  DC   C'<TimeStamp>'                                                   
TAG_TIMEX DC   C'</TimeStamp>'                                                  
                                                                                
                                                                                
                                                                                
*                                                                               
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
* FAXTRAINF                                                                     
       ++INCLUDE FAXTRAINF                                                      
         EJECT                                                                  
* DDLISTD                                                                       
LISTIOD  DSECT                                                                  
       ++INCLUDE DDLISTD                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* TWA AND SAVED STORAGE                                                         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSF7D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSE7D          GRIDS SCREEN                                 
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSD7D                                                       
         ORG                                                                    
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* PERSON RECORD EFFECTIVE DATE KEY SAVE TABLE                                   
* LIST OF THE EFFECTIVE DATES (IN COMPLEMENT FORM) OF THE RECORDS               
* DATED FOR A GIVEN PERSONAL ID, LATEST TO FIRST                                
* EACH WITH CORRESPONDING PASSWORD FOR THAT RECORD                              
*----------------------------------------------------------------------         
         ORG   SAVOVER                                                          
SAVPEKEY DS    CL(L'SAPEKEY)       SAVE PID KEY FOR COPY/GRID ACTION            
SAV0KEY  DS    CL(L'SA0KEY)        SAVE PWD KEY FOR COPY ACTION                 
MAXIOPID DS    CL(L'SAPEPID)       NONZERO IF HAD MAXIO (SAVES SAVPID)          
OCURPDEF DS    A                   OFFSET TO CURRENT RECORD                     
ODISPDEF DS    A                   OFFSET TO DISPLAYED RECORD                   
OFRSTDEF DS    A                   OFFSET TO FIRST RECORD AT END                
PDEFPID  DS    CL(L'SAPEPID)       SAVE LAST PID DEF TABLE PID VALUE            
PDEFTLEN EQU   L'SAPEDEF+L'SA0KCODE                                             
PDEFTAB  DS    (PDEFTNUM)XL(PDEFTLEN)                                           
PDEFTEND EQU   *                                                                
PDEFTABX DS    2XL(PDEFTLEN)       OVERFLOW AREA                                
PDEFTNUM EQU   60                  MAX NUMBER OF ENTRIES IN LIST                
PDEFXFLG DS    CL1                 MAX ENTRIES FLAG                             
EXPFLAG  DS    CL1                 TERMINATED PERSON FLAG FOR GETSEL            
*                                                                               
ASE      DS    A                                                                
APGM     DS    A                                                                
APGMLST  DS    A                         START OF PROGRAM LIST                  
APGMLSTX DS    A                         END OF PROGRAM LIST                    
PLSTLEN  DS    XL2                                                              
SAVASF   DS    XL8                                                              
*                                                                               
SVSELOPT DS    CL(SELOPTL)               SAVED OPTIONS                          
*                                                                               
PGMSTCNT DS    XL2                                                              
PGMSTAB  DS    (MAXSTAB)CL(L'PGMNAME+L'PGMNUM)                                  
MAXSTAB  EQU   64                                                               
                                                                                
***********************************************************************         
* SECURITY AGENCY TABLE - USED IN CHKPER ROUTINE                                
***********************************************************************         
SECATBD  DSECT                                                                  
SECASEID DS    0CL2                                                             
SECASE   DS    XL1                 SYSTEM SE NUMBER                             
SECABID  DS    XL1                 SECURITY BINARY ID                           
SECALNQ  EQU   *-SECATBD                                                        
                                                                                
***********************************************************************         
* LIST/SELECT LINE LAYOUT                                                       
***********************************************************************         
LISTD    DSECT                                                                  
LISTSELH DS    XL8                                                              
LISTSEL  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLINX DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTPID  DS    CL8                 PERSONAL ID                                  
         DS    CL1                                                              
LISTNAME DS    CL24                LAST NAME                                    
         DS    CL1                                                              
LISTPWD  DS    CL10                PASSWORD                                     
         DS    CL1                                                              
         ORG   LISTPWD                                                          
LISTAGR  DS    CL8                 ACCESS GROUP                                 
         DS    CL1                                                              
LISTLAG  DS    CL8                 LIMIT ACCESS GROUP                           
         DS    CL1                                                              
LISTOFF  DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
LISTDID  DS    CL3                 DEPARTMENT CODE                              
         DS    CL1                                                              
LISTEXT  DS    CL5                 EXTENSION NUMBER                             
LISTTERM DS    CL1                 EXPIRED/TERMINATED FLAG                      
LISTDEF  DS    CL8                 EFFECTIVE DATE                               
         DS    XL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
*                                                                               
NHIDS    DSECT                     NHI FORMAT DSECT                             
NHIDSC   DS    0CL(L'NHIDSP+L'NHIDSN+L'NHIDSS)          NHI CODE                
NHIDSP   DS    CL2                 - CHARACTER PREFIX                           
NHIDSN   DS    CL6                 - NUMERIC PORTION                            
NHIDSS   DS    CL1                 - CHARACTER SUFFIX                           
NHIDST   DS    CL1                 NHI TYPE                                     
NHIDSQ   EQU   *-NHIDSN                                                         
*                                                                               
NHICOMBD DSECT                     NHI COMBINATION TABLE DSECT                  
NHICELEN DS    XL1                 L'TABLE ENTRY                                
NHICFRST DS    CL1                 FIRST CHARACTER                              
NHICSCND DS    0CL1                LIST OF VALID SECOND CHARACTERS              
         EJECT                                                                  
                                                                                
***********************************************************************         
* DSECT TO COVER LOCAL W/S                                                      
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
SAVPARM  DS    8F                                                               
RETURN   DS    A                                                                
AGCTBL   DS    A                   GRIDS COLUMN TABLE                           
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
*                                                                               
EXITC    DS    X                   EXIT CONDITION                               
*                                                                               
SA0KEYSV DS    XL(L'APRECKEY)      PASSWORD RECORD KEY SAVE                     
SA0KEYS2 DS    XL(L'APRECKEY)      PASSWORD RECORD KEY SAVE 2                   
PIDIOERR DS    XL(L'IOERR)         PERSON RECORD IOERR                          
PWDIOERR DS    XL(L'IOERR)         PASSWORD RECORD IOERR                        
IOERRSV  DS    XL(L'IOERR)         IOERR SAVE                                   
PIDREAD  DS    XL1                 PERSON RECORD READ PASS FLAG                 
PWDREAD  DS    XL1                 PASSWORD RECORD READ PASS FLAG               
PIDIND   DS    XL1                 PERSONAL ID CODE INPUT FLAG                  
PWDIND   DS    XL1                 PASSWORD CODE INPUT FLAG                     
TODAY    DS    XL2                 TODAYS DATE COMPRESSED BINARY                
TODAYC   DS    XL2                 TODAYS DATE COMPLEMENT COMPRESSED            
DATETIME DS    XL4                 DATE/TIME VALUE FROM DATTIM                  
DATETIMC DS    XL4                 DATE/TIME VALUE 1'S COMPLEMENTED             
ADDATE   DS    CL8                 ADDAY DATE WORK SPACE YYMMDD EBCDIC          
PNUMSAVE DS    XL2                 PASSWORD # GENERAL SAVE AREA                 
*                                                                               
SVSEBLK  DS    0XL4                BLOCK-LOGIN ID SE#/SYS SE#/BIN ID            
SVCNTLSE DS    XL1                 CURRENT LOGIN IDS SE#                        
SVLIDSE  DS    XL1                 CURRENT LOGIN ACC IDS SE#                    
SVSEBID  DS    0CL2                SAVED ARE FOR SYS SE# AND BIN ID             
SVSYSSE  DS    XL1                 SYSTEM SE #                                  
SVBINID  DS    XL1                 AGENCY BINARY ID                             
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
PIDREQD  DS    XL1                 PERSONID REQUIRED WITH PASSWD=Y              
PWDINFO  DS    0XL5                PASSWORD INFO FROM ACCESS RECORD             
PWDFLAG  DS    XL1                 PASSWORD FLAG                                
PWDTOUT  DS    XL1                 PASSWORD TIMEOUT IN DAYS                     
PWDMINLN DS    XL1                 PASSWORD MINIMUM LENGTH                      
PWDREUSE DS    XL1                 PASSWORD REUSE MAXIMUM NUMBER                
PWDRULE  DS    XL1                 PASSWORD RULE NUMBER                         
*                                                                               
GETSEQF  DS    XL1                 GETSEL READ SEQUENCE BROKEN FLAG             
FLDCNT   DS    XL1                 SCREEN FIELD COUNTER                         
GCOSEL   DS    XL1                 GRIDS COLUMN SELECTOR                        
ANYUPDID DS    XL1                 ANY UPDATIVE IDS (Y/N)                       
RDOLYFST DS    XL1                 ADD READ-ONLY IDS FIRST (Y/N)                
COPYFLAG DS    XL1                 FLAG COPY ACTION SECOND KEY                  
DEFCUR   DS    XL1                 FLAG EFFECTIVE DATE FIELD CURSOR             
CURRPID  DS    CL1                 CURRENT PERSONAL ID FLAG FOR GETSEL          
WILDCLEN DS    XL1                                                              
TERMCHG  DS    XL2                 UPDATED TERMINATION DATE                     
AEPUIOV  DS    XL1                 X'80' DDLINK AEP USER IDS OVERRIDDEN         
*                                                                               
SAVPID   DS    CL(L'SAPEPID)       SAVE LAST PID ENTERED FOR LIST               
SAVPWD   DS    CL(L'SA0KCODE)      SAVE LAST PWD READ FOR LIST                  
SAVPWD#  DS    CL(L'SA0KNUM)       SAVE LAST PWD# READ FOR LIST                 
*                                                                               
PNAMEKEY DS    CL(L'SA0KEYS)       SAVE OLD PASSWD NAME KEY FOR CONVERT         
*                                                                               
PEFSTASV DS    XL(L'SAPEFSTA)      PASSWORD EFFECTIVE START DATE SAVE           
PEFENDSV DS    XL(L'SAPEFEND)      PASSWORD EFFECTIVE END DATE SAVE             
PELLOSV  DS    XL(SALLOLNQ)        PERSON LAST LOGON ELEMENT SAVE               
PELLODE  DS    XL(SALLOLNQ)        PERSON LAST LOGON ELEMENT DEFAULT            
*                                                                               
PROGRAM  DS    CL1                 PROGRAM                                      
PROGCTRY DS    CL1                 PROGRAM COUNTRY CODE                         
ACCASF   DS    CL1                                                              
PACCVAL  DS    XL(L'SASYSALL)      PROGRAM ACCESS CODE                          
CLSTACC  DS    XL4                 CLIENT LIST LIMIT ACCESS VALUE               
DATAGRP  DS    XL2                 DATA ACCESS GROUP NUMBER                     
ASYSEL   DS    A                   SYSTEM ELEMENT                               
*                                  RECORD DATA SAVED BEFORE UPDATE              
OLDVALS  DS    0C                  FOR PERSON AND PASSWORD RECORD PAIR          
OLDPID   DS    CL(L'SAPEPID)       PERSONAL ID                                  
OLDOID   DS    CL(L'SAOFOID)       OFFICE CODE                                  
OLDDID   DS    CL(L'SADPDID)       DEPARTMENT ID                                
OLDPWD   DS    XL(L'SA0KNUM)       PASSWORD NUMBER                              
OLDCODE  DS    CL(L'SA0KCODE)      PASSWORD CODE                                
OLDCODU  DS    CL(L'SA0KCODE)      PASSWORD CODE IN UPPER CASE                  
OLDAGC   DS    CL(L'SAAGAGR)       ACCESS GROUP CODE                            
OLDAGN   DS    CL(L'SAAGCNUM)      ACCESS GROUP NUMBER                          
OLDAPC   DS    CL(L'SAAPAGR)       APPROVER GROUP CODE                          
OLDAPN   DS    CL(L'SAAPGNUM)      APPROVER GROUP NUMBER                        
OLDLAC   DS    CL(L'SALAAGR)       LIMIT ACCESS GROUP CODE                      
OLDLACN  DS    CL(L'SALAAGN)       LIMIT ACCESS GROUP NUMBER                    
OLDDEF   DS    XL(L'SAPEDEF)       EFFECTIVE DATE BINARY                        
OLDDEFC  DS    XL(L'SAPEDEF)       EFFECTIVE DATE COMPLEMENT                    
OLDOVF   DS    XL(L'SAPEROVF)      SECURITY MANAGER OVERIDE FLAG                
OLDPCN   DS    XL(L'SAPERPCN)      PASSWORD CONTROL VALUE                       
OLDVALSL EQU   *-OLDVALS                                                        
*                                  RECORD DATA SAVED FOR UPDATE                 
NEWVALS  DS    0C                  OF PERSON AND PASSWORD RECORD PAIR           
NEWPID   DS    CL(L'SAPEPID)       PERSONAL ID                                  
NEWOID   DS    CL(L'SAOFOID)       OFFICE CODE                                  
NEWDID   DS    CL(L'SADPDID)       DEPARTMENT CODE                              
NEWPWD   DS    XL(L'SA0KNUM)       PASSWORD NUMBER                              
NEWCODE  DS    CL(L'SA0KCODE)      PASSWORD CODE                                
NEWCODU  DS    CL(L'SA0KCODE)      PASSWORD CODE IN UPPER CASE                  
NEWAGC   DS    CL(L'SAAGAGR)       ACCESS GROUP                                 
NEWAGN   DS    CL(L'SAAGCNUM)      ACCESS GROUP NUMBER                          
NEWAPC   DS    CL(L'SAAPAGR)       APPROVER GROUP CODE                          
NEWAPN   DS    CL(L'SAAPGNUM)      APPROVER GROUP NUMBER                        
NEWLAC   DS    CL(L'SALAAGR)       LIMIT ACCESS GROUP                           
NEWLACN  DS    CL(L'SALAAGN)       LIMIT ACCESS GROUP NUMBER                    
NEWDEF   DS    XL(L'SAPEDEF)       EFFECTIVE DATE BINARY                        
NEWDEFC  DS    XL(L'SAPEDEF)       EFFECTIVE DATE COMPLEMENT                    
NEWVALSL EQU   *-NEWVALS                                                        
*                                                                               
LASTCDT  DS    XL(L'GACTCDT)       DATE LAST CHANGED (DATCON TYPE 3)            
LASTCTM  DS    XL(L'GACTTIM)       TIME LAST CHANGED (BINARY SEC/100)           
*                                                                               
OFFAPC   DS    CL(L'SAAPAGR)       APPROVER GROUP CODE FROM OFFICE              
OFFAPN   DS    CL(L'SAAPGNUM)      APPROVER GROUP NUMBER FROM OFFICE            
DEPAPC   DS    CL(L'SAAPAGR)       APPROVER GROUP CODE FROM DEPT.               
DEPAPN   DS    CL(L'SAAPGNUM)      APPROVER GROUP NUMBER FROM DEPT.             
*                                                                               
DDLKPIN  DS    CL5                 DDLINK: PIN                                  
DDLKPWD  DS    CL10                DDLINK: PASSWORD                             
DDLKAEP  DS    CL8                 DDLINK: ACCESS EQUIVALENT PID                
DDLKAEPN DS    XL2                 DDLINK: PIN *DERIVED FROM DDLKAEP*           
DDLKAGR# DS    CL5                 DDLINK: ACCESS GROUP NUMBER                  
DDLKACDT DS    CL10                DDLINK: ACTIVITY DATE YYYY-MM-DD             
DDLKACTM DS    CL8                 DDLINK: ACTIVITY TIME HH:MM:SS               
*                                                                               
SELOPT   DS    0X                  SELECT OPTIONS                               
SELPID   DS    CL(L'SAPEPID)       PERSONAL ID FILTER                           
SELPIDL  DS    CL1                 PERSONAL ID FILTER LENGTH                    
SELPIDSP DS    CL1                 PERSONAL ID SPECIAL CHAR                     
SELKEYCL DS    CL1                                                              
SELNAM   DS    CL20                SURNAME FILTER                               
SELNAML  DS    XL1                 SURNAME FILTER LENGTH                        
SELPWD   DS    CL10                PASSWORD CODE FILTER SAVE                    
SELPWDF  DS    XL1                 PASSWORD FILTER FLAG                         
SELDEFC  DS    XL2                 EFFECTIVE DATE COMPLEMENT FILTER             
SELDEFF  DS    XL1                 EFFECTIVE DATE PRESENT ONLY FLAG             
SELUSR   DS    CL(L'SAID)          USER ID FILTER                               
SELAGR   DS    CL8                 ACCESS GROUP FILTER                          
SELAGRF  DS    XL1                 ACCESS GROUP FILTER FLAG                     
SELAPC   DS    CL8                 APPROVER GROUP FILTER                        
SELAPCF  DS    XL1                 APPROVER GROUP FILTER FLAG                   
SELOFF   DS    CL2                 OFFICE CODE FILTER                           
SELDID   DS    CL3                 DEPARTMENT ID CODE FILTER                    
SELOPI   DS    CL(L'SAPEPID)       OLD PERSON ID                                
SELSYS   DS    0XL1                SYSTEM                                       
SELOPIF  DS    XL1                 OLD PERSON ID FILTER FLAG                    
SELPGM   DS    XL1                 PROGRAM                                      
SELOPTL  EQU   *-SELOPT                                                         
*                                                                               
BLKCNT1  DS    XL1                                                              
BLKCNT2  DS    XL1                                                              
BLKCNT3  DS    XL1                                                              
BLKCNT4  DS    XL1                                                              
*                                                                               
BLOCK1   DS    20CL32              SCANNER BLOCKS                               
BLOCK2   DS    20CL32                                                           
BLOCK3   DS    20CL32                                                           
BLOCK4   DS    20CL32                                                           
BLKNTYQ  EQU   (*-BLOCK1)/(L'BLOCK1)  MAX# OF ENTRY FOR SCANNER BLOCKS          
BLKSECQ  EQU   (*-BLOCK2)/SECALNQ  MAXIMUM NUMBER OF ENTRY IN SECATB            
*                                                                               
GVALS    DS    XL(GVALSL)          TEMPORARY GRIDS VALUES                       
*                                                                               
LOCALX   EQU   *                                                                
                                                                                
***********************************************************************         
* TEMPORARY GRIDS VALUES                                                        
***********************************************************************         
GVALSD   DSECT                                                                  
GVPSWD   DS    CL(L'SA0KCODE)      PASSWORD CODE                                
GVPWHDTE DS    CL(L'SAPWHDTE)      PASSWORD EFFECTIVE DATE                      
GVPWHPSB DS    CL8                 PASSWORD SET BY                              
GVPWSTAT DS    CL8                 PASSWORD STATUS                              
GVPCN    DS    CL1                 PASSWORD CONTROL VALUE                       
GVPLODTE DS    CL10                PERSON LAST LOGON DATE                       
GVFNAME  DS    CL20                FIRST NAME                                   
GVMNAME  DS    CL20                MIDDLE NAME                                  
GVLNAME  DS    CL58                LAST NAME                                    
GVOFFN   DS    CL30                OFFICE NAME                                  
GVDEPN   DS    CL30                DEPARTMENT NAME                              
GVAPPGN  DS    CL30                APPROVER NAME                                
GVACCGN  DS    CL30                ACCESS GROUP NAME                            
GVDACGN  DS    CL30                DATA ACCESS GROUP NAME                       
GVRIDS   DS    CL160               READ ONLY IDS                                
GVUIDS   DS    CL160               WRITE ACCESS IDS                             
GVVSYS   DS    CL160               VALID SYSTEMS                                
*&&US                                                                           
GVLACC   DS    CL180               LIMITED ACCESS                               
*&&                                                                             
*&&UK                                                                           
GVOACCG  DS    CL8                 OLD ACCESS GROUP (IF EXPIRED)                
GVLACC   DS    CL15                LIMITED ACCESS                               
GVCLAC   DS    CL50                CLIENT LIMITED ACCESS +/- & 10 LALS          
*&&                                                                             
GVPGMS   DS    (MAXSTAB)CL(GVPGMLQ)                                             
GVALSL   EQU   *-GVALSD                                                         
                                                                                
***********************************************************************         
* CHECK LIST WORK DSECT                                                         
***********************************************************************         
CLWORKD  DSECT                                                                  
CLIDLST  DS    2000X                                                            
CLWORKX  EQU   *                                                                
                                                                                
***********************************************************************         
* GRID SYSTEM PROGRAM DSECT                                                     
***********************************************************************         
GVPGMSD  DSECT                                                                  
GVPGMC   DS    CL1                 PROGRAM CODE                                 
GVPGMN   DS    CL7                 PROGRAM NAME                                 
GVPGMV   DS    CL4                 PROGRAM VALUE                                
GVPGMF   DS    CL1                 PROGRAM FLAG                                 
GVPANOQ  EQU   X'80'               . AUTH=NO OVERRIDE                           
GVPGMLQ  EQU   *-GVPGMSD                                                        
*                                                                               
       ++INCLUDE DDCTRYEQUS                                                     
*&&US                                                                           
       ++INCLUDE GEGENPOS                                                       
       ++INCLUDE GEGENDOM                                                       
*&&                                                                             
*                                                                               
*&&UK                                                                           
*GEDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE GEDDEQUS                                                       
         PRINT ON                                                               
*&&                                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SEACS08   03/19/20'                                      
         END                                                                    
