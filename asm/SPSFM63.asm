*          DATA SET SPSFM63    AT LEVEL 007 AS OF 12/11/13                      
*PHASE T21763A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21763  -- AGENCY MAINTENANCE & LIST                 *         
*                                                                     *         
*  COMMENTS:                                                          *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM5C (MAINT) & SCSFM5B (LIST)              *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21763 - AGENCY'                                                
T21763   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1763**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY          VALIDATE RECORD KEY                         
         BE    VK                                                               
         CLI   MODE,VALREC          VALIDATE RECORD                             
         BE    VR                                                               
         CLI   MODE,DISPKEY         DISPLAY KEY (FOR LIST)                      
         BE    DK                                                               
         CLI   MODE,DISPREC         DISPLAY RECORD                              
         BE    DR                                                               
         CLI   MODE,LISTRECS        LIST RECORDS                                
         BE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       LA    R2,AGYAGENH          AGENCY FIELD HEADER                         
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    VK10                 YES                                         
*                                                                               
VK01     XC    KEY,KEY              GET AGENCY RECORD                           
         LA    R4,KEY                                                           
         LA    R2,AGYAGENH          AGENCY FIELD HEADER                         
         CLI   5(R2),0              ANY INPUT?                                  
         BNE   VK05                 YES                                         
         MVC   TEMPACD,=C' A'       START READING KEYS W/AGENCY C' A'           
         MVI   9(R2),C'A'           MOVE 'A' TO LIST SCREEN                     
         OI    6(R2),X'80'          XMIT                                        
         B     VKX                                                              
                                                                                
         USING SPAGYD,R4                                                        
VK05     MVI   AGYKTYPE,AGYKTYPQ    BUILD KEY WITH X'06'...                     
         MVC   AGYKAGY,AGYAGEN      AND 2 BYTE AGENCY CODE FROM SCREEN          
         MVC   TEMPACD,AGYAGEN      SAVE IT FOR LISTREC                         
         CLI   ACTEQU,ACTLIST       IS IT A LIST ACTION?                        
         BE    VKX                  YES...USE KEY AS START-AT POINT             
         MVC   SVKEY(3),KEY         SAVE FIRST THREE BYTES                      
         GOTO1 HIGH                 TRY TO READ THAT RECORD                     
         CLC   SVKEY(3),KEY         DID WE FIND OUR RECORD?                     
         BNE   ERRREC               NO...ERROR                                  
         DROP  R4                                                               
                                                                                
         USING SPAGYD,R4            BUILD THE KEY                               
VK10     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   AGYKTYPE,AGYKTYPQ    X'06'                                       
         MVC   AGYKAGY,AGYAGEN      2 BYTE AGENCY CODE FROM SCREEN              
         MVC   SVKEY(3),KEY         SAVE FIRST THREE BYTES                      
         DROP  R4                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       L     R6,AIO                                                           
         XC    FLAG,FLAG                                                        
         XC    TEMPLOCK,TEMPLOCK    FOR LOCK DATE                               
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    VR0                  YES                                         
                                                                                
         MVI   ELCODE,X'01'         ELEMENT X'01'                               
         BAS   RE,GETEL             THIS ELEMENT IS REQUIRED                    
         BE    VR00                                                             
         DC    H'0'                                                             
VR0      LA    R6,ELEM              256 BYTE FIELD TO HOLD TEMP ELEMENT         
         XC    ELEM,ELEM                                                        
         USING AGYEL,R6                                                         
VR00     MVI   AGYEL,X'01'          MOVE THE ELEMENT CODE IN                    
         MVI   AGYELEN,X'7E'        ELEMENT LENGTH X'7E'                        
*                                                                               
VR1      LA    R2,AGYSNMEH          AGENCY NAME HEADER                          
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR2                  YES...SKIP                                  
         GOTO1 ANY                                                              
         MVC   AGYNAME,8(R2)        MOVE AGY NAME FROM SCREEN TO RECORD         
                                                                                
VR2      LA    R2,AGYSADDH                                                      
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR3                  YES...SKIP                                  
         GOTO1 ANY                                                              
         MVC   AGYADDR,8(R2)                                                    
*                                                                               
VR3      LA    R2,AGYSIDH                                                       
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR4                  YES...SKIP                                  
         GOTO1 ANY                                                              
         CLI   5(R2),2              IS THE ID AT LEAST 2 CHARS LONG?            
         BL    ERRLT2               NO...LESS THAN 2 ERROR                      
         MVC   AGYID,8(R2)                                                      
*                                                                               
VR4      LA    R2,AGYALPHH          R2 PTR TO AGENCY ALPHA HEADER               
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    VR4A                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR5                  YES...SKIP                                  
         MVC   8(2,R2),AGYPALPH     CHANGE IT BACK ON THE SCREEN                
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
         OI    6(R2),X'80'          TRANSMIT                                    
         B     ERRCHG               "MAY NOT CHANGE THIS FIELD"                 
VR4A     GOTO1 ANY                                                              
         CLI   8(R2),X'40'          DID USER BEGIN INPUT WITH SPACE?            
         BE    ERRSPA               YES...ERROR                                 
         CLI   9(R2),X'40'          IS SECOND POSITION BLANK?                   
         BE    ERRSPA               YES...ERROR                                 
         MVC   AGYPALPH,8(R2)       AGENCY ALPHA                                
*                                                                               
VR5      LA    R2,AGYHEXH                                                       
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    VR5A                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR5A                 YES                                         
         MVC   8(1,R2),AGYPAHEX     CHANGE IT BACK ON THE SCREEN                
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
         OI    6(R2),X'80'          TRANSMIT                                    
         B     ERRCHG               NOCHANGE ERROR                              
*                                                                               
VR5A     LA    R3,AGYTABL                                                       
         BAS   RE,TABLOOK2          LOOK UP AGENCY CODE                         
         BNE   ERRAGY               INVALID AGENCY HEX                          
         MVC   BYTE2,BYTE           SAVE AGENCY CODE IN BYTE2                   
*                                                                               
         CLI   8(R2),C'0'           IS AGENCY HEX A 0?                          
         BNE   VR5F                 NO                                          
***      CLI   SVKEY+1,C'Z'         SPECIAL DDS AGYHEADERS?                     
***      BE    VR5F                 YES                                         
         XC    CTKEY,CTKEY          CLEAR THE CTFILE KEY                        
         LA    R5,CTKEY             CTFILE KEY                                  
         USING CT5REC,R5            SYSTEM ACCESS RECORD DSECT                  
         MVI   CT5KTYP,CT5KTYPQ     RECORD TYPE '5'                             
         MVC   CT5KALPH,SVKEY+1     ALPHA AGENCY                                
         L     R5,AIO2              READ ACCESS RECORD INTO AIO2                
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',CTKEY,(R5)                
         CLI   8(R1),0              ERRORS?                                     
         BE    *+6                  NO                                          
         DC    H'0'                 YES - DEATH                                 
*                                                                               
         LA    R5,28(R5)            A(FIRST ELEMENT)                            
         XR    RE,RE                CLEAR RE                                    
*                                                                               
VR5B     CLI   0(R5),0              END OF RECORD?                              
         BNE   *+6                  NO                                          
         DC    H'0'                 YES - DEATH                                 
         CLI   0(R5),CTAADELQ       X'B9' ELEMENT?                              
         BE    VR5C                 YES - TEST CROSSFILE                        
         IC    RE,1(R5)             ELEMENT LENGTH                              
         AR    R5,RE                BUMP TO NEXT ELEMENT                        
         B     VR5B                 TEST NEXT ELEMENT                           
*                                                                               
         USING CTAADD,R5            ACCESS DETAILS ELEMENT DSECT                
VR5C     CLI   CTAACSTN,X'FE'       CROSS FILE ID?                              
         BNE   ERRDHX               NO - 0 IS INVALID                           
         DROP  R5                   DROP USING                                  
*                                                                               
VR5F     MVC   AGYPAHEX,8(R2)       AGENCY HEX                                  
*                                                                               
VR6      LA    R2,AGYSACCH                                                      
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR7                  YES                                         
         GOTO1 ANY                                                              
         MVC   AGYACCT,8(R2)                                                    
*                                                                               
VR7      CLI   SCRNFLAG,0           DID EVERYTHING FIT ON DISPLAY?              
         BE    VR7A                 YES - SO CONTINUE                           
*                                                                               
         MVC   AGYOPTS,SPACES       ELSE - BLANK OUT THE FIELD                  
         MVC   AGYOPTS,=C'CANNOT CHANGE OPTIONS-CONTACT DDS'                    
         OI    AGYOPTSH+6,X'80'     TRANSMIT                                    
         B     VR7B                 AND SKIP THE OPTIONS                        
*                                                                               
VR7A     BAS   RE,EDOPTS            EDIT ALL OPTIONS & SET FLAGS                
         BNE   ERRSYN               BAD OPT SYNTAX FROM SCANNER                 
*                                                                               
VR7B     LA    R2,AGYRATSH                                                      
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR8                  YES                                         
         GOTO1 ANY                                                              
         LA    R3,RATSTABL          R3 POINTER TO TABLE                         
         BAS   RE,TABLOOK1          MAKE SURE DATA IS IN TABLE                  
         BNE   ERRRAT                                                           
         MVC   AGYPRATS,8(R2)                                                   
*                                                                               
VR8      LA    R2,AGYCLRGH          RATING SERVICE                              
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR9                  YES                                         
         GOTO1 ANY                                                              
         LA    R3,CLRGTABL                                                      
         BAS   RE,TABLOOK1                                                      
         BNE   ERRCLR                                                           
         MVC   AGYPCLIR,8(R2)       CLIENT REGIONS                              
*                                                                               
VR9      LA    R2,AGYBLPCH                                                      
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR10                 YES                                         
         GOTO1 ANY                                                              
         TM    4(R2),X'08'          VALID NUMERIC?                              
         BZ    ERRNNM               NO...NOT NUM ERR                            
         CLI   5(R2),2              BILLING PERCENTAGE 2 DIGITS?                
         BNE   ERRBLG               NOT 2 ERR                                   
         MVC   AGYPBPCT,8(R2)       BILLING PERCENTAGES                         
*                                                                               
VR10     LA    R2,AGYEXDMH                                                      
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR11                 YES                                         
         GOTO1 ANY                                                              
         LA    R3,EXDMTABL                                                      
         BAS   RE,TABLOOK1                                                      
         BNE   ERREXD                                                           
         MVC   AGYPEDEM,8(R2)       EXTENDED DEMOS                              
*                                                                               
VR11     LA    R2,AGYBOTOH                                                      
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR12                 YES                                         
         GOTO1 ANY                                                              
         LA    R3,BOTOTABL                                                      
         BAS   RE,TABLOOK1                                                      
         BNE   ERRBOT                                                           
         MVC   AGYPBOTO,8(R2)       BUY PERIOD'S OTO'S                          
*                                                                               
VR12     LA    R2,AGYCANH                                                       
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    VR12A                YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR13                 YES                                         
         CLI   AGYPCNDA,C'C'        CANADA STORED IN RECORD?                    
         BNE   VR12A                NO                                          
         MVC   8(1,R2),AGYPCNDA     CANADIAN RECORDS CANNOT BE CHANGED          
         OI    6(R2),X'80'          MOVE A 'C' TO THE SCREEN AND                
         OI    4(R2),X'20'          GIVE AN ERROR MESSAGE                       
         B     ERRCHG               NO CHANGES ALLOWED                          
VR12A    GOTO1 ANY                                                              
         LA    R3,CANTABL                                                       
         BAS   RE,TABLOOK1                                                      
         BNE   ERRCAN                                                           
         MVC   AGYPCNDA,8(R2)       CANADA                                      
*                                                                               
VR13     LA    R2,AGYTMSHH                                                      
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR14                 YES                                         
         GOTO1 ANY                                                              
         LA    R3,NWTATABL                                                      
         BAS   RE,TABLOOK1                                                      
         BNE   ERRPTS                                                           
         MVC   AGYPOPTS,8(R2)       OLD POOL TIME SHEETS                        
*                                                                               
VR14     LA    R2,AGYBILLH                                                      
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR14E                YES                                         
         GOTO1 ANY                                                              
         LA    R3,BILLTABL                                                      
         BAS   RE,TABLOOK1                                                      
         BNE   ERRBIL                                                           
         MVC   AGYPBLNG,8(R2)       BILLING                                     
*                                                                               
VR14E    LA    R2,AGYBIRQH                                                      
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BE    *+12                 YES                                         
         TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR14F                YES                                         
         GOTO1 ANY                                                              
         LA    R3,BIDTABL                                                       
         BAS   RE,TABLOOK1                                                      
         BNE   ERRBID                                                           
         MVC   AGYPBREQ,8(R2)       BILLING REQUIRED                            
*                                                                               
VR14F    MVI   AGYPCBLM,C'0'        MOVE A 0 IN CREDIT BUY LIMIT                
         LA    R2,AGYCBLH                                                       
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR14H                NO                                          
         TM    4(R2),X'08'          DOES FIELD CONTAIN NUMERIC DATA?            
         BZ    ERRNNM               NO...NOT NUM ERR                            
         MVC   AGYPCBLM,8(R2)       CREDIT BUY LIMIT                            
*                                                                               
VR14H    LA    R2,AGYRPOLH                                                      
         MVI   AGYPBPLR,C'0'        MOVE A 0 IN BRAND POOL RADIO                
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR14H6               NO                                          
         CLI   8(R2),C'Y'           IS IT A Y?                                  
         BE    VR14H2               YES                                         
         CLI   8(R2),C'0'           IS IT A 0?                                  
         BNE   ERRBPR               NO...ERROR                                  
VR14H2   MVC   AGYPBPLR,8(R2)       BRAND POOL RADIO                            
*                                                                               
VR14H6   LA    R2,AGYXOTOH                                                      
         MVI   AGYPSOCD,C'N'        MOVE AN N IN SPECIAL OTO CODE               
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR14H9               NO                                          
         CLI   8(R2),C' '           ALLOW ANY CHARACTER                         
         BE    ERRSPA               NO SPACES ALLOWED ERROR                     
         CLI   8(R2),C'+'                                                       
         BE    ERRINV               NO + SIGN ALLOWED ERROR                     
         CLI   8(R2),C'-'                                                       
         BE    ERRINV               NO - SIGN ALLOWED ERROR                     
*                                                                               
VR14H8   MVC   AGYPSOCD,8(R2)       SPECIAL OTO CODE                            
*                                                                               
VR14H9   LA    R2,AGYMGDMH                                                      
         MVI   AGYPMGMM,C'N'        MOVE AN N IN MAKEGOODS                      
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR14HE               NO                                          
         CLI   8(R2),C'Y'           IS IT A Y?                                  
         BE    VR14HA               YES                                         
         CLI   8(R2),C'N'           IS IT AN N?                                 
         BNE   ERRMKG               NO...ERROR                                  
VR14HA   MVC   AGYPMGMM,8(R2)       MAKEGOODS IN MISSED MONTHS                  
*                                                                               
VR14HE   LA    R2,AGYSAUTH                                                      
         MVI   AGYPSAUT,C'N'        MOVE AN N IN -S AUTH REQUIRED               
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR14HG               NO                                          
         CLI   8(R2),C'Y'           IS IT A Y?                                  
         BE    VR14HF               YES                                         
         CLI   8(R2),C'N'           IS IT AN N?                                 
         BNE   ERRSAR               NO...ERROR                                  
VR14HF   MVC   AGYPSAUT,8(R2)       -S AUTH REQUIRED                            
*                                                                               
VR14HG   LA    R2,AGYOFRQH                                                      
         MVI   AGYPOREQ,C'N'        MOVE AN N IN OFFICE REQUIRED                
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR14I                NO                                          
         CLI   8(R2),C'Y'           IS IT A Y?                                  
         BE    VR14HH               YES                                         
         CLI   8(R2),C'N'           IS IT AN N?                                 
         BNE   ERROFR               NO...ERROR                                  
VR14HH   MVC   AGYPOREQ,8(R2)       OFFICE REQUIRED                             
*                                                                               
VR14I    LA    R2,AGYBYBLH                                                      
         MVI   AGYPROF+14,C'N'      MOVE AN N IN BUYER/BILLER                   
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR14IO               NO                                          
         CLI   8(R2),C'Y'           IS IT A Y?                                  
         BE    VR14II               YES                                         
         CLI   8(R2),C'N'           IS IT AN N?                                 
         BNE   ERRBYB               NO...ERROR                                  
VR14II   MVC   AGYPROF+14(1),8(R2)  BUYER/BILLER                                
*                                                                               
VR14IO   LA    R2,AGYACOCH                                                      
         MVI   AGYOFC2,C'N'         MOVE AN N INTO ACC OFFICE CODE              
         CLI   8(R2),C'Y'           IS A Y ON THE SCREEN?                       
         BNE   *+8                  NO                                          
         MVI   AGYOFC2,C'Y'         2 CHARACTER OFFICE CODE REQUIRED            
         CLI   ACTEQU,ACTADD        IS THE ACTION ADD?                          
         BNE   VR14J                NO                                          
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
*ELEMENT X'70'                                                                  
*                                                                               
VR14J    LR    R5,R6                SAVE ELEMENT 1                              
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'         ELEMENT X'70'                               
         GOTO1 REMELEM              THIS ELEMENT IS NOT REQUIRED                
         LA    R6,ELEM              256 BYTE FIELD TO HOLD TEMP ELEMENT         
         XC    ELEM,ELEM                                                        
         USING AGYIDEL,R6           ID TITLE ELEMENT                            
         MVI   AGYIDEL,AGYIDELQ     ELEMENT X'70'                               
         MVI   AGYIDLN,AGYIDLNQ     LENGTH=12                                   
         CLI   AGYTTLEH+5,0         ANY INPUT?                                  
         BE    VR14K                NO                                          
         MVC   AGYTITLE,AGYTTLE     MOVE TO RECORD IO                           
         OC    AGYTITLE,SPACES      PAD NULLS WITH BLANKS                       
         GOTO1 ADDELEM              ADD IT TO THE RECORD                        
         B     VR14Q                                                            
*                                                                               
         USING AGYEL,R5                                                         
VR14K    CLI   AGYPBREQ,C'Y'        IS BILLING REQUIRED?                        
         BE    ERRIDB               YES THEN ID TITLE IS ALSO REQUIRED          
         CLI   AGYPBREQ,C'A'        IS BILLING REQUIRED?                        
         BE    ERRIDB               YES THEN ID TITLE IS ALSO REQUIRED          
         DROP  R5                                                               
*                                                                               
VR14Q    XC    WORK(2),WORK                                                     
         LA    R2,AGYRFPIH          RFP ID                                      
         CLI   5(R2),0              ANY INPUT?                                  
         BE    VR14T                NO...NEXT ELEMENT                           
*                                                                               
         MVC   WORK+2(10),8(R2)                                                 
         BAS   RE,VALID             VALIDATE ID                                 
         BNE   ERRRFP                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
*ELEMENT 71                                                                     
*                                                                               
VR14T    OC    WORK(2),WORK         DO WE EVEN NEED TO ADD ELEM                 
         BNZ   *+14                 YES...MUST ADD SOMETHING                    
         OC    TEMPLOCK,TEMPLOCK    DO WE NEED TO ADD LOCK DATE?                
         BZ    VRACC                NO...NO ELEMENT NEEDED                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'71'         ELEMENT X'71'                               
         GOTO1 REMELEM                                                          
         LA    R6,ELEM              256 BYTE FIELD TO HOLD TEMP ELEMENT         
         XC    ELEM,ELEM                                                        
         USING AGYEXTEL,R6          ID TITLE ELEMENT                            
         MVI   AGYEXTEL,AGYEXTEQ    ELEMENT X'71'                               
         MVI   AGYEXTLN,AGYEXTLQ    LENGTH=32                                   
         MVC   AGYPRNID,WORK        PRINCIPAL ID FOR RFP                        
         MVC   AGYLOCK,TEMPLOCK     MOVE THE LOCK DATE                          
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
*ELEMENT X'03'                                                                  
*                                                                               
VRACC    L     R6,AIO                                                           
         MVI   ELCODE,X'03'         ELEMENT X'03'                               
         GOTO1 REMELEM              THIS ELEMENT IS NOT REQUIRED                
         LA    R6,ELEM              256 BYTE FIELD TO HOLD TEMP ELEMENT         
         XC    ELEM,ELEM                                                        
         USING AGYACCEL,R6          ID TITLE ELEMENT                            
         MVI   AGYACCEL,AGYACCEQ    ELEMENT X'03'                               
         MVI   AGYACCLN,AGYACCLQ    LENGTH=26                                   
         CLI   AGYACCH+5,0          NO INPUT                                    
         BE    VR15                 MISSING...NO ELEMENT 3                      
         LA    R3,AGYACC            ON SCREEN                                   
         LA    R4,AGYACCAG          IN RECORD                                   
VRACCLP  CLI   0(R3),C' '           BLANK ENTRY?                                
         BNH   VRACC1               YES...GO UPDATE RECORD                      
         MVC   0(2,R4),0(R3)        MOVE 2 BYTES FROM SCREEN TO RECORD          
         LA    R3,3(R3)             BUMP SCREEN PTR BY 3                        
         LA    R4,2(R4)             BUMP RECORD PTR BY 2                        
         B     VRACCLP                                                          
VRACC1   GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
*ELEMENT X'02' (1 OR MORE ELEMENTS REQUIRED)                                    
*                                                                               
VR15     LA    R2,AGYMEDH           HEADER                                      
         L     R6,AIO                                                           
         USING AGYMEDEL,R6          ID TITLE ELEMENT                            
         XC    MEDCOUNT,MEDCOUNT    CLEAR COUNTER                               
         CLI   ACTEQU,ACTADD        ACTION ADD?                                 
         BE    VR15A                YES                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL             ELEMENT X'02' EXISTS?                       
         BE    VR15A1               YES                                         
         DC    H'0'                 NO...THIS ELEMENT IS REQUIRED               
VR15A    CLI   5(R2),0              ANY INPUT?                                  
         BE    ENDCHK               NO...DONE                                   
         CLI   ACTEQU,ACTADD        ACTION ADD?                                 
         BE    VR1500               YES                                         
         TM    FLAG,X'80'           ADD NEW ELEMENT?                            
         BO    VR1500               YES                                         
         BAS   RE,NEXTEL                                                        
         BE    VR15A1                                                           
         OI    FLAG,X'80'                                                       
         B     VR1500                                                           
VR15A1   TM    4(R2),X'20'          PREVIOUSLY VALIDATED?                       
         BO    VR1501               YES                                         
         MVC   8(1,R2),AGYMEDCD     CHANGE IT BACK ON THE SCREEN                
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
         OI    6(R2),X'80'          TRANSMIT                                    
         BNO   ERRCHG               NO...CHANGED DATA...ERROR                   
VR1500   LA    R6,ELEM              256 BYTE FIELD TO HOLD TEMP ELEMENT         
         XC    ELEM,ELEM                                                        
VR1501   MVI   AGYMEDEL,X'02'       ELEMENT X'02'                               
         MVI   AGYMEDLN,X'22'       LENGTH=34                                   
         LA    R3,MEDTABL                                                       
         BAS   RE,TABLOOK2          CHECK MEDIA CODE ALLEN                      
         BNE   ERRMED                                                           
         MVC   AGYMEDBT,BYTE        CREATE AGY/MEDIA CODE                       
         OC    AGYMEDBT,BYTE2                                                   
         MVC   AGYMEDCD,8(R2)                                                   
VR15C    BAS   R1,VRMD              EDIT MEDIA DESC                             
         BAS   R1,VRVD              DO MED TYPE AND VENDOR                      
         CLI   ACTEQU,ACTADD                                                    
         BE    *+12                                                             
         CLI   FLAG,X'80'           ACTION ADD/ADD NEW ELEM?                    
         BNE   VR15D                                                            
         GOTO1 ADDELEM                                                          
*                                                                               
VR15D    ZIC   R3,MEDCOUNT                                                      
         AHI   R3,1                 COUNT MEDIA                                 
         STC   R3,MEDCOUNT                                                      
         CLI   MEDCOUNT,5           LAST MEDIA - ARE WE DONE                    
         BE    VRX                                                              
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                ADVANCE SCREEN PTR                          
         B     VR15A                                                            
*                                                                               
ENDCHK   CLI   MEDCOUNT,1           MUST BE AT LEAST 1                          
         BL    ERRMIS                                                           
ENDCHK2  ZIC   R3,0(R2)             ADVANCE SCREEN                              
         AR    R2,R3                                                            
         CLI   5(R2),0              ANYTHING THERE                              
         BNE   ERRINV                                                           
         ZIC   R3,0(R2)             CHECK VENDOR SLOT                           
         AR    R2,R3                                                            
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
         ZIC   R3,MEDCOUNT                                                      
         LA    R3,1(R3)             GET NEXT MEDIA LINE                         
         STC   R3,MEDCOUNT                                                      
         CLI   MEDCOUNT,5           FINISHED                                    
         BE    VRX                                                              
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         CLI   5(R2),0              IS ANYTHING THERE                           
         BNE   ERRINV                                                           
         B     ENDCHK2              LOOP TILL ALL MEDIA CHKED                   
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
VRX      B     DR                   RE-DISPLAY RECORD                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       BAS   RE,CLRSCRN           CLEAR THE SCREEN                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'         ELEMENT X'01'                               
         BAS   RE,GETEL             GET FIRST ELEMENT                           
         BE    *+6                  THIS ELEMENT IS REQUIRED                    
         DC    H'0'                                                             
         USING AGYEL,R6                                                         
         LA    R2,AGYSNMEH                                                      
         MVC   AGYSNME,AGYNAME      MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYSADDH                                                      
         MVC   AGYSADD,AGYADDR      MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYSIDH                                                       
         MVC   AGYSID,AGYID         MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYALPHH                                                      
         MVC   AGYALPH,AGYPALPH     MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYHEXH                                                       
         MVC   AGYHEX,AGYPAHEX      MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYSACCH                                                      
         MVC   AGYSACC,AGYACCT      MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
* FORMAT FREEFORM OPTIONS                                                       
*                                                                               
         XC    SCRNFLAG,SCRNFLAG    INITIALIZE THE FLAG                         
         LA    R2,AGYOPTSH                                                      
         XC    8(L'AGYOPTS,R2),8(R2) CLEAR 62 BYTES ON SCREEN                   
         OI    6(R2),X'80'          TRANSMIT                                    
         LA    R4,8(R2)             POINT TO FIELD AGYOPTS                      
         TM    AGYFLAG1,X'80'       IS ADDS INTERFACE SUPPORTED?                
         BZ    DR2A                 NO                                          
*                                                                               
         LA    R0,7                 LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2A                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(7,R4),=C'ADDS=Y,'                                              
         LA    R4,7(R4)                                                         
DR2A     TM    AGYFLAG1,X'40'       SPECIAL MEDIA NAMES?                        
         BZ    DR2B                 NO                                          
         LA    R0,10                LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2B                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(10,R4),=C'MEDNAME=Y,'                                          
         LA    R4,10(R4)                                                        
DR2B     TM    AGYFLAG1,X'20'       CONTRACT ANALYSIS REQUIRED?                 
         BZ    DR2C                 NO                                          
*                                                                               
         LA    R0,6                 LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2C                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(6,R4),=C'CTA=Y,'                                               
         LA    R4,6(R4)                                                         
DR2C     CLC   AGYCTAGY,=C'  '      CTFILE AGENCY CODE BLANK?                   
         BNH   DR2D                 YES                                         
*                                                                               
         LA    R0,10                LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2D                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(10,R4),=C'CTFILE=XX,'                                          
         MVC   7(2,R4),AGYCTAGY                                                 
         LA    R4,10(R4)                                                        
DR2D     TM    AGYFLAG1,X'10'       OFF=HEX OPTION?                             
         BZ    DR2E                 NO                                          
*                                                                               
         LA    R0,8                 LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2E                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(8,R4),=C'OFF=HEX,'                                             
         LA    R4,8(R4)                                                         
DR2E     TM    AGYFLAG1,X'08'       TEST AGENCY FLAG?                           
         BZ    DR2F                 NO                                          
*                                                                               
         LA    R0,7                 LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2F                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(7,R4),=C'TEST=Y,'                                              
         LA    R4,7(R4)                                                         
*                                                                               
DR2F     TM    AGYFLAG1,AGYPRDQ     TEST DDS BILLING BY PRODUCT?                
         BZ    DR2G                 NO                                          
*                                                                               
         LA    R0,9                 LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2G                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(8,R4),=C'DDSB=PRD'                                             
         MVI   8(R4),C','                                                       
         LA    R4,9(R4)                                                         
*                                                                               
DR2G     TM    AGYFLAG2,AGYFLAG2_PW ALLOW PW IN CLT/EST RECORDS?                
         BZ    DR2K                 NO                                          
         MVC   0(3,R4),=C'PW,'                                                  
         LA    R4,3(R4)                                                         
         DROP  R6                                                               
*                                                                               
* MUST GET ELEMENT X'71' BUT STILL USE ELEM1 TO CHECK OPTIONS AFTER             
*                                                                               
DR2K     LR    R5,R6                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'71'         ELEMENT X'01'                               
         BAS   RE,GETEL             GET FIRST ELEMENT                           
         BNE   DR2L                 THIS ELEMENT IS NOT REQUIRED                
         USING AGYEXTEL,R6                                                      
         OC    AGYLOCK,AGYLOCK      IS LOCKED BUY DATE NULLS?                   
         BZ    DR2L                 YES                                         
*                                                                               
         LA    R0,14                LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2L                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(5,R4),=C'LOCK='                                                
         GOTO1 DATCON,DMCB,(3,AGYLOCK),(5,5(R4))                                
         MVI   13(R4),C','                                                      
         LA    R4,14(R4)                                                        
         DROP  R6                                                               
*                                                                               
*RESTORE FIRST ELEMENT                                                          
*                                                                               
DR2L     LR    R6,R5                RESTORE PTR TO ELEMENT X'01'                
         USING AGYEL,R6                                                         
         TM    AGYFLAG1,AGYTRDQ     TRADE AGENCY?                               
         BZ    DR2M                 NO - SO CONTINUE                            
*                                                                               
         LA    R0,4                 LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2M                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(4,R4),=C'TRD,'     ELSE - MOVE OUT LITERAL                     
         LA    R4,4(R4)             INC FIELD POSITION                          
*                                                                               
DR2M     TM    AGYFLAG1,AGYCOS2Q    COST FACTOR REQUIRED?                       
         BZ    DR2N                 NO - SO CONTINUE                            
*                                                                               
         LA    R0,4                 LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2N                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(5,R4),=C'COS2,'    ELSE - MOVE OUT LITERAL                     
         LA    R4,5(R4)             INC FIELD POSITION                          
*                                                                               
DR2N     TM    AGYFLAG2,AGYFLAG2_UID   TEST UIDS PROCESSED                      
         BZ    DR2P                 NO                                          
*                                                                               
         LA    R0,3                 LENGTH OF OUTPUT DATA                       
         BAS   RE,CHECKOPT          WILL IT FIT IN FIELD?                       
         BNZ   DR2P                 NO - SO CONTINUE                            
*                                                                               
         MVC   0(4,R4),=C'UID,'                                                 
         AHI   R4,4                                                             
*                                                                               
DR2P     TM    AGYFLAG2,AGYFLAG2_PBD   TEST ADJUST POSTBUY DEMOS                
         BZ    DR2X                                                             
         LHI   R0,6                                                             
         BAS   RE,CHECKOPT                                                      
         BNZ   DR2X                                                             
         MVC   0(6,R4),=C'PBDADJ'                                               
         AHI   R4,6                                                             
*                                                                               
DR2X     CLI   SCRNFLAG,0           DID EVERYTHING FIT?                         
         BE    DR2XX                YES - SO CONTINUE                           
*                                                                               
         MVI   0(R1),C'*'           ELSE - MOVE OUT 'DIDN'T FIT' FLAG           
         LA    R1,1(R1)             INC A(FIELD POSITION)                       
*                                                                               
DR2XX    BCTR  R4,0                 MOVE BACK ONE POSITION ON SCREEN            
         CLI   0(R4),C','           IS IT A COMMA?                              
         BNE   DR10                 NO                                          
         MVI   0(R4),0              REMOVE LAST COMMA                           
*                                                                               
DR10     LA    R2,AGYRATSH                                                      
         MVC   AGYRATS,AGYPRATS     MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYCLRGH                                                      
         MVC   AGYCLRG,AGYPCLIR     MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYBLPCH                                                      
         MVC   AGYBLPC,AGYPBPCT     MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYEXDMH                                                      
         MVC   AGYEXDM,AGYPEDEM     MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYBOTOH                                                      
         MVC   AGYBOTO,AGYPBOTO     MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYCANH                                                       
         MVC   AGYCAN,AGYPCNDA      MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYTMSHH                                                      
         MVC   AGYTMSH,AGYPOPTS     MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYBILLH                                                      
         MVC   AGYBILL,AGYPBLNG     MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         LA    R2,AGYCBLH                                                       
         MVC   AGYCBL,AGYPCBLM      MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         CLI   AGYPBREQ,C'Y'        BILLING REQUIRED?                           
         BE    DRBID                YES                                         
         CLI   AGYPBREQ,C'A'        BILLING REQUIRED?                           
         BE    DRBID                YES                                         
         MVI   AGYPBREQ,C'N'        MOVE AN 'N' INTO BILLING REQUIRED           
DRBID    LA    R2,AGYBIRQH                                                      
         MVC   AGYBIRQ,AGYPBREQ     MOVE FROM RECORD TO SCREEN                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
*                                                                               
         CLI   AGYPBPLR,C'Y'        BRAND POOL RADIO?                           
         BE    *+8                  YES                                         
         MVI   AGYPBPLR,C'0'        NO...MOVE A ZERO IN                         
         MVC   AGYRPOL,AGYPBPLR     MOVE FROM RECORD TO SCREEN                  
         OI    AGYRPOLH+6,X'80'     TRANSMIT                                    
*                                                                               
         MVC   AGYXOTO,AGYPSOCD     MOVE FROM RECORD TO SCREEN                  
         OI    AGYXOTOH+6,X'80'     TRANSMIT                                    
*                                                                               
         CLI   AGYPMGMM,C'Y'        MAKEGOODS IN MISSED MONTHS?                 
         BE    *+8                  YES                                         
         MVI   AGYPMGMM,C'N'        NO...MOVE AN 'N' IN                         
         MVC   AGYMGDM,AGYPMGMM     MOVE FROM RECORD TO SCREEN                  
         OI    AGYMGDMH+6,X'80'     TRANSMIT                                    
*                                                                               
         CLI   AGYPSAUT,C'Y'        -S AUTH REQUIRED?                           
         BE    *+8                  YES                                         
         MVI   AGYPSAUT,C'N'        NO...MOVE AN 'N' IN                         
         MVC   AGYSAUT,AGYPSAUT     MOVE FROM RECORD TO SCREEN                  
         OI    AGYSAUTH+6,X'80'     TRANSMIT                                    
*                                                                               
         CLI   AGYPOREQ,C'Y'        OFFICE REQUIRED?                            
         BE    *+8                  YES                                         
         MVI   AGYPOREQ,C'N'        NO...MOVE AN 'N' IN                         
         MVC   AGYOFRQ,AGYPOREQ     MOVE FROM RECORD TO SCREEN                  
         OI    AGYOFRQH+6,X'80'     TRANSMIT                                    
*                                                                               
         CLI   AGYPBYBR,C'Y'        BUYER/BILLER?                               
         BE    *+8                  YES                                         
         MVI   AGYPBYBR,C'N'        NO...MOVE AN 'N' IN                         
         MVC   AGYBYBL,AGYPBYBR     MOVE FROM RECORD TO SCREEN                  
         OI    AGYBYBLH+6,X'80'     TRANSMIT                                    
*                                                                               
         LA    R2,AGYACOCH          ACC OFFICE CODE                             
         MVC   AGYACOC,AGYOFC2                                                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
         DROP  R6                                                               
*                                                                               
* ELEMENT X'70'                                                                 
*                                                                               
DRID     XC    AGYTTLE,AGYTTLE      CLEAR FIELD                                 
         OI    AGYTTLEH+6,X'80'     TRANSMIT                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'         ELEMENT X'70'                               
         BAS   RE,GETEL             GET FIRST ELEMENT                           
         BNE   DRRFPID              THIS ELEMENT IS NOT REQUIRED                
         USING AGYIDEL,R6                                                       
         MVC   AGYTTLE,AGYTITLE                                                 
         DROP  R6                                                               
*                                                                               
DRRFPID  XC    AGYRFPI,AGYRFPI      CLEAR FIELD                                 
         OI    AGYRFPIH+6,X'80'     TRANSMIT                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'71'         ELEMENT X'71'                               
         BAS   RE,GETEL             GET FIRST ELEMENT                           
         BNE   DRACC1               THIS ELEMENT IS NOT REQUIRED                
         USING AGYEXTEL,R6                                                      
         OC    AGYPRNID,AGYPRNID    PRINCIPAL ID NULLS?                         
         BZ    DRACC1               YES                                         
         MVC   TEMPID,AGYPRNID      PASS TO DISPID                              
         BAS   RE,DISPID            DISPLAY ID                                  
         DROP  R6                                                               
*                                                                               
DRACC1   LA    R2,AGYACCH           ACC AGY CODE LIST                           
         XC    AGYACC,AGYACC                                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
         OI    6(R2),X'80'          TRANSMIT                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'         ELEMENT X'03'                               
         BAS   RE,GETEL             GET FIRST ELEMENT                           
         BNE   DRACCX               THIS ELEMENT IS NOT REQUIRED                
         USING AGYACCEL,R6                                                      
         LA    R2,8(R2)             POINT TO AGYACC                             
         LA    R3,AGYACCAG                                                      
         LA    R1,8                 MAX 8 ACC ENTRIES IN RECORD                 
         CLI   0(R3),C' '           ENTRY A SPACE?                              
         BNH   DRACCX               YES                                         
DRACC    MVC   0(2,R2),0(R3)        MOVE 2 CHARS TO SCREEN                      
         LA    R3,2(R3)             BUMP TO NEXT ONE IN RECORD                  
         CLI   0(R3),C' '           SPACE?                                      
         BNH   DRACCX               YES                                         
         LA    R2,2(R2)             BUMP TO NEXT POSITION ON SCREEN             
         MVI   0(R2),C','           BUT A COMMA THERE                           
         LA    R2,1(R2)             BUMP PAST THE COMMA                         
         BCT   R1,DRACC                                                         
DRACCX   DS    0H                                                               
         DROP  R6                                                               
*                                                                               
* ELEMENT X'02'                                                                 
*                                                                               
DRID2    LA    R2,AGYMEDH                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'         ELEMENT X'02'                               
         BAS   RE,GETEL             GET FIRST ELEMENT                           
         BNE   DRX                  THIS ELEMENT IS NOT REQUIRED                
         B     DR00                                                             
DR0      BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
         USING AGYMEDEL,R6                                                      
DR00     MVC   8(1,R2),AGYMEDCD     MEDIA CODE                                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
         ZIC   R3,0(R2)             BUMP TO NEXT FIELD                          
         AR    R2,R3                                                            
         MVC   8(10,R2),AGYMEDEX    MEDIA TYPE                                  
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
         ZIC   R3,0(R2)             BUMP TO NEXT FIELD                          
         AR    R2,R3                                                            
         MVC   8(7,R2),AGYVENEX     VENDOR                                      
         OI    6(R2),X'80'          TRANSMIT                                    
         OI    4(R2),X'20'          PREVIOUSLY VALIDATED                        
         ZIC   R3,0(R2)             BUMP                                        
         AR    R2,R3                                                            
         B     DR0                                                              
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                            DK                                       *         
***********************************************************************         
DK       DS    0X                                                               
         L     R4,AIO                                                           
         USING SPAGYD,R4                                                        
         MVC   AGYAGEN,AGYKAGY      DISP AGY CODE ON MAINTENACE SCREEN          
         OI    AGYAGENH+6,X'80'     TRANSMIT                                    
         DROP  R4                                                               
DKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                            LR                                       *         
***********************************************************************         
LR       LA    R4,KEY               R4 POINTER TO KEY                           
         MVI   NLISTS,14            14 DISPLAYS PER SCREEN                      
         OC    KEY,KEY              FIRST TIME THROUGH?                         
         BNZ   LR10                 NO                                          
                                                                                
         USING SPAGYD,R4                                                        
         MVI   AGYKTYPE,AGYKTYPQ    X'06'                                       
         MVC   AGYKAGY,TEMPACD      2 BYTE AGENCY CODE FROM SCREEN              
         MVC   SAVEKEY,KEY          SAVE THE KEY                                
LR10     GOTO1 HIGH                 FIRST RECORD                                
         CLI   DMCB+8,0             IS THERE AN ERROR?                          
         BE    LR30                 NO                                          
         DC    H'0'                 SHOULD NEVER BE AN ERROR ON HIGH            
*                                                                               
LR20     LA    R4,KEY               R4 POINTER TO KEY                           
         GOTO1 SEQ                  NEXT RECORD                                 
         CLI   DMCB+8,0             IS THERE AN ERROR?                          
         BE    LR30                 NO                                          
         DC    H'0'                 SHOULD NEVER BE AN ERROR ON SEQ             
*                                                                               
LR30     CLC   KEY(1),SAVEKEY       AGENCY RECORD?                              
         BNE   LRX                  NO MORE DATATYPES TO LIST                   
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0             DID WE GET THE RECORD?                      
         BE    *+6                  YES                                         
         DC    H'0'                 SHOULD ALWAYS GET A RECORD                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    LR35                                                             
         DC    H'0'                                                             
         USING AGYEL,R6                                                         
LR35     MVC   LSAGYID,AGYID        MOVE ID TO SCREEN                           
         MVI   LSAGYID+3,C'/'                                                   
         MVC   LSAGYNAM,AGYNAME     MOVE NAME TO SCREEN                         
         MVC   LSAGYPRF,AGYPROF     MOVE 20 BYTES OF PROFILE                    
         MVC   LSAGYREQ,AGYKAGY     MOVE AGENCY KEY TO SCREEN                   
         DROP  R6                                                               
         GOTO1 LISTMON              SEND RECORD TO SCREEN                       
         B     LR20                 NEXT RECORD                                 
                                                                                
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                          CHECKOPT                                   *         
***********************************************************************         
CHECKOPT NTR1                                                                   
         AR    R0,R4               A(NEW FIELD POSITION)                        
         LA    R3,AGYOPTS          A(SCREEN FIELD)                              
         LA    R3,L'AGYOPTS-1(R3)  A(LAST POSTITION IN FIELD)                   
         CR    R0,R3               WILL CURRENT OPTION FIT IN FIELD?            
         BH    CKOPERR             NO - SO ERROR                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
CKOPX    B     XIT                 RETURN TO CALLER                             
*                                                                               
CKOPERR  ZIC   R3,SCRNFLAG         SAVE OLD COUNT OF MISSED OPTS                
         LA    R3,1(R3)            INC COUNT                                    
         STC   R3,SCRNFLAG         STORE IT                                     
         LTR   RC,RC               SET ERROR CC                                 
         B     CKOPX               AND RETURN                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
*                          TABLOOK2                                   *         
***********************************************************************         
TABLOOK2 NTR1                                                                   
TAB2A    CLI   0(R3),0             END OF TABLE?                                
         BE    TAB2NO              YES...ERROR                                  
         CLC   0(1,R3),8(R2)       MATCH?                                       
         BE    TAB2B               YES                                          
         LA    R3,2(R3)            BUMP                                         
         B     TAB2A                                                            
TAB2B    MVC   BYTE,1(R3)          RETURN HEX CODE                              
         SR    R3,R3                                                            
TAB2NO   LTR   R3,R3                                                            
         B     XIT                                                              
***********************************************************************         
*                          TABLOOK1                                   *         
***********************************************************************         
TABLOOK1 NTR1                                                                   
TAB1A    CLI   0(R3),0             END OF TABLE?                                
         BE    TAB1NO              YES...ERROR                                  
         CLC   0(1,R3),8(R2)       MATCH?                                       
         BE    TAB1YES             YES                                          
         LA    R3,1(R3)            BUMP                                         
         B     TAB1A                                                            
TAB1YES  SR    R3,R3                                                            
TAB1NO   LTR   R3,R3                                                            
TAB1B    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                            VRMD                                     *         
***********************************************************************         
         USING AGYMEDEL,R6          ID TITLE ELEMENT                            
VRMD     ZIC   R3,0(R2)             MEDIA DESC.                                 
         AR    R2,R3                                                            
         GOTO1 ANY                                                              
         MVC   AGYMEDEX,8(R2)                                                   
VRMD2    BR    R1                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                            VRVD                                     *         
***********************************************************************         
         USING AGYMEDEL,R6          ID TITLE ELEMENT                            
VRVD     ZIC   R3,0(R2)             VENDOR DESC.                                
         AR    R2,R3                                                            
         GOTO1 ANY                                                              
         MVC   AGYVENEX,8(R2)                                                   
VRVD2    BR    R1                                                               
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*                         CLRSCRN                                     *         
***********************************************************************         
CLRSCRN  NTR1                                                                   
         LA    R2,AGYSNMEH          FIRST DATA ON SCREEN                        
         LA    R3,AGYTAB            CLEAR UP TO THIS FIELD                      
                                                                                
CLR10    TM    1(R2),X'20'          IS IT PROTECTED?                            
         BO    CLR20                YES...JUST BUMP                             
         ZIC   R1,0(R2)             HEADER + DATA                               
         SHI   R1,9                 SUBTRACT HEADER LENGTH +1                   
         TM    1(R2),X'02'          IS THERE AN EXTENDED FIELD HEADER?          
         BZ    *+8                  NO                                          
         SHI   R1,8                 SUBTRACT EXTENDED HEADER LENGTH             
*                                                                               
         EX    R1,*+8               PAD WITH BLANKS                             
         B     *+10                                                             
         MVC   8(0,R2),=33X'40'                                                 
         OI    6(R2),X'80'          TRANSMIT                                    
*                                                                               
CLR20    ZIC   R0,0(R2)             BUMP TO NEXT FIELD                          
         AR    R2,R0                                                            
         CR    R2,R3                IS END OF SCREEN?                           
         BL    CLR10                NO                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                             TEST YES                                *         
***********************************************************************         
TESTYES  NTR1                                                                   
         USING SCAND,R5                                                         
         CLI   FLD2LEN,3                                                        
         BH    ERRINV                                                           
         ZIC   R1,FLD2LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD2(0),=C'YES'                                                  
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*                             TEST NO                                 *         
***********************************************************************         
TESTNO   NTR1                                                                   
         USING SCAND,R5                                                         
         CLI   FLD2LEN,2                                                        
         BH    ERRINV                                                           
         ZIC   R1,FLD2LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD2(0),=C'NO'                                                   
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*                          VALID                                      *         
***********************************************************************         
VALID    NTR1                                                                   
         XC    ELEM,ELEM           VALIDATE ID AND GET ID NUMBER                
         LA    R3,ELEM                                                          
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKID,WORK+2       ID                                           
         OC    CTIKID,SPACES                                                    
         L     R6,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',ELEM,0(R6)                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ELEM(L'CTIKEY),0(R6)                                             
         BNE   VIDERR                                                           
         LA    R3,0(R6)                                                         
         LA    R3,CTIDATA          FIRST ELEM                                   
         DROP  R3                                                               
*                                                                               
VID10    CLI   0(R3),0             END OF RECORD?                               
         BE    VIDERR              YES...CAN'T FIND ID NUMBER                   
         CLI   0(R3),X'02'         X'02' ELEMENT?                               
         BE    VID20               YES                                          
         ZIC   R1,1(R3)            BUMP                                         
         AR    R3,R1                                                            
         B     VID10                                                            
VID20    MVC   WORK(2),2(R3)       SAVE ID NUMBER                               
         SR    R1,R1                                                            
VIDERR   LTR   R1,R1                                                            
         B     XIT                                                              
               EJECT                                                            
***********************************************************************         
*                           DISPID                                    *         
***********************************************************************         
DISPID   NTR1                                                                   
         XC    ELEM,ELEM           GET ID NAME FROM NUMBER                      
         LA    R3,ELEM                                                          
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKNUM,TEMPID                                                   
         L     R6,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',ELEM,0(R6)                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ELEM(L'CTIKEY),0(R6)                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         LA    R3,0(R6)                                                         
         LA    R3,CTIDATA          FIRST ELEM                                   
         DROP  R3                                                               
*                                                                               
DID10    CLI   0(R3),0                                                          
         BNE   *+6                 CAN'T FIND ID NAME                           
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    DID20                                                            
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DID10                                                            
DID20    MVC   AGYRFPI,2(R3)       DISPLAY ID                                   
         OI    AGYRFPIH+6,X'80'    TRANSMIT                                     
         XIT1                                                                   
***********************************************************************         
*                            EDOPTS                                   *         
***********************************************************************         
EDOPTS   NTR1                                                                   
         USING AGYEL,R6                                                         
         MVI   BYTE,0                                                           
         MVI   AGYFLAG1,0                                                       
         MVI   AGYFLAG2,0                                                       
         XC    AGYCTAGY,AGYCTAGY    IN X'01' ELEMENT                            
         LA    R2,AGYOPTSH          OPTIONS FIELD HEADER                        
         CLI   5(R2),0              ANY INPUT?                                  
         BE    EDOPT100             NO...EXIT                                   
*                                                                               
         LA    R4,SCANBLK          CLEAR THE SCANBLK                            
         LA    R5,480                                                           
         SR    R1,R1                                                            
         MVCL  R4,R0                                                            
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(12,SCANBLK)                                   
         CLI   DMCB+4,0             INVALID SYNTAX?                             
         BE    NO                   YES...EXIT WITH BAD CC                      
*                                                                               
         LA    R5,SCANBLK-32                                                    
         USING SCAND,R5                                                         
         SR    R1,R1                CLEAR COUNTER                               
*                                                                               
EDOPT2   LA    R5,32(R5)            NEXT OPTION                                 
         CLI   0(R5),0              TEST NO MORE                                
         BE    EDOPT100                                                         
         AHI   R1,1                 BUMP OPTION COUNTER                         
         ZIC   RE,FLD1LEN                                                       
         BCTR  RE,0                                                             
*                                                                               
         EX    RE,TESTADDS                                                      
         BE    EDOPT10                                                          
*                                                                               
         EX    RE,TESTMEDN                                                      
         BE    EDOPT20                                                          
*                                                                               
         EX    RE,TESTCTA                                                       
         BE    EDOPT30                                                          
*                                                                               
         EX    RE,TESTCTF                                                       
         BE    EDOPT40                                                          
*                                                                               
         EX    RE,TESTLOCK                                                      
         BE    EDOPT50                                                          
*                                                                               
         EX    RE,TESTOFF                                                       
         BE    EDOPT60                                                          
*                                                                               
         EX    RE,TESTTEST                                                      
         BE    EDOPT70                                                          
*                                                                               
         EX    RE,TESTDDSB                                                      
         BE    EDOPT75                                                          
*                                                                               
         EX    RE,TESTTRAD          TRADE AGENCY?                               
         BE    EDOPT80              YES - SO GO PROCESS IT                      
*                                                                               
         EX    RE,TESTCOS2          COST FACTOR REQUIRED?                       
         BE    EDOPT82              YES - SO GO PROCESS IT                      
*                                                                               
         EX    RE,TESTPW                                                        
         BE    EDOPT84                                                          
*                                                                               
         EX    RE,TESTUID          STATION UIDS PROCESSED                       
         BE    EDOPT86                                                          
*                                                                               
         EX    RE,TESTPBD          AUTO ADJUST POST BUY DEMOS                   
         BE    EDOPT88                                                          
*                                                                               
ERROPTS  CHI   R1,1                                                             
         BE    ERROP1                                                           
         CHI   R1,2                                                             
         BE    ERROP2                                                           
         CHI   R1,3                                                             
         BE    ERROP3                                                           
         CHI   R1,4                                                             
         BE    ERROP4                                                           
         CHI   R1,5                                                             
         BE    ERROP5                                                           
         CHI   R1,6                                                             
         BE    ERROP6                                                           
         CHI   R1,7                                                             
         BE    ERROP7                                                           
         CHI   R1,8                                                             
         BE    ERROP8                                                           
         CHI   R1,9                                                             
         BE    ERROP9                                                           
         B     ERROP9               MAX 9 OPTIONS FIT                           
*                                                                               
TESTADDS CLC   FLD1(0),=C'ADDS'                                                 
TESTMEDN CLC   FLD1(0),=C'MEDNAME'                                              
TESTCTA  CLC   FLD1(0),=C'CTA'                                                  
TESTCTF  CLC   FLD1(0),=C'CTFILE'                                               
TESTLOCK CLC   FLD1(0),=C'LOCK'                                                 
TESTOFF  CLC   FLD1(0),=C'OFF'                                                  
TESTTEST CLC   FLD1(0),=C'TEST'                                                 
TESTDDSB CLC   FLD1(0),=C'DDSB'                                                 
TESTTRAD CLC   FLD1(0),=C'TRD'      TRADE AGENCY?                               
TESTCOS2 CLC   FLD1(0),=C'COS2'     COST FACTOR REQUIRED?                       
TESTPW   CLC   FLD1(0),=C'PW'                                                   
TESTUID  CLC   FLD1(0),=C'UID'                                                  
TESTPBD  CLC   FLD1(0),=C'PBDADJ'                                               
*                                                                               
EDOPT10  BAS   RE,TESTYES           DARE=Y/N                                    
         BNE   *+12                                                             
         OI    AGYFLAG1,X'80'                                                   
         B     EDOPT2                                                           
         BAS   RE,TESTNO                                                        
         BE    EDOPT2                                                           
         B     ERROPTS                                                          
*                                                                               
EDOPT20  BAS   RE,TESTYES          MEDNAME=Y/N                                  
         BNE   *+12                                                             
         OI    AGYFLAG1,X'40'                                                   
         B     EDOPT2                                                           
         BAS   RE,TESTNO                                                        
         BE    EDOPT2                                                           
         B     ERROPTS                                                          
*                                                                               
EDOPT30  DS    0H                  CTA=Y/N                                      
         BAS   RE,TESTYES                                                       
         BNE   *+12                                                             
         OI    AGYFLAG1,X'20'                                                   
         B     EDOPT2                                                           
         BAS   RE,TESTNO                                                        
         BE    EDOPT2                                                           
         B     ERROPTS                                                          
*                                                                               
EDOPT40  DS    0H                  CTFILE=XX                                    
         CLI   FLD2LEN,2                                                        
         BNE   ERROPTS                                                          
         MVC   AGYCTAGY,FLD2                                                    
         B     EDOPT2                                                           
*                                                                               
EDOPT50  DS    0H                   LOCK=DATE                                   
         OI    BYTE,X'80'           LOCK EXISTS FLAG                            
         LR    R4,R1                                                            
         GOTO1 DATVAL,DMCB,(0,FLD2),(0,WORK)                                    
         CLC   0(4,R1),=F'0'                                                    
         BE    ERRDAT                                                           
         MVC   DMCB+4(3),=X'D9000A' GET A(GETBRD)                               
         MVI   DMCB+7,X'1D'                                                     
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB              A(GETBRD) IN RF                             
         GOTO1 (RF),DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                           
         CLI   0(R1),X'FF'                                                      
         BE    ERRDAT                                                           
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,WORK)                                 
         MVC   TEMPLOCK(3),WORK     MOVE LOCK DATE FOR VR                       
         LR    R1,R4                RESTORE OPTION COUNTER                      
         B     EDOPT2                                                           
*                                                                               
EDOPT60  DS    0H                   OFF=HEX                                     
         CLC   =C'HEX',FLD2                                                     
         BNE   ERROPTS                                                          
         OI    AGYFLAG1,X'10'                                                   
         B     EDOPT2                                                           
*                                                                               
EDOPT70  BAS   RE,TESTYES           TEST=Y/N                                    
         BNE   *+12                                                             
         OI    AGYFLAG1,AGYTESTQ   X'08'                                        
         B     EDOPT2                                                           
         BAS   RE,TESTNO                                                        
         BE    EDOPT2                                                           
         B     ERROPTS                                                          
*                                                                               
EDOPT75  CLI   FLD2LEN,3                                                        
         BNE   ERROPTS                                                          
         CLC   =C'CLT',FLD2                                                     
         BE    EDOPT2                                                           
         CLC   =C'PRD',FLD2                                                     
         BNE   ERROPTS                                                          
         OI    AGYFLAG1,AGYPRDQ                                                 
         B     EDOPT2                                                           
*                                                                               
EDOPT80  OI    AGYFLAG1,AGYTRDQ     SET TRADE BIT                               
         B     EDOPT2               AND LOOP BACK                               
*                                                                               
EDOPT82  OI    AGYFLAG1,AGYCOS2Q    SET COST FACTOR BIT                         
         B     EDOPT2               AND LOOP BACK                               
*                                                                               
EDOPT84  OI    AGYFLAG2,AGYFLAG2_PW SET PW AGENCY FLAG                          
         B     EDOPT2               AND LOOP BACK                               
*                                                                               
EDOPT86  CLI   T217FFD+1,C'*'      TEST DDS TERM                                
         BNE   ERROPTS                                                          
         OI    AGYFLAG2,AGYFLAG2_UID SET ON IN RECORD                           
         B     EDOPT2               AND LOOP BACK                               
*                                                                               
EDOPT88  OI    AGYFLAG2,AGYFLAG2_PBD  SET AUTO ADJUST POSTBUY DEMOS             
         B     EDOPT2                                                           
*                                                                               
EDOPT100 DS    0H                                                               
         DROP  R5,R6                                                            
*                                                                               
EDOPTX   DS    0H                                                               
YES      SR    R3,R3                                                            
NO       LTR   R3,R3                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVC   ERRNUM,=AL2(2)       INVALID INPUT                               
         B     SPERREX                                                          
ERRMIS   MVC   ERRNUM,=AL2(1)       MISSING INPUT                               
         B     SPERREX                                                          
ERRCHG   MVC   ERRNUM,=AL2(181)     MAY NOT CHANGE THIS FIELD                   
         B     SPERREX                                                          
ERRNNM   MVC   ERRNUM,=AL2(3)       NUMERIC DATA ONLY                           
         B     SPERREX                                                          
ERRBLG   MVC   ERRNUM,=AL2(889)     BILLING PERCENTAGE MUST BE 2 DIGITS         
         B     SPERREX                                                          
ERRDAT   MVC   ERRNUM,=AL2(20)      INVALID DATE FORMAT                         
         B     SPERREX                                                          
ERRLT2   MVC   ERRNUM,=AL2(867)     ENTRY MUST HAVE 2 CHARS                     
         B     SPERREX                                                          
ERRSPA   MVC   ERRNUM,=AL2(868)     NO SPACES ALLOWED                           
         B     SPERREX                                                          
ERRDHX   MVC   ERRNUM,=AL2(869)     AGENCY HEX '0' FOR DDS ONLY                 
         B     SPERREX                                                          
ERRSYN   MVC   ERRNUM,=AL2(870)     INVALID SYNTAX FOR OPTIONS                  
         B     SPERREX                                                          
ERROP1   MVC   ERRNUM,=AL2(871)     OPTION 1 NOT VALID                          
         B     SPERREX                                                          
ERROP2   MVC   ERRNUM,=AL2(872)     OPTION 2 NOT VALID                          
         B     SPERREX                                                          
ERROP3   MVC   ERRNUM,=AL2(873)     OPTION 3 NOT VALID                          
         B     SPERREX                                                          
ERROP4   MVC   ERRNUM,=AL2(874)     OPTION 4 NOT VALID                          
         B     SPERREX                                                          
ERROP5   MVC   ERRNUM,=AL2(875)     OPTION 5 NOT VALID                          
         B     SPERREX                                                          
ERROP6   MVC   ERRNUM,=AL2(876)     OPTION 6 NOT VALID                          
         B     SPERREX                                                          
ERROP7   MVC   ERRNUM,=AL2(877)     OPTION 7 NOT VALID                          
         B     SPERREX                                                          
ERROP8   MVC   ERRNUM,=AL2(878)     OPTION 8 NOT VALID                          
         B     SPERREX                                                          
ERROP9   MVC   ERRNUM,=AL2(879)     OPTION 9 NOT VALID                          
         B     SPERREX                                                          
ERRRAT   MVC   ERRNUM,=AL2(880)     RATING SERVICE MUST BE 0,1 OR 2             
         B     SPERREX                                                          
ERRCLR   MVC   ERRNUM,=AL2(881)     CLIENT REGIONS MUST BE 0,R, OR C            
         B     SPERREX                                                          
ERREXD   MVC   ERRNUM,=AL2(882)     EXTENDED DEMOS MUST BE 0 OR E               
         B     SPERREX                                                          
ERRBOT   MVC   ERRNUM,=AL2(883)     BUY PERIOD OTO'S MUST BE 0 OR 1             
         B     SPERREX                                                          
ERRCAN   MVC   ERRNUM,=AL2(884)     CANADIAN MUST BE 0 OR C                     
         B     SPERREX                                                          
ERRPTS   MVC   ERRNUM,=AL2(885)     OLD POOL TIME SHEETS MUST BE 0 OR Y         
         B     SPERREX                                                          
ERRBIL   MVC   ERRNUM,=AL2(886)     BILLING MUST BE 0,1,2 OR 3                  
         B     SPERREX                                                          
ERRBID   MVC   ERRNUM,=AL2(887)     BUY ID REQUIRED MUST BE N,A OR Y            
         B     SPERREX                                                          
ERRAGY   MVC   ERRNUM,=AL2(888)     INVALID AGENCY HEX                          
         B     SPERREX                                                          
ERRMED   MVC   ERRNUM,=AL2(889)     INVALID MEDIA                               
         B     SPERREX                                                          
ERRBPR   MVC   ERRNUM,=AL2(890)     BRAND POOL RADIO MUST BE 0 OR Y             
         B     SPERREX                                                          
ERRMKG   MVC   ERRNUM,=AL2(891)     MAKEGOODS IN MISSED MONTHS 0 OR Y           
         B     SPERREX                                                          
ERRSAR   MVC   ERRNUM,=AL2(892)     -S AUTH REQUIRED MUST BE Y OR N             
         B     SPERREX                                                          
ERROFR   MVC   ERRNUM,=AL2(893)     OFFICE REQUIRED MUST BE Y OR N              
         B     SPERREX                                                          
ERRBYB   MVC   ERRNUM,=AL2(894)     BUYER/BILLER MUST BE Y OR N                 
         B     SPERREX                                                          
ERRIDB   MVC   ERRNUM,=AL2(895)     ID TITLE REQ. IF BLNG IS Y OR A             
         B     SPERREX                                                          
ERRRFP   MVC   ERRNUM,=AL2(896)     RFP ID NOT FOUND IN RECORD                  
         B     SPERREX                                                          
ERRREC   MVC   ERRNUM,=AL2(53)      RECORD NOT FOUND                            
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
* NOTE F0 ONLY FOR AGENCIES THAT START WITH 'Z' - DDS SPECIALS                  
*                                                                               
AGYTABL  DC    X'F010F110F220F330F440F550F660F770F880F990C1A0C2B0C3C0C4X        
                       D0C5E0C6F00000'                                          
MEDTABL  DC    X'E301D902D503E704C3080000'                                      
RATSTABL DC    C'012',X'00'         RATING SERVICE                              
CLRGTABL DC    C'0RC',X'00'         CLIENT REGIONS                              
EXDMTABL DC    C'0E',X'00'          EXTENDED DEMOS                              
BOTOTABL DC    C'01',X'00'          BUY PERIOD OTO'S                            
CANTABL  DC    C'0C',X'00'          CANADIAN SPOTPAK                            
NWTATABL DC    C'0Y',X'00'          OLD POOL TIMESHEETS                         
BILLTABL DC    C'0123',X'00'        BILLING                                     
BIDTABL  DC    C'NAY',X'00'         BUY ID REQUIRED                             
                                                                                
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*CTGENFILE          TO READ AGENCY NAME FROM CTFILE                             
*SPGENAGY           AGENCY RECORD DSECT                                         
*DDSCANBLKD         FOR SCANNER                                                 
*SPSFMFFD                                                                       
*SCSFM5CD           MAINTENACE SCREEN                                           
*SCSFM5BD           LIST SCREEN                                                 
*DDGENTWA                                                                       
*FAGETTXTD          ERROR MSGS                                                  
*SPSFMWORKD                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE          TO READ AGENCY NAME FROM CTFILE             
         EJECT                                                                  
SPAGYD   DSECT                                                                  
       ++INCLUDE SPGENAGY           AGENCY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD         FOR SCANNER                                 
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM5CD           MAINTENACE SCREEN                           
         EJECT                                                                  
       ++INCLUDE SCSFM5BD           LIST SCREEN                                 
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE FAGETTXTD          ERROR MSGS                                  
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
         ORG   SYSSPARE                                                         
RELO     DS    A                    RELOCATION FACTOR                           
ERRNUM   DS    XL2                  FOR ERROR MESSAGES                          
TEMPID   DS    CL2                  STORES ID OF CLIENT RECORDS                 
SAVEKEY  DS    CL25                 CHECKS RECORD AFTER DMRDHI                  
SCRNFLAG DS    X                    DID ALL OPTIONS FIT FLAG                    
BYTE2    DS    X                                                                
MEDCOUNT DS    X                    COUNTS # OF ELEMENT X'03'                   
FLAG     DS    X                                                                
TEMPACD  DS    CL2                  AGENCY CODE FROM LIST SCREEN                
TEMPLOCK DS    CL3                  LOCK DATE FOR ADDELEM IN VR                 
SCANBLK  DS    15CL32               SCANNER BLOCK                               
CTKEY    DS    CL28                 CONTROL FILE KEY                            
SCAND    DSECT                                                                  
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSAGYID  DS    CL3                 AGENCY ID                                    
         DS    CL1                                                              
LSAGYNAM DS    CL33                AGENCY NAME                                  
         DS    CL1                                                              
LSAGYPRF DS    CL20                20 BYTES OF PROFILE                          
         DS    CL4                                                              
LSAGYREQ DS    CL2                 REQUEST CODE (KEY)                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPSFM63   12/11/13'                                      
         END                                                                    
