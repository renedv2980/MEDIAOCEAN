*          DATA SET DEDALYI    AT LEVEL 022 AS OF 01/03/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEDALYIA                                                                 
         TITLE 'DEMO CONVERSION - LOCAL DAILY DATA (LOCAL + CABLE)'             
*                                                                               
***********************************************************************         
* SEE DEDAILYSRT (AND DEPREDALY) TO UNDERSTAND HOW THIS PROGRAM GETS            
* ITS INPUT FILE.                                                               
***********************************************************************         
*                                                                               
DEDALYI  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEDALYI,R3                                                     
         USING DEMCOND,R8                                                       
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA                                                      
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     DONE                                                             
         EJECT                                                                  
***********************************************************************         
*===================== GET INPUT (RREC --> IREC) =====================*         
*                                                                               
READ     CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
         CLI   RELOFRST,1          INIT STEPS WHICH RUNS ONLY ONCE              
         BNE   OPENOK                                                           
         MVI   RELOFRST,0                                                       
*                                                                               
         XC    SVMKCNT,SVMKCNT     KEEP A MARKET COUNT                          
         LA    RE,NEWCABS                                                       
         LHI   RF,L'NEWCABS*NEWCABS#                                            
         XCEF                                                                   
*                                                                               
         MVI   INTKEY,0            RELEASE A BLANK RECORD                       
         J     EXIT                AS SIGNAL TO DEIS' MERGE                     
*                                                                               
OPENOK   L     R5,ARREC                                                         
*                                                                               
         CLI   BYPREAD,0           WANT A NEW RECORD?                           
         BNE   READOK              OR PROCESS THE LAST ONE AGAIN                
*                                                                               
* READ A RECORD                                                                 
*                                                                               
         GET   IN1,(R5)                                                         
         USING IDAILYD,R5                                                       
         L     RE,ANIINPUT         CONTROLLER KEPT RECORD COUNT                 
         L     R0,0(RE)                                                         
         AHI   R0,1                                                             
         ST    R0,0(RE)                                                         
*                                                                               
READOK   LA    R5,4(R5)                                                         
         CLC   IDRECDE,=C'01'      MARKET INFO RECORD                           
         BE    M1REC                                                            
*                                                                               
         CLI   MRKTFLAG,X'FF'      ARE WE PROCESSING THIS MARKET?               
         BNE   SKIP                NO: SKIP ALL SUBSIDIARY RECORDS              
*                                                                               
         CLC   IDRECDE,=C'02'      STATION INFORMATION                          
         BE    M2REC                                                            
         CLC   IDRECDE,=C'04'      UNIVERSE                                     
         BE    M4REC                                                            
         CLC   IDRECDE,=C'07'      HUT-PUT                                      
         BE    QHDATA                                                           
         CLC   IDRECDE,=C'08'      IMPRESSIONS                                  
         BE    QHDATA                                                           
         B     OPENOK              IGNORE RECORD                                
         EJECT                                                                  
*============================================================                   
*        MARKET RECORD ROUTINES                                                 
*============================================================                   
*                                                                               
M1REC    DS    0H                                                               
************************************************************                    
*        INIT  :  SOME STANDARD INIT STUFF FOR EACH MARKET *                    
************************************************************                    
         LH    R1,SVMKCNT                                                       
         AHI   R1,1                                                             
         STH   R1,SVMKCNT                                                       
*                                                                               
         MVI   BYPREAD,0                                                        
         MVI   MRKTFLAG,0          INDICATE START OF MARKET                     
*                                                                               
         XC    PREVSTAT,PREVSTAT                                                
         XC    PREVBOOK,PREVBOOK                                                
*                                                                               
         LAY   RE,M2STALST                                                      
         MVI   0(RE),X'FF'                                                      
         LA    RE,1(RE)                                                         
         LHI   RF,(M2STALST#*M2STATLN)-1 CLEAR TABLE TO NULLS                   
         XCEF                                                                   
*                                                                               
         LAY   RE,STALIST                                                       
         MVI   0(RE),X'FF'                                                      
         LA    RE,1(RE)                                                         
         LHI   RF,(STALIST#*5)-1         CLEAR TABLE TO NULLS                   
         XCEF                                                                   
*                                                                               
         LAY   RE,UNIVTAB                                                       
         LA    RF,RRECL                                                         
         XCEF                                                                   
*                                                                               
         OC    NEWCABS,NEWCABS    ANY NEW STATION LEFT OVER FROM                
         BZ    M1REC05            PREVIOUS MARKET!                              
*        BAS   RE,CABMAIL         SEND EMAIL                                    
         LA    RE,NEWCABS         TRYING NOT TO CLEAR EVERY MKT                 
         LA    RF,350             LET'S SEE IF THIS CAUSES A PROBLEM            
         XCEF                                                                   
*                                                                               
**************************************************************                  
* SEARCH THROUGH OUR MARKET TABLE TO SEARCH FOR MARKET NUMBER*                  
**************************************************************                  
*                                                                               
M1REC05  GOTO1 CDEMTABS,DMCB,ONITEMKT                                           
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)           LENGTH OF TABLE ENTRIES                       
*                                                                               
         USING ONITMKTD,RE                                                      
         CLI   0(RE),0            MUST FIND MARKET IN TABLE                     
         JE    *+2                DEMTABS TABLE IS EMPTY ?!?                    
                                                                                
         CLC   IDMMKT(3),=C'124'  WE FORCE ATLANTA TO USE                       
         BNE   *+10                                                             
         MVC   IDMMKT(3),=C'168'  THE OLD MKT # TO MATCH MONTHLY                
*                                                                               
         PACK  DUB,IDMMKT(3)                                                    
         CVB   R0,DUB                                                           
         STCM  R0,3,INTMRKT                                                     
*                                                                               
         ST    RE,FULL             SAVE RE BEFORE SUBROUTINE CALL               
         BAS   RE,FLTMKT           CHECK AGAINST MARKET FILTER LIST             
         L     RE,FULL             RESTORE RE                                   
         CLI   PASSFLT,C'Y'        MKT SURVIVED FILTER (IF ANY)?                
         BE    *+12                                                             
         MVI   BYPREAD,0           NO                                           
         B     SKIP                                                             
*                                                                               
         MVI   MRKTFLAG,X'FF'     REMEMBER: WE'RE PROCESSING THIS MRKT          
         MVC   MYSVMKT,INTMRKT                                                  
*                                                                               
         CLC   IDMABBV(3),=C'   '                                               
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_STANDARD   NULL (0) BOOKTYPE                     
         B     M1BTYP                                                           
M1REC20  CLI   0(RE),X'00'                                                      
         BNE   *+6                                                              
         DC    H'0'               MARKET IS NOT IN "ONITEMKT" TABLE!            
         CLC   IDMABBV(L'ONITAMKT),ONITAMKT                                     
         BE    M1REC30                                                          
         AR    RE,RF                                                            
         B     M1REC20                                                          
*                                                                               
M1REC30  MVC   MYBTYP,ONITBKTP    GRAB THE BOOKTYPE OUT OF TABLE                
         DROP  RE                                                               
******************************************************************              
M1BTYP   DS    0H                                                               
         MVI   HWFLG,0                                                          
         CLC   =C'HARD-WIRED CABLE',IDMSAMTY                                    
         BNE   *+8                                                              
         MVI   HWFLG,1            HARD-WIRED?                                   
*                                                                               
         MVI   CBFLG,0                                                          
         CLC   =C'CABLE',IDMDSTY                                                
         BNE   *+8                                                              
         MVI   CBFLG,1            CABLE?                                        
*                                                                               
         MVI   P7FLG,0            PLUS+7?                                       
         CLC   =C'P7',IDMPLYTY                                                  
         BNE   *+8                                                              
         MVI   P7FLG,1                                                          
*                                                                               
         MVI   P3FLG,0            PLUS+3?                                       
         CLC   =C'P3',IDMPLYTY                                                  
         BNE   *+8                                                              
         MVI   P3FLG,1                                                          
*                                                                               
         MVI   PSFLG,0            PLUS+SD?                                      
         CLC   =C'PS',IDMPLYTY                                                  
         BNE   *+8                                                              
         MVI   PSFLG,1                                                          
*                                                                               
         MVI   P1FLG,0            PLUS+1?                                       
         CLC   =C'P1',IDMPLYTY                                                  
         BNE   *+8                                                              
         MVI   P1FLG,1                                                          
*                                                                               
         CLI   MYBTYP,BOOKTYPE_STANDARD    NULL (0) BOOKTYPE                    
         BNE   M1BT30                                                           
*                                                                               
************************************************************                    
* DEFAULT BOOKTYPE IS ALWAYS THE LIVE+7 ONE                                     
************************************************************                    
* STANDARD                                                                      
************************************************************                    
         MVI   MYBTYP,BOOKTYPE_STANDARD P7 STANDARD IS 0                        
*                                                                               
         CLI   PSFLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_LS PS STANDARD IS LS                             
         B     M1BTW                                                            
*                                                                               
         CLI   P3FLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_L3 P3 STANDARD IS L3                             
         B     M1BTW                                                            
*                                                                               
         CLI   P1FLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_L1 P1 STANDARD IS L1                             
         B     M1BTW                                                            
*                                                                               
         CLI   P7FLG,1                                                          
         BE    *+8                                                              
         MVI   MYBTYP,BOOKTYPE_L  LIVE IS L                                     
************************************************************                    
* WIRED                                                                         
************************************************************                    
M1BTW    CLI   HWFLG,1            WIRED TREAT CABLE BROADCAST SAME              
         BNE   M1BTC                                                            
         MVI   MYBTYP,BOOKTYPE_W  WIRED P7 = W                                  
*                                                                               
         CLI   PSFLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_WS WIRED PS = WS                                 
         B     M1BTYPX                                                          
*                                                                               
         CLI   P3FLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_W3 WIRED P3 = W3                                 
         B     M1BTYPX                                                          
*                                                                               
         CLI   P1FLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_W1 WIRED P1 = W1                                 
         B     M1BTYPX                                                          
*                                                                               
         CLI   P7FLG,1                                                          
         BE    *+8                                                              
         MVI   MYBTYP,BOOKTYPE_Z  WIRED LIVE = Z                                
         B     M1BTYPX                                                          
************************************************************                    
* CABLE                                                                         
************************************************************                    
M1BTC    CLI   CBFLG,1            CABLE P7 = C                                  
         BNE   M1BTYPX                                                          
         MVI   MYBTYP,BOOKTYPE_C                                                
*                                                                               
         CLI   PSFLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_LS CABLE PS = LS                                 
         B     M1BTYPX                                                          
*                                                                               
         CLI   P3FLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_C3 CABLE P3 = C3                                 
         B     M1BTYPX                                                          
*                                                                               
         CLI   P1FLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_C1 CABLE P1 = C1                                 
         B     M1BTYPX                                                          
*                                                                               
         CLI   P7FLG,1                                                          
         BE    *+8                                                              
         MVI   MYBTYP,BOOKTYPE_U  CABLE LIVE = U                                
         B     M1BTYPX                                                          
*                                                                               
************************************************************                    
* PRE-ASSIGNED BOOKTYPES (HISPANIC)                                             
* IF THE BOOKTYPE IS NOT HISPANIC, WE USE THE ASSIGNED                          
*  BOOKTYPE GIVEN IN THE ONITEMKT TABLE IN DEMTABS. E.G.,                       
*  BOOKTYPE_T (TRADE) AND BOOKTYPE_M (METRO).                                   
************************************************************                    
M1BT30   DS    0H                 FOR BOOKTYPE RELATED SOFT CODES               
         CLI   MYBTYP,BOOKTYPE_H  HISPANIC L7 = H                               
         BNE   M1BTYPX                                                          
*                                                                               
         CLI   PSFLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_HS HISPANIC LS = HS                              
         B     M1BTYPX                                                          
*                                                                               
         CLI   P3FLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_H3 HISPANIC L3 = H3                              
         B     M1BTYPX                                                          
*                                                                               
         CLI   P1FLG,1                                                          
         BNE   *+12                                                             
         MVI   MYBTYP,BOOKTYPE_H1 HISPANIC L1 = H1                              
         B     M1BTYPX                                                          
*                                                                               
         CLI   P7FLG,1                                                          
         BE    *+8                                                              
         MVI   MYBTYP,BOOKTYPE_J  HISPANIC LIVE = J                             
*                                                                               
M1BTYPX  DS    0H                                                               
******************************************************************              
         CLI   FORCE,C'Y'                                                       
         BNE   *+14                                                             
         MVC   NSIBKYM,FILTBOOK+1 SET FORCE BOOK VALUE                          
         B     M1REC60                                                          
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,IDMMTRD),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   YYMMDD,DUB                                                       
*                                                                               
         XC    DMCB(6*4),DMCB                                                   
         MVI   BYTE,1             SET START DAY TO MONDAY                       
         GOTO1 VNSIWEEK,DMCB,DUB,(BYTE,VGETDAY),VADDAY,VDATCON                  
*                                                                               
         MVC   NSIBKYM+1(1),0(R1)        WEEK                                   
         MVC   NSIBKYM(1),4(R1)          YEAR                                   
*                                                                               
M1REC60  DS    0H                                                               
         MVC   SVMKCODE,IDMMKT                                                  
*                                                                               
         MVC   PMEYE,=C'PROCESSED MKT - '                                       
         MVC   PMMKT,IDMMKT                                                     
         MVC   PMABBV,IDMABBV                                                   
         MVC   PMGEON,IDMGEON                                                   
         MVC   PMGEOI,IDMGEOI                                                   
         MVC   PMMTRD,IDMMTRD                                                   
         MVC   PMRSRVC,IDMRSRVC                                                 
         MVC   PMSAMTY,IDMSAMTY                                                 
         MVC   PMPLYTY,IDMPLYTY                                                 
         MVC   PMDSTY,IDMDSTY                                                   
         MVC   TEMP(L'P),P         SAVE PRINT LINE                              
         GOTO1 VPRINTER                                                         
         MVC   P,TEMP              RESTORE PRINT LINE                           
         GOTO1 VDEPRNT2                                                         
         XC    TEMP,TEMP           (JUST IN CASE)                               
*                                                                               
         B     OPENOK                                                           
*============================================================                   
*       M2 STATION INFO RECORDS                             *                   
*============================================================                   
M2REC    DS    0H                                                               
*                                                                               
         MVC   P+5(L'IDRSTCDE),IDRSTCDE                                         
         MVC   P+8+L'IDRSTCDE(L'IDRCALL),IDRCALL                                
*********GOTO1 VPRINTER                                                         
         GOTO1 VDEPRNT2                                                         
*                                                                               
         CLI   CBFLG,0                                                          
         BE    M2REC05                                                          
         MVC   STACODE,IDRSTCDE                                                 
         BAS   RE,GETCAB                                                        
         BE    M2RECX              FOUND IN CABLE TABLE                         
*                                                                               
M2REC05  LAY   R1,M2STALST                                                      
         USING M2STATD,R1                                                       
*                                                                               
M2REC10  CLI   0(R1),X'FF'                                                      
         BE    M2REC20                                                          
         LA    R1,M2STATLN(R1)                                                  
         B     M2REC10                                                          
*                                                                               
M2REC20  DS    0H                                                               
         MVC   M2TCODE,IDRSTCDE    CODE                                         
         MVC   M2TCALL,IDRCALL     CALL LETTER                                  
         MVC   M2TAFFL,IDRAFFIL    AFFILIATES                                   
         MVC   M2TPRNT,IDRPRENT    PARENT                                       
         MVC   M2TSATE,IDRSATAL    SATELLITE                                    
         CLC   IDRORIG(3),=C'124'                                               
         BNE   *+10                                                             
         MVC   IDRORIG(3),=C'168'                                               
*                                                                               
         CLC   IDRORIG,=C'   '                                                  
         BNE   *+8                                                              
         MVI   M2REGION,C'Y'                                                    
         CLC   IDRORIG,SVMKCODE                                                 
         BNE   *+12                                                             
         MVI   M2SPILL,C'N'                                                     
         B     *+8                                                              
         MVI   M2SPILL,C'Y'        SPILL?                                       
         LA    R1,M2STATLN(R1)                                                  
         CLC   =C'$$EOT$$',0(R1)   ANY MORE ROOM IN TABLE?                      
         JE    *+2                 NO: INCREASE M2STALST#                       
         MVI   0(R1),X'FF'                                                      
*                                                                               
M2RECX   B     OPENOK                                                           
         DROP  R1                                                               
*=====================================================================          
*       M4 RECORD                                                               
*       UNIVERSE ESTIMATES RECORD                                               
*=====================================================================          
M4REC    DS    0H                                                               
*                                                                               
         L     R4,ARREC                                                         
         BAS   RE,SVM4REC                                                       
*                                                                               
         MVI   INTRTYP,C'U'                                                     
         MVC   INTMRKT,MYSVMKT                                                  
         EDIT  (B2,INTMRKT),(4,INTSTA)                                          
         MVI   INTSTA+4,C'U'                                                    
         OC    INTSTA(4),=C'0000'                                               
         MVC   INTBOOK(2),NSIBKYM                                               
*                                                                               
         LA    R4,CUNV2INT         TABLE CONTROL UNIVERSE                       
         LA    R7,IDUHHLD          UNIVERSE IN                                  
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
*                                                                               
         MVI   BYPREAD,0           RESET SWITCH                                 
         MVI   STASW,0                                                          
         MVC   INTBTYP,MYBTYP      SET BOOKTYPE                                 
         BAS   RE,SETKEY                                                        
                                                                                
         J     EXIT                                                             
*============================================================                   
*       QH DEMOGRAPHIC RECORDS - '07' AND '08' RECORDS                          
*============================================================                   
QHDATA   DS    0H                                                               
*                                                                               
         MVI   EXFLG,0                                                          
         CLI   IDHEXC,C'Y'                                                      
         BNE   *+8                                                              
         MVI   EXFLG,1                                                          
*                                                                               
         MVC   INTBTYP,MYBTYP      SET BOOKTYPE                                 
*                                                                               
         CLC   IDRECDE,=C'07'      QHUT/PUT RECORD                              
         BE    QHD50                                                            
*                                                                               
         CLC   IDHSTAC(5),=C'99998'                                             
         BNE   *+12                                                             
         MVI   BYPREAD,0                                                        
         B     OPENOK                                                           
*                                                                               
         PACK  DUB,IDHSTAC(5)      STORE CODE IN KEY                            
         CVB   R0,DUB              THIS IS NOT THE PROGRAM #                    
         STCM  R0,7,INTPNUM        IM BORROWING THE FIELD FOR STA-CODE          
*                                                                               
         CLI   CBFLG,0             READ M2STALST FOR BROADCAST STATIONS         
         BE    QHD05                                                            
         MVC   STACODE,IDHSTAC                                                  
         BAS   RE,GETCAB                                                        
         BE    QHD01                                                            
         CLI   IDHSTAC+4,X'40'     LET'S TRY TO TAKE CARE OF CALL               
         BNE   QHD05               LETTERS SHORTER THAN 5 OURSELVES             
         MVI   BYPREAD,0                                                        
         B     OPENOK                                                           
*                                                                               
QHD01    MVC   INTSTA(4),RETCALL    SAVE STATION RETRIEVED FROM DEMTABS         
         B     QHD20                                                            
*                                                                               
QHD05    LAY   R1,M2STALST         MATCH AGAINST STATION TABLE                  
         USING M2STATD,R1                                                       
*                                                                               
QHD10    CLI   0(R1),X'FF'         A MATCH HAS TO BE FOUND                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   IDHSTAC,M2TCODE     A MATCH?                                     
         BNE   QHD15                                                            
*                                                                               
         MVC   INTSTA,M2TCALL      CALL LETTER                                  
         MVC   INTAFFL,M2TAFFL     AFFILIATION                                  
         MVC   INTSPILL,M2SPILL                                                 
*                                                                               
         CLI   M2REGION,C'Y'                                                    
         BNE   QHD14                                                            
         CLI   INTBTYP,BOOKTYPE_STANDARD    CHANGE INTO CABLE                   
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_C                                               
         CLI   INTBTYP,BOOKTYPE_L3                                              
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_C3                                              
         CLI   INTBTYP,BOOKTYPE_L                                               
         BNE   QHD14                                                            
         MVI   INTBTYP,BOOKTYPE_U                                               
*                                                                               
QHD14    XC    INTSTYP,INTSTYP     STATION TYPE                                 
         CLI   M2TPRNT,C'Y'                                                     
         BNE   QHD20                                                            
         MVI   INTSTYP,PARENTE     PARENT                                       
***********************************************************************         
* ASSIGN BOOKTYPES FOR PARENT ONLY STATIONS                                     
***********************************************************************         
         CLI   INTBTYP,BOOKTYPE_STANDARD                                        
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_A                                               
*                                                                               
         CLI   INTBTYP,BOOKTYPE_L3                                              
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_Q3                                              
*                                                                               
         CLI   INTBTYP,BOOKTYPE_LS                                              
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_QS                                              
*                                                                               
         CLI   INTBTYP,BOOKTYPE_L                                               
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_Q                                               
*                                                                               
         CLI   INTBTYP,BOOKTYPE_W                                               
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_R                                               
*                                                                               
         CLI   INTBTYP,BOOKTYPE_W3                                              
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_S3                                              
*                                                                               
         CLI   INTBTYP,BOOKTYPE_WS                                              
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_SS                                              
*                                                                               
         CLI   INTBTYP,BOOKTYPE_Z                                               
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_S                                               
*                                                                               
         B     QHD20                                                            
*                                                                               
QHD15    LA    R1,M2STATLN(R1)                                                  
         B     QHD10                                                            
*                                                                               
QHD20    MVI   INTSTA+4,C'T'       FORMAT CALL LETTERS                          
         CLI   INTSTA+3,C'+'                                                    
         BNE   *+8                                                              
         MVI   INTSTA+3,X'40'                                                   
*                                                                               
         LAY   RE,AFFTAB           GET AFFILIATION CODE                         
         CLI   CBFLG,0                                                          
         BNE   QHD50                                                            
QHD30    CLC   M2TAFFL,0(RE)                                                    
         BE    QHD40                                                            
         LA    RE,L'AFFTAB(RE)                                                  
         CLI   0(RE),0                                                          
         BE    QHD50                                                            
         B     QHD30                                                            
QHD40    MVC   INTAFFL,SPACES                                                   
         MVC   INTAFFL(3),7(RE)                                                 
         DROP  R1                                                               
*                                                                               
*------------------------- QUARTER HOURS -----------------------------*         
*                                                                               
QHD50    DS    0H                                                               
         LA    RE,IDHSTT                                                        
         XC    DUB,DUB                                                          
         MVC   DUB(2),0(RE)                                                     
         MVC   DUB+2(3),3(RE)                                                   
         GOTO1 VHRTOQH,DMCB,(1,DUB),INTSQH                                      
         MVC   INTEQH,INTSQH                                                    
*----------------------------- DAY OF WEEK ---------------------------*         
         GOTO1 VDATVAL,DMCB,(0,IDHSTD),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                DATE ON FILE HAS TO BE VALID                 
*                                                                               
***      CLC   =X'7305',INTBOOK    FEB0115 FOR TEST DATA                        
* LOCAL DAILIES START OF DAY IS 3AM ON DEC03/2015                               
* BUT DUE TO A BUG IN THE COMPARE LOGIC IN THIS CONVERSION PROGRAM              
* WE WERE INCORRECTLY STILL PROCESSING AS IF WE ARE 5AM START OF DAY            
* IN ORDER NO TO CHANGE THE EXISTING POSTING NUMBERS WE ARE GOING               
* TO SET THE DATE TO THE DATE THIS CODE WILL GO INTO AFFECT.                    
         GOTO1 VDATCON,DMCB,(9,=C'20151203'),DUB2                               
         CLC   DUB,DUB2            CHECK IF PRIOR TO 12/03/2015                 
*********CLC   IDHSTD,=C'12/03/2015' NOV3015 FOR TEST DATA                      
         BNL   QHD50B              CHANGE TO X'7331' DEC0115                    
*                                  WHEN GOING LIVE                              
* ON FILE  12-245A HAS A A CALENDAR  DAY AHEAD                                  
* EXAMPLE 12A-245A =02/18/15... 3A-1145P = 02/17/15                             
* PRE 3A START TIME- SUB DAY 12A-545A                                           
         CLI   INTEQH,X'5C'        12A-545A MINUS A DAY                         
         BNL   QHD51               SO 3A-445A GOES TO PREVIOUS                  
         CLI   INTSQH,X'48'        DAY OF WEEK AND WILL MERGE W PREV            
         BL    QHD51               DAY                                          
         GOTO1 VADDAY,DMCB,DUB,DUB,F'-1'                                        
         B     QHD51                                                            
*                                                                               
* 3A START TIME LOGIC ONLY SUBTRACT TIME FROM 12A-245A TO GET IT                
* BACK TO THE SAME INTERNAL DDS DAY OF WEEK                                     
* 3A-445A DO NOT SUBTRACT BECAUSE WE WANT TO KEEP IT TO THE CURRENT             
* WEEK OF DAY                                                                   
QHD50B   CLI   INTEQH,X'53'        12A-245A MINUS A DAY                         
         BH    QHD51                                                            
         CLI   INTSQH,X'48'                                                     
         BL    QHD51                                                            
         GOTO1 VADDAY,DMCB,DUB,DUB,F'-1'                                        
*                                                                               
QHD51    MVC   DUB2,DUB                                                         
         GOTO1 VGETDAY,DMCB,DUB,DUB                                             
*                                                                               
         LA    RF,DAYTABL          GET DAYS                                     
QHD52    CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DUB(3),0(RF)                                                     
         BE    *+12                                                             
         AHI   RF,L'DAYTABL                                                     
         B     QHD52                                                            
         MVC   INTDAY,3(RF)                                                     
         MVI   INTWEEKS,1          SET UP SINGLE WEEK                           
*                                                                               
*-------------------------------- BOOKS ------------------------------*         
*                                                                               
         MVC   DUB,DUB2                                                         
         XC    DMCB(6*4),DMCB                                                   
         MVI   BYTE,1              SET START DAY TO MONDAY                      
         GOTO1 VNSIWEEK,DMCB,DUB,(BYTE,VGETDAY),VADDAY,VDATCON                  
                                                                                
         MVC   NSIBKYM+1(1),0(R1)  WEEK                                         
         MVC   NSIBKYM(1),4(R1)    YEAR                                         
*                                                                               
         CLC   PREVBOOK,NSIBKYM                                                 
         BE    SAMEBOOK                                                         
         LAY   RE,STALIST                                                       
         MVI   0(RE),X'FF'                                                      
         LA    RE,1(RE)                                                         
         LHI   RF,(STALIST#*5)-1         CLEAR TABLE TO NULLS                   
         XCEF                                                                   
*                                                                               
SAMEBOOK MVC   INTBOOK,NSIBKYM                                                  
         MVC   PREVBOOK,NSIBKYM                                                 
         MVC   INTBOOK,NSIBKYM     BOOK                                         
         MVC   INTMRKT,MYSVMKT     MARKET                                       
*                                                                               
         CLC   IDRECDE,=C'07'                                                   
         BNE   QHD55                                                            
         EDIT  (B2,INTMRKT),(4,INTSTA)                                          
         MVI   INTSTA+4,C'T'                                                    
         OC    INTSTA(4),=C'0000'                                               
*                                                                               
** CHECK FOR FIRST TIME FOR STATION                                             
*                                                                               
QHD55    CLI   EXFLG,1             DON'T CREATE PASSIVE FOR EXCLUSIONS          
         BE    QHD70                                                            
*                                                                               
         LAY   RF,STALIST                                                       
QHD55A   CLI   0(RF),X'FF'         END OF LIST                                  
         BNE   QHD60               NO TRY NEXT                                  
         MVC   0(5,RF),INTSTA                                                   
         CLC   =C'$$EOT$$',5(RF)   ANY MORE ROOM IN TABLE?                      
         JE    *+2                 NO: INCREASE STALIST#                        
         MVI   5(RF),X'FF'                                                      
         MVI   BYPREAD,1           YES - INSERT AND SEND 'M' RECORD             
         MVI   STASW,1                                                          
         BAS   RE,SETKEY                                                        
         J     EXIT                                                             
*                                                                               
QHD60    CLC   0(5,RF),INTSTA      STATION IN LIST                              
         BE    QHD70                                                            
         LA    RF,5(RF)                                                         
         B     QHD55A                                                           
*                                                                               
QHD70    DS    0H                                                               
         MVI   STASW,0             RESET CREATE 'M' RECORD SWITCHES             
         MVI   BYPREAD,0                                                        
*                                                                               
*------------------------- PROGRAM STUFF -----------------------------*         
*                                                                               
         MVC   INTPNAM(13),=C'NOT AVAILABLE'                                    
         MVC   INTPRSRC,INTAFFL    PROGRAM SOURCE SAME AS AFFL                  
         CLI   EXFLG,1             NO NEED FOR OTHER INFO FOR XCLUSIONS         
         BE    QHD80                                                            
*------------------------- SLOT DEMOS --------------------------------*         
*                                                                               
         CLC   IDRECDE,=C'07'      IF MKT TOTAL RECORD                          
         BNE   QHD75               THEN SAVE THE RECORD IN HPUTTAB              
         L     R4,ARREC                                                         
         BAS   RE,SVM7REC                                                       
         LA    R7,IDHHHLD          HUTPUT                                       
         B     QHD77                                                            
*                                                                               
QHD75    DS    0H                                                               
         LA    R7,IDQHHLD                                                       
*                                                                               
QHD77    DS    0H                                                               
         LA    R4,CDMA2INT         IMPRESSIONS                                  
         LA    R6,INTACCS                                                       
         BAS   RE,CNVRTOI                                                       
*                                                                               
         LA    R4,CUNV2INT         UNIVERSES                                    
         LA    R6,INTACCS                                                       
         LA    R7,IDUHHLD-IDURCDE+UNIVTAB                                       
         BAS   RE,CNVRTOI                                                       
* CONVERT DMA TOTALS FROM SAVED MKT TOTAL RECORD                                
         LA    R4,CTOT2INT         HUT/PUT DMA TOTAL                            
         LA    R7,IDHHHLD-IDHRCDE+HPUTTAB                                       
         LA    R6,INTACCS                                                       
         BAS   RE,CNVRTOI                                                       
*                                                                               
*------------------ FILE CONVERSION ROUTINES GO HERE -----------------*         
*                                                                               
QHD80    BAS   RE,SETKEY                                                        
*                                                                               
         J     EXIT                ONCE WE FINISH TOTALS RECORD                 
         SPACE 1                   RERUN FOR RECORD JUST READ                   
***********************************************************************         
         EJECT                                                                  
*=====================================================================          
*  UNIVERSE RECORD "04" RECORD                                       *          
*=====================================================================          
SVM4REC  NTR1                                                                   
* MOVE IN HUT/PUTS INTO HUTREC - SO WE CAN SLOT THEM IN LATER ALONG             
* WITH STATION QHR RECORD                                                       
*                                                                               
         LA    R1,RRECL                                                         
         LA    R5,UNIVTAB                                                       
         LA    R4,4(R4)                                                         
         MOVE  ((R5),(R1)),(R4)                                                 
         XIT1                                                                   
         EJECT                                                                  
*=====================================================================          
*  QH HUT/PUT RECORD "07" RECORD                                     *          
*=====================================================================          
SVM7REC  NTR1                                                                   
* MOVE IN HUT/PUTS INTO HUTREC - SO WE CAN SLOT THEM IN LATER ALONG             
* WITH STATION QHR RECORD                                                       
*                                                                               
         LA    R1,RRECL                                                         
         LA    R5,HPUTTAB                                                       
         LA    R4,4(R4)                                                         
         MOVE  ((R5),(R1)),(R4)                                                 
         XIT1                                                                   
         EJECT                                                                  
*=====================================================================          
*  READ DEMTABS FOR CABLE STATION CALL LETTERS                       *          
* STACODE : INPUT PARAMETER (STATION CODE)                                      
* RETCALL : OUTPUT CALL LETTERS                                                 
* CC      : EQ = FOUND NEQ = NOT FOUND                                          
*=====================================================================          
GETCAB   NTR1                                                                   
*                                                                               
         GOTO1 CDEMTABS,DMCB,NSICABLE                                           
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                  LENGTH OF TABLE ENTRY                  
*                                                                               
         LA    R6,0(RE)                  ADDRESS OF TABLE                       
         USING NSICBLD,R6                                                       
*                                                                               
         CLC   STACODE,=C'99998'                                                
         BNE   GC15                                                             
         MVC   RETCALL,=C'ALL '                                                 
         B     GCXY                                                             
GC10     CLC   0(2,R6),=X'FFFF'          STATION NOT FOUND!! UH-O               
         BE    GC40                                                             
*                                                                               
GC15     PACK  DUB,STACODE(5)            COMPARING BINARY CODE                  
         CVB   R0,DUB                                                           
         CLM   R0,7,NSICBNML             COMPARE THE NEW 5 DIGIT CODE           
         BE    GC20                                                             
         AR    R6,RF                                                            
         B     GC10                                                             
*                                                                               
GC20     DS    0H                                                               
         CLC   NSICBBK,=AL2(0)                                                  
         BE    GC30                                                             
         CLC   NSIBKYM,NSICBBK                                                  
         BH    GC30                                                             
         AR    R6,RF                                                            
         B     GC10                                                             
*                                                                               
GC30     MVC   RETCALL,NSICCALL          FOUND!                                 
         B     GCXY                                                             
*                                                                               
GC40     DS    0H                                                               
         LA    R1,NEWCABS                FIND AN OPEN SPOT                      
GC45     OC    0(5,R1),0(R1)             IN NEW CABLE TABLE                     
         BZ    GC50                                                             
         CLI   0(R1),X'FF'               ANY ROOM LEFT IN TABLE?                
         BNE   *+6                                                              
         DC    H'0'                      NO: TOO MANY NEW STATIONS!             
*                                                                               
         CLC   STACODE,0(R1)             TO STORE NON-DUPLICATED                
         BE    GC55                      NEW CABLE STATIONS                     
         LA    R1,L'STACODE(R1)                                                 
         B     GC45                                                             
GC50     MVC   0(L'STACODE,R1),STACODE                                          
GC55     B     GCXN                                                             
*                                                                               
GCX      CLI   RETCALL,0                                                        
GCXY     SR    RC,RC                                                            
GCXN     LTR   RC,RC                                                            
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================          
*  SEND OUT NEW STATION EMAILS                                                  
*=====================================================================          
CABMAIL  NTR1                                                                   
*                                                                               
         LA    R1,NEWCABS                                                       
CM10     OC    0(5,R1),0(R1)                                                    
         BZ    CMX                 SKIP THIS FOR NOW                            
         GOTO1 VPRINTER                                                         
         MVC   P(19),=C'NEW CABLE STATION: '                                    
         MVC   P+20(L'IDHSTAC),0(R1)                                            
         GOTO1 VPRINTER                                                         
         LA    R1,L'IDHSTAC(R1)                                                 
         B     CM10                                                             
*                                                                               
         EXTRACT ASIDFLD,'S',FIELDS=(ASID)                                      
         LA    R1,ASIDFLD                                                       
         L     R5,0(R1)                                                         
         LOCASCB ASID=(R5)                                                      
         L     R5,ASCBASSB-ASCB(R1)                                             
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     R5,ASSBJSAB-ASSB(R5) R5 = A(JSAB)                                
         USING JSAB,R5                                                          
         MVC   JOBNAME,JSABJBID                                                 
         DROP  R5                                                               
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVC   WARNMSG2+1(L'IDHSTAC),NEWCABS                                    
         MVC   WARNMSG2+9(8),JOBNAME                                            
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',('WARNMSGQ',WARNMSG)                     
*                                                                               
CMX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
CNVWR    L     R2,ASREC            SET TO SORT RECORD                           
*                                                                               
         OC    INTKEY,INTKEY                                                    
         BNZ   *+10                                                             
         MVC   INTKEY(1),MERGFLG                                                
*                                                                               
         OI    INTWEEKS,B'00101111'  SET TYPICAL SW SET WEEKS TO X'2F'          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*------------USE TABLE TO CONVERT FROM RREC TO IREC                             
* R4 = A(CONVERSION TABLE)                                                      
* R7 = A(START OF RATING SERVICE DEMO DATA)                                     
* R6 = A(START OF INTERIM RECORD DEMO DATA)                                     
CNVRTOI  NTR1                                                                   
         SHI   R6,4                                                             
         SHI   R7,10                                                            
CNVRTOI1 CLI   0(R4),X'FF'                                                      
         BE    CNVRTOIX                                                         
         ZIC   R5,0(R4)            GET RS FIELD NUMBER                          
         MHI   R5,10               ADJUST FOR FLD LEN                           
         AR    R5,R7                                                            
                                                                                
         LA    RF,0                                                             
         CLC   0(10,R5),=10C'0'                                                 
         BE    CNVRTOI2                                                         
         CLC   0(10,R5),=10C' '                                                 
         BE    CNVRTOI2                                                         
         OC    0(10,R5),0(R5)                                                   
         BZ    CNVRTOI2                                                         
         PACK  DUB,0(10,R5)         PACK IT                                     
         CVB   RF,DUB              AND CONVERT                                  
CNVRTOI2 ZIC   R5,1(R4)            GET INT FIELD NUMBER                         
         MHI   R5,4                ADJUST FOR FLD LEN                           
         AR    R5,R6                                                            
         ST    RF,0(R5)            SAVE IT                                      
         LA    R4,2(R4)            NEXT FIELD                                   
         B     CNVRTOI1                                                         
                                                                                
CNVRTOIX XIT1                                                                   
***********************************************************************         
*--------------------------- SORT RECORD -----------------------------*         
*                                                                               
SETKEY   NTR1                                                                   
         L     R6,AIREC                                                         
         USING DRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         XC    INTKEY,INTKEY                                                    
         MVI   DRCODE,DRCODEQU                                                  
         CLI   EXFLG,1                                                          
         BNE   SETKEY0                                                          
         MVI   MERGFLG,1                                                        
         MVI   INTRTYP,C'X'                                                     
         B     *+8                                                              
SETKEY0  MVI   INTRTYP,C'R'                                                     
         MVI   DRMEDIA,C'O'                                                     
         MVI   DRSRC,C'N'                                                       
         MVC   DRSTAT,INTSTA                                                    
*        MVI   INTSPILL,C'N'                                                    
         CLC   =C'OTHR',DRSTAT    OTHER IS SPILL                                
         BNE   *+14                                                             
         MVI   INTSPILL,C'Y'                                                    
         MVC   DRKMKT,INTMRKT                                                   
         CLI   CBFLG,0                                                          
         BE    *+14                                                             
         MVI   INTSPILL,C'Y'                                                    
         MVC   DRKMKT,INTMRKT                                                   
*  **********************************************************                   
         MVC   DRSTYP,INTSTYP      SET STATYP IN KEY                            
         MVI   INTSTYP,0           SET STATYP TO ZERO FOR OUTPUT PHASE          
         MVC   DRBOOK,INTBOOK                                                   
         MVC   DRBTYP,INTBTYP      SET INTERNAL BOOK TYPE                       
                                                                                
         CLI   STASW,1             CREAT 'M' RECORD SWITCH                      
         BE    SETKEY3                                                          
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTSQH                                                   
         ZIC   RF,INTWEEKS                                                      
         SR    RE,RE                                                            
         SLL   RF,28                                                            
         LA    R0,4                                                             
         LA    R1,0                                                             
SETKEY2  SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         LA    R1,1(R1)                                                         
         SR    RE,RE               CLEAR WEEK INDICATOR                         
         BCT   R0,SETKEY2                                                       
         STC   R1,DRHIQHR+1                                                     
         MVC   DRHIQHR+6(L'INTWEEKS),INTWEEKS                                   
         B     SETKEYX                                                          
                                                                                
SETKEY3  MVI   DRCODE,C'M'                                                      
         MVI   INTRTYP,C'M'                                                     
*                                                                               
SETKEYX  XIT1  ,                   DEIS JAN/2019: CHANGED FROM "XIT"            
                                                                                
         EJECT                                                                  
***********************************************************************         
FLTMKT   NTR1  ,                                                                
*                                                                               
         SR    RF,RF                                                            
         MVI   PASSFLT,C'Y'                                                     
         ICM   RF,1,FILTMRKT                                                    
         BZ    FLTMKTX                                                          
         OC    INTMRKT,INTMRKT                                                  
         BZ    FLTMKTX                                                          
         LA    R1,FILTMRKT+1                                                    
*                                                                               
         TM    FLAGS1,NEGATIVE_FILTMRKT                                         
         BZ    FLTMKT10                                                         
FLTMKT05 CLC   INTMRKT,0(R1)                                                    
         BNE   *+12                                                             
         MVI   PASSFLT,C'N'                                                     
         B     FLTMKTX                                                          
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,FLTMKT05                                                      
         B     FLTMKTX                                                          
*                                                                               
FLTMKT10 CLC   INTMRKT,0(R1)                                                    
         BE    FLTMKTX                                                          
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,FLTMKT10                                                      
         MVI   PASSFLT,C'N'                                                     
*                                                                               
FLTMKTX  DS    0H                                                               
         XIT1  ,                                                                
*                                                                               
************************************************************                    
         EJECT                                                                  
MORET    DS    0H'0'                                                            
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVC   P(43),=C'IPHASE: TOTAL NUMBER OF MARKETS PROCESSED -'            
         EDIT  SVMKCNT,(3,P+44),ZERO=NOBLANK,COMMAS=YES                         
         GOTO1 VPRINTER                                                         
         MVI   INTAPESW,X'02'                                                   
         B     DONE                                                             
         SPACE 2                                                                
DONE     DS    0H                                                               
*        OC    NEWCABS,NEWCABS    ANY NEW STATION LEFT OVER FROM                
*        BZ    EXIT                                                             
*        BAS   RE,CABMAIL                                                       
EXIT     XMOD1 1                                                                
*                                                                               
SKIP     DS    0H                                                               
         MVI   INTAPESW,X'40'      DROP RECORD                                  
         B     EXIT                RETURN TO DEMCNV                             
         LTORG                                                                  
         EJECT                                                                  
PS1E     EQU   1                                                                
PARENTE  EQU   2                                                                
PS2E     EQU   4                                                                
S1EQU    EQU   8                                                                
S2EQU    EQU   16                                                               
OUTMARE  EQU   32                                                               
CANMARE  EQU   64                                                               
METROAE  EQU   1                                                                
METROBE  EQU   2                                                                
GLOBIND  EQU  4                                                                 
*                                                                               
DUB2     DS    D                                                                
HWFLG    DS    XL1                                                              
CBFLG    DS    XL1                                                              
PSFLG    DS    XL1                                                              
P3FLG    DS    XL1                                                              
P7FLG    DS    XL1                                                              
P1FLG    DS    XL1                                                              
EXFLG    DS    XL1                                                              
MERGFLG  DS    XL1                                                              
RETCALL  DS    CL4                                                              
STACODE  DS    XL5                                                              
SAVEKEY  DS    XL18                                                             
*                                                                               
DAYTABL  DS    0CL4                                                             
         DC    C'MON',X'10'         MON                                         
         DC    C'TUE',X'20'         TUE                                         
         DC    C'WED',X'30'         WED                                         
         DC    C'THU',X'40'         THU                                         
         DC    C'FRI',X'50'         FRI                                         
         DC    C'SAT',X'60'         SAT                                         
         DC    C'SUN',X'70'         SUN                                         
         DC    X'00'                                                            
*                                                                               
AFFTAB   DS    0CL10                                                            
         DC    CL7'ABC',CL3'A'     ABC NETWORK                                  
         DC    CL7'CBS',CL3'C'     CBS NETWORK                                  
         DC    CL7'FOX',CL3'F'     FOX NETWORK                                  
         DC    CL7'NBC',CL3'N'     NBC NETWORK                                  
         DC    CL7'ION',CL3'ION'   ION MEDIA NETWORK                            
         DC    CL7'IND',CL3'I'     INDEPENDENT                                  
         DC    CL7'PBS',CL3'P'     PBS                                          
         DC    CL7'TEL',CL3'T'     TELEMUNDO                                    
         DC    CL7'TF ',CL3'TF'    TELEFUTURA                                   
         DC    CL7'UNI',CL3'U'     UNIVISION                                    
         DC    CL7'AZA',CL3'AZA'   AZTECA AMERICA                               
         DC    CL7'CW ',CL3'CW'    THE CW NETWORK                               
         DC    CL7'MNT',CL3'MNT'   MY NETWORK TV                                
         DC    CL7'UMA',CL3'UMA'   UNIMAS (NAME CHANGE FROM TELEFUTURA)         
         DC    X'FF'                                                            
*                                                                               
SVMKCODE DS    CL3                                                              
SVMKCNT  DS    H                                                                
*                                                                               
* TABLE TO CONVERT UNIVERSES FROM NSI TO DDS INTREC (M2 RECORD)                 
CUNV2INT DS    0C                                                               
         DC    AL1(RRHOMES,OUHOMES)                                             
         DC    AL1(RRV25,OUV25)                                                 
         DC    AL1(RRV611,OUV611)                                               
         DC    AL1(RRM1214,OUM1214)                                             
         DC    AL1(RRM1517,OUM1517)                                             
         DC    AL1(RRM1820,OUM1820)                                             
         DC    AL1(RRM2124,OUM2124)                                             
         DC    AL1(RRM2534,OUM2534)                                             
         DC    AL1(RRM3549,OUM3549)                                             
         DC    AL1(RRM5054,OUM5054)                                             
         DC    AL1(RRM5564,OUM5564)                                             
         DC    AL1(RRM65O,OUM65O)                                               
         DC    AL1(RRW1214,OUW1214)                                             
         DC    AL1(RRW1517,OUW1517)                                             
         DC    AL1(RRW1820,OUW1820)                                             
         DC    AL1(RRW2124,OUW2124)                                             
         DC    AL1(RRW2534,OUW2534)                                             
         DC    AL1(RRW3549,OUW3549)                                             
         DC    AL1(RRW5054,OUW5054)                                             
         DC    AL1(RRW5564,OUW5564)                                             
         DC    AL1(RRW65O,OUW65O)                                               
         DC    AL1(RRWWRK,OUWWRK)                                               
         DC    X'FF'                                                            
*                                                                               
CDMA2INT DS    0C                          DMA VIEWERS                          
         DC    AL1(RRHOMES,ODHOMES)        NEW SINCE HOMES IS TOGETHER          
         DC    AL1(RRV25,ODV25)                                                 
         DC    AL1(RRV611,ODV611)                                               
         DC    AL1(RRM1214,ODM1214)                                             
         DC    AL1(RRM1517,ODM1517)                                             
         DC    AL1(RRM1820,ODM1820)                                             
         DC    AL1(RRM2124,ODM2124)                                             
         DC    AL1(RRM2534,ODM2534)                                             
         DC    AL1(RRM3549,ODM3549)                                             
         DC    AL1(RRM5054,ODM5054)                                             
         DC    AL1(RRM5564,ODM5564)                                             
         DC    AL1(RRM65O,ODM65O)                                               
         DC    AL1(RRW1214,ODW1214)                                             
         DC    AL1(RRW1517,ODW1517)                                             
         DC    AL1(RRW1820,ODW1820)                                             
         DC    AL1(RRW2124,ODW2124)                                             
         DC    AL1(RRW2534,ODW2534)                                             
         DC    AL1(RRW3549,ODW3549)                                             
         DC    AL1(RRW5054,ODW5054)                                             
         DC    AL1(RRW5564,ODW5564)                                             
         DC    AL1(RRW65O,ODW65O)                                               
         DC    AL1(RRWWRK,ODWWRK)                                               
         DC    X'FF'                                                            
*                                                                               
CTOT2INT DS    0C                          DMA TOTALS                           
         DC    AL1(RRHOMES,OMHOMES)        NEW SINCE HOMES IS TOGETHER          
         DC    AL1(RRV25,OMV25)                                                 
         DC    AL1(RRV611,OMV611)                                               
         DC    AL1(RRM1214,OMM1214)                                             
         DC    AL1(RRM1517,OMM1517)                                             
         DC    AL1(RRM1820,OMM1820)                                             
         DC    AL1(RRM2124,OMM2124)                                             
         DC    AL1(RRM2534,OMM2534)                                             
         DC    AL1(RRM3549,OMM3549)                                             
         DC    AL1(RRM5054,OMM5054)                                             
         DC    AL1(RRM5564,OMM5564)                                             
         DC    AL1(RRM65O,OMM65O)                                               
         DC    AL1(RRW1214,OMW1214)                                             
         DC    AL1(RRW1517,OMW1517)                                             
         DC    AL1(RRW1820,OMW1820)                                             
         DC    AL1(RRW2124,OMW2124)                                             
         DC    AL1(RRW2534,OMW2534)                                             
         DC    AL1(RRW3549,OMW3549)                                             
         DC    AL1(RRW5054,OMW5054)                                             
         DC    AL1(RRW5564,OMW5564)                                             
         DC    AL1(RRW65O,OMW65O)                                               
         DC    AL1(RRWWRK,OMWWRK)                                               
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
PASSFLT  DC    C'Y'                                                             
PREVQH   DC    X'00'                                                            
NFRST    DC    X'01'                                                            
FRST     DC    X'01'                                                            
RELOFRST DC    X'01'                                                            
BYPREAD  DC    X'00'                                                            
STASW    DC    X'00'                                                            
SAMPLEBT DC    X'00'                                                            
MRKTFLAG DS    X                   X'FF' = WE'RE PROCESSING THIS MARKET         
ASIDFLD  DC    F'0'                                                             
JOBNAME  DS    CL8                                                              
*                                                                               
MYBTYP   DS    CL(L'BOOKTYPE)                                                   
NSIBOOKY DS    CL1                                                              
NSIBOOKM DS    CL1                                                              
NSIBKYM  DS    CL2                                                              
PREVSTAT DS    CL5                                                              
PREVBOOK DS    XL2                                                              
MYSVMKT  DS    XL2                                                              
YYMMDD   DS    CL6                                                              
BYTE     DS    CL1                                                              
*                                                                               
WARNMSG  DS    0C                                                               
         DC    C'AUTONOTE*US-MFDEMOSPROGRAMMERS:NEW CABLE STATION! '            
WARNMSG2 DC    C'(XXXXX): NNNNNNNN'                                             
WARNMSGQ EQU   *-WARNMSG                                                        
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=RRECL,                                            X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
*                                                                               
NEWCABS# EQU   400                                                              
NEWCABS  DS    (NEWCABS#)CL5                                                    
         DC    X'FF'                                                            
*                                                                               
UNIVTAB  DS    (RRECL)C                                                         
HPUTTAB  DS    (RRECL)C                                                         
*                                                                               
         DS    0D                                                               
         DC    C'*STALIST'                                                      
STALIST# EQU   400                                                              
STALIST  DS    (STALIST#)CL5                                                    
         DC    C'$$EOT$$'                                                       
*                                                                               
         DS    0D                                                               
         DC    C'M2STALST'                                                      
M2STALST# EQU  300                                                              
M2STALST DS    (M2STALST#)CL(M2STATLN)        STATIONS FOR M2 RECORDS           
         DC    C'$$EOT$$'                                                       
         EJECT                                                                  
* INTERIM RECORD DISPLACEMENTS     THIS CORRESPONDS TO DEMDISP                  
ORHOMES  EQU   1                   SOME MAY NOT APPLY!!                         
ORHH1    EQU   2                   BUT UNLESS WE WANT TO CREATE                 
ORHH2    EQU   3                   A NEW DEMDISP WITH NEW F/M/S                 
ORHH3    EQU   4                   THE ORDER OF THESE DEMOS                     
ORHH4    EQU   5                   MUST REMAIN!!                                
OSHOMES  EQU   6                                                                
OPMETROA EQU   7                                                                
OSMETROA EQU   8                                                                
ORMETROA EQU   9                                                                
OPMETROB EQU   10                                                               
OSMETROB EQU   11                                                               
ORMETROB EQU   12                                                               
OTHOMES  EQU   13                                                               
ODHOMES  EQU   14                                                               
OTWWRK   EQU   15                                                               
ODWWRK   EQU   16                                                               
OTW65O   EQU   17                                                               
ODW65O   EQU   18                                                               
OTM65O   EQU   19                                                               
ODM65O   EQU   20                                                               
ODMETROA EQU   21                                                               
ODMETROB EQU   22                                                               
OTM1214  EQU   23                                                               
OTW1214  EQU   24                                                               
OTM1517  EQU   25                                                               
OTW1517  EQU   26                                                               
OTM1820  EQU   27                                                               
OTW1820  EQU   28                                                               
OTM2124  EQU   29                                                               
OTW2124  EQU   30                                                               
OTM2534  EQU   31                                                               
OTW2534  EQU   32                                                               
OTM3549  EQU   33                                                               
OTW3549  EQU   34                                                               
OTM5564  EQU   35                                                               
OTW5564  EQU   36                                                               
OTM5054  EQU   37                                                               
OTW5054  EQU   38                                                               
OTV25    EQU   39                                                               
ODV25    EQU   40                                                               
OTV611   EQU   41                                                               
ODV611   EQU   42                                                               
OUHOMES  EQU   43                                                               
OUM65O   EQU   44                                                               
OUW65O   EQU   45                                                               
OUWWRK   EQU   46                                                               
OUA1HH   EQU   47                                                               
OUA2HH   EQU   48                                                               
OUA3HH   EQU   49                                                               
OUMETROA EQU   50                                                               
OUMETROB EQU   51                                                               
OUM1214  EQU   52                                                               
OUW1214  EQU   53                                                               
OUM1517  EQU   54                                                               
OUW1517  EQU   55                                                               
OUM1820  EQU   56                                                               
OUW1820  EQU   57                                                               
OUM2124  EQU   58                                                               
OUW2124  EQU   59                                                               
OUM2534  EQU   60                                                               
OUW2534  EQU   61                                                               
OUM3549  EQU   62                                                               
OUW3549  EQU   63                                                               
OUM5564  EQU   64                                                               
OUW5564  EQU   65                                                               
OUM5054  EQU   66                                                               
OUW5054  EQU   67                                                               
OUV25    EQU   68                                                               
OUV611   EQU   69                                                               
ODM1214  EQU   70                                                               
ODW1214  EQU   71                                                               
ODM1517  EQU   72                                                               
ODW1517  EQU   73                                                               
ODM1820  EQU   74                                                               
ODW1820  EQU   75                                                               
ODM2124  EQU   76                                                               
ODW2124  EQU   77                                                               
ODM2534  EQU   78                                                               
ODW2534  EQU   79                                                               
ODM3549  EQU   80                                                               
ODW3549  EQU   81                                                               
ODM5564  EQU   82                                                               
ODW5564  EQU   83                                                               
ODM5054  EQU   84                                                               
ODW5054  EQU   85                                                               
OMHOMES  EQU   86                                                               
OMWWRK   EQU   87                                                               
OMW65O   EQU   88                                                               
OMM65O   EQU   89                                                               
OMMETROA EQU   90                                                               
OMMETROB EQU   91                                                               
OMM1214  EQU   92                                                               
OMW1214  EQU   93                                                               
OMM1517  EQU   94                                                               
OMW1517  EQU   95                                                               
OMM1820  EQU   96                                                               
OMW1820  EQU   97                                                               
OMM2124  EQU   98                                                               
OMW2124  EQU   99                                                               
OMM2534  EQU   100                                                              
OMW2534  EQU   101                                                              
OMM3549  EQU   102                                                              
OMW3549  EQU   103                                                              
OMM5564  EQU   104                                                              
OMW5564  EQU   105                                                              
OMM5054  EQU   106                                                              
OMW5054  EQU   107                                                              
OMV25    EQU   108                                                              
OMV611   EQU   109                                                              
OQHOMES  EQU   110                                                              
OQWWRK   EQU   111                                                              
OQW65O   EQU   112                                                              
OQM65O   EQU   113                                                              
OQM1214  EQU   114                                                              
OQW1214  EQU   115                                                              
OQM1517  EQU   116                                                              
OQW1517  EQU   117                                                              
OQM1820  EQU   118                                                              
OQW1820  EQU   119                                                              
OQM2124  EQU   120                                                              
OQW2124  EQU   121                                                              
OQM2534  EQU   122                                                              
OQW2534  EQU   123                                                              
OQM3549  EQU   124                                                              
OQW3549  EQU   125                                                              
OQM5564  EQU   126                                                              
OQW5564  EQU   127                                                              
OQM5054  EQU   128                                                              
OQW5054  EQU   129                                                              
OQV25    EQU   130                                                              
OQV211   EQU   131                                                              
ORV2O    EQU   132                                                              
ORV18O   EQU   133                                                              
ORV1234  EQU   134                                                              
ORV1224  EQU   135                                                              
ORV1217  EQU   136                                                              
ORV611   EQU   137                                                              
ORV211   EQU   138                                                              
ORW18O   EQU   139                                                              
ORW1834  EQU   140                                                              
ORW1849  EQU   141                                                              
ORW2549  EQU   142                                                              
ORW2554  EQU   143                                                              
ORW1224  EQU   144                                                              
ORW2564  EQU   145                                                              
ORW1234  EQU   146                                                              
ORM18O   EQU   147                                                              
ORM1834  EQU   148                                                              
ORM1849  EQU   149                                                              
ORM2554  EQU   150                                                              
ORM2564  EQU   151                                                              
ORWWRK   EQU   152                                                              
ORM2549  EQU   153                                                              
ORA1849  EQU   154                                                              
ORA1834  EQU   155                                                              
ORA2554  EQU   156                                                              
OPV2O    EQU   157                                                              
OPV18O   EQU   158                                                              
OPV1234  EQU   159                                                              
OPV1224  EQU   160                                                              
OPV1217  EQU   161                                                              
OPV611   EQU   162                                                              
OPV211   EQU   163                                                              
OPW18O   EQU   164                                                              
OPW1834  EQU   165                                                              
OPW1849  EQU   166                                                              
OPW2549  EQU   167                                                              
OPW2554  EQU   168                                                              
OPW1224  EQU   169                                                              
OPW2564  EQU   170                                                              
OPW1234  EQU   171                                                              
OPM18O   EQU   172                                                              
OPM1834  EQU   173                                                              
OPM1849  EQU   174                                                              
OPM2554  EQU   175                                                              
OPM2564  EQU   176                                                              
OPWWRK   EQU   177                                                              
OPM2549  EQU   178                                                              
OPA1849  EQU   179                                                              
OPA1834  EQU   180                                                              
OPA2554  EQU   181                                                              
         EJECT                                                                  
                                                                                
* FIELD DISPLACEMENTS FOR RATING SERVICE RECORDS                                
RRHOMES  EQU   1                                                                
RRV25    EQU   2                                                                
RRV611   EQU   3                                                                
RRM1214  EQU   4                                                                
RRM1517  EQU   5                                                                
RRM1820  EQU   6                                                                
RRM2124  EQU   7                                                                
RRM2534  EQU   8                                                                
RRM3549  EQU   9                                                                
RRM5054  EQU   10                                                               
RRM5564  EQU   11                                                               
RRM65O   EQU   12                                                               
RRW1214  EQU   13                                                               
RRW1517  EQU   14                                                               
RRW1820  EQU   15                                                               
RRW2124  EQU   16                                                               
RRW2534  EQU   17                                                               
RRW3549  EQU   18                                                               
RRW5054  EQU   19                                                               
RRW5564  EQU   20                                                               
RRW65O   EQU   21                                                               
RRWWRK   EQU   22                                                               
*                                                                               
RRECL    EQU   350                                                              
         EJECT                                                                  
*                                                                               
*============================= DEDEMFILE =============================*         
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
***********************************************************************         
*============================= DEDEMCNVD =============================*         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
***********************************************************************         
*============================= DDCOMFACS =============================*         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
***********************************************************************         
*============================= RREC DSECT ============================*         
         EJECT                                                                  
       ++INCLUDE DEDAILYD                                                       
***********************************************************************         
*============================= IREC DSECT ============================*         
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTTPT3D                                                     
***********************************************************************         
*============================= DEDEMTABD =============================*         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
***********************************************************************         
*============================= DDDPRINT ==============================*         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         ORG   P                                                                
PMEYE    DS    CL16                                                             
PMABBV   DS    CL6                                                              
         DS    C                                                                
PMGEON   DS    CL30                                                             
         DS    C                                                                
PMMKT    DS    CL3                                                              
         DS    C                                                                
PMGEOI   DS    CL10                                                             
         DS    C                                                                
PMMTRD   DS    CL10                                                             
         DS    C                                                                
PMRSRVC  DS    CL5                                                              
         DS    C                                                                
PMSAMTY  DS    CL16                                                             
         DS    C                                                                
PMPLYTY  DS    CL3                                                              
         DS    C                                                                
PMDSTY   DS    CL9                                                              
         ORG                                                                    
***********************************************************************         
         EJECT                                                                  
*                                                                               
INTABD   DSECT ,                   DSECT FOR NET-IN-TAB-COUNTS                  
ITMETA   DS    CL3       P         METRO A                                      
ITMETB   DS    CL3       P         METRO B                                      
ITDMA    DS    CL3       P         DMA                                          
ITNTA    DS    CL3       P         NSI AREA                                     
*                                                                               
QHTD     DSECT ,                   DSECT FOR QUARTER-HOUR TRENDS                
QHTMO    DS    CL2       C         REPORT PERIOD                                
QHTYR    DS    CL2       C         REPORT YEAR                                  
*                                                                               
M2STATD  DSECT ,                   DSECT FOR REPORTABLE STATIONS                
M2TCODE  DS    CL5       C         STATION CODE                                 
M2TCALL  DS    CL4       C         STATION CALL LETTER                          
M2TAFFL  DS    CL7       C         NETWORK AFFILIATION                          
M2TPRNT  DS    CL1       C         PARENT INDICATOR                             
M2TSATE  DS    CL1       C         SATELLITE INDICATOR                          
M2SPILL  DS    CL1       C         SPILL                                        
M2REGION DS    CL1       C         REGIONAL CABLE                               
M2STATLN EQU   *-M2TCODE                                                        
         EJECT                                                                  
*                                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022DEDALYI   01/03/20'                                      
         END                                                                    
