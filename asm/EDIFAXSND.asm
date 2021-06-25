*          DATA SET EDIFAXSND  AT LEVEL 025 AS OF 05/02/08                      
*PHASE EDIFAXSA                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCANNER                                                                
*                                                                               
EDIFAXS  START                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDFAXSND,WORK=VWRKAREA                                         
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         L     RA,ACOMMON                                                       
         LHI   R9,4096                                                          
         AR    R9,RA                                                            
         USING COMMON,RA,R9                                                     
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         ST    RD,SAVERD           SET EXIT FROM CHAIN RD                       
*                                                                               
         L     RC,AWORKD           GET SOME W/S FOR THIS MODULE                 
         USING WORKD,RC                                                         
         USING DESTTABD,DESTNTRY                                                
         BRAS  RE,INIT             INITIALISE IT ALL                            
         BNE   MAINX                                                            
*                                                                               
MAIN02   BRAS  RE,WAIT             ISSUE 'GET' AND WAIT FOR A MESSAGE           
         BNE   MAINX               OPERATOR REQUESTED CANCEL JOB                
*                                                                               
         OC    DATALEN,DATALEN     EMPTY MESSAGE?                               
         BZ    MAIN02              YES - SKIP THIS ONE, GET NEXT ONE            
         BRAS  RE,SENDFAX          SEND FAX OUT                                 
         B     MAIN02              AND BACK FOR NEXT                            
*                                                                               
MAINX    BRAS  RE,MQCLS            FREE ANY MQ CONNECTIONS THAT EXIST           
         B     XBASE                                                            
*                                                                               
VWRKAREA DC    V(WORKAREA)                                                      
ACOMMON  DC    A(COMMON)                                                        
AWORKD   DC    A(WORKD)                                                         
         EJECT                                                                  
***********************************************************************         
* SEND MESSAGE OUT AS A FAX                                           *         
***********************************************************************         
         SPACE 1                                                                
SENDFAX  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRFLDS          CLEAN UP BEFORE PROCESSING THIS MSG          
         BRAS  RE,ON31                                                          
*                                                                               
         L     R7,ADSTAEFT         R7 = CURRENT DESTINATION TABLE ENTRY         
         USING DSTAEFTD,R7                                                      
*                                                                               
         L     R4,AMSGIN           INPUT MESSAGE BUFFER                         
         USING MSGHDRD,R4                                                       
         MVC   USERID,MSGUSR       GET LOCAL COPIES OF HEADER INFO              
         MVC   USERIDH,MSGUSRNO                                                 
         MVC   RPTSUBID,MSGPQSUB                                                
         MVC   RPTPQNUM,MSGPQNUM                                                
         MVC   RPTHTIME,MSGHTIME                                                
         MVC   RPTLOGNO,MSGLOGNO                                                
         MVC   RPTPQTYP,MSGPQTYP                                                
*                                                                               
         GOTO1 VHEXIN,DMCB,USERIDH,USERIDNO,4,0                                 
*                                                                               
         PACK  DUB,MSGHLEN         GO PAST HEADER                               
         CVB   R0,DUB                                                           
         AR    R4,R0               R4 = FIRST LINE OF MESSAGE                   
*                                                                               
SFX02    BRAS  RE,GETNXTL          GET NEXT LINE INTO R / RLEN                  
         BNE   SFX04               EOB                                          
*                                                                               
         CLI   GOTHDR,C'Y'         TEST ALREADY HAVE HEADER                     
         BE    *+12                YES                                          
         BRAS  RE,PRCHDR           PROCESS THIS CARD AS IF HEADER               
         B     SFX02               NEXT CARD                                    
*                                                                               
         CLC   =C'++DDS',R+1       IS THIS A DDS CONTROL CARD                   
         BNE   *+12                NO - THEN WE ARE INTO THE FAX PROPER         
         BRAS  RE,PRCDDS                                                        
         B     SFX02                                                            
*                                                                               
SFX04    CLI   GOTHDR,C'Y'         TEST ALREADY HAVE HEADER                     
         BNE   NOHDR               PRT WARNING MSG AND EXIT                     
*                                                                               
         BRAS  RE,BLDDEST          NOW WE ARE INTO THE FAX PROPER               
*                                                                               
         MVC   MAILBOX,DESTREPN+2  USE REP MAILBOX NO. (WITHOUT '62')           
         CLI   FACTYPE,C'R'        REP?                                         
         BE    *+10                                                             
         MVC   MAILBOX,DESTADVN+2  USE ADV MAILBOX NO. (WITHOUT '62')           
*                                                                               
         ICM   R3,15,AMSGOUT       R3 = MQ FAX OUTPUT AREA                      
         CLI   HDR+67,C'D'         IS THIS A DARE FAX?                          
         BNE   SFX06               NO                                           
*                                                                               
         OC    TRNLEN,TRNLEN       TRN CARD INPUT?                              
         BZ    *+10                NO                                           
         MVC   REQUESTR,TRN+35                                                  
*                                                                               
* FOR MULTIPLE DESTINATIONS, THIS FAX SUBCODE WILL APPLY TO ALL DESTS           
*                                                                               
         OC    DXCLEN,DXCLEN       DXC CARD INPUT?                              
         BZ    *+10                                                             
         MVC   FAXSUBC,DXC+15                                                   
*                                                                               
* START BUILDING FAX MESSAGE IN MSGOUT                                          
*                                                                               
SFX06    XC    NUMLINES,NUMLINES   RESET LINE COUNTER                           
         BRAS  RE,GETIDN           GET SOME ID INFORMATION FROM CTIREC          
*                                                                               
         TIME  BIN                                                              
         ST    R0,STRTTIME         REMEMBER TIME WE START SENDING               
*                                                                               
         BRAS  RE,ON31                                                          
*                                                                               
         L     R0,AMSGOUT          CLEARING LIKE THIS IS LIKELY                 
         L     R1,MAXMSGLN         OVERKILL                                     
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   TRACE,C'F'          IF TRACING OUTPUT LEAVE SPACE LINE           
         BNE   *+8                                                              
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   USERIDOV,USERID                                                  
         CLI   FACTYPE,C'Q'        FQA?                                         
         BNE   *+16                                                             
         MVC   USERIDOV,=CL08'TEST    '                                         
         MVC   MAILBOX,=CL06'001082'                                            
*                                                                               
         CLI   FACTYPE,C'T'        TST?                                         
         BNE   *+16                                                             
         MVC   USERIDOV,=CL08'TEST    '                                         
         MVC   MAILBOX,=CL06'000003'                                            
*                                                                               
SFX08    L     R3,AMSGOUT          FIRST BUILD HEADER INFO FOR EASYLINK         
         MVC   00(10,R3),=CL10'SIGNON DDS'                                      
         MVC   10(06,R3),MAILBOX                                                
         MVI   16(R3),C' '                                                      
         AHI   R3,17                                                            
*                                                                               
         MVC   0(L'USERID,R3),USERIDOV                                          
         AHI   R3,L'USERID-1                                                    
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
*                                                                               
         CLI   FACTYPE,C'A'        ADV?                                         
         BE    *+12                                                             
         CLI   FACTYPE,C'R'        REP?                                         
         BNE   SFX09Q                                                           
         MVC   1(04,R3),=CL04'.DDS'   PASSWORD                                  
         AHI   R3,05                  BUMP PAST PASSWORD                        
         B     SFX09X                                                           
*                                                                               
SFX09Q   CLI   FACTYPE,C'Q'        FQA?                                         
         BNE   SFX09T                                                           
         MVC   1(04,R3),=CL04'.FQA'   PASSWORD                                  
         AHI   R3,05                  BUMP PAST PASSWORD                        
         B     SFX09X                                                           
*                                                                               
SFX09T   MVC   1(08,R3),=CL08'.DDSTEST'   PASSWORD FOR TST                      
         AHI   R3,09                      BUMP PAST PASSWORD                    
*                                                                               
SFX09X   BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
* READ DESTINATION TABLE ONE ENTRY AT A TIME AND BUILD /NTF AND                 
* CUSTOM REF #, TEL#, BILLING CODE...                                           
*                                                                               
         L     R7,ADSTAEFT         A(DESTINATION TABLE)                         
         USING DSTAEFTD,R7                                                      
*                                  CHECK THE 1ST DEST                           
         CLC   =C'FAXW',DSTAEDST   DESTINATION BEGIN W/ FAX/FAXW?               
         BNE   SFX10                                                            
         CLI   HDR+34,C'L'         LANDSCAPE MODE SET?                          
         BE    SFX10               YES - DON'T SET WIDE MODE                    
         MVI   HDR+34,C'W'         NO - SET WIDE REPORT                         
*                                                                               
SFX10    EQU   *                                                                
         CLI   DEBUG,C'Y'          DEBUG FORCES ALL TO REDILIST MQTEST          
         BNE   SFX12                                                            
         MVC   DSTAEDST,=CL25'MQTEST'                                           
         MVI   DSTTYPE,EDFDSTRQ    ITS A REDILIST NAME                          
         B     SFX24                                                            
*                                                                               
SFX12    CLC   DSTAEDST,SPACESL    SPACES?                                      
         BE    SFX22               ALL SPACES - JUNK DEST                       
*                                                                               
         CLC   =C'FAX',DSTAEDST    IS DESTINATION A FAX MACHINE?                
         BNE   *+12                                                             
         MVI   DSTTYPE,EDFDSTFQ    IT'S A FAX NUMBER                            
         B     SFX24                                                            
*                                                                               
         MVI   LONGFXPG,C'N'       ASSUME FAX PAGES AREN'T "LONG"               
         CLC   =C'FXKEY=',DSTAEDST IS DESTINATION A FAX RECORD KEY?             
         BE    SFX14                                                            
         CLC   =C'FXKYL=',DSTAEDST (FOR "LONG" FAX PAGES)                       
         BNE   SFX16                                                            
         MVI   LONGFXPG,C'Y'       EASYLINK MUST GENERATE "LONG" PAGES          
*                                                                               
SFX14    MVI   DSTTYPE,EDFDSTXQ    YES                                          
         MVC   FAXKEY,DSTAEDST+6                                                
         BRAS  RE,BLDCVR           BUILD COVER PAGE                             
         OC    DSTAEDST,DSTAEDST   WAS THE FAX RECORD FOUND?                    
         BZ    SFX22               NO - ERROR                                   
         B     SFX24                                                            
*                                                                               
SFX16    CLI   HDR+67,C'D'         IS THIS A DARE FAX?                          
         BNE   SFX18               SPECIAL TREATMENT FOR DARE FAXES             
*                                                                               
         BRAS  RE,DAREFAX                                                       
         MVC   FAXKEY,WORK         RETURNED BY DAREFAX ROUTINE                  
*                                                                               
         OC    FAXKEY,FAXKEY       DID WE BUILD A RECEIVING ID?                 
         BZ    SFX22               NO - ERROR                                   
         MVC   EASYDATA+14(7),FAXKEY                                            
*                                                                               
         CLI   DSTTYPE,EDFDSTHQ    HOME MARKET W/O ID?                          
         BE    *+10                                                             
         MVC   AGYPOWER,=C'D7'     NO, ALWAYS USE USERID 'DARADM'               
*                                                                               
         MVI   LONGFXPG,C'N'       STANDARD SIZE FAX PAGES                      
         BRAS  RE,BLDCVR           BUILD COVER PAGE                             
         OC    DSTAEDST,DSTAEDST   WAS THE FAX RECORD FOUND?                    
         BZ    SFX22               NO - ERROR                                   
         B     SFX24                                                            
*                                                                               
SFX18    MVI   DSTTYPE,EDFDSTMQ    ASSUME IT'S A MAILBOX NUMBER                 
         CLI   DSTAEDST,C'9'       IS IT A MAILBOX NUMBER?                      
         BH    *+12                NO - REDILIST NAME                           
         CLI   DSTAEDST,C'0'                                                    
         BNL   SFX24               YES - MAILBOX NUMBER                         
*                                                                               
         MVI   DSTTYPE,EDFDSTRQ    IT'S A REDILIST NAME                         
         CLI   DSTAEDST+3,C' '     STATION CALL LETTERS ARE ASSUMED             
         BH    *+8                 IF THERE ARE ONLY 3 CHARACTERS. . .          
         MVI   DSTAEDST+3,C'9'     . . .THEN INSERT A '9' (EX:  WGN9)           
         CLI   DSTAEDST+4,C' '     MEDIA 'T'?                                   
         BH    *+8                                                              
         MVI   DSTAEDST+4,C'T'     YES -- INSERT THE 'T' (EX:  WGN9T)           
*                                                                               
         CLI   DSTAEDST+3,C'9'     DID I INSERT A '9'?                          
         BNE   SFX24               NO                                           
*                                  YES - CHECK EZLINK COMMAND LIST              
         LA    RF,EZRSVCMD                                                      
SFX20    CLI   0(RF),X'FF'                                                      
         BE    SFX24               END OF LIST                                  
         CLC   0(3,RF),DSTAEDST                                                 
         BE    *+12                THIS IS A RESERVED EZ COMMAND                
         AHI   RF,3                                                             
         B     SFX20                                                            
*                                  ADD Z9 IN THE FRONT OF REDILIST NAME         
         MVC   WORK(2),=C'Z9'                                                   
         MVC   WORK+2(L'DSTAEDST-2),DSTAEDST                                    
         MVC   DSTAEDST,WORK                                                    
         B     SFX24                                                            
*                                                                               
SFX22    EQU   *                                                                
*                                                                               
         OI    DSTAEFLG,DSTAEFJQ   FLAG THIS DEST AS JUNK                       
         B     SFX45               NEXT DEST                                    
*                                                                               
SFX24    CLI   SENDDLNS,C'Y'       DO WE WANT DELIVERY NOTIFICATIONS?           
         BNE   SFX26                                                            
         MVC   0(4,R3),=CL04'/NTF' SO WE GET NOTIFICATIONS                      
         AHI   R3,4                                                             
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
SFX26    MVI   LSTSDST,C'Y'        GOT A GOOD DESTINATION TO SEND TO            
*                                                                               
         CLI   DRTEST,C'Y'         DISASTER RECOVERY TEST MODE?                 
         BNE   *+18                                                             
         MVC   00(16,R3),=CL16'FAXW 18457594610'                                
         AHI   R3,16               YES -- IGNORE APPLIC. FAX NUMBER             
         B     SFX34                                                            
*                                                                               
         L     RF,ATSTIDS          TABLE OF DDS TEST USERIDS                    
SFX28    CLI   0(RF),X'FF'                                                      
         BE    SFX28A                                                           
         CLC   USERID,0(RF)                                                     
         BE    SFX28E                                                           
         AHI   RF,21                                                            
         B     SFX28                                                            
*                                                                               
SFX28A   L     RF,ATSTAGS          TABLE OF DDS TEST AGENCIES                   
SFX28B   CLI   0(RF),X'FF'                                                      
         BE    SFX30                                                            
         CLC   SVAGYPOW,0(RF)                                                   
         BE    SFX28E                                                           
         AHI   RF,21                                                            
         B     SFX28B                                                           
*                                                                               
SFX28E   EQU   *                                                                
         MVC   00(05,R3),=C'FAXW ' DDS USERID: USE OUR FAX NUMBER               
         MVC   05(11,R3),10(RF)                                                 
         AHI   R3,16                                                            
         B     SFX34                                                            
*                                                                               
SFX30    MVC   00(25,R3),DSTAEDST  DESTINATION                                  
         LA    RF,25(R3)                                                        
         LHI   R0,25                                                            
SFX32    CLI   0(RF),C'.'          REPLACE . BY - (EZ CAN'T HANDLE)             
         BNE   *+8                                                              
         MVI   0(RF),C'-'                                                       
         BCTR  RF,0                                                             
         BCT   R0,SFX32                                                         
*                                                                               
         AHI   R3,25               FIND END OF TEXT                             
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         AHI   R3,1                                                             
*                                                                               
SFX34    MVI   0(R3),C';'          SEMICOLON (DELIMITER) FOR CUSTREF            
         AHI   R3,1                                                             
*                                                                               
         BRAS  RE,BLDCREF          REFERENCE NUMBER RETURNS IN CUSTREF          
         MVC   0(L'CUSTREF,R3),CUSTREF                                          
         AHI   R3,L'CUSTREF                                                     
         MVI   0(R3),C':'          COLON (DELIMITER)                            
         AHI   R3,1                                                             
*                                                                               
         MVC   0(L'USERID,R3),USERID START BILLCODE WITH USERID                 
*                                                                               
         CLI   FACTYPE,C'A'        ADV?                                         
         BE    SFX42               YES -- NO OVERRIDE ALLOWED                   
*                                                                               
         CLC   HDR(4),SPACESL      OVERRIDE ORIGIN GIVEN?                       
         BNH   SFX42                                                            
         MVC   0(4,R3),HDR         YES                                          
         MVC   4(4,R3),SPACESL                                                  
*                                                                               
SFX42    MVC   08(13,R3),HDR+54    REMAINDER OF BILLING INFORMATION             
         AHI   R3,20               A(LAST BILLING CHARACTER)                    
         CLI   0(R3),C' '          FIND LAST NON-BLANK CHARACTER                
         BH    *+8                 GOT IT                                       
         BCT   R3,*-8              BACK UP                                      
         AHI   R3,1                                                             
*                                                                               
* LOOK FOR MORE DESTINATIONS                                                    
*                                                                               
SFX45    EQU   *                                                                
         TM    DSTAEFLG,DSTAEFJQ   JUNK DEST?                                   
         BO    SFX46               YES                                          
         MVI   0(R3),C','          DELIMITER, BUT MORE DEST TO COME             
         AHI   R3,1                                                             
         BRAS  RE,ADDCRLF                                                       
*                                  + LINE# COUNTER BUT NOT PRINT IT             
         L     R0,NUMLINES                                                      
         AHI   R0,1                                                             
         ST    R0,NUMLINES                                                      
*                                                                               
SFX46    AHI   R7,DSTAEFTL         NEXT DESTINATION TABLE ENTRY                 
         CLI   0(R7),X'FF'         EOT?                                         
         BE    SFX50                                                            
         OC    DSTAEDST,DSTAEDST   FINISHED?                                    
         BZ    SFX50                                                            
         B     SFX10                                                            
         DROP  R7                                                               
*                                                                               
SFX50    CLI   LSTSDST,C'Y'    *** FOUND AT LEAST ONE GOOD DESTINATION?         
         BNE   SFX100              WRITE OUT ERROR MESSAGE AND QUIT             
*                                                                               
         LR    RE,R3                                                            
         SHI   RE,3                PT BACK TO LAST ","                          
         CLI   0(RE),C','                                                       
         BNE   *+8                                                              
         MVI   0(RE),C'+'          DELIMITER FOR LAST DEST                      
         BRAS  RE,LINESUP                                                       
*                                                                               
*don't need this subject line anymore, yyun, 3/13/06                            
*        MVC   0(35,R3),=CL35'SUBJECT MQ TESTING FROM DDS'                      
*        AHI   R3,35                                                            
*        BRAS  RE,ADDCRLF                                                       
*        BRAS  RE,LINESUP                                                       
*                                                                               
         CLI   HDR+35,C'P'         WANT TO REPLACE X'89'S WITH /PAGE?           
         BNE   SFX52                                                            
         MVI   LASTEJCT,C'Y'       LAST LINE WAS A /PAGE                        
         MVC   00(10,R3),=CL10'/FORM/PAGE'                                      
         AHI   R3,10                                                            
*                                                                               
SFX52    CLI   HDR+34,C'W'         WIDE REPORT?                                 
         BNE   *+14                                                             
         MVC   00(05,R3),=CL05'/WIDE'                                           
         AHI   R3,5                                                             
*                                                                               
         CLI   HDR+34,C'L'         LANDSCAPE REPORT?                            
         BNE   *+14                                                             
         MVC   00(10,R3),=CL10'/LANDSCAPE'                                      
         AHI   R3,10                                                            
*                                                                               
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
         CLI   DRTEST,C'Y'         DISASTER RECOVERY TEST MODE?                 
         BNE   *+8                                                              
         BRAS  RE,PDRTEST                                                       
*                                                                               
         OC    COVPGLNS,COVPGLNS   NOW OUTPUT COVER PAGE IF REQUIRED            
         BZ    SFX58                                                            
         BRAS  RE,PRCCVR                                                        
         B     SFX58               CURRENT LINE ALREADY IN R / RLEN             
*                                                                               
SFX55    BRAS  RE,GETNXTL          GET NEXT LINE INTO R / RLEN                  
         BNE   SFX70               EOB                                          
*                                                                               
         LR    RF,R3               MAKE SURE WE CAN FIT MORE DATA               
         AH    RF,RLEN                                                          
         AHI   RF,32               APPROXIMATION FOR MAX SIZE                   
         S     RF,AMSGOUT                                                       
         C     RF,MAXMSGLN                                                      
         BH    TOOBIG                                                           
*                                                                               
SFX58    CLC   R+1(L'R-1),SPACESL  IS THIS AN EMPTY LINE?                       
         BE    SFX60               YES                                          
*                                                                               
         LR    RE,R3               RE = MOVE TO POINT                           
         LH    R1,RLEN                                                          
         AHI   R1,-1                                                            
         AR    R3,R1               POINT R3 TO END OF MOVE BLOCK                
         LA    R0,R+1              IGNORE PRINTER CC                            
         LR    RF,R1                                                            
         MVCL  RE,R0               MOVE IN DATA                                 
*                                                                               
SFX60    BRAS  RE,ADDCRLF          NEXT LINE                                    
         BRAS  RE,LINESUP                                                       
*                                                                               
         CLI   HDR+35,C'P'         REPLACE X'89'S WITH /PAGE?                   
         BNE   SFX65                                                            
*                                                                               
         CLI   R,X'89'             REQUEST PAGE THROW?                          
         BNE   *+14                                                             
         CLC   R+1(L'R-1),SPACESL  ONLY IF BLANK LINE                           
         BE    *+12                                                             
         MVI   LASTEJCT,C'N'                                                    
         B     SFX65               THIS IS NOT A PAGE THROW                     
*                                                                               
         CLI   LASTEJCT,C'Y'       IF LAST LINE ALSO EJECT IGNORE THIS          
         BE    SFX65                                                            
*                                                                               
         LH    RF,NUMPAGES         INCREMENT PAGE COUNTER                       
         AHI   RF,1                                                             
         STH   RF,NUMPAGES                                                      
*                                                                               
         MVI   LASTEJCT,C'Y'       LAST LINE WAS A /PAGE                        
         MVC   0(5,R3),=CL05'/PAGE'                                             
         AHI   R3,5                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
SFX65    CLI   R,X'19'             TRIPLE SPACE                                 
         BNE   *+12                                                             
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         CLI   R,X'11'             DOUBLE SPACE                                 
         BNE   *+8                                                              
         BRAS  RE,ADDCRLF                                                       
         B     SFX55                                                            
*                                                                               
SFX70    MVC   0(4,R3),=CL4'MMMM'  END OF MESSAGE BLOCK                         
         AHI   R3,4                                                             
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
         MVC   MSGDESC_ACCOUNTINGTOKEN,TSTHDRF                                  
         MVC   MSGDESC_ACCOUNTINGTOKEN(6),TSTHDRT                               
         CLI   FACTYPE,C'A'        ADV?                                         
         BNE   *+10                                                             
         MVC   MSGDESC_ACCOUNTINGTOKEN(6),TSTHDRA                               
         CLI   FACTYPE,C'R'        REP?                                         
         BNE   *+10                                                             
         MVC   MSGDESC_ACCOUNTINGTOKEN(6),TSTHDRR                               
         CLI   FACTYPE,C'Q'        FQA?                                         
         BNE   *+10                                                             
         MVC   MSGDESC_ACCOUNTINGTOKEN(6),TSTHDRQ                               
         MVC   MSGDESC_FORMAT,=CL8'MQSTR'                                       
         ICM   RF,15,PUTOPTS_OPTIONS                                            
         A     RF,=AL4(MQPMO_SET_IDENTITY_CONTEXT)                              
         STCM  RF,15,PUTOPTS_OPTIONS                                            
*                                                                               
         MVC   MQPUTBUF,AMSGOUT    PUT THIS MESSAGE TO MQ                       
         S     R3,AMSGOUT                                                       
         ST    R3,DATALEN          SET MESSAGE LENGTH                           
*                                                                               
         LA    R2,MQPUT            MQ PUT                                       
         BRAS  RE,CALLMQ                                                        
         BNE   CANTSEND                                                         
*                                                                               
         LA    R2,MQCMIT           MQ COMMIT                                    
         BRAS  RE,CALLMQ                                                        
         BNE   CANTSEND                                                         
*                                                                               
         ICM   RF,15,PUTOPTS_OPTIONS                                            
         S     RF,=AL4(MQPMO_SET_IDENTITY_CONTEXT)                              
         STCM  RF,15,PUTOPTS_OPTIONS                                            
*                                                                               
         CLI   TRACE,C'F'          IF TRACING OUTPUT LEAVE SPACE LINE           
         BNE   *+8                                                              
         BRAS  RE,PRNT                                                          
         BRAS  RE,SENTMSG          RPT MESSAGE AS SENT                          
*                                                                               
         L     R7,ADSTAEFT     *** MARK ALL DESTINATIONS SENT                   
         USING DSTAEFTD,R7                                                      
SFX81    CLI   0(R7),X'FF'         EOT                                          
         BE    EXITOK                                                           
         OC    DSTAEDST,DSTAEDST                                                
         BZ    EXITOK                                                           
*                                                                               
         BRAS  RE,POSTSENT         ADD MQ MESSAGE FOR THIS ONE                  
         BNE   SFX90                                                            
*                                                                               
         TM    DSTAEFLG,DSTAEFJQ   IS THIS DEST GOOD?                           
         BNO   SFX85               NO                                           
*                                                                               
         MVC   PLINE+00(L'JUNKRPT),JUNKRPT                                      
         LA    RF,PLINE+L'JUNKRPT                                               
         MVC   0(L'DSTAEDST,RF),DSTAEDST                                        
         AHI   RF,L'DSTAEDST+3                                                  
         MVC   0(L'CUSTREF,RF),CUSTREF                                          
*                                                                               
         MVC   ERRMSG1R,PLINE                                                   
         GOTO1 VDMGR,DMCB,=C'OPMSG',('ERRMSG1Q',ERRMSG1)                        
         BRAS  RE,PRNT                                                          
*                                                                               
         L     RE,JREPCNT                                                       
         AHI   RE,1                                                             
         ST    RE,JREPCNT          INCREMENT JUNK REPORT COUNTER                
         B     SFX90                                                            
*                                                                               
SFX85    MVC   PLINE(L'SENTRPT),SENTRPT                                         
         MVC   PLINE+L'SENTRPT(L'DSTAEDST),DSTAEDST                             
         MVC   PLINE+L'SENTRPT+L'DSTAEDST+3(L'CUSTREF),CUSTREF                  
         BRAS  RE,PRNT                                                          
*                                                                               
SFX90    AHI   R7,DSTAEFTL         NEXT DESTINATION                             
         B     SFX81                                                            
         DROP  R7                                                               
*                                                                               
* NO VALID DESTINATIONS - OUTPUT ERROR MESSAGE                                  
*                                                                               
SFX100   MVC   PLINE(L'CANTSND),CANTSND                                         
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R7,ADSTAEFT     *** MARK ALL DESTINATIONS JUNK                   
         USING DSTAEFTD,R7                                                      
SFX120   CLI   0(R7),X'FF'         EOT                                          
         BE    SFX140                                                           
         OC    DSTAEDST,DSTAEDST                                                
         BZ    SFX140                                                           
*                                                                               
         MVI   EERRORCD,EDFERNFQ   NO FAX NUMBER                                
         BRAS  RE,POSTSENT                                                      
         BNE   SFX130                                                           
*                                                                               
         MVC   PLINE+00(L'JUNKRPT),JUNKRPT                                      
         LA    RF,PLINE+L'JUNKRPT                                               
         MVC   0(L'DSTAEDST,RF),DSTAEDST                                        
         AHI   RF,L'DSTAEDST+3                                                  
         MVC   0(L'CUSTREF,RF),CUSTREF                                          
*                                                                               
         MVC   ERRMSG1R,PLINE                                                   
         GOTO1 VDMGR,DMCB,=C'OPMSG',('ERRMSG1Q',ERRMSG1)                        
         BRAS  RE,PRNT                                                          
*                                                                               
         L     RE,JREPCNT                                                       
         AHI   RE,1                                                             
         ST    RE,JREPCNT          INCREMENT JUNK REPORT COUNTER                
*                                                                               
SFX130   AHI   R7,DSTAEFTL         NEXT DESTINATION                             
         B     SFX120                                                           
         DROP  R7                                                               
*                                                                               
SFX140   CLC   JREPCNT,JREPMAX                                                  
         BH    TOOJREP                                                          
         B     EXITOK                                                           
*                                                                               
NOHDR    EQU   *                                                                
         MVC   HDRMUID,USERID                                                   
         MVC   HDRMSUB,RPTSUBID                                                 
*                                                                               
         GOTO1 VHEXIN,DMCB,RPTPQNUM,HALF,4,0                                    
         SR    R0,R0                                                            
         ICM   R0,3,HALF                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HDRMNUM,DUB                                                      
*                                                                               
         MVC   PLINE(HDRMISSQ),HDRMISS                                          
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
TOOJREP  WTO   'EDIFAXSND - TOO MANY JUNK REPORTS, PLEASE RECYCLE THIS +        
               JOB.'                                                            
         DC    H'0'                                                             
*                                                                               
CANTSEND GOTO1 VLOGIO,DMCB,X'FF000001',=C'EDIFAXSND - Error sending to +        
               EASYLINK - Transmissions suspended, please check'                
         ABEND 701                                                              
*                                                                               
TOOBIG   GOTO1 VLOGIO,DMCB,X'FF000001',=C'EDIFAXSND - Report is too    +        
               big to fit in buffer - must increase MAXMSGLEN'                  
         ABEND 701,DUMP                                                         
*                                                                               
JUNKRPT  DC    C'Report marked junk - '                                         
SENTRPT  DC    C'Report marked sent - '                                         
CANTSND  DC    C'Unable to send report - no valid destinations '                
HDRMISS  DC    C'Missing Header, skip report: '                                 
HDRMUID  DS    CL8                                                              
         DC    C','                                                             
HDRMSUB  DS    CL3                                                              
         DC    C','                                                             
HDRMNUM  DS    CL5                                                              
HDRMISSQ EQU   *-HDRMISS                                                        
*                                                                               
TSTHDRT  DC    X'444453545354'     DDSTST                                       
TSTHDRA  DC    X'444453414456'     DDSADV                                       
TSTHDRR  DC    X'444453524550'     DDSREP                                       
TSTHDRQ  DC    X'444453465141'     DDSFQA                                       
TSTHDRF  DC    32X'20'                                                          
         EJECT                                                                  
***********************************************************************         
* EASYLINK 3 CHAR RESERVED COMMAND LIST                               *         
***********************************************************************         
         SPACE 1                                                                
EZRSVCMD DS    0CL3                                                             
         DC    C'CFM',C'ELN',C'ESL',C'FCH',C'CMG',C'ZIP',C'CLS',C'DLY'          
         DC    C'FON',C'FWD',C'FYI',C'ICS',C'PLS',C'FCR',C'INT',C'MBX'          
         DC    C'MMX',C'MGM',C'PMS',C'GRT',C'ITS',C'ITX',C'NFT',C'NTF'          
         DC    C'NTS',C'NTY',C'PRI',C'RCA',C'TLX',C'TRT',C'TWX',C'WUW'          
         DC    C'FAX',C'FCR',C'EDI',C'FXL',C'WUI',C'EXT',C'COL',C'COM'          
         DC    C'FTC',C'RPT',C'WWU',C'SVC',C'FLW',C'FWL',C'FXW'                 
         DC    XL3'00'                                                          
         EJECT                                                                  
***********************************************************************         
* CLEAR ALL FIELDS BEFORE STARTING PROCESSING FAX                     *         
***********************************************************************         
         SPACE 1                                                                
CLRFLDS  NTR1  BASE=*,LABEL=*                                                   
         MVI   GOTHDR,C'N'                                                      
         XC    REQUESTR,REQUESTR                                                
         XC    LASTOUT,LASTOUT                                                  
         XC    FAXSUBC,FAXSUBC                                                  
         XC    NUMLINES,NUMLINES                                                
         XC    COVPGLNS,COVPGLNS   NO COVER PAGE YET                            
         XC    EASYDATA,EASYDATA                                                
         MVC   NUMPAGES,=H'1'      AT LEAST ONE PAGE WILL BE SENT               
         XC    LSTSDST,LSTSDST     ASSUME NO REPORT IS SENDABLE                 
         XC    RPTLDSTS,RPTLDSTS                                                
         XC    HDR,HDR                                                          
*                                                                               
         L     R2,ALINETAB         CLEAR OUT THE ++DDS LINES                    
         USING LINETABD,R2                                                      
CFL02    XC    LINELEN,LINELEN                                                  
         MVC   LINEIN,SPACESL                                                   
         AHI   R2,LINETABL                                                      
         CLI   LINEID,X'FF'                                                     
         BNE   CFL02                                                            
*                                                                               
         L     R0,ADSTAEFT         CLEAR OUT DESTINATION TABLE                  
         LHI   R1,DSTAEFTL*DSTMAXQ                                              
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET NEXT LINE INTO R AND RLEN                                       *         
* NTRY: R4 = CURRENT BUFFER POINTER                                   *         
*       R3 = A(OUTPUT BUFFER) AFTER GETTING THE HDR                   *         
***********************************************************************         
         SPACE 1                                                                
GETNXTL  NTR1  BASE=*,LABEL=*                                                   
GNL10    XC    RLEN,RLEN           WE SET LENGTH OF THIS LINE HERE              
         MVC   R,SPACESL           WE MOVE INPUT A LINE AT A TIME HERE          
*                                                                               
         LR    R0,R4               CHECK NOT PAST END OF BUFFER                 
         S     R0,AMSGIN                                                        
         C     R0,DATALEN                                                       
         BNL   EXITL               EOB                                          
*                                                                               
         PACK  DUB,0(4,R4)         GET LENGTH OF THIS LINE                      
         CVB   R1,DUB                                                           
         AHI   R4,4                GO PAST LENGTH                               
*                                                                               
         LR    R0,R4               COPY LINE INTO R AND LEN INTO RLEN           
         STH   R1,RLEN                                                          
         LA    RE,R                                                             
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         AH    R4,RLEN                                                          
         CLC   =C'/PAGE',R+1       /PAGE?                                       
         BNE   GNL20               NO - CONTINUE                                
         CLI   HDR+35,C'P'         ALREADY PAGE BREAK BY CC=X'89'?              
         BE    GNL10               YES - SKIP THIS /PAGE                        
*                                                                               
         CLI   GOTHDR,C'Y'         TEST ALREADY HAVE HEADER                     
         BNE   GNL20               NO - CONTINUE                                
*                                  R3=A(OUTPUT BUFFER)                          
         AHI   R3,-9                                                            
         CLC   =X'61E6C9C4C50D25',0(R3)  /WIDE(CRLF)?                           
         BE    GNL10               YES - SKIP THIS 1ST /PAGE                    
*                                                                               
GNL20    TR    R+1(L'R-1),TRTAB    TRANSLATE OUT UNPRINTABLES                   
*                                                                               
         LHI   R0,81                                                            
         CLI   HDR+34,C'W'         WIDE REPORT?                                 
         BNE   *+8                                                              
         LHI   R0,133                                                           
         CLI   HDR+34,C'L'         LANDSCAPE REPORT?                            
         BNE   *+8                                                              
         LHI   R0,133                                                           
         CH    R0,RLEN                                                          
         BH    *+8                                                              
         STH   R0,RLEN                                                          
*                                                                               
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
***********************************************************************         
* PROCESS *HDR* CARD - THIS SHOULD BE THE FIRST CARD                  *         
***********************************************************************         
         SPACE 1                                                                
         USING DSTAEFTD,R7                                                      
PRCHDR   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'*HDR*',R+5       IS THIS THE HEADER                           
         BNE   EXITL               NO - LOOP UNTIL YOU GET IT                   
         MVI   GOTHDR,C'Y'                                                      
*                                                                               
         CLC   =C'EDICT=',R+10     OVERRIDE FOR KEY OF RECORD?                  
         BNE   *+10                NO                                           
         MVC   USERID,R+16                                                      
         MVC   DSTAEDST,R+10       SET FIRST DESTINATION                        
         MVC   DSTAEFRM,R+39       SET FORMATTED DESTINATION                    
         AHI   R7,DSTAEFTL         GO TO NEXT                                   
*                                                                               
         LH    R1,RLEN             SAVE HEADER STRIPPING OFF THE CC             
         AHI   R1,-1                                                            
         STH   R1,HDRLEN                                                        
         LA    R0,HDR                                                           
         LA    RE,R+1                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     EXITR7              CONTINUE                                     
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ++DDS CARD(S)                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING DSTAEFTD,R7                                                      
PRCDDS   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'DST',R+12        WE CAN HAVE MULTIPLE DST CARDS               
         BNE   PDDS02              THEY GET STACKED FOR USE LATER               
*                                                                               
         MVC   DSTAEDST,R+16       DESTINATION                                  
         MVC   DSTAEFRM,R+42       SET FORMATTED DESTINATION                    
         AHI   R7,DSTAEFTL         GO TO NEXT                                   
         CLI   0(R7),X'FF'         REACHED EOT?                                 
         BNE   EXITR7              NO                                           
*                                                                               
         LHI   R0,29               INCREASE REPEAT COUNT ON DSTTAB              
         BRAS  RE,SYSMESS                                                       
         ABEND 911,DUMP                                                         
*                                                                               
PDDS02   L     R2,ALINETAB         NOW DEAL WITH OTHER CONTROL CARDS            
         USING LINETABD,R2         THESE SHOULD BE SINGLETONS                   
*                                                                               
PDDS04   CLC   LINEID,R+12         MATCH CONTROL CARD?                          
         BE    PDDS06              YES                                          
         AHI   R2,LINETABL                                                      
         CLI   LINEID,X'FF'        EOT?                                         
         BNE   PDDS04              NO - NEXT CARD                               
*                                                                               
         MVI   FERN,20             UNKNOWN ++DDS CARD                           
         BRAS  RE,ERRMSG                                                        
         MVC   PLINE,R+1                                                        
         BRAS  RE,PRNT                                                          
         B     EXITL               IGNORE IT FOR NOW                            
*                                                                               
PDDS06   LH    R1,RLEN             MOVE FROM R  - STRIP OFF LEADING CC          
         AHI   R1,-1                                                            
         STH   R1,LINELEN                                                       
         LA    R0,LINEIN                                                        
         LA    RE,R+1                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     EXITOK              GET NEXT CARD                                
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* OUTPUT DRTEST HEADER ON FAX                                         *         
***********************************************************************         
         SPACE 1                                                                
PDRTEST  NTR1  BASE=*,LABEL=*                                                   
         MVC   00(50,R3),STARS     LINE OF ASTERIXES                            
         AHI   R3,50                                                            
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
         BRAS  RE,ADDCRLF          SPACE LINE                                   
         BRAS  RE,LINESUP                                                       
*                                                                               
         MVC   00(L'TSTFAX,R3),TSTFAX                                           
         AHI   R3,L'TSTFAX                                                      
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
         BRAS  RE,ADDCRLF          SPACE LINE                                   
         BRAS  RE,LINESUP                                                       
*                                                                               
         MVC   00(50,R3),STARS     LINE OF ASTERIXES                            
         AHI   R3,50                                                            
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
         BRAS  RE,ADDCRLF          SPACE LINE                                   
         BRAS  RE,LINESUP                                                       
         B     EXITR3                                                           
*                                                                               
TSTFAX   DC    C'THIS IS A TEST FAX TRANSMISSION - PLEASE DISREGARD'            
         EJECT                                                                  
***********************************************************************         
* ADD COVERSHEET TO OUTPUT BUFFER                                     *         
***********************************************************************         
         SPACE 1                                                                
PRCCVR   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACOVSHT                                                       
         LH    R0,COVPGLNS                                                      
PCVR02   CLC   SPACESL(132),0(R2)  BLANK LINE                                   
         BE    PCVR04                                                           
*                                                                               
         MVC   00(132,R3),0(R2)    MOVE IN THIS LINE                            
         AHI   R3,131                                                           
         CLI   0(R3),C' '          FIND TEXT END (REDUCES BUFFER SIZE)          
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         AHI   R3,1                                                             
*                                                                               
PCVR04   BRAS  RE,ADDCRLF          EOL                                          
         BRAS  RE,LINESUP                                                       
         AHI   R2,132              NEXT IN COVER SHEET                          
         BCT   R0,PCVR02                                                        
*                                                                               
         LH    RF,NUMPAGES         INCREMENT PAGE COUNTER                       
         AHI   RF,1                                                             
         STH   RF,NUMPAGES                                                      
*                                                                               
         MVC   00(05,R3),=CL05'/PAGE'                                           
         AHI   R3,5                                                             
         BRAS  RE,LINESUP                                                       
         B     EXITR3                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT 'FAX SENT' MESSAGE TO CONSOLE                     *         
***********************************************************************         
         SPACE 1                                                                
SENTMSG  NTR1  BASE=*,LABEL=*                                                   
         MVC   PLINE(L'FAXSNT),FAXSNT                                           
         LA    R2,PLINE+L'FAXSNT                                                
         EDIT  NUMLINES,(6,0(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVC   0(L'LINESNT,R2),LINESNT                                          
         AHI   R2,L'LINESNT                                                     
         EDIT  NUMPAGES,(6,0(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVC   0(L'PAGESNT,R2),PAGESNT                                          
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE(19),=CL19'Reference number = '                             
         MVC   PLINE+19(L'CUSTREF),CUSTREF                                      
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
FAXSNT   DC    C'Fax sent to Easylink '                                         
LINESNT  DC    C' lines in '                                                    
PAGESNT  DC    C' pages using MQ buffer'                                        
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WAIT UNTIL POSTED OR A MESSAGE ARRIVES                   *         
***********************************************************************         
         SPACE 1                                                                
WAIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
WAIT02   XC    GETECB,GETECB       CLEAR SIGNAL ECB                             
         XC    MSGDESC_MSGID,MSGDESC_MSGID                                      
         XC    MSGDESC_CORRELID,MSGDESC_CORRELID                                
*                                                                               
         LHI   RF,MQGMO_SET_SIGNAL+MQGMO_ACCEPT_TRUNCATED_MSG                   
         ST    RF,GETOPTS_OPTIONS                                               
         LA    RF,GETECB           TELL IT TO SIGNAL AND SET ECB                
         ST    RF,GETOPTS_SIGNAL1                                               
         LHI   RF,MQWI_UNLIMITED                                                
         ST    RF,GETOPTS_WAITINTERVAL                                          
*                                                                               
         MVC   MQGETBUF,AMSGIN     MESSAGE                                      
*                                                                               
         LA    R2,MQGET                                                         
         BRAS  RE,CALLMQ           CALL MQ TO FLAG WHEN MESSAGE ARRIVES         
         BE    EXITOK              YOU DON'T NEED TO WAIT                       
         BL    WAIT02              ERROR ON GET - IGNORE AND CONTINUE           
*                                                                               
WAIT04   WAIT  1,ECBLIST=ECBLST                                                 
*                                                                               
         TM    GETECB,X'40'        MQ SIGNALS MESSAGE ARRIVED                   
         BO    WAIT02              YES - GO GET IT                              
*                                                                               
         L     RF,AOPERECB                                                      
         TM    0(RF),X'40'         OPERATOR INTERRUPT                           
         BZ    *+8                                                              
         BRAS  RE,CHKOPER                                                       
         CLI   OPERSTOP,C'Y'       OPERATOR STOP REQUESTED?                     
         BE    EXITL               YES                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET DATA FROM EDICT RECORD FOR THIS USER                            *         
***********************************************************************         
         SPACE 1                                                                
BLDDEST  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,KEY                                                           
         USING EDIKEYD,R5                                                       
         XC    EDIKEY,EDIKEY       CLEAR KEY                                    
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
         MVC   EDINAME,USERID      USER ALPHA                                   
*                                  FORCE TO READ FROM DISK                      
         GOTO1 VDMGR,DMCB,(X'24',DMRDHI),CTFILE,(R5),AIO,0                      
*                                                                               
         L     R5,AIO                                                           
         CLC   EDIKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BD02     L     R4,AIO                                                           
         AHI   R4,28                                                            
         XR    RF,RF                                                            
         USING EDILNKD,R4                                                       
*                                                                               
BD04     CLI   EDILNKEL,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   EDILNKEL,EDILNKEQ                                                
         BE    *+12                                                             
         IC    RF,EDILNKLN                                                      
         BXH   R4,RF,BD04                                                       
*                                                                               
         MVC   DESTNAME,EDINAME    USERID                                       
         MVC   DESTMETS,EDIMETHS   METHOD OF SENDING TRANSMISSIONS              
         MVC   DESTMETR,EDIMETHR   METHOD OF RECEIVING TRANSMISSIONS            
         MVC   DESTADVN,EDIADVNO   ADV EASYLINK MAILBOX NUMBER                  
         MVC   DESTREPN,EDIREPNO   REP EASYLINK MAILBOX NUMBER                  
         MVC   DESTUIDN,USERIDNO                                                
         TR    DESTNTRY,TRTAB                                                   
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,1                                                             
         BRAS  RE,SYSMESS          STARTING INITIALISE                          
*                                                                               
         LR    R0,RC                                                            
         LHI   R1,WORKL                                                         
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR NBASE W/S                              
*                                                                               
         ZAP   LINE,P99                                                         
         MVC   TITLE,VCTITLE                                                    
*                                                                               
         LA    R2,WORK             EXTRACT MVS JOBNAME                          
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,WORK                                                          
         MVC   MVSNAME,0(R2)                                                    
*                                                                               
         BRAS  RE,GETCARDS         READ IN INPUT CARDS                          
         BL    EXITL                                                            
         BRAS  RE,VALCARDS         VALIDATE INPUT CARDS                         
         BL    EXITL                                                            
         BRAS  RE,OPENCTFL         OPEN CTFILE IF NOT DONE YET                  
*                                                                               
         ZAP   LINE,P99                                                         
         MVC   TITLE,DWTITLE                                                    
*                                                                               
         BRAS  RE,MQINIT                                                        
         BL    EXITL                                                            
         BRAS  RE,SETOPS           SET UP OPERATOR INPUT                        
         BL    EXITL                                                            
*                                                                               
         L     R0,MAXMSGLN         TWO BUFFERS REQUIRED INPUT + OUTPUT          
         SLL   R0,1                                                             
*                                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    INIT02                                                           
         LHI   R0,21                                                            
         BRAS  RE,SYSMESS          GETMAIN FAILED                               
         ABEND 911,DUMP                                                         
*                                                                               
INIT02   ST    R1,AMSGIN           A(INPUT BUFFER)                              
         A     R1,MAXMSGLN                                                      
         ST    R1,AMSGOUT          A(OUTPUT BUFFER)                             
*                                                                               
         LHI   R0,2                                                             
         BRAS  RE,SYSMESS          COMPLETED INITIALISE                         
         B     EXITOK                                                           
*                                                                               
VCTITLE  DC    CL(L'TITLE)'Input cards to MQ EDI fax receive job'               
DWTITLE  DC    CL(L'TITLE)'MQ EDI fax send output log'                          
         EJECT                                                                  
***********************************************************************         
* INITIALISE MQ QUEUES                                                *         
* NOTE: IN A DELIBERATE ATTEMPT TO SIMPLIFY THE NAMES OF THE MQ       *         
*       QUEUE MANAGER, THE MASTER QUEUE AND LOG QUEUES ARE ALL HARD   *         
***********************************************************************         
         SPACE 1                                                                
MQINIT   NTR1  BASE=*,LABEL=*                                                   
         MVI   CONOK,C'N'                                                       
         LHI   R0,9                                                             
         BRAS  RE,SYSMESS          BEGINNING MQ INITIALISE                      
*                                                                               
         MVC   INPQ_OBJECTNAME,QINPUTA                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTA                                         
         MVC   LOGQ_OBJECTNAME,QLOGA                                            
         CLI   ETYPE,C'A'          ADV QUEUES                                   
         BE    MQINI50                                                          
*                                                                               
         CLI   ETYPE,C'R'          REP QUEUES                                   
         BNE   MQINI30                                                          
         MVC   INPQ_OBJECTNAME,QINPUTR                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTR                                         
         MVC   LOGQ_OBJECTNAME,QLOGR                                            
*                                                                               
MQINI30  CLI   ETYPE,C'Q'          FQA QUEUES                                   
         BNE   MQINI40                                                          
         MVC   INPQ_OBJECTNAME,QINPUTQ                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTQ                                         
         MVC   LOGQ_OBJECTNAME,QLOGQ                                            
*                                                                               
MQINI40  CLI   ETYPE,C'T'          TEST QUEUES                                  
         BNE   MQINI50                                                          
         MVC   INPQ_OBJECTNAME,QINPUTT                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTT                                         
         MVC   LOGQ_OBJECTNAME,QLOGT                                            
*                                                                               
MQINI50  MVC   PLINE(20),=CL20'Queue Manager: '                                 
         MVC   PLINE+21(L'QMGR),QMGR                                            
         BRAS  RE,PRNT                                                          
         MVC   PLINE(20),=CL20'Input Queue:'                                    
         MVC   PLINE+21(L'INPQ_OBJECTNAME),INPQ_OBJECTNAME                      
         BRAS  RE,PRNT                                                          
         MVC   PLINE(20),=CL20'Output Queue:'                                   
         MVC   PLINE+21(L'OUTQ_OBJECTNAME),OUTQ_OBJECTNAME                      
         BRAS  RE,PRNT                                                          
         MVC   PLINE(20),=CL20'Logging Queue:'                                  
         MVC   PLINE+21(L'LOGQ_OBJECTNAME),LOGQ_OBJECTNAME                      
         BRAS  RE,PRNT                                                          
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF EXTERNAL ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,AMQCT                                                         
         LOAD  DE=CSQBGET                                                       
         ST    R0,AMQGET                                                        
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,AMQOPENQ                                                      
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,AMQCMIT                                                       
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,AMQCLOSE                                                      
         LOAD  DE=CSQBDISC                                                      
         ST    R0,AMQDISC                                                       
         LOAD  DE=CSQBPUT                                                       
         ST    R0,AMQPUT                                                        
*                                                                               
         LA    R2,MQCT                                                          
         BRAS  RE,CALLMQ           CONNECT TO MQ                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,11                                                            
         BRAS  RE,SYSMESS          CONNECTED TO MQ QMGR                         
*                                                                               
         LA    RF,INPQ             SET QUEUE OBJECT FOR INPUT QUEUE             
         ST    RF,MQOPNQNM                                                      
         LA    RF,INPQHOB          SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         LHI   RF,MQOO_INPUT_AS_Q_DEF+MQOO_SAVE_ALL_CONTEXT                     
         ST    RF,OPN_OPTS                                                      
         LA    R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN MASTER QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,OUTQ             SET QUEUE OBJECT FOR OUTPUT QUEUE            
         ST    RF,MQOPNQNM                                                      
         LA    RF,OUTQHOB          SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         L     RF,=AL4(MQOO_OUTPUT+MQOO_SET_IDENTITY_CONTEXT)                   
         ST    RF,OPN_OPTS                                                      
         LA    R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN OUTPUT QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,LOGQ             SET QUEUE OBJECT FOR LOG QUEUE               
         ST    RF,MQOPNQNM                                                      
         LA    RF,LOGQHOB          SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         LHI   RF,MQOO_OUTPUT+MQOO_PASS_ALL_CONTEXT                             
         ST    RF,OPN_OPTS                                                      
         LA    R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN OUTPUT QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,12                                                            
         BRAS  RE,SYSMESS          OPENED MASTER QUEUE                          
*                                                                               
         LHI   R0,10                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ INITIALISE                      
         MVI   CONOK,C'Y'                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* QUEUE INFORMATION                                                   *         
***********************************************************************         
         SPACE 1                                                                
QMASTER  DC    CL48' '                                                          
*                                                                               
QINPUTA  DC    CL48'DDS.FAXSND.ADV.LOCALQ'                                      
QINPUTR  DC    CL48'DDS.FAXSND.REP.LOCALQ'                                      
QINPUTT  DC    CL48'DDS.FAXSND.TEST.LOCALQ'                                     
QINPUTQ  DC    CL48'DDS.FAXSND.FQA.LOCALQ'                                      
*                                                                               
QOUTPUTA DC    CL48'EZFAX.ADV.QUEUE'                                            
QOUTPUTR DC    CL48'EZFAX.REP.QUEUE'                                            
QOUTPUTT DC    CL48'EZFAX.TEST.QUEUE'                                           
**QOUTPUTT DC    CL48'MQDEV01.MSWA.QREMOTE'                                     
QOUTPUTQ DC    CL48'EZFAX.FQA.QUEUE'                                            
*                                                                               
QLOGA    DC    CL48'MQ1P.EZFAX.ADV.LOCALQ'                                      
QLOGR    DC    CL48'MQ1P.EZFAX.REP.LOCALQ'                                      
QLOGT    DC    CL48'MQ1P.EZFAX.TEST.LOCALQ'                                     
QLOGQ    DC    CL48'MQ1P.EZFAX.FQA.LOCALQ'                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISCONNECT FROM MQ CLEANLY                               *         
***********************************************************************         
         SPACE 1                                                                
MQCLS    NTR1  BASE=*,LABEL=*                                                   
         CLI   CONOK,C'Y'                                                       
         BNE   EXITOK                                                           
*                                                                               
         LHI   R0,14                                                            
         BRAS  RE,SYSMESS          CLOSING QUEUES                               
*                                                                               
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LA    RF,INPQHOB                                                       
         ST    RF,MQCLSHOB                                                      
         LA    R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE INPUT QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LA    RF,OUTQHOB                                                       
         ST    RF,MQCLSHOB                                                      
         LA    R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE OUTPUT QUEUE                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LA    RF,LOGQHOB                                                       
         ST    RF,MQCLSHOB                                                      
         LA    R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE LOG QUEUE                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,16                                                            
         BRAS  RE,SYSMESS          CLOSED MQ MASTER QUEUE                       
         LA    R2,MQDISC                                                        
         BRAS  RE,CALLMQ           DISCONNECT MQ QMGR                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,15                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ DEALLOCATION                    
         MVI   CONOK,C'N'                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL MQ                                                  *         
* NTRY: R2          = A(PARAMETER STRUCTURE)                          *         
* EXIT: MQ_CC       = MQ COMPLETION CODE                              *         
*       MQ_RC       = MQ REASON CODE                                  *         
*       CC SET EQ     WHEN CALL OKAY                                  *         
***********************************************************************         
         SPACE 1                                                                
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
*                                                                               
         L     RF,16(R2)           RF = A(MQ ROUTINE)                           
         LA    R3,20(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         SAM24                                                                  
         CLC   MQ_CC,=A(MQCC_OK)                                                
         BE    CMQ02                                                            
*                                                                               
         LA    RF,MQGET            MQGET CAN HAVE NZ CC                         
         CR    R2,RF               IF SO THE FOLLOWING 2 FIELDS ARE SET         
         BNE   CMQ04                                                            
         CLC   MQ_CC,=A(MQCC_WARNING)                                           
         BNE   CMQ04                                                            
         CLC   MQ_RC,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                           
         BNE   CMQ04                                                            
*                                                                               
         CLI   TRACE,C'N'                                                       
         BE    EXITH                                                            
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(35),=CL35'WAITING FOR NEW MESSAGE'                      
         BRAS  RE,PRNT                                                          
         B     EXITH               FLAG THAT WE NEED TO WAIT                    
*                                                                               
CMQ02    CLI   TRACE,C'N'                                                       
         BE    EXITOK                                                           
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(12),=C'Completed ok'                                    
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
CMQ04    MVI   PLINE,C'+'                                                       
         MVC   PLINE+1(16),0(R2)                                                
         MVC   PLINE+20(09),=C'**ERROR**'                                       
         MVC   PLINE+30(08),=CL08'Warning '                                     
         CLC   MQ_CC,=A(MQCC_WARNING)                                           
         BE    CMQ06                                                            
         MVC   PLINE+30(08),=CL08'Failed  '                                     
         CLC   MQ_CC,=A(MQCC_FAILED)                                            
         BE    CMQ06                                                            
         MVC   PLINE+30(08),=CL08'Unknown '                                     
         EDIT  MQ_CC,(7,PLINE+38),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
CMQ06    MVC   PLINE+46(3),=C'RC='                                              
         EDIT  MQ_RC,(5,PLINE+49),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
         L     RF,AWHY                                                          
CMQ08    CLI   0(RF),X'FF'         SEE IF WE HAVE TEXT FOR THE PROBLEM          
         BE    CMQ10                                                            
         CLC   MQ_RC,0(RF)                                                      
         BE    *+12                                                             
         AHI   RF,28                                                            
         B     CMQ08                                                            
*                                                                               
         MVC   PLINE+60(24),4(RF)                                               
*                                                                               
CMQ10    BRAS  RE,PRNT                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
         SPACE 1                                                                
SETOPS   NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,7                                                             
         BRAS  RE,SYSMESS          BEGINNING SETTING OPERATOR COMMS             
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         MVC   ECBLST,AOPERECB                                                  
*                                                                               
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
*                                                                               
         LHI   R0,8                                                             
         BRAS  RE,SYSMESS          COMPLETED SETTING OPERATOR COMMS             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD A COVER PAGE FROM A FAX RECORD.                               *         
* NTRY: R7         = DSTAEFTD                                         *         
*       FAXKEY     = KEY OF FAX RECORD TO USE                         *         
* EXIT: DSTAEDST   = DESTINATION OR ZEROS (ON ERROR)                  *         
***********************************************************************         
         SPACE 1                                                                
         USING DSTAEFTD,R7                                                      
BLDCVR   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACOVSHT          BUILD COVER PAGE HERE                        
         LR    R0,R2                                                            
         LHI   R1,COVSHTL                                                       
         LHI   RF,C' '                                                          
         SLL   RF,32-8                                                          
         MVCL  R0,RE               FIRST SPACE FILL THE COVERSHEET              
*                                                                               
         CLC   FAXKEY(4),DSTAEDST  READING FAX RECORD FOR THE STATION?          
         BNE   CPG02               NO, BETTER NOT RUIN FAX KEY FOR REP          
         CLI   DSTAEDST+4,C'T'     MEDIA T?                                     
         BNE   CPG02               **SHOULD CHECK RADIO AND OTHER LATER         
         MVC   FAXKEY(4),DSTAEDST  USE STATION CALL LETTERS (4)                 
         MVC   FAXKEY+4(3),SPACESL                                              
*                                                                               
CPG02    CLC   =C'NOREP',FAXKEY                                                 
         BNE   CPG04                                                            
         CLI   DSTAEDST+4,C'T'     MEDIA T?                                     
         BNE   CPG04               **SHOULD CHECK RADIO AND OTHER LATER         
         MVC   AGYPOWER,SVAGYPOW   USE AGENCY POWER CODE, NOT "DARADM"          
*                                                                               
CPG04    XC    DSTAEDST,DSTAEDST   FAX NUMBER WILL GO HERE                      
*                                                                               
         XC    KEY,KEY             READ FAX RECORD                              
X        USING CTFXREC,KEY                                                      
         MVI   X.CTFXKTYP,CTFXEQU                                               
         MVC   X.CTFXAGY,AGYPOWER                                               
         MVC   X.CTFXCODE,FAXKEY                                                
*                                                                               
         CLI   HDR+67,C'D'         IS THIS A DARE FAX?                          
         BNE   CPG06                                                            
         OC    FAXSUBC(3),FAXSUBC  1ST SUB CODE IS THERE?                       
         BZ    CPG08               NO - TRY 2ND SUB CODE                        
         MVC   X.CTFXSUBC,SPACESL                                               
         MVC   X.CTFXSUBC(3),FAXSUBC                                            
*                                                                               
CPG06    GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
*                                                                               
         CLI   HDR+67,C'D'         IS THIS A DARE FAX?                          
         BE    *+12                YES                                          
         BRAS  RE,CPGTST                                                        
         B     EXITOK                                                           
*                                                                               
         BRAS  RE,CPGTST                                                        
*                                                                               
CPG08    OC    FAXSUBC+3(3),FAXSUBC+3   2ND SUB CODE IS THERE?                  
         BZ    CPG10                    NO - TRY WITHOUT SUB CODE               
*                                                                               
         XC    KEY,KEY             READ FAX RECORD                              
         MVI   X.CTFXKTYP,CTFXEQU                                               
         MVC   X.CTFXAGY,AGYPOWER                                               
         MVC   X.CTFXCODE,FAXKEY                                                
         MVC   X.CTFXSUBC,SPACESL                                               
         MVC   X.CTFXSUBC(3),FAXSUBC+3                                          
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
*                                                                               
         BRAS  RE,CPGTST                                                        
*                                                                               
CPG10    XC    KEY,KEY             READ FAX RECORD                              
         USING CTFXREC,R4                                                       
         MVI   X.CTFXKTYP,CTFXEQU                                               
         MVC   X.CTFXAGY,AGYPOWER                                               
         MVC   X.CTFXCODE,FAXKEY                                                
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
*                                                                               
         BRAS  RE,CPGTST                                                        
         B     EXITOK              COVER PAGE NOT FOUND                         
*                                                                               
CPGTST   CLI   8(R1),0             NORMAL RETURN                                
         BNE   *+12                                                             
         MVI   DSTTYPE,EDFDSTXQ                                                 
         B     CPG12                                                            
*                                                                               
         TM    8(R1),X'10'         CODE FOUND?                                  
         BOR   RE                  NO                                           
         DC    H'0'                BAD PROBLEM !!                               
*                                                                               
CPG12    CLI   DSTTYPE,EDFDSTHQ    FAX NUMBER (HOME MARKET W/O ID)?             
         BNE   *+8                                                              
         MVI   DSTTYPE,EDFDSTXQ    YES, RESET TO FAX RECORD KEY                 
*                                                                               
         MVC   34(10,R2),=C'FAX HEADER'                                         
         AHI   R2,132                                                           
         MVC   34(10,R2),=C'----------'                                         
         AHI   R2,132                                                           
         AHI   R2,132                                                           
         MVC   21(8,R2),=C'SENT TO:'                                            
*                                                                               
         CLI   DSTTYPE,EDFDSTHQ    FAX NUMBER (HOME MARKET W/O ID)?             
         BE    CPG14                                                            
*                                                                               
         BRAS  RE,OFF31                                                         
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX2ELQ',AIO),0,0                   
         BRAS  RE,ON31                                                          
         CLI   12(R1),0                                                         
         BNE   CPG14                                                            
         ICM   R4,15,12(R1)                                                     
         USING CTFXATT,R4                                                       
         XR    RF,RF                                                            
         IC    RF,CTFX2LEN                                                      
         AHI   R1,-(CTFX2ATT-CTFXATT+1)                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   34(0,R2),CTFX2ATT                                                
         AHI   R2,132                                                           
*                                                                               
CPG14    MVC   34(4,R2),=C'FAX='                                                
         CLI   DSTTYPE,EDFDSTHQ    FAX NUMBER (HOME MARKET W/O ID)?             
         BE    CPG16                                                            
*                                                                               
         L     R4,AIO                                                           
         USING CTFXREC,R4                                                       
         MVC   DSTAEDST(4),=C'FAX '                                             
         LA    RE,DSTAEDST+4                                                    
         CLI   LONGFXPG,C'Y'       GENERATE "LONG" FAX PAGES?                   
         BNE   *+14                                                             
         MVC   DSTAEDST(5),=C'FAXL '                                            
         LA    RE,DSTAEDST+5                                                    
*                                                                               
         XR    R1,R1                                                            
         IC    R1,CTFX1LEN         FAX NUMBER                                   
         AHI   R1,-(CTFX1NUM-CTFX1EL+1)                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),CTFX1NUM    MOVE IN FAX NUMBER                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   38(0,R2),CTFX1NUM                                                
         LA    R1,38(R2)                                                        
         BAS   RE,EDITTEL                                                       
*                                                                               
CPG16    AHI   R2,132                                                           
         AHI   R2,132                                                           
         LR    R5,R2                                                            
         DROP  R4                                                               
*                                                                               
         CLI   HDR+70,C'N'         NO COVER PAGE?                               
         BNE   *+14                                                             
         XC    COVPGLNS,COVPGLNS                                                
         B     EXITOK                                                           
*                                                                               
         CLI   DSTTYPE,EDFDSTHQ    FAX NUMBER (HOME MARKET W/O ID)?             
         BE    CPG20                                                            
*                                                                               
         BRAS  RE,OFF31                                                         
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX3ELQ',AIO),0,0                   
         BRAS  RE,ON31                                                          
         CLI   12(R1),0                                                         
         BNE   CPG20                                                            
         ICM   R4,15,12(R1)                                                     
         USING CTFXMSG,R4                                                       
*                                                                               
CPG18    XR    RF,RF                                                            
         IC    RF,CTFX3LIN                                                      
         BCTR  RF,0                                                             
         MHI   RF,132                                                           
         LA    R2,0(RF,R5)         POINT TO CORRECT LINE FOR MESSAGE            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,CTFX3LEN                                                      
         AHI   RF,-(CTFX3MSG-CTFXMSG+1)                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   34(0,R2),CTFX3MSG   MOVE IN MESSAGE                              
         AHI   R2,132                                                           
*                                                                               
         IC    RF,CTFX3LEN         NEXT IN CLUSTER                              
         AR    R4,RF                                                            
         CLI   CTFX3EL,CTFX3ELQ                                                 
         BE    CPG18                                                            
         DROP  R4                                                               
*                                                                               
CPG20    AHI   R2,132                                                           
         MVC   21(10,R2),=C'SENT FROM:'                                         
*                                                                               
         MVI   BYTE2,C'N'                                                       
         OC    REQUESTR,REQUESTR                                                
         BZ    CPG24                                                            
*                                                                               
         XC    KEY,KEY             READ FAX RECORD FOR REQUESTOR                
         MVI   X.CTFXKTYP,CTFXEQU                                               
         MVC   X.CTFXAGY,SVAGYPOW                                               
         MVC   X.CTFXCODE(2),=C'**'  LOOK FOR **RRR (REQUESTOR)                 
         MVC   X.CTFXCODE+2(3),REQUESTR                                         
         MVC   X.CTFXCODE+5(2),SPACESL                                          
*                                                                               
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
         CLI   8(R1),0                                                          
         BE    CPG22                                                            
         TM    8(R1),X'10'                                                      
         BO    CPG24               REQUESTOR DOESN'T HAVE A FAX ELEMENT         
         DC    H'0'                                                             
*                                                                               
CPG22    MVI   BYTE2,C'Y'          SET HAVE REQUESTOR FAX DATA                  
         BRAS  RE,OFF31                                                         
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX2ELQ',AIO),0,0                   
         BRAS  RE,ON31                                                          
         CLI   12(R1),0                                                         
         BNE   CPG24                                                            
*                                                                               
         ICM   R4,15,12(R1)        ATTENTION ELEMENT                            
         USING CTFXATT,R4                                                       
         XR    R1,R1                                                            
         IC    R1,CTFX2LEN                                                      
         AHI   R1,-(CTFX2ATT-CTFXATT+1)                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   34(0,R2),CTFX2ATT                                                
         AHI   R2,132                                                           
         DROP  R4                                                               
*                                                                               
CPG24    MVC   34(33,R2),AGYNAME                                                
         AHI   R2,132                                                           
         MVC   34(33,R2),AGYADDR                                                
         AHI   R2,132                                                           
         MVC   34(3,R2),=C'ID='                                                 
         MVC   37(10,R2),USERID                                                 
         AHI   R2,132                                                           
*                                                                               
         OC    REQUESTR,REQUESTR                                                
         BZ    CPG30                                                            
         MVC   34(10,R2),=C'REQUESTOR='                                         
         MVC   44(3,R2),REQUESTR                                                
         AHI   R2,132                                                           
*                                                                               
         CLI   BYTE2,C'Y'          DO WE HAVE REQUESTOR FAX DATA?               
         BNE   CPG30                                                            
         BRAS  RE,OFF31                                                         
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX4ELQ',AIO),0,0                   
         BRAS  RE,ON31                                                          
         CLI   12(R1),0                                                         
         BNE   CPG26                                                            
*                                                                               
         ICM   R4,15,12(R1)                                                     
         USING CTFXTEL,R4                                                       
         MVC   34(10,R2),=C'TELEPHONE='                                         
         XR    R1,R1                                                            
         IC    R1,CTFX4LEN                                                      
         AHI   R1,-(CTFX4TEL-CTFXTEL+1)                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   44(0,R2),CTFX4TEL                                                
         LA    R1,44(R2)                                                        
         BAS   RE,EDITTEL                                                       
         AHI   R2,132                                                           
         DROP  R4                                                               
*                                                                               
CPG26    L     R4,AIO                                                           
         USING CTFXREC,R4                                                       
         MVC   34(4,R2),=C'FAX='   FAX NUMBER                                   
         XR    R1,R1                                                            
         IC    R1,CTFX1LEN                                                      
         AHI   R1,-(CTFX1NUM-CTFX1EL+1)                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   38(0,R2),CTFX1NUM                                                
         LA    R1,38(R2)                                                        
         BAS   RE,EDITTEL                                                       
         AHI   R2,132                                                           
         AHI   R2,132                                                           
         LR    R5,R2                                                            
*                                                                               
         XR    R0,R0                                                            
         BRAS  RE,OFF31                                                         
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX3ELQ',AIO),0,0                   
         BRAS  RE,ON31                                                          
         CLI   12(R1),0                                                         
         BNE   CPG30                                                            
         ICM   R4,15,12(R1)                                                     
         USING CTFXMSG,R4                                                       
*                                                                               
CPG28    XR    RF,RF                                                            
         IC    RF,CTFX3LIN         LINE NUMBER                                  
         CR    R0,RF                                                            
         BH    *+6                                                              
         LR    R0,RF               SAVE HIGH WATER                              
         BCTR  RF,0                                                             
         MHI   RF,132                                                           
         LA    R2,0(RF,R5)         POINT TO CORRECT LINE FOR MESSAGE            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,CTFX3LEN                                                      
         AHI   R1,-(CTFX3MSG-CTFXMSG+1)                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   34(0,R2),CTFX3MSG                                                
*                                                                               
         XR    RF,RF               NEXT ELEMENT                                 
         IC    RF,CTFX3LEN                                                      
         AR    R4,RF                                                            
         CLI   CTFX3EL,CTFX3ELQ                                                 
         BE    CPG28                                                            
*                                                                               
         AHI   R0,1                ADD 1 BLANK LINE                             
         MHI   R0,132                                                           
         LR    R2,R5                                                            
         AR    R2,R0               GO PAST ALL THE LINES YOU PUT OUT            
         DROP  R4                                                               
*                                                                               
CPG30    MVC   21(12,R2),=C'FAX SENT ON:'   DATE                                
         GOTO1 VDATCON,DMCB,(5,0),(8,34(R2))                                    
*                                                                               
         AHI   R2,132                                                           
         MVC   21(10,R2),=C'TIME SENT:'                                         
         THMS  DDSTIME=YES                                                      
         ST    R1,FULL             0HHMMSS+ (DDS TIME)                          
         ST    R0,PACKOF4B         0060000+ (DDS CLOCK TIME DIFFERENCE)         
         AP    FULL,PACKOF4B       FULL = ACTUAL TIME                           
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),FULL                                                    
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         CHI   R1,2400                                                          
         BL    *+8                                                              
         AHI   R1,-2400                                                         
         EDIT  (R1),(5,34(R2)),2                                                
         AHI   R2,132                                                           
*                                                                               
         S     R2,ACOVSHT                                                       
         SRDL  R2,32               PREPARE FOR DIVIDE                           
         D     R2,=F'132'                                                       
         AHI   R3,1                R3 = NUMBER OF LINES IN COVER PAGE           
         STH   R3,COVPGLNS                                                      
         B     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* EDIT TELEPHONE NUMBER TO PUT IN () & -                              *         
* NTRY:  R1    = A(CL25 TELEPHONE)                                    *         
***********************************************************************         
         SPACE 1                                                                
EDITTEL  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(25),0(R1)                                                   
         MVC   0(25,R1),SPACESL                                                 
         LA    R2,WORK                                                          
         CLC   WORK+7(4),SPACESL   IF > 7 CHARACTERS                            
         BE    EDITEL20                                                         
         CLI   WORK,C'1'                                                        
         BH    EDITEL10                                                         
         MVC   0(2,R1),=C'1-'                                                   
         LA    R1,2(R1)                                                         
         LA    R2,1(R2)                                                         
*                                                                               
EDITEL10 MVI   0(R1),C'('          ASSUME AREA CODE TO START                    
         MVC   1(3,R1),0(R2)                                                    
         MVI   4(R1),C')'                                                       
         LA    R2,3(R2)                                                         
         LA    R1,6(R1)                                                         
*                                                                               
EDITEL20 MVC   0(3,R1),0(R2)                                                    
         MVI   3(R1),C'-'                                                       
         MVC   4(10,R1),3(R2)                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ IN ALL INPUT CARDS                                             *         
* FORMAT INTO A SCANNER BLOCK AND SAVE FOR VALIDATION                 *         
***********************************************************************         
         SPACE 1                                                                
GETCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,3                                                             
         BRAS  RE,SYSMESS          STARTING READING INPUT CARDS                 
*                                                                               
         L     R3,ACARDSIN                                                      
GCD02    GOTO1 VCARDS,PLIST,CARDIO,=C'RE00'                                     
         MVC   PLINE(L'CARDIO),CARDIO                                           
         BRAS  RE,PRNT             ALWAYS PRINT THE LINE                        
         CLI   CARDIO,C'*'         IGNORE COMMENTS                              
         BE    GCD02                                                            
         CLC   =C'/*',CARDIO       END OF CARDS?                                
         BE    GCDX                YES                                          
*                                                                               
         CLI   0(R3),X'FF'         OVERFLOW                                     
         BNE   GCD04                                                            
         MVI   FERN,05             TOO MANY PARAMETERS                          
         BRAS  RE,ERRMSG                                                        
         B     EXITL                                                            
*                                                                               
GCD04    MVC   0(L'CARDIO,R3),CARDIO                                            
         AHI   R3,L'CARDIO                                                      
         B     GCD02               NEXT                                         
*                                                                               
GCDX     MVI   0(R3),X'FF'         FLAG END OF CARDS                            
         LHI   R0,4                                                             
         BRAS  RE,SYSMESS          COMPLETED READING INPUT CARDS                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS IN SCANNER BLOCK                           *         
***********************************************************************         
         SPACE 1                                                                
VALCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,5                                                             
         BRAS  RE,SYSMESS          STARTING VALIDATE INPUT CARDS                
         MVI   GOTERR,C'N'                                                      
*                                                                               
         L     R3,ACARDSIN                                                      
VCD02    CLI   0(R3),X'FF'                                                      
         BE    VCDX                                                             
*                                                                               
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         USING CARDTABD,R4                                                      
VCD04    CLI   0(R4),255           END OF TABLE?                                
         BE    VCD08                                                            
         TM    CFLAG,CFBIG         BIG CARDS ARE NOT SCANNED                    
         BZ    VCD06                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,CMAX                                                          
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD20                                                            
         CLC   CARDTXT(0),0(R3)                                                 
*                                                                               
VCD06    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD04                                                            
*                                                                               
VCD08    GOTO1 VSCANNER,PLIST,(C'C',0(R3)),ASCANTAB,0                           
         XR    R2,R2                                                            
         ICM   R2,1,4(R1)          RF=NUMBER OF INPUT PARAMETERS                
         BNZ   VCD10                                                            
         MVC   PLINE(L'CARDIO),0(R3)                                            
         BRAS  RE,PRNT                                                          
         MVI   FERN,04                                                          
         BRAS  RE,ERRMSG                                                        
         B     VCD18                                                            
*                                                                               
VCD10    L     R5,ASCANTAB                                                      
         USING SCANBLKD,R5                                                      
*                                                                               
VCD12    XR    RF,RF               RECONSTRUCT INPUT PARAMETER                  
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),SC1STFLD                                                
         LA    RE,PLINE+1(RF)                                                   
*                                                                               
         CLI   SC2NDLEN,0          ANY SECOND HALF TO PARAMETER                 
         BE    VCD14                                                            
         MVI   0(RE),C'='                                                       
         MVC   1(L'SC2NDFLD,RE),SC2NDFLD                                        
*                                                                               
VCD14    BRAS  RE,PRNT             PRINT RECONSTRUCTED PARAMETER                
*                                                                               
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         USING CARDTABD,R4                                                      
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         LENGTH OF PARAMETER                          
         BCTR  RF,0                                                             
*                                                                               
VCD16    CLI   0(R4),255           END OF TABLE?                                
         BNE   *+16                                                             
         MVI   FERN,01             FLAG UNKNOWN PARAMETER CARD                  
         BRAS  RE,ERRMSG                                                        
         B     VCD22                                                            
*                                                                               
         CLM   RF,1,CMIN           CHECK LENGTHS ARE OK                         
         BL    VCD18                                                            
         CLM   RF,1,CMAX                                                        
         BH    VCD18                                                            
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD20                                                            
         CLC   SC1STFLD(0),CARDTXT                                              
*                                                                               
VCD18    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD16                                                            
*                                                                               
VCD20    ICM   RF,15,CARDVAL       GO AND VALIDATE THIS INPUT                   
         BASR  RE,RF                                                            
         BE    *+8                                                              
         BRAS  RE,ERRMSG           PRINT ERROR MESSAGE                          
*                                                                               
VCD22    AHI   R5,SCBLKLQ          NEXT LINE IN SCANTAB                         
         BCT   R2,VCD12            ANY MORE PARMS INPUT?                        
*                                                                               
VCD24    AHI   R3,L'CARDIO                                                      
         B     VCD02                                                            
*                                                                               
VCDX     ZAP   LINE,P99            FORCE PAGE THROW                             
         LHI   R0,6                                                             
         BRAS  RE,SYSMESS          ENDED VALIDATE INPUT CARDS                   
*                                                                               
         CLI   GOTERR,C'Y'         SET CC BASED ON ERRORS                       
         BNE   EXITOK                                                           
         B     EXITL                                                            
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DSPACE= CARD                                          *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R5                                                      
VCDSPACE NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,1                                                       
         BE    *+12                                                             
         MVI   FERN,02                                                          
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,ASSB                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),SC2NDFLD                          
*                                                                               
         MVC   ETYPE,SSODSPAC-SSOOFF(RF)                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DDSIO= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCDDSIO  NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,8                                                       
         BNH   *+12                                                             
         MVI   FERN,03                                                          
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,VDDSIO                                                     
         MVC   0(8,RF),SC2NDFLD                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DRTEST= CARD                                          *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCDR     NTR1  BASE=*,LABEL=*                                                   
         MVC   DRTEST,SC2NDFLD                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DEBUG= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCDEBUG  NTR1  BASE=*,LABEL=*                                                   
         MVC   DEBUG,SC2NDFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF TRACE= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCTRACE  NTR1  BASE=*,LABEL=*                                                   
         MVC   TRACE,SC2NDFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF QMGR= OVERRIDE CARD                                   *         
* NTRY: R3     = A(INPUT CARD)                                        *         
***********************************************************************         
         SPACE 1                                                                
VCQMGR   NTR1  BASE=*,LABEL=*                                                   
         MVC   QMGR(L'SC2NDFLD),SC2NDFLD                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF LOGGING=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')  *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCLOG    NTR1  BASE=*,LABEL=*                                                   
         MVC   LOG,SC2NDFLD                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DLNS=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')     *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCDLNS   NTR1  BASE=*,LABEL=*                                                   
         MVC   SENDDLNS,SC2NDFLD                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF TYPE=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')     *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCTYPE   NTR1  BASE=*,LABEL=*                                                   
         MVC   FACTYPE,SC2NDFLD                                                 
         CLI   FACTYPE,C'A'                                                     
         BE    EXITOK                                                           
         CLI   FACTYPE,C'R'                                                     
         BE    EXITOK                                                           
         CLI   FACTYPE,C'T'                                                     
         BE    EXITOK                                                           
         CLI   FACTYPE,C'Q'                                                     
         BE    EXITOK                                                           
         MVI   FERN,19                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF JUNKREP=CARD  (NUMBER)                                *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VJREP    NTR1  BASE=*,LABEL=*                                                   
         OI    SC2NDVAL,SCNUMQ     IS THIS A NUMBER?                            
         BZ    EXITOK              NO - USE DEFAULT AND EXIT OK                 
         MVC   JREPMAX,SC2NDNUM                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR MESSAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
ERRMSG   NTR1  BASE=*,LABEL=*                                                   
         MVI   GOTERR,C'Y'         SET GOT AN ERROR                             
*                                                                               
         LA    RF,ERRND                                                         
         CLI   FERN,0              UNDEFINED ERROR                              
         BE    ERRM02                                                           
*                                                                               
         XR    RF,RF               INDEX INTO ERROR TABLE                       
         IC    RF,FERN                                                          
         BCTR  RF,0                                                             
         MHI   RF,L'ERRMSG                                                      
         A     RF,AERRMSG          RF = A(ERROR MESSAGE)                        
*                                                                               
ERRM02   MVC   PLINE,SPACESL                                                    
         MVC   PLINE(L'ERRHDR),ERRHDR                                           
         MVC   PLINE+L'ERRHDR(L'ERRMSG),0(RF)                                   
         BRAS  RE,PRNT                                                          
*                                                                               
         MVI   FERN,0                                                           
         B     EXITOK                                                           
*                                                                               
ERRHDR   DC    C'@  *** ERROR *** '                                             
ERRND    DC    CL45'Improperly defined error'                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PLINE TO A PRINT LINE                             *         
***********************************************************************         
         SPACE 1                                                                
PRNT     NTR1  BASE=*,LABEL=*                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+00(2),PRNTTIME                                                 
         MVI   P+02,C':'                                                        
         MVC   P+03(2),PRNTTIME+2                                               
         MVI   P+05,C':'                                                        
         MVC   P+06(2),PRNTTIME+4                                               
         MVI   P+08,C'.'                                                        
         MVC   P+09(2),PRNTTIME+6                                               
         MVC   P+12(L'PLINE),PLINE                                              
         MVC   PLINE,SPACESL                                                    
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
*                                                                               
PRNTDUB  DS    D                   FOR PRNT ROUTINE ONLY                        
PRNTTIME DS    CL9                 FOR PRNT ROUTINE ONLY                        
         EJECT                                                                  
***********************************************************************         
* PUT A MESSAGE TO THE OPERATOR CONSOLE AND OPTIONALLY GET A REPLY.   *         
* NTRY: R0        = MESSAGE NUMBER                                    *         
*       XTRAMESS  = OPTIONAL CL8 TO BE INSERTED INTO MESSAGE          *         
*       XTRAMES2  = OPTIONAL CL16 TO BE INSERTED INTO MESSAGE         *         
*       R0 ==  0  = SPECIAL MESSAGE AT 0(R1)                          *         
*                                                                     *         
* 1. IF THE FIRST CHARACTER OF THE MESSAGE IS AN 'X' JUST PUT OUT THE *         
* MESSAGE                                                             *         
*                                                                     *         
* 2. IF IT IS A NUMBER THIS IS THE # OF CHARACTERS FOR THE RESPONSE   *         
* RESPONSE IS RETURNED IN 'REPLY' - "DUMP" AND "EOJ" HANDLED IN HERE  *         
*                                                                     *         
* 3. ELSE THIS IS A MULTILINE MESSAGE AND IT LOOPS UNTIL (1) OR (2)   *         
***********************************************************************         
         SPACE 1                                                                
SYSMESS  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         AHI   R0,-1                                                            
         BM    SYM02               R0 == 0 MEANS R1=A(MESSAGE)                  
*                                                                               
         MHI   R0,L'SYSMSGS                                                     
         L     R2,ASYSMSGS                                                      
         AR    R2,R0               R2=A(SYSMSGS ENTRY)                          
*                                                                               
SYM02    MVC   MESSAGE,SPACESL     BUILD MESSAGE                                
         MVC   MESSAGE(8),=CL08'EDPQSCAN'                                       
         MVC   MESSAGE+9(L'SYSMSGS-1),1(R2)                                     
*                                                                               
         LA    R0,MESSAGE          NOW REPLACE SUBSTITUTE CHARS                 
         LA    R1,MESSAGE+L'MESSAGE-1                                           
SYM04    CR    R0,R1                                                            
         BE    SYM10                                                            
         CLC   0(18,R1),=18C'X'                                                 
         BE    SYM08                                                            
         CLC   0(8,R1),=18C'X'                                                  
         BE    SYM06                                                            
         BCT   R1,SYM04                                                         
         DC    H'0'                                                             
*                                                                               
SYM06    MVC   0(8,R1),XTRAMESS                                                 
         B     SYM10                                                            
*                                                                               
SYM08    MVC   0(18,R1),XTRAMES2                                                
         B     SYM10                                                            
*                                                                               
SYM10    CLI   0(R2),C'0'                                                       
         BH    SYM12                                                            
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'MESSAGE,MESSAGE)                      
         CLI   0(R2),C'X'                                                       
         BE    SYM16                                                            
         AHI   R2,L'SYSMSGS        SPECIAL MULTILINE MESSAGE                    
         B     SYM10                                                            
*                                                                               
SYM12    GOTO1 VLOGIO,DMCB,1,(L'MESSAGE,MESSAGE)                                
                                                                                
         XR    R0,R0                                                            
         ICM   R0,1,0(R2)                                                       
         N     R0,=X'0000000F'                                                  
         GOTO1 VLOGIO,DMCB,0,((R0),REPLY)                                       
*                                                                               
         CLC   REPLY(4),=C'DUMP'   CHECK FOR DUMP REPLY                         
         BNE   SYM14                                                            
         ABEND 666,DUMP                                                         
*                                                                               
SYM14    CLC   REPLY(3),=C'EOJ'    CHECK FOR EOJ REPLY                          
         BNE   SYM16                                                            
         ABEND 666                                                              
*                                                                               
SYM16    MVC   XTRAMESS,SPACESL    CLEAR THESE OUT                              
         MVC   XTRAMES2,SPACESL                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OPEN CONTROL FILE IF REQUIRED                            *         
***********************************************************************         
         SPACE 1                                                                
OPENCTFL NTR1  BASE=*,LABEL=*                                                   
         CLI   CTOPEN,C'Y'                                                      
         BE    EXITOK                                                           
         MVI   CTOPEN,C'Y'                                                      
         GOTO1 VDMGR,DMCB,DMOPEN,CONTROL,CTFLIST,AIO,0                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DARE FAX                                                            *         
* NTRY:  R7    = DSTAEFTD                                             *         
* EXIT:  WORK  = RECEIVING FAX ID                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING DSTAEFTD,R7                                                      
DAREFAX  NTR1  BASE=*,LABEL=*                                                   
         MVI   DSTTYPE,EDFDSTXQ    FAX RECORD KEY                               
         XC    WORK,WORK           RECEIVING ID WILL BE BUILT HERE              
*                                                                               
         MVI   MEDIA,C'T'          DEFAULT MEDIA IS TV                          
         CLI   DSTAEDST+4,C'T'     T MEANS TV                                   
         BE    DFX02                                                            
         CLI   DSTAEDST+4,C' '     SO DOES A BLANK                              
         BE    DFX02                                                            
*                                                                               
         CLI   DSTAEDST+4,C'A'     AM RADIO                                     
         BE    *+12                                                             
         CLI   DSTAEDST+4,C'F'     FM RADIO                                     
         BNE   *+8                                                              
         MVI   MEDIA,C'R'                                                       
*                                                                               
DFX02    XC    KEY,KEY             BUILD DARE STATION RECORD KEY                
X        USING STAKEYD,KEY                                                      
         MVI   X.STAKSYS,STAKSYSQ                                               
         MVI   X.STAKTYP,STAKTYPQ                                               
         MVC   X.STAKMEDA,MEDIA                                                 
         MVC   X.STAKSTIN,DSTAEDST                                              
*                                  GET TODAY AND INVERT IT                      
         GOTO1 VDATCON,DMCB,(5,0),(3,FULL)                                      
         ZICM  RF,FULL,3                                                        
         LNR   RF,RF                                                            
         STCM  RF,7,X.STAKEFDA                                                  
*                                                                               
         GOTO1 VDMGR,DMCB,DMRDHI,GENDIR,KEY,AIO,0                               
         CLI   8(R1),0                                                          
         BNE   EXITOK                                                           
         L     RF,AIO                                                           
         CLC   KEY(STAKEFDA-STAKEYD),0(RF)                                      
         BNE   EXITOK              NOT FOUND, EXIT                              
*                                                                               
         GOTO1 VDMGR,DMCB,GETREC,GENFIL,36(RF),AIO,DMWORK                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,OFF31                                                         
         GOTO1 VHELLO,DMCB,(C'G',GENFIL),('STAHOMCQ',AIO),0,0                   
         BRAS  RE,ON31                                                          
         CLI   12(R1),0                                                         
         BNE   DFX04               NO HOME MKT ELEM, FIND THE REP               
*                                                                               
         ICM   R4,15,12(R1)        HOME MARKET ELEMENT                          
         USING STAHOMD,R4                                                       
         CLC   STAHOMCT,ROUTCODE+3 MATCHES OUR CITY?                            
         BNE   DFX04               NO                                           
         OC    STAHOMIB,STAHOMIB   HAVE A CITY BUT NO RECEIVING ID?             
         BNZ   DFX04                 STRANGE...USE DEFAULT INSTEAD              
*                                                                               
         MVI   DSTTYPE,EDFDSTHQ    HOME MARKET W/O ID                           
         MVC   WORK(L'DSTAEDST),DSTAEDST  USE STATION AS RECEIVER               
         B     EXITOK                                                           
*                                                                               
DFX04    EQU   *                                                                
         BRAS  RE,OFF31                                                         
         GOTO1 VHELLO,DMCB,(C'G',GENFIL),('STAREPCQ',AIO),0,0                   
         BRAS  RE,ON31                                                          
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO REP ELEMENT (SHOULDN'T HAPPEN)            
*                                                                               
         ICM   R4,15,12(R1)        REP ELEMENT                                  
         USING STAREPD,R4                                                       
         MVC   REPCODE,STAREPCR    CURRENT REPCODE                              
         GOTO1 VDATCON,DMCB,(5,0),(15,FULL)                                     
         CLC   STAREPED,FULL                                                    
         BNH   *+10                                                             
         MVC   REPCODE,STAREPPR    PREVIOUS REPCODE                             
         DROP  R4                                                               
*                                                                               
         L     R3,AREPIDS          TABLE OF RECEIVING REPS                      
DFX06    CLI   0(R3),X'FF'                                                      
         BE    EXITOK              MISSING ENTRY -- SHOULD NEVER HAPPEN         
         CLC   REPCODE,0(R3)                                                    
         BE    *+12                GOT IT                                       
         AHI   R3,L'REPIDS         SKIP TO NEXT ENTRY                           
         B     DFX06                                                            
*                                                                               
         MVC   WORK,SPACESL                                                     
         XR    R4,R4                                                            
         IC    R4,14(R3)           CONSTRUCT RECEIVING ID IN WORK               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),15(R3)      REP-ID                                       
         TM    13(R3),X'80'        APPEND OFFICE CODE?                          
         BO    EXITOK              NO                                           
*                                                                               
         LA    R3,WORK+1(R4)       POINT BEYOND REP-ID                          
         MVC   0(2,R3),ROUTCODE+3  OFFICE                                       
*                                                                               
         XC    KEY,KEY             BUILD DARE STATION RECORD KEY                
X        USING AGRKEYD,KEY                                                      
         MVI   X.AGRKSYS,AGRKSYSQ                                               
         MVI   X.AGRKTYP,AGRKTYPQ                                               
         MVC   X.AGRKMEDA,MEDIA                                                 
         MVC   X.AGRKAGRT,ROUTCODE                                              
*                                                                               
         GOTO1 VDMGR,DMCB,DMREAD,GENDIR,KEY,KEY                                 
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'10'                                                      
         BO    EXITOK              NO OFFICE EXCEPTION RECORD                   
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDMGR,DMCB,GETREC,GENFIL,KEY+36,AIO,DMWORK                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,OFF31                                                         
         GOTO1 VHELLO,DMCB,(C'G',GENFIL),('AGROVRCQ',AIO),0,0                   
         BRAS  RE,ON31                                                          
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO REP ELEMENT (SHOULDN'T HAPPEN)            
         ICM   R4,15,12(R1)                                                     
         USING AGROVRD,R4                                                       
*                                                                               
         XR    RF,RF                                                            
DFX08    CLC   REPCODE,AGROVRCR    IS THERE AN OVERRIDE FOR THIS REP?           
         BNE   *+14                YES                                          
         MVC   0(2,R3),AGROVROF    PUT OFFICE OVERRIDE IN RECEIVING ID          
         B     EXITOK                                                           
*                                                                               
         IC    RF,AGROVRLN         NEXT OVERRIDE ELEMENT                        
         AR    R4,RF                                                            
         CLI   AGROVRC,AGROVRCQ                                                 
         BE    DFX08                                                            
         B     EXITOK                                                           
         DROP  X,R4,R7                                                          
         EJECT                                                                  
***********************************************************************         
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'       *         
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.      *         
***********************************************************************         
         SPACE 1                                                                
CHKOPER  NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,19                                                            
         BRAS  RE,SYSMESS          OPERATOR COMMAND MESSAGE                     
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CH02                                                             
         MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         LHI   R0,20                                                            
         BRAS  RE,SYSMESS          OUTPUT OPERATOR STOP MESSAGE                 
         B     CHX                                                              
*                                                                               
CH02     CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT DID THE OPERATOR DO?                    
*                                                                               
         LA    R4,COMMTAB                                                       
         USING COMMTABD,R4                                                      
         XR    RF,RF                                                            
CH04     CLI   0(R4),X'FF'         EOT                                          
         BE    CHBAD               BAD COMMAND                                  
*                                                                               
         IC    RF,COMMLEN          GET MINIMUM LENGTH                           
         CH    RF,CIBDATLN         CHECK STRING LENGTH                          
         BL    CH06                                                             
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8              MATCH COMMAND IN TABLE                       
         BE    CH08                PASS                                         
         CLC   COMMCMD(0),CIBDATA                                               
*                                                                               
CH06     AHI   R4,COMMTABL         NEXT ENTRY                                   
         B     CH04                                                             
*                                                                               
CH08     ICM   RF,15,COMMRTN       GET PROCESSING ROUTINE                       
         BASR  RE,RF                                                            
         B     CHX                                                              
*                                                                               
CHBAD    LHI   R0,21                                                            
         BRAS  RE,SYSMESS          OUTPUT UNKNOWN COMMAND MESSAGE               
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
COMMTAB  DC    CL8'????????',AL1(8,0,0,0),AL4(OPDUMMY)                          
         DC    X'FF'                                                            
*                                                                               
OPDUMMY  BR    RE                                                               
*                                                                               
COMMTABD DSECT                     COVERS COMMTAB ABOVE                         
COMMCMD  DS    CL8                 INPUT COMMAND                                
COMMLEN  DS    X                   MINIMUM LENGTH                               
         DS    XL3                                                              
COMMRTN  DS    AL4                 A(PROCESSING ROUTINE)                        
COMMTABL EQU   *-COMMTABD                                                       
*                                                                               
EDIFAXS  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SENT MQ MESSAGE TO LOG FOR THIS DESTINATION                         *         
* NTRY:  R7    = DSTAEFTD                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING DSTAEFTD,R7                                                      
POSTSENT NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
*                                                                               
         MVC   0(3,R3),=C'ADD'                                                  
         TM    DSTAEFLG,DSTAEFJQ   IS THIS DEST GOOD?                           
         BNO   *+10                YES                                          
         MVC   0(3,R3),=C'JNK'                                                  
         AHI   R3,3                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         BRAS  RE,BLDCREF          REFERENCE NUMBER RETURNS IN CUSTREF          
*                                                                               
         MVC   0(L'CUSTREF,R3),CUSTREF                                          
         AHI   R3,L'CUSTREF                                                     
         BRAS  RE,ADDCRLF                                                       
*                                  SET METH=EASYLINK                            
         MVI   0(R3),EDIEASYQ                                                   
         AHI   R3,1                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(2,R3),=C'62'                                                   
         AHI   R3,2                                                             
         MVC   0(L'MAILBOX,R3),MAILBOX                                          
         AHI   R3,L'MAILBOX                                                     
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'DSTTYPE,R3),DSTTYPE                                          
         AHI   R3,L'DSTTYPE                                                     
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(80,R3),HDR        WE WANT ALL THE *HDR* CARD                   
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(80,R3),TRN        WE WANT ALL THE ++TRN CARD                   
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         OC    MQNLEN,MQNLEN                                                    
         BZ    *+14                                                             
         MVC   0(80,R3),MQN        WE WANT ALL THE ++MQN CARD IF THERE          
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         OC    PQSLEN,PQSLEN                                                    
         BZ    *+14                                                             
         MVC   0(80,R3),PQS        WE WANT ALL THE ++PQS CARD IF THERE          
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'DSTAEDST,R3),DSTAEDST                                        
         AHI   R3,L'DSTAEDST                                                    
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'DSTAEFRM,R3),DSTAEFRM                                        
         AHI   R3,L'DSTAEFRM                                                    
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'RPTPQTYP,R3),RPTPQTYP                                        
         AHI   R3,L'RPTPQTYP                                                    
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVI   0(R3),C' '          BDE SENDER ID, N/A HERE                      
         AHI   R3,1                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   MQPUTQ,=A(LOGQHOB)                                               
         LA    RF,WORK                                                          
         ST    RF,MQPUTBUF                                                      
         SR    R3,RF                                                            
         ST    R3,DATALEN                                                       
*                                                                               
         LA    R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
         LA    R2,MQCMIT           MQ COMMIT                                    
         BRAS  RE,CALLMQ                                                        
*                                                                               
         MVC   MQPUTQ,=A(OUTQHOB)                                               
         XC    EACTION,EACTION                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET INFORMATION FROM USER ID RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
GETIDN   NTR1  BASE=*,LABEL=*                                                   
         XC    ROUTCODE,ROUTCODE                                                
         XC    AGYPOWER,AGYPOWER                                                
         XC    SVAGYPOW,SVAGYPOW                                                
         XC    AGYNAME,AGYNAME                                                  
         XC    AGYADDR,AGYADDR                                                  
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
X        USING CTIREC,KEY                                                       
         MVI   X.CTIKTYP,CTIKTYPQ                                               
         MVC   X.CTIKNUM,USERIDNO                                               
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
         CLI   8(R1),0             USERID NOT FOUND                             
         BNE   EXITL                                                            
         DROP  X                                                                
*                                                                               
         L     R4,AIO                                                           
         AHI   R4,28                                                            
         XR    RF,RF                                                            
*                                                                               
         USING CTDSCD,R4                                                        
GIDN02   CLI   0(R4),0                                                          
         BE    EXITL                                                            
         CLI   0(R4),CTAGYELQ      GET AGENCY POWERCODE                         
         BNE   GIDN04                                                           
*                                                                               
         MVC   AGYPOWER,CTAGYID-CTAGYD(R4)                                      
         MVC   SVAGYPOW,AGYPOWER                                                
*                                                                               
GIDN04   CLI   0(R4),CTORGELQ      GET ORIGIN DETAILS                           
         BNE   GIDN06                                                           
         MVC   AGYNAME,CTORGNAM-CTORGD(R4)                                      
         MVC   AGYADDR,CTORGADD-CTORGD(R4)                                      
*                                                                               
GIDN06   CLI   0(R4),CTUSAELQ      GET US AGENCY EXTRA INFO                     
         BNE   GIDN08                                                           
         MVC   ROUTCODE,CTUSADRC-CTUSAD(R4) DARE ROUTING CODE                   
*                                                                               
GIDN08   IC    RF,CTDSCLEN                                                      
         BXH   R4,RF,GIDN02                                                     
         EJECT                                                                  
***********************************************************************         
* CREATE UNIQUE REFERENCE NUMBER FOR ETI FILE                         *         
* NTRY: R7 = A(CURRENT DEST IN THE TABLE)                             *         
* EXIT: CUSTREF   = REFERENCE NUMBER                                  *         
***********************************************************************         
         SPACE 1                                                                
BLDCREF  NTR1  BASE=*,LABEL=*                                                   
         MVC   CUSTREF,SPACESL                                                  
         LA    R2,CUSTREF                                                       
         MVI   0(R2),C'1'          VERSION 1                                    
         MVC   1(1,R2),FACTYPE     ADV/REP                                      
         CLI   DRTEST,C'Y'                                                      
         BNE   *+8                                                              
         MVI   0(R2),C'D'          DR                                           
         AHI   R2,2                                                             
*                                                                               
         MVC   0(L'USERIDH,R2),USERIDH                                          
         AHI   R2,L'USERIDH                                                     
         MVC   0(L'RPTSUBID,R2),RPTSUBID                                        
         AHI   R2,L'RPTSUBID                                                    
         MVC   0(L'RPTPQNUM,R2),RPTPQNUM                                        
         AHI   R2,L'RPTPQNUM                                                    
         MVC   0(L'RPTHTIME,R2),RPTHTIME                                        
         AHI   R2,L'RPTHTIME                                                    
         MVC   0(L'RPTLOGNO,R2),RPTLOGNO                                        
         AHI   R2,L'RPTLOGNO                                                    
*                                                                               
         LR    RF,R7                                                            
         L     RE,ADSTAEFT                                                      
         SR    RF,RE               ADDRESS DIFFERENCE                           
         SR    RE,RE               PREPARE FOR DIVIDE                           
         LHI   R1,DSTAEFTL                                                      
         DR    RE,R1                                                            
         AHI   RF,1                DEST#                                        
         STCM  RF,1,RPTLDSTS                                                    
*                                                                               
         LR    RF,R7                                                            
         AHI   RF,DSTAEFTL         TEST THIS IS LAST DESTINATION                
         CLI   0(RF),X'FF'         EOT?                                         
         BE    *+14                                                             
         OC    DSTAEDST-DSTAEFTD(L'DSTAEDST,RF),DSTAEDST-DSTAEFTD(RF)           
         BNZ   *+8                                                              
         OI    RPTLDSTS,X'80'      FLAG AS LAST DESTINATION                     
*                                                                               
         GOTO1 VHEXOUT,DMCB,RPTLDSTS,0(R2),L'RPTLDSTS                           
         AHI   R2,L'RPTLDSTS*2                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* COMMON STORAGE/ROUTINES                                             *         
***********************************************************************         
         SPACE 1                                                                
         ORG   EDIFAXS+(((*-EDIFAXS)/4096)+1)*4096                              
COMMON   DC    CL8'*COMMON*'                                                    
SAVERD   DC    F'0'                                                             
*                                                                               
ON31     O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
EXITR3   CR    RB,RB                                                            
         XIT1  REGS=(R3)                                                        
EXITR7   CR    RB,RB                                                            
         XIT1  REGS=(R7)                                                        
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE ,                                                                
*                                                                               
ADDCRLF  MVC   0(L'CRLF,R3),CRLF                                                
         AHI   R3,L'CRLF                                                        
         BR    RE                                                               
*                                                                               
CRLF     DC    X'0D25'             EBCDIC <CRLF>                                
*                                                                               
LINESUP  NTR1  ,                   INCREMENT LINE COUNT AND LOG IF REQ          
         L     R0,NUMLINES                                                      
         AHI   R0,1                                                             
         ST    R0,NUMLINES                                                      
*                                                                               
         CLI   TRACE,C'F'          FULL TRACE PRINTS ALL LINES                  
         BNE   EXITOK                                                           
         ICM   RF,15,LASTOUT                                                    
         BNZ   *+8                                                              
         L     RF,AMSGOUT                                                       
         ST    R3,LASTOUT                                                       
*                                                                               
         LR    R1,R3                                                            
         SR    R1,RF                                                            
         BM    EXITOK                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),0(RF)                                                   
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
ARZERO   DC    16F'0'                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'HCONN==>'                                                    
HCONN    DC    F'0',F'0'           MQ QMGR CONNECTION HANDLE                    
*                                                                               
         DC    CL8'QMGR===>'                                                    
QMGR     DC    CL48' '                                                          
*                                                                               
         DC    CL8'MSGBUFFS'                                                    
AMSGIN   DC    A(0)                INPUT MESSAGE BUFFER                         
AMSGOUT  DC    A(0)                OUTPUT MESSAGE BUFFER                        
MAXMSGLN DC    A(2*1024*1024)         MAX MESSAGE LENGTH                        
*                                                                               
         LTORG                                                                  
*                                                                               
TRACE    DC    C'N'                                                             
GOTHDR   DC    C'N'                                                             
DRTEST   DC    C'N'                                                             
DEBUG    DC    C'N'                                                             
SENDDLNS DC    C'Y'                'Y' IF WE WANT DLNS FROM EASYLINK            
LOG      DC    C'N'                'Y' FOR FULL LOGGING                         
FACTYPE  DC    CL3'ADV'                                                         
JREPMAX  DC    F'10'               MAX # OF JUNK REP ALLOWED                    
JREPCNT  DC    F'1'                # OF JUNK REP COUNTER                        
*                                                                               
VADDAY   DC    V(ADDAY)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VCARDS   DC    V(CARDS)                                                         
VDDSIO   DC    V(DDSIO)                                                         
VDMGR    DC    V(DATAMGR)                                                       
VDADDS   DC    V(DADDS)                                                         
VDATCON  DC    V(DATCON)                                                        
VHELLO   DC    V(HELLO)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VLOGIO   DC    V(LOGIO)                                                         
VSCANNER DC    V(SCANNER)                                                       
*                                                                               
ASYSMSGS DC    A(SYSMSGS)                                                       
ALINETAB DC    A(LINETAB)                                                       
ASCANTAB DC    A(SCANTAB)                                                       
AERRMSG  DC    A(ERRMSGS)                                                       
ACARDTAB DC    A(CARDTAB)                                                       
ACARDSIN DC    A(CARDSIN)                                                       
*                                                                               
AREPIDS  DC    A(REPIDS)                                                        
ACOVSHT  DC    A(COVSHT)                                                        
ATSTIDS  DC    A(TSTIDTAB)                                                      
ATSTAGS  DC    A(TSTAGTAB)                                                      
ADSTAEFT DC    A(DSTAEFT)                                                       
AWHY     DC    A(MQ_REASON_CODE_TABLE)                                          
ASSB     DC    A(SSB)                                                           
AUTL     DC    A(UTL)                                                           
AIO      DC    A(IO)                                                            
*                                                                               
LASTOUT  DC    A(0)                                                             
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
ACOMM    DC    A(0)                A(COMMUNICATIONS PARAMETER LIST)             
*                                                                               
MESSAGE  DS    CL60                                                             
XTRAMESS DS    XL8'00'                                                          
XTRAMES2 DS    XL18'00'                                                         
*                                                                               
DMOPEN   DC    CL8'DMOPEN  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
GENDIR   DC    CL8'GENDIR  '                                                    
GENFIL   DC    CL8'GENFIL  '                                                    
*                                                                               
CTOPEN   DC    C'N'                                                             
CONTROL  DC    CL8'CONTROL '                                                    
CTFLIST  DC    C'NCTFILE NGENDIR NGENFIL X'                                     
*                                                                               
P99      DC    P'99'                                                            
*                                                                               
         DS    0D                                                               
         DC    CL08'ECBLIST*'                                                   
ECBLST   DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    X'00',AL3(GETECB)                                                
         DC    X'80',AL3(TIMERECB) TIMER POP                                    
         DC    CL08'ECBLISTX'                                                   
*                                                                               
GETECB   DC    A(0),CL12'<===GETECB'                                            
TIMERECB DC    A(0),CL12'<===TIMERECB'                                          
*                                                                               
         DC    CL8'RLEN===>'                                                    
RLEN     DS    H                                                                
R        DS    XL256                                                            
         DC    CL8'HDRLEN=>'                                                    
HDRLEN   DS    H                                                                
HDR      DS    XL256                                                            
*                                                                               
STARS    DC    80C'*'                                                           
SPACESL  DC    256C' '                                                          
*                                                                               
ERRMSG1  DS    0C                                                               
         DC    C'AUTONOTE*YYUNN,AATKN:'                                         
ERRMSG1R DS    CL(L'JUNKRPT+L'DSTAEDST)                                         
ERRMSG1Q EQU   *-ERRMSG1                                                        
         EJECT                                                                  
***********************************************************************         
* PARAMETER LISTS TO FACILITATE MQ CALLS                              *         
* CL16 EBCDIC  ROUTINE NAME                                           *         
* A    A(ROUTINE)                                                     *         
* PARAMETERS (STANDARD IBM FORMAT)                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
MQCT     DC    CL16'MQ QMGR connect'                                            
AMQCT    DC    A(0)                                                             
         DC    A(QMGR)                                                          
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQOPENQ  DC    CL16'MQ Open queue'                                              
AMQOPENQ DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQOPNQNM DC    A(0)                CMQODA                                       
         DC    A(OPN_OPTS)                                                      
MQOPNHOB DC    A(0)                HANDLE TO OBJECT (RETURNED)                  
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQCMIT   DC    CL16'MQ Commit'                                                  
AMQCMIT  DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQCLOSE  DC    CL16'MQ Close Queue'                                             
AMQCLOSE DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQCLSHOB DC    A(0)                                                             
         DC    A(CLS_OPTS)                                                      
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQDISC   DC    CL16'MQ Disconnect'                                              
AMQDISC  DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQGET    DC    CL16'MQ Get'                                                     
AMQGET   DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(INPQHOB)          QUEUE HOB                                    
         DC    A(MSGDESC)                                                       
         DC    A(GETOPTS)                                                       
         DC    A(MAXMSGLN)         MAX BUFFER LENGTH                            
MQGETBUF DC    A(0)                BUFFER ADDRESS                               
         DC    A(DATALEN)                                                       
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQPUT    DC    CL16'MQ Put'                                                     
AMQPUT   DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQPUTQ   DC    A(OUTQHOB)          QUEUE NAME                                   
         DC    A(MSGDESC)                                                       
         DC    A(PUTOPTS)                                                       
MQPUTLEN DC    A(DATALEN)                                                       
MQPUTBUF DC    A(0)                                                             
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
OPN_OPTS DS    F                   MQOPEN OPTIONS                               
CLS_OPTS DS    F                   MQCLOSE OPTIONS                              
MQ_CC    DS    F                   COMPLETION CODE                              
MQ_RC    DS    F                   QUALIFIES COMPLETION CODE                    
DATALEN  DS    F                   LENGTH OF THE MESSAGE                        
*                                                                               
         DS    0D                                                               
         DC    CL8'INPQ===>'                                                    
INPQHOB  DC    F'0',C'<==>'                                                     
INPQ     CMQODA  LIST=YES          OBJECT DESCRIPTOR                            
*                                                                               
         DS    0D                                                               
         DC    CL8'OUTQ===>'                                                    
OUTQHOB  DC    F'0',C'<==>'                                                     
OUTQ     CMQODA  LIST=YES          OBJECT DESCRIPTOR                            
*                                                                               
         DS    0D                                                               
         DC    CL8'LOGQ===>'                                                    
LOGQHOB  DC    F'0',C'<==>'                                                     
LOGQ     CMQODA  LIST=YES          OBJECT DESCRIPTOR                            
*                                                                               
         DS    0D                                                               
         DC    CL8'MSGDESC>'                                                    
MSGDESC  CMQMDA  LIST=YES          MESSAGE DESCRIPTOR                           
*                                                                               
         DS    0D                                                               
         DC    CL8'GETOPTS>'                                                    
GETOPTS  CMQGMOA LIST=YES          GET MESSAGE OPTIONS                          
*                                                                               
         DS    0D                                                               
         DC    CL8'PUTOPTS>'                                                    
PUTOPTS  CMQPMOA DSECT=NO,LIST=YES PUT MESSAGE OPTIONS                          
         EJECT                                                                  
***********************************************************************         
* TABLE OF INPUT LINES ON DDS CONTROL CARDS                           *         
***********************************************************************         
         SPACE 1                                                                
LINETAB  DS    0F                                                               
         DC    CL3'SUB',X'00'                                                   
SUBLEN   DS    H                                                                
SUB      DS    XL256                                                            
         DC    CL3'RCP',X'00'                                                   
RCPLEN   DS    H                                                                
RCP      DS    XL256                                                            
         DC    CL3'FIL',X'00'                                                   
FILLEN   DS    H                                                                
FIL      DS    XL256                                                            
         DC    CL3'EXT',X'00'                                                   
EXTLEN   DS    H                                                                
EXT      DS    XL256                                                            
         DC    CL3'DSN',X'00'                                                   
DSNLEN   DS    H                                                                
DSN      DS    XL256                                                            
         DC    CL3'TRN',X'00'                                                   
TRNLEN   DS    H                                                                
TRN      DS    XL256                                                            
         DC    CL3'DXC',X'00'                                                   
DXCLEN   DS    H                                                                
DXC      DS    XL256                                                            
         DC    CL3'MQN',X'00'                                                   
MQNLEN   DS    H                                                                
MQN      DS    XL256                                                            
         DC    CL3'PQS',X'00'                                                   
PQSLEN   DS    H                                                                
PQS      DS    XL256                                                            
         DC    X'FF'                                                            
*                                                                               
LINETABD DSECT                                                                  
LINEID   DS    CL3                                                              
         DS    C                                                                
LINELEN  DS    H                                                                
LINEIN   DS    CL256                                                            
LINETABL EQU   *-LINETABD                                                       
*                                                                               
EDIFAXS  CSECT                                                                  
*                                                                               
TRTAB    DS    0XL256                                                           
         DC    X'40404040404040404040404040404040' 00-0F                        
         DC    X'40404040404040404040404040404040' 10-1F                        
         DC    X'40404040404040404040404040404040' 20-2F                        
         DC    X'40404040404040404040404040404040' 30-3F                        
         DC    X'404040404040404040404A4B4C4D4E4F' 40-4F                        
         DC    X'504040404040404040405A5B5C5D5E5F' 50-5F                        
         DC    X'606140404040404040406A6B6C6D6E6F' 60-6F                        
         DC    X'404040404040404040797A7B7C7D7E7F' 70-7F                        
         DC    X'40818283848586878889404040404040' 80-8F                        
         DC    X'4091929394959697989940404040409F' 90-9F                        
         DC    X'40A1A2A3A4A5A6A7A8A9404040404040' A0-AF                        
         DC    X'40404040404040404040404040404040' B0-BF                        
         DC    X'C0C1C2C3C4C5C6C7C8C9404040404040' C0-CF                        
         DC    X'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                        
         DC    X'E040E2E3E4E5E6E7E8E9404040404040' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F9404040404040' F0-FF                        
         EJECT                                                                  
***********************************************************************         
* NON-ADDRESSIBLE STORAGE AREAS                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL16'DSTADST+DSTADST+'                                           
DSTAEFT  DS    (DSTMAXQ)XL(DSTAEFTL)                                            
         DC    X'FFFFFFFF'                                                      
         DC    CL16'DSTADST-DSTADST-'                                           
DSTMAXQ  EQU   100                                                              
*                                                                               
DSTAEFTD DSECT                                                                  
DSTTYPE  DS    C                   DESTINATION TYPE (FAX OR REDILIST)           
DSTAEFLG DS    X                   STATUS                                       
DSTAEFJQ EQU   X'80'               JUNK                                         
DSTAEDST DS    CL25                FAX DESTINATION                              
DSTAEFRM DS    CL16                FORMATTED DESTINATION                        
DSTAEFTL EQU   *-DSTAEFTD                                                       
*                                                                               
EDIFAXS  CSECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL16'SCANTAB+SCANTAB+'                                           
SCANTAB  DS    (PARMCNTQ)CL(SCBLKLQ)                                            
         DC    CL16'SCANTAB-SCANTAB-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'SSB+SSB+SSB+SSB+'                                           
SSB      DC    XL2'0000',X'FF',1021X'00'                                        
         DC    CL16'SSB-SSB-SSB-SSB-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'UTL+UTL+UTL+UTL+'                                           
UTL      DC    F'0',X'0A',251X'00'                                              
         DC    CL16'UTL-UTL-UTL-UTL-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'+IO++IO++IO++IO+'                                           
IO       DC    2048X'00'           CTFILE/GENFIL I/O AREA                       
         DC    CL16'-IO--IO--IO--IO-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'+COVERSHEET+++++'                                           
COVSHT   DS    60CL132             COVER PAGE HERE IF NECESSARY                 
COVSHTL  EQU   *-COVSHT                                                         
         DC    CL16'-COVERSHEET-----'                                           
         EJECT                                                                  
***********************************************************************         
* INCLUDED DATA                                                       *         
***********************************************************************         
         SPACE 1                                                                
*DDMQREASON                        MQ REASON CODES                              
         PRINT OFF                                                              
       ++INCLUDE DDMQREASON                                                     
         PRINT ON                                                               
*TSTIDTAB                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDEDITSTID                                                     
         PRINT ON                                                               
*DDDARETAB                         FOR REPIDS                                   
         PRINT OFF                                                              
       ++INCLUDE DDDARETAB                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* VALID INPUT CARD TABLE                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
CARDTAB  DC    CL10'DSPACE    ',AL1(5,5,0,0),AL4(VCDSPACE)                      
         DC    CL10'DDSIO     ',AL1(4,4,0,0),AL4(VCDDSIO)                       
         DC    CL10'DEBUG     ',AL1(4,4,0,0),AL4(VCDEBUG)                       
         DC    CL10'DRTEST    ',AL1(5,5,0,0),AL4(VCDR)                          
         DC    CL10'TRACE     ',AL1(4,4,0,0),AL4(VCTRACE)                       
         DC    CL10'LOGGING   ',AL1(6,6,0,0),AL4(VCLOG)                         
         DC    CL10'DLNS      ',AL1(3,3,0,0),AL4(VCDLNS)                        
         DC    CL10'TYPE      ',AL1(3,3,0,0),AL4(VCTYPE)                        
         DC    CL10'QMGR      ',AL1(3,3,CFBIG,0),AL4(VCQMGR)                    
         DC    CL10'JUNKREP   ',AL1(6,6,0,0),AL4(VJREP)                         
         DC    X'FF'                                                            
*                                                                               
CARDSIN  DC    15CL80' '                                                        
         DC    X'FF'                                                            
*                                                                               
CARDTABD DSECT                     INPUT CARD PARAMETERS                        
CARDTXT  DS    CL10                TEXT                                         
CMIN     DS    X                   MIN LEN FOR EXECUTE (-1)                     
CMAX     DS    X                   MAX LEN FOR EXECUTE (-1)                     
CFLAG    DS    X                   FLAGS                                        
CFBIG    EQU   X'80'                                                            
         DS    X                   N/D                                          
CARDVAL  DS    AL4                 A(VALIDATION ROUTINE)                        
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
EDIFAXS  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                       *         
***********************************************************************         
         SPACE 1                                                                
ERRMSGS  DS    0CL45                                                            
EMSG01   DC    CL45'Undefined parameter card                    '               
EMSG02   DC    CL45'DSPACE= parameter can only be 1 byte long   '               
EMSG03   DC    CL45'DDSIO= parameter cannot be more than 8 long '               
EMSG04   DC    CL45'Input line is not valid for SCANNER         '               
EMSG05   DC    CL45'Too many parameters in total                '               
EMSG06   DC    CL45'The parameter must be a number              '               
EMSG07   DC    CL45'This userid is not valid                    '               
EMSG08   DC    CL45'This userid has no 02 element - this is bad '               
EMSG09   DC    CL45'Too many filters - this one is ignored      '               
EMSG10   DC    CL45'This user has no EDICT record               '               
EMSG11   DC    CL45'Unable to open print queue report           '               
EMSG12   DC    CL45'Unable to write first line to PQ            '               
EMSG13   DC    CL45'Unable to write page eject to PQ            '               
EMSG14   DC    CL45'Print line is too long to handle            '               
EMSG15   DC    CL45'Unable to write print line to PQ            '               
EMSG16   DC    CL45'Unable to close print queue report          '               
EMSG17   DC    CL45'Must erase partial PQ report                '               
EMSG18   DC    CL45'Unable to find incoming EDICT user id       '               
EMSG19   DC    CL45'TYPE= must be A(DV) or R(EP)                '               
EMSG20   DC    CL45'Unknown ++DDS card - please check           '               
         EJECT                                                                  
***********************************************************************         
* CONSOLE MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                     *         
***********************************************************************         
         SPACE 1                                                                
SYSMSGS  DS    0CL50                                                            
SMSG01   DC    CL50'XBeginning initialisation                      '            
SMSG02   DC    CL50'XCompleted initialisation                      '            
SMSG03   DC    CL50'XBeginning reading input cards                 '            
SMSG04   DC    CL50'XCompleted reading input cards                 '            
SMSG05   DC    CL50'XBeginning validating input cards              '            
SMSG06   DC    CL50'XCompleted validating input cards              '            
SMSG07   DC    CL50'XBeginning setting operator comms              '            
SMSG08   DC    CL50'XCompleted setting operator comms              '            
SMSG09   DC    CL50'XBeginning MQ Initialisation                   '            
SMSG10   DC    CL50'XCompleted MQ Initialisation                   '            
SMSG11   DC    CL50'XObtained  MQ QMGR handle                      '            
SMSG12   DC    CL50'XOpened    MQ Master Queue                     '            
SMSG13   DC    CL50'XOpened    MQ Log Queue                        '            
SMSG14   DC    CL50'XBeginning MQ Deallocation                     '            
SMSG15   DC    CL50'XCompleted MQ Deallocation                     '            
SMSG16   DC    CL50'XClosed    MQ Master Queue                     '            
SMSG17   DC    CL50'XClosed    MQ Log Queue                        '            
SMSG18   DC    CL50'X** WARNING ** Logging Suppressed              '            
SMSG19   DC    CL50'XIncoming operator command                     '            
SMSG20   DC    CL50'X** WARNING ** Operator requested "STOP"       '            
SMSG21   DC    CL50'X** WARNING ** Unknown operator command ignored'            
SMSG22   DC    CL50'XOpened all subsidiary MQ Queues               '            
SMSG23   DC    CL50'XClosed all subsidiary MQ Queues               '            
SMSG24   DC    CL50'XNUMPRTS inadequate - abending call programmer '            
SMSG25   DC    CL50'X** ERROR ** Unknown userid XXXXXXXXXXXXXXXXXX '            
SMSG26   DC    CL50'XBeginning building PQ list                    '            
SMSG27   DC    CL50'XCompleted building PQ list                    '            
SMSG28   DC    CL50'X** ERROR ** Mailbox XXXXXXXX is unknown       '            
SMSG29   DC    CL50'4** ERROR ** DSTTAB size inadequate must ABEND '            
SMSG30   DC    CL50'XEASYLINK -- Transmissions suspended (MQ issue)'            
         EJECT                                                                  
***********************************************************************         
* W/S AREA                                                            *         
***********************************************************************         
         SPACE 1                                                                
         ORG   EDIFAXS+(((*-EDIFAXS)/4096)+1)*4096                              
         DC    CL16'*WORKD***WORKD**'                                           
WORKD    DS    0X                                                               
DUB      DS    D                                                                
DMWORK   DS    12D                                                              
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
STRTTIME DS    F                                                                
*                                                                               
PLIST    DS    8F                                                               
DMCB     DS    8F                                                               
*                                                                               
PACKOF4B DS    PL4                 KEEP FULLWORD ALIGNED                        
WORK     DS    XL256               DITTO                                        
*                                                                               
NUMLINES DS    F                   NUMBER OF TRANSMITTED LINES/REPORT           
NUMPAGES DS    H                   PAGE COUNTER FOR FAX TRANSMISSION            
*                                                                               
REQUESTR DS    CL3                                                              
AGYPOWER DS    CL2                 AGENCY POWER CODE                            
SVAGYPOW DS    CL2                 SAVED AGENCY POWER CODE                      
AGYNAME  DS    CL33                                                             
AGYADDR  DS    CL33                                                             
EASYDATA DS    XL21                EASYLINK-SPECIFIC TRANSFER DATA              
ROUTCODE DS    CL5                 DARE ROUTING CODE                            
MEDIA    DS    C                   DARE MEDIA                                   
REPCODE  DS    CL3                 DARE REP CODE                                
CUSTREF  DS    CL27                VFUUUUSSSNNNNNTTTTTTTTLLLLSS                 
DESTNTRY DS    XL(DESTTBLQ)        TEMP STORAGE FOR 1 DESTTBL  ENTRY            
*                                                                               
MAILBOX  DS    CL6                 EASYLINK MAILBOX NUMBER                      
DSTNAME  DS    CL8                 DESTINATION NAME                             
DSTUIDN  DS    XL2                 USERID                                       
FAXKEY   DS    CL7                 KEY OF FAX RECORD                            
FAXSUBC  DS    CL6                 FAX SUBCODE RECORD                           
LONGFXPG DS    C                   'Y' = WE SHOULD SEND 'FAXL' COMMAND          
LASTEJCT DS    C                   'Y' = LAST LINE PRINTED WAS /PAGE            
*                                                                               
LSTSDST  DS    C                                                                
*                                                                               
CONOK    DS    X                                                                
OPERSTOP DS    X                                                                
PLINE    DS    CL100               OUTPUT PRINT LINE                            
REPLY    DS    CL8                                                              
CARDIO   DS    CL80                                                             
COVPGLNS DS    H                   NUMBER OF LINES IN COVER PAGE                
FERN     DS    X                                                                
ETYPE    DS    X                                                                
GOTERR   DS    X                                                                
PARMCNT  DS    X                   NO OF I/P CARDS                              
PARMCNTQ EQU   100                 MAX NUMBER OF I/P CARDS                      
MVSNAME  DS    CL8                                                              
KEY      DS    XL64                FOR CTFILE/GENDIR READS                      
*                                                                               
EACTION  DS    X                   ACTION(S)                                    
EACTJNKQ EQU   C'J'                 UNSENDABLE                                  
*                                                                               
EERRORCD DS    X                   ERROR CODE                                   
EDAYNUM  DS    X                   PWOS DAY SENT/DLVRD/CAN                      
*                                                                               
USERID   DS    CL8                 REPORT USERID (ALPHA)                        
USERIDOV DS    CL8                 REPORT USERID (ALPHA)                        
USERIDH  DS    CL4                 REPORT USERID (HEX AS CHARS)                 
USERIDNO DS    XL2                 REPORT USERID                                
RPTSUBID DS    CL3                 REPORT SUB-ID                                
RPTPQNUM DS    CL4                 REPORT REFERENCE NUMBER                      
RPTHTIME DS    CL8                 REPORT CREATION DATE/TIME                    
RPTLOGNO DS    CL4                 LOGICAL REPORT SEQ. WITHIN PHYSICAL          
RPTLDSTS DS    XL1                 LOG. REP. DEST. SEQ. WITHIN PHYSICAL         
RPTPQTYP DS    XL2                 PQ REPORT TYPE                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
         DS    0D                                                               
         DC    CL16'ENTRYPTSENTRYPTS'                                           
ENTRYPTS DC    Y((ENTRYLSQ-ENTRYSTQ)/60) NUMBER OF TABLE ENTRIES                
         DC    H'60'                     MUST REMAIN AS 60                      
ENTRYSTQ EQU   *                                                                
CSQBCLOS DC    CL8'CSQBCLOS'                                                    
         DC    XL52'00'                                                         
CSQBCOMM DC    CL8'CSQBCOMM'                                                    
         DC    XL52'00'                                                         
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBGET  DC    CL8'CSQBGET'                                                     
         DC    XL52'00'                                                         
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
         DC    XL52'00'                                                         
CSQBPUT  DC    CL8'CSQBPUT'                                                     
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
*                                                                               
         CMQA    LIST=YES,EQUONLY=NO                                            
         EJECT                                                                  
***********************************************************************         
* RD CHAIN CSECT                                                      *         
***********************************************************************         
         SPACE 1                                                                
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* DESTINATION TABLE DSECT (CUT DOWN FROM EDICT)                       *         
***********************************************************************         
         SPACE 1                                                                
DESTTABD DSECT                                                                  
DESTNAME DS    CL8                 DESTINATION NAME                             
DESTUIDN DS    XL2                 DDS USERID NUMBER                            
DESTMETS DS    C                   METHOD OF SENDING TRANSMISSIONS              
DESTMETR DS    C                   METHOD OF RECEVING TRANSMISSIONS             
DESTADVN DS    CL8                 ADV EASYLINK MAILBOX NUMBER                  
DESTREPN DS    CL8                 REP EASYLINK MAILBOX NUMBER                  
DESTTBLQ EQU   *-DESTTABD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDEDICTFIL                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDEDICTFIL                                                     
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* CTGENEDICT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
* CTGENSTAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENSTAD                                                      
         PRINT ON                                                               
* CTGENAGRD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENAGRD                                                      
         PRINT ON                                                               
* EDISCANHDR                                                                    
         PRINT OFF                                                              
       ++INCLUDE EDISCANHDR                                                     
         PRINT ON                                                               
* IEZCIB                                                                        
         PRINT OFF                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         PRINT ON                                                               
* IEZCOM                                                                        
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
* IHASDWA                                                                       
         PRINT OFF                                                              
         IHASDWA                                                                
         PRINT ON                                                               
* IEFZB4D2                                                                      
         PRINT OFF                                                              
         IEFZB4D2                                                               
         PRINT ON                                                               
* IHAPSA                                                                        
         PRINT OFF                                                              
         IHAPSA                                                                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025EDIFAXSND 05/02/08'                                      
         END                                                                    
