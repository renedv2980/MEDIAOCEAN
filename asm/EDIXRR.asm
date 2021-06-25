*          DATA SET EDIXRR     AT LEVEL 040 AS OF 05/27/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE EDIXRRA                                                                  
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
***********************************************************************         
* Extreme Reach Transmition of faxes                                            
* pass sub-media SUB=Y                                                          
***********************************************************************         
*&&      SET   SUB=N                                                            
EDIXRR   START                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDIXRR,WORK=VWRKAREA                                           
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
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SEND MESSAGE OUT AS A FAX                                           *         
***********************************************************************         
SENDFAX  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRFLDS          CLEAN UP BEFORE PROCESSING THIS MSG          
         SAM31                                                                  
*                                                                               
         L     R7,ADSTAEFT         R7 = CURRENT DESTINATION TABLE ENTRY         
         USING DSTAEFTD,R7                                                      
*                                                                               
         L     R4,AMSGIN           INPUT MESSAGE BUFFER                         
         USING MSGHDRD,R4                                                       
         PACK  DUB,MSGHLEN         Convert header length into binary            
         CVB   R1,DUB                                                           
         STH   R1,HDRLEN                                                        
*                                                                               
         CHI   R1,MSGHDRLQ         Wrong size expecting MSGHDLQ2                
         JNH   *+2                                                              
         MVC   USERID,MSGUSR       GET LOCAL COPIES OF HEADER INFO              
         MVC   USERIDH,MSGUSRNO    PQ user id number                            
         MVC   RPTSUBID,MSGPQSUB   PQ initials                                  
         MVC   RPTPQNUM,MSGPQNUM   PQ report number         (Chr Hex)           
         MVC   RPTHDATE,MSGHTIME   Creation Date            (Chr Hex)           
         MVC   RPTHTIME,MSGHTIME+4 Creation Time            (Chr Hex)           
         MVC   RPTLOGNO,MSGLOGNO   Logical report number                        
         MVC   RPTPQTYP,MSGPQTYP   PQ report type                               
         MVC   RPTGRPID,MSGGRPID   Group Id                                     
         MVC   SECAGY,MSGSECAG     Security agency                              
         MVC   PQDSPACE,MSGDSPCE   PQ DSPACE                                    
*                                                                               
         GOTO1 VHEXI31,DMCB,MSGUSRNO,PQUSER#,L'MSGUSRNO,0                       
         GOTO1 VHEXI31,DMCB,MSGPQNUM,PQRPT#,L'MSGPQNUM,0                        
         GOTO1 VHEXI31,DMCB,MSGPERID,PQPID#,L'MSGPERID,0                        
                                                                                
         MVI   EOR,NO                                                           
         CLI   MSGEOR,YES                                                       
         BNE   *+8                                                              
         MVI   EOR,YES                                                          
         USING GROUPIDD,GROUPID                                                 
SFX01    UNPK  GRPICODE,MSGGRPID                                                
*ah      GOTO1 VHEXIN,DMCB,USERIDH,USERIDNO,4,0                                 
*                                                                               
*        PACK  DUB,MSGHLEN         GO PAST HEADER                               
*        CVB   R0,DUB                                                           
*        AR    R4,R0               R4 = FIRST LINE OF MESSAGE                   
*                                                                               
         AH    R4,HDRLEN           R4 = FIRST LINE OF MESSAGE                   
         BRAS  RE,PERINFO          Get person information                       
         BE    SFX02                                                            
         BRAS  RE,ERRMSG                                                        
         J     EXITOK                                                           
*                                                                               
SFX02    BRAS  RE,GETNXTL          GET NEXT LINE INTO R / RLEN                  
         BNE   SFX04               EOB                                          
*                                                                               
         CLI   GOTHDR,YES          TEST ALREADY HAVE HEADER                     
         BE    SFX03               YES                                          
         BRAS  RE,PRCHDR           PROCESS THIS CARD AS IF HEADER               
         B     SFX02               NEXT CARD                                    
*                                                                               
SFX03    CLC   =C'++DDS',R+1       IS THIS A DDS CONTROL CARD                   
         BNE   SFX04               NO - THEN WE ARE INTO THE FAX PROPER         
         BRAS  RE,PRCDDS                                                        
         B     SFX02                                                            
*                                                                               
SFX04    CLI   GOTHDR,YES          TEST ALREADY HAVE HEADER                     
         BNE   NOHDR               PRT WARNING MSG AND EXIT                     
         BRAS  RE,BLDDEST          NOW WE ARE INTO THE FAX PROPER               
         MVC   XRRACC,DESTXRRN     USE EXTREME REACH                            
         BRAS  RE,PRCTRN           Workout TRN card values                      
*                                                                               
         ICM   R3,15,AMSGOUT       R3 = MQ FAX OUTPUT AREA                      
***********************************************************************         
* FOR MULTIPLE DESTINATIONS, THIS FAX SUBCODE WILL APPLY TO ALL DESTS           
***********************************************************************         
         USING HDRCARDD,HDR                                                     
         CLI   EDIDARE,C'D'        IS THIS A DARE FAX?                          
         BNE   SFX06               NO                                           
         OC    DXCLEN,DXCLEN       DXC CARD INPUT?                              
         BZ    *+10                                                             
         MVC   FAXSUBC,DXC+15                                                   
                                                                                
***********************************************************************         
* START BUILDING FAX MESSAGE IN MSGOUT                                          
***********************************************************************         
SFX06    XC    NUMLINES,NUMLINES   RESET LINE COUNTER                           
         BRAS  RE,GETIDN           GET SOME ID INFORMATION FROM CTIREC          
*                                                                               
         TIME  BIN                                                              
         ST    R0,STRTTIME         REMEMBER TIME WE START SENDING               
*                                                                               
         SAM31                                                                  
         L     R0,AMSGOUT          CLEARING LIKE THIS IS LIKELY                 
         L     R1,MAXMSGLN         OVERKILL                                     
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,TRACELOG                                                      
*                                                                               
*        MVC   USERIDOV,USERID                                                  
*                                                                               
SFX08    L     R3,AMSGOUT          FIRST BUILD HEADER INFO FOR XRR              
         BRAS  RE,DATETIME         Build date/time information                  
         BRAS  RE,BLDXREF          Build unique reference for tracking          
         BRAS  RE,DELIVERY         Build delivery destination codes             
         BRAS  RE,BLDXML           Build xml header information                 
*                                                                               
         GOTOR XMLTAG,XMLPARM,=C'delivery-doc',STARTAG                          
         MVC   0(L'CDATA,R3),CDATA XML <! CDATA                                 
         AHI   R3,L'CDATA                                                       
*        BRAS  RE,ADDCRLF                                                       
*        BRAS  RE,LINESUP                                                       
                                                                                
**********************************************************************          
* READ DESTINATION TABLE ONE ENTRY AT A TIME AND BUILD /NTF AND                 
* CUSTOM REF #, TEL#, BILLING CODE...                                           
**********************************************************************          
         L     R7,ADSTAEFT         A(DESTINATION TABLE)                         
         USING DSTAEFTD,R7                                                      
*                                  CHECK THE 1ST DEST                           
         CLC   =C'FAXW',DSTAEDST   DESTINATION BEGIN W/ FAX/FAXW?               
         BNE   SFX10                                                            
         CLI   EDIWIDE,C'L'        LANDSCAPE MODE SET?                          
         BE    SFX10               YES - DON'T SET WIDE MODE                    
         MVI   EDIWIDE,C'W'        SET WIDE REPORT                              
*                                                                               
SFX10    EQU   *                                                                
         CLI   DEBUG,YES           DEBUG FORCES ALL TO REDILIST MQTEST          
         BNE   SFX12                                                            
         MVC   DSTAEDST,=CL25'MQTEST'                                           
         MVI   DSTTYPE,EDFDSTRQ    ITS A REDILIST NAME                          
         B     SFX24                                                            
*                                                                               
SFX12    CLC   DSTAEDST,XSPACES    SPACES?                                      
         BE    SFX22               ALL SPACES - JUNK DEST                       
*                                                                               
         CLC   =C'FAX',DSTAEDST    IS DESTINATION A FAX MACHINE?                
         BNE   *+12                                                             
         MVI   DSTTYPE,EDFDSTFQ    IT'S A FAX NUMBER                            
         B     SFX24                                                            
*                                                                               
         MVI   LONGFXPG,NO         ASSUME FAX PAGES AREN'T "LONG"               
         CLC   =C'FXKEY=',DSTAEDST IS DESTINATION A FAX RECORD KEY?             
         BE    SFX14                                                            
         CLC   =C'FXKYL=',DSTAEDST (FOR "LONG" FAX PAGES)                       
         BNE   SFX16                                                            
         MVI   LONGFXPG,YES        XRR MUST GENERATE "LONG" PAGES               
*                                                                               
SFX14    MVI   DSTTYPE,EDFDSTXQ    YES                                          
         MVC   FAXKEY,DSTAEDST+6                                                
         BRAS  RE,BLDCVR           BUILD COVER PAGE                             
         OC    DSTAEDST,DSTAEDST   WAS THE FAX RECORD FOUND?                    
         BZ    SFX22               NO - ERROR                                   
         B     SFX24                                                            
*                                                                               
SFX16    CLI   EDIDARE,EDIDARQ     IS THIS A DARE FAX?                          
         BNE   SFX18               SPECIAL TREATMENT FOR DARE FAXES             
*                                                                               
         BRAS  RE,DAREFAX                                                       
         MVC   FAXKEY,WORK         RETURNED BY DAREFAX ROUTINE                  
*                                                                               
         OC    FAXKEY,FAXKEY       DID WE BUILD A RECEIVING ID?                 
         BZ    SFX22               NO - ERROR                                   
         MVC   XRRDATA+14(7),FAXKEY                                             
*                                                                               
         CLI   DSTTYPE,EDFDSTHQ    HOME MARKET W/O ID?                          
         BE    *+10                                                             
         MVC   AGYALPHA,=C'D7'     NO, ALWAYS USE USERID 'DARADM'               
*                                                                               
         MVI   LONGFXPG,NO         STANDARD SIZE FAX PAGES                      
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
         B     SFX24                                                            
*                                                                               
SFX22    EQU   *                                                                
         OI    DSTAEFLG,DSTAEFJQ   FLAG THIS DEST AS JUNK                       
         B     SFX46               NEXT DEST                                    
*                                                                               
SFX24    DS    0H                                                               
SFX26    MVI   LSTSDST,YES         GOT A GOOD DESTINATION TO SEND TO            
SFX46    AHI   R7,DSTAEFTL         NEXT DESTINATION TABLE ENTRY                 
         CLI   0(R7),EOT           EOT?                                         
         BE    SFX50                                                            
         OC    DSTAEDST,DSTAEDST   FINISHED?                                    
         BZ    SFX50                                                            
         DC    H'00'               For now only one dest allowed                
         B     SFX10                                                            
         DROP  R7                                                               
*                                                                               
SFX50    CLI   LSTSDST,YES     *** FOUND AT LEAST ONE GOOD DESTINATION?         
         BNE   SFX100              WRITE OUT ERROR MESSAGE AND QUIT             
         CLI   EDIPAGE,EDIPAGEQ    WANT TO REPLACE X'89'S WITH /PAGE?           
         BNE   SFX52                                                            
         MVI   LASTEJCT,YES        LAST LINE WAS A /PAGE                        
         MVC   00(10,R3),=CL10'/FORM/PAGE'                                      
         AHI   R3,10                                                            
*                                                                               
SFX52    CLI   EDIWIDE,EDIWIDEQ    WIDE REPORT?                                 
         BNE   *+14                                                             
         MVC   00(05,R3),=CL05'/WIDE'                                           
         AHI   R3,5                                                             
*                                                                               
         CLI   EDIWIDE,EDILANDQ    LANDSCAPE REPORT?                            
         BNE   *+14                                                             
         MVC   00(10,R3),=CL10'/LANDSCAPE'                                      
         AHI   R3,10                                                            
*                                                                               
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
         CLI   DRTEST,YES          DISASTER RECOVERY TEST MODE?                 
         BNE   *+8                                                              
         BRAS  RE,PDRTEST                                                       
*                                                                               
         OC    COVPGLNS,COVPGLNS   NOW OUTPUT COVER PAGE IF REQUIRED            
         BZ    SFX58                                                            
         BRAS  RE,PRCCVR           Not sure how this works                      
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
SFX58    CLC   R+1(L'R-1),XSPACES  IS THIS AN EMPTY LINE?                       
         BE    SFX60               YES                                          
*                                                                               
         MVI   GOTDATA,YES         NOT EMPTY FAX                                
*                                                                               
         CLC   =C'*** BXF REPORT SECTION ***',R+1                               
         BNE   SFX59                                                            
**********************************************************************          
* Finish up XML  **AH3**                                                        
**********************************************************************          
         MVI   HASBXF,YES                                                       
         MVC   0(L'CDATAX,R3),CDATAX      XML ]]>                               
         AHI   R3,L'CDATAX                                                      
*                                                                               
         GOTOR XMLTAG,XMLPARM,=C'delivery-doc',ENDTAG                           
         GOTOR XMLTAG,XMLPARM,=C'deliveries',ENDTAG                             
         GOTOR XMLTAG,XMLPARM,=C'bxf',STARTAG                                   
         BRAS  RE,BXFXML                                                        
         GOTOR XMLTAG,XMLPARM,=C'bxf',ENDTAG                                    
         GOTOR XMLTAG,XMLPARM,=C'requestPayload',ENDTAG                         
         GOTOR XMLTAG,XMLPARM,=C'message',ENDTAG                                
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
         B     SFX80               Done done                                    
*                                                                               
SFX59    LR    RE,R3               RE = MOVE TO POINT                           
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
         CLI   EDIPAGE,EDIPAGEQ    REPLACE X'89'S WITH /PAGE?                   
         BNE   SFX65                                                            
*                                                                               
         CLI   R,X'89'             REQUEST PAGE THROW?                          
         BNE   *+14                                                             
         CLC   R+1(L'R-1),XSPACES  ONLY IF BLANK LINE                           
         BE    *+12                                                             
         MVI   LASTEJCT,NO                                                      
         B     SFX65               THIS IS NOT A PAGE THROW                     
*                                                                               
         CLI   LASTEJCT,YES        IF LAST LINE ALSO EJECT IGNORE THIS          
         BE    SFX65                                                            
*                                                                               
         LH    RF,NUMPAGES         INCREMENT PAGE COUNTER                       
         AHI   RF,1                                                             
         STH   RF,NUMPAGES                                                      
*                                                                               
         MVI   LASTEJCT,YES        LAST LINE WAS A /PAGE                        
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
SFX70    EQU   *                                                                
         CLI   GOTDATA,YES         EMPTY FAX?                                   
         BNE   NODATA              YES - SKIP THIS                              
**********************************************************************          
* Finish up XML                                                                 
**********************************************************************          
         MVC   0(L'CDATAX,R3),CDATAX      XML ]]>                               
         AHI   R3,L'CDATAX                                                      
*                                                                               
         CLI   HASBXF,YES          Has BXF in PQ report ?                       
         JE    SFX78               Yes                                          
         CLI   DESTPM360,YES                                                    
         JNE   SFX78                                                            
         BRAS  RE,FIXDLV           Fix delivery code problem                    
*                                                                               
SFX78    GOTOR XMLTAG,XMLPARM,=C'delivery-doc',ENDTAG                           
         GOTOR XMLTAG,XMLPARM,=C'deliveries',ENDTAG                             
         GOTOR XMLTAG,XMLPARM,=C'requestPayload',ENDTAG                         
         GOTOR XMLTAG,XMLPARM,=C'message',ENDTAG                                
*                                                                               
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
**********************************************************************          
* Set token in MQ message description and put message out                       
**********************************************************************          
SFX80    MVC   MSGDESC_ACCOUNTINGTOKEN,TSTHDRF                                  
         CLI   DSPACE,C'T'         TST?                                         
         BNE   *+10                                                             
         MVC   MSGDESC_ACCOUNTINGTOKEN(6),TSTHDRT                               
         CLI   DSPACE,C'C'         CSC?                                         
         BNE   *+10                                                             
         MVC   MSGDESC_ACCOUNTINGTOKEN(6),TSTHDRC                               
         CLI   DSPACE,C'A'         ADV?                                         
         BNE   *+10                                                             
         MVC   MSGDESC_ACCOUNTINGTOKEN(6),TSTHDRA                               
         CLI   DSPACE,C'R'         REP?                                         
         BNE   *+10                                                             
         MVC   MSGDESC_ACCOUNTINGTOKEN(6),TSTHDRR                               
         CLI   DSPACE,C'Q'         FQA?                                         
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
         CLI   EOR,YES             Is it end of report?                         
         BNE   SFX81                                                            
         GOTOR XRRRPT,2            Build message                                
*                                                                               
         CLI   DESTMETS,EDIEXRRQ   Destination Extreme Reach                    
         BE    SFX80A                                                           
         MVC   PLINE(34),=C'+MQ NOT PUT, Summary is not for ER'                 
         BRAS  RE,PRNT                                                          
         B     SFX80X                                                           
*                                                                               
SFX80A   MVC   MQPUTBUF,AMSGOUT    PUT THIS MESSAGE TO MQ                       
         S     R3,AMSGOUT                                                       
         ST    R3,DATALEN          SET MESSAGE LENGTH                           
*                                                                               
         LA    R2,MQPUT            MQ PUT                                       
         BRAS  RE,CALLMQ                                                        
         BNE   CANTSUMY                                                         
*                                                                               
         LA    R2,MQCMIT           MQ COMMIT                                    
         BRAS  RE,CALLMQ                                                        
         BNE   CANTSUMY                                                         
*                                                                               
SFX80X   GOTOR XRRRPT,1            Reset summary report areas                   
*                                                                               
SFX81    ICM   RF,15,PUTOPTS_OPTIONS                                            
         S     RF,=AL4(MQPMO_SET_IDENTITY_CONTEXT)                              
         STCM  RF,15,PUTOPTS_OPTIONS                                            
*                                                                               
         BRAS  RE,TRACELOG                                                      
         BRAS  RE,SENTMSG          RPT MESSAGE AS SENT                          
***********************************************************************         
* Add message info to ETI for each destination                                  
***********************************************************************         
         L     R7,ADSTAEFT     *** MARK ALL DESTINATIONS SENT                   
         USING DSTAEFTD,R7                                                      
SFX82    CLI   0(R7),EOT           EOT                                          
         JE    EXITOK                                                           
         OC    DSTAEDST,DSTAEDST   No more                                      
         JZ    EXITOK                                                           
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
         MVC   0(L'REFNUM,RF),REFNUM                                            
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
         MVC   PLINE+L'SENTRPT+L'DSTAEDST+3(L'REFNUM),REFNUM                    
         BRAS  RE,PRNT                                                          
*                                                                               
SFX90    AHI   R7,DSTAEFTL         NEXT DESTINATION                             
         B     SFX82                                                            
         DROP  R7                                                               
***********************************************************************         
* NO VALID DESTINATIONS - OUTPUT ERROR MESSAGE                                  
***********************************************************************         
SFX100   MVC   PLINE(L'CANTSND),CANTSND                                         
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R7,ADSTAEFT     *** MARK ALL DESTINATIONS JUNK                   
         USING DSTAEFTD,R7                                                      
SFX120   CLI   0(R7),EOT           EOT                                          
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
         MVC   0(L'REFNUM,RF),REFNUM                                            
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
         J     EXITOK                                                           
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
         J     EXITOK                                                           
*                                                                               
NODATA   EQU   *                                                                
         MVC   DATMUID,USERID                                                   
         MVC   DATMSUB,RPTSUBID                                                 
*                                                                               
         GOTO1 VHEXIN,DMCB,RPTPQNUM,HALF,4,0                                    
         SR    R0,R0                                                            
         ICM   R0,3,HALF                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DATMNUM,DUB                                                      
*                                                                               
         MVC   PLINE(DATMISSQ),DATMISS                                          
         BRAS  RE,PRNT                                                          
         J     EXITOK                                                           
                                                                                
TOOJREP  WTO   'EDIXRR - TOO MANY JUNK REPORTS, PLEASE RECYCLE THIS JOB+        
               .'                                                               
         DC    H'0'                                                             
*                                                                               
CANTSEND GOTO1 VLOGIO,DMCB,X'FF000001',=C'EDIXRR - Error sending to ER +        
               - Transmissions suspended, please check'                         
         ABEND 701                                                              
*                                                                               
CANTSUMY GOTO1 VLOGIO,DMCB,X'FF000001',=C'EDIXRR - Report Summary to ER+        
               - Transmissions suspended, please check'                         
         ABEND 701                                                              
*                                                                               
TOOBIG   GOTO1 VLOGIO,DMCB,X'FF000001',=C'EDIXRR - Report is too big to+        
                fit in buffer - must increase MAXMSGLEN'                        
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
DATMISS  DC    C'Empty fax, skip report: '                                      
DATMUID  DS    CL8                                                              
         DC    C','                                                             
DATMSUB  DS    CL3                                                              
         DC    C','                                                             
DATMNUM  DS    CL5                                                              
DATMISSQ EQU   *-DATMISS                                                        
*                                                                               
TSTHDRT  DC    X'444453545354'     DDSTST                                       
TSTHDRC  DC    X'444453435343'     DDSCSC                                       
TSTHDRA  DC    X'444453414456'     DDSADV                                       
TSTHDRR  DC    X'444453524550'     DDSREP                                       
TSTHDRQ  DC    X'444453465141'     DDSFQA                                       
TSTHDRF  DC    32X'20'                                                          
*                                                                               
CDATA    DC    CL9'<! CDATA '                                                   
         ORG   CDATA+2                                                          
         DC    X'BA'                                                            
         ORG   CDATA+8                                                          
         DC    X'BA'                                                            
CDATAX   DC    CL3'  >'                                                         
         ORG   CDATAX                                                           
         DC    X'BBBB'                                                          
         ORG                                                                    
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* If BXF informaton not included in Fax on PQ so need to remove                 
* PM3 from destination since this is all they want.                             
* This is here because we build the list of destination codes before            
* we know if the PQ has BXF or not.                                             
***********************************************************************         
FIXDLV   NTR1  BASE=*,LABEL=*                                                   
         L     RE,@DELVCDE                                                      
         XR    RF,RF                                                            
         ICM   RF,1,DESTLSLN                                                    
         JZ    EXIT                                                             
*                                                                               
FIXDLV10 CLC   =C'PM3',0(RE)                                                    
         JE    FIXDLV20                                                         
         AHI   RE,1                                                             
         JCT   RF,FIXDLV10                                                      
         J     EXIT                Not found, sort of strange                   
*                                                                               
FIXDLV20 MVC   0(3,RE),XSPACES                                                  
         J     EXIT                Clear PM3 out as a destination               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* Delivery destination codes                                                    
***********************************************************************         
DELIVERY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DESTLIST,DESTLIST                                                
         MVI   DESTLSLN,0                                                       
*                                                                               
         LA    RE,DESTLIST                                                      
         CLI   DESTMETS,EDIEXRRQ   Destination Extreme Reach                    
         JNE   DELV10                                                           
         MVC   0(3,RE),=C'ER,'                                                  
         AHI   RE,3                                                             
*                                                                               
DELV10   CLI   DESTPM360,YES                                                    
         JNE   DELV20                                                           
         MVC   0(4,RE),=C'PM3,'    Destination PM360                            
         AHI   RE,4                                                             
*                                                                               
DELV20   SHI   RE,1                                                             
         JNP   *+2                 Trap for now                                 
*        BNP   EXIT                                                             
         LA    RF,DESTLIST                                                      
         SR    RE,RF                                                            
         STC   RE,DESTLSLN                                                      
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* Move BXF data to MQ OUTPUT buffer                                             
***********************************************************************         
BXFXML   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
BXFXML20 BRAS  RE,GETNXTL                                                       
         BNE   BXFXML90                                                         
         LR    RF,R3               MAKE SURE WE CAN FIT MORE DATA               
         AH    RF,RLEN                                                          
         AHI   RF,32               APPROXIMATION FOR MAX SIZE                   
         S     RF,AMSGOUT                                                       
         C     RF,MAXMSGLN                                                      
         JH    *+2                 Won't fit                                    
         LR    RE,R3               RE = MOVE TO POINT                           
         LH    R1,RLEN                                                          
         AHI   R1,-1                                                            
         AR    R3,R1               POINT R3 TO END OF MOVE BLOCK                
         LA    R0,R+1              IGNORE PRINTER CC                            
         LR    RF,R1                                                            
         MVCL  RE,R0               MOVE IN DATA                                 
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
         B     BXFXML20                                                         
*                                                                               
BXFXML90 J     EXITR3                                                           
         LTORG                                                                  
                                                                                
***********************************************************************         
* BLD XML information for ER processing                               *         
***********************************************************************         
BLDXML   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AMSGOUT          Start of Message out buffer                  
*                                                                               
         GOTOR XMLTAG,XMLPARM,=C'message',STARTAG                               
         GOTOR XMLTAG,XMLPARM,=C'key',STARTAG                                   
         GOTOR XMLTAGS,XMLPARM,=C'moSystem',=C'DS'                              
         GOTOR XMLTAGS,XMLPARM,=C'hostEnvironment',(L'SYS,SYS)                  
         GOTOR XMLTAGS,XMLPARM,=C'tenant',(L'AGYALPHA,AGYALPHA)                 
         GOTOR XMLTAGS,XMLPARM,=C'client',(L'CLIENT,CLIENT)                     
         GOTOR XMLTAGS,XMLPARM,=C'hostUniqueReference',                +        
               (L'REFNUM,REFNUM)                                                
         GOTOR XMLTAGS,XMLPARM,=C'trafficEnvironment',(L'ENV,ENV)               
         GOTOR XMLTAGS,XMLPARM,=C'uuid',NODATAQ                                 
         GOTOR XMLTAG,XMLPARM,=C'key',ENDTAG                                    
         GOTOR XMLTAG,XMLPARM,=C'requestPayload',STARTAG                        
         GOTOR XMLTAG,XMLPARM,=C'deliveries',STARTAG                            
         GOTOR XMLTAGS,XMLPARM,=C'delivery-type',(L'REPTYPE,REPTYPE)            
         LA    RE,L'=C'delivery-destination'(,R3)                               
         ST    RE,@DELVCDE                                                      
         GOTOR XMLTAGS,XMLPARM,=C'delivery-destination',               +        
               (DESTLSLN,DESTLIST)                                              
         GOTOR XMLTAG,XMLPARM,=C'delivery-metadata',STARTAG                     
         GOTOR XMLTAGS,XMLPARM,=C'advertiserId',(L'CLIENT,CLIENT)               
         GOTOR XMLTAGS,XMLPARM,=C'system',(L'MEDSYS,MEDSYS)                     
         GOTOR XMLTAGS,XMLPARM,=C'media',(L'MEDIA,MEDIA)                        
         CLI   SKIPPROD,YES     Skip product XML                                
         BE    BLDXML28         Yep                                             
         GOTOR XMLTAGS,XMLPARM,=C'brandId',(L'PRODUCT,PRODUCT)                  
*                                                                               
BLDXML28 GOTOR XMLTAGS,XMLPARM,=C'userId',(L'PID,PID)                           
         GOTOR XMLTAGS,XMLPARM,=C'firstName',(L'FNAME,FNAME)                    
         GOTOR XMLTAGS,XMLPARM,=C'lastName',(L'LNAME,LNAME)                     
         GOTOR XMLTAGS,XMLPARM,=C'destinationId',(L'DESTCODE,DESTCODE)          
*&&SUB                                                                          
         CLI   SUBMEDIA,C' '                                                    
         BNH   BLDXML49                                                         
         GOTOR XMLTAGS,XMLPARM,=C'subMedia',(L'SUBMEDIA,SUBMEDIA)               
*&&                                                                             
BLDXML52 CLC   REPTYPE,=C'NWX'                                                  
         BNE   BLDXML60                                                         
         CLI   IS_SYND,YES                 Was this set?                        
         BNE   BLDXML53                                                         
         GOTOR XMLTAGS,XMLPARM,=C'isSyndicatedDestination',            +        
               (L'IS_SYND,IS_SYND)                                              
*                                                                               
BLDXML53 CLI   MEDIA,C'N'                                                       
         BNE   BLDXML60                                                         
         GOTOR XMLTAGS,XMLPARM,=C'programId',(L'PGMID,PGMID)                    
         GOTOR XMLTAGS,XMLPARM,=C'programName',(L'PGMNM,PGMNM)                  
         GOTOR XMLTAGS,XMLPARM,=C'daypartId',(L'DAYPART,DAYPART)                
         GOTOR XMLTAG,XMLPARM,=C'programDayTimes',STARTAG                       
         GOTOR XMLTAG,XMLPARM,=C'programDayTime',STARTAG                        
         GOTOR XMLTAGS,XMLPARM,=C'programDays',(L'DAYS,DAYS)                    
         GOTOR XMLTAGS,XMLPARM,=C'programTime',(L'TIME,TIME)                    
         GOTOR XMLTAG,XMLPARM,=C'programDayTime',ENDTAG                         
         GOTOR XMLTAG,XMLPARM,=C'programDayTimes',ENDTAG                        
         GOTOR XMLTAGS,XMLPARM,=C'endDate',(L'ENDDTE,ENDDTE)                    
         CLI   IS_SYND,YES                 Was this set?                        
         BNE   BLDXML60                                                         
         GOTOR XMLTAGS,XMLPARM,=C'broadcastYear',0                              
*                                                                               
BLDXML60 GOTOR XMLTAGS,XMLPARM,=C'instructionGroupId',                 +        
               (L'GROUPID,GROUPID)                                              
         GOTOR XMLTAG,XMLPARM,=C'delivery-metadata',ENDTAG                      
                                                                                
**********************************************************************          
* Make sure the XML has all printable characters.                               
* You may need to use the correct language XLATE table one day                  
**********************************************************************          
         L     RE,AMSGOUT          Start of Message out buffer                  
         LR    R1,R3               R3 = Address within MQ buffer                
         SR    R1,RE               R1 = length of data to translate             
*                                                                               
BLDXML70 CHI   R1,256                                                           
         BNH   BLDXML72                                                         
         TR    0(256,RE),CHARTAB                                                
         AHI   RE,256                                                           
         SHI   R1,256                                                           
         BP    BLDXML70            More to translate                            
         B     BLDXML90                                                         
*                                                                               
BLDXML72 BCTR  R1,0                                                             
         EX    R1,TRNSLTE          Get rid of funky characters                  
*                                                                               
BLDXML90 B     EXITR3                                                           
                                                                                
***********************************************************************         
* Remove spaces at end of value                                                 
***********************************************************************         
REMSPACE CLI   0(R3),C' '          Remove space                                 
         JH    REMSPACX                                                         
         BRCT  R3,REMSPACE                                                      
*                                                                               
REMSPACX AHI   R3,1                Back to space                                
         BR    RE                                                               
                                                                                
***********************************************************************         
* Format single XML tag                                                         
* RC = WORKD                                                                    
* P1 = tag                                                                      
* P2 = 0 start tag                                                              
*      1 end   tag                                                              
***********************************************************************         
XMLTAG   STM   RE,R2,XMLREGS                                                    
         MVI   0(R3),C'<'          Open < for XML tag                           
         AHI   R3,1                                                             
         CLI   7(R1),1             P2=0 start, P2=1 end                         
         JL    XMLS10                                                           
         JH    *+2                 Invalid call                                 
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
*                                                                               
XMLS10   LLC   RF,0(R1)            Length of tag                                
         BCTR  RF,0                                                             
         L     RE,0(,R1)                                                        
         NILH  GRE,X'00FF'                                                      
         EXRL  RF,EX_TAG                                                        
         LA    R3,1(RF,R3)         Increment R3                                 
         MVI   0(R3),C'>'          Close tag                                    
         AHI   R3,1                                                             
         LM    RE,R2,XMLREGS                                                    
         BSM   0,RE                                                             
                                                                                
***********************************************************************         
* RB is unreliable since this is relative code                                  
* RC needs to point to WORKD area                                               
* Format XML tags and field                                                     
***********************************************************************         
XMLTAGS  STM   RE,R2,XMLREGS                                                    
         MVI   0(R3),C'<'          Open < for XML tag                           
         AHI   R3,1                                                             
         LLC   RF,0(R1)            Length of tag                                
         BCTR  RF,0                                                             
         L     RE,0(,R1)                                                        
         NILH  GRE,X'00FF'                                                      
         EXRL  RF,EX_TAG                                                        
         LA    R3,1(RF,R3)         Increment R3                                 
         MVI   0(R3),C'>'          Close tag                                    
         AHI   R3,1                                                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,4(R1)          Length of field                              
         JZ    XMLT62              No length then just end XML                  
         BCTR  RF,0                                                             
         L     RE,4(,R1)                                                        
         NILH  GRE,X'00FF'         Clear HOB                                    
*                                                                               
         LARL  R2,XMLTRTAB         R1 will point to masked charater             
         EXRL  RF,TRTXML           Replace &, <, > with control char            
         JZ    XMLT50              Good as is                                   
         AHI   RF,1                                                             
*                                                                               
XMLT05   CLI   0(RE),C'&&'         found '&' so replace with '&amp'             
         JNE   XMLT10                                                           
         MVI   0(R3),C'&&'                                                      
         MVI   1(R3),C'a'                                                       
         MVI   2(R3),C'm'                                                       
         MVI   3(R3),C'p'                                                       
         MVI   4(R3),X'5E'         Semicolon                                    
         AHI   R3,5                                                             
         J     XMLT40                                                           
*                                                                               
XMLT10   CLI   0(RE),C'<'         found '<' so replace with '&lt'               
         JNE   XMLT20                                                           
         MVI   0(R3),C'&&'                                                      
         MVI   1(R3),C'l'                                                       
         MVI   2(R3),C't'                                                       
         MVI   3(R3),X'5E'         Semicolon                                    
         AHI   R3,4                                                             
         J     XMLT40                                                           
*                                                                               
XMLT20   CLI   0(RE),C'>'         found '>' so replace with '&gt'               
         JNE   XMLT30                                                           
         MVI   0(R3),C'&&'                                                      
         MVI   1(R3),C'g'                                                       
         MVI   2(R3),C't'                                                       
         MVI   3(R3),X'5E'         Semicolon                                    
         AHI   R3,4                                                             
         J     XMLT40                                                           
*                                                                               
XMLT30   MVC   0(1,R3),0(RE)       Move one character at a time                 
         AHI   R3,1                                                             
*                                                                               
XMLT40   AHI   RE,1                                                             
         JCT   RF,XMLT05           Next character                               
         J     XMLT60                                                           
*                                                                               
XMLT50   DS    0H                                                               
*        LLC   RF,4(R1)            Length of field                              
*        BCTR  RF,0                                                             
         EXRL  RF,EX_XML                                                        
         LA    R3,1(RF,R3)                                                      
*                                                                               
XMLT60   BRAS  RE,REMSPACE                                                      
         LM    RE,R2,XMLREGS       Reload regs                                  
XMLT62   MVI   0(R3),C'<'          End XML tag                                  
         MVI   1(R3),C'/'                                                       
         AHI   R3,2                                                             
         L     RE,0(,R1)           Reload Tag address                           
         NILH  GRE,X'00FF'                                                      
         LLC   RF,0(,R1)           Tag length                                   
         BCTR  RF,0                                                             
         EXRL  RF,EX_TAG           Move in tag                                  
         LA    R3,1(RF,R3)         Increment R3                                 
         MVI   0(R3),C'>'          Close tag                                    
         AHI   R3,1                                                             
*                                                                               
         LM    RE,R2,XMLREGS                                                    
         BSM   0,RE                                                             
         LTORG                                                                  
*                                                                               
STARTAG  EQU   0                                                                
ENDTAG   EQU   1                                                                
NODATAQ  EQU   0                                                                
TRNSLTE  TR    0(0,RE),CHARTAB                                                  
TRTXML   TRT   0(0,RE),0(R2)                                                    
EX_TAG   DS    0H                                                               
EX_XML   MVC   0(0,R3),0(RE)                                                    
         DROP  RB                                                               
***********************************************************************         
* CLEAR ALL FIELDS BEFORE STARTING PROCESSING FAX                     *         
***********************************************************************         
CLRFLDS  NTR1  BASE=*,LABEL=*                                                   
         MVI   GOTHDR,NO                                                        
         MVI   GOTDATA,NO                                                       
         XC    REQUESTR,REQUESTR                                                
         XC    LASTOUT,LASTOUT                                                  
         XC    FAXSUBC,FAXSUBC                                                  
         XC    NUMLINES,NUMLINES                                                
         XC    COVPGLNS,COVPGLNS   NO COVER PAGE YET                            
         XC    XRRDATA,XRRDATA                                                  
         MVC   NUMPAGES,=H'1'      AT LEAST ONE PAGE WILL BE SENT               
         XC    LSTSDST,LSTSDST     ASSUME NO REPORT IS SENDABLE                 
         XC    RPTLDSTS,RPTLDSTS                                                
         XC    HDR,HDR                                                          
*                                                                               
         L     R2,ALINETAB         CLEAR OUT THE ++DDS LINES                    
         USING LINETABD,R2                                                      
CFL02    XC    LINELEN,LINELEN                                                  
         MVC   LINEIN,XSPACES                                                   
         AHI   R2,LINETABL                                                      
         CLI   LINEID,X'FF'                                                     
         BNE   CFL02                                                            
*                                                                               
         L     R0,ADSTAEFT         CLEAR OUT DESTINATION TABLE                  
         LHI   R1,DSTAEFTL*DSTMAXQ                                              
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITOK                                                           
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* Extreem Reach XML Summary report                                              
* Build list of Unique Fax ids to send ER a report                    *         
***********************************************************************         
XRRRPT   NTR1  BASE=*,LABEL=*                                                   
         STC   R1,XMLMODE                                                       
         SAM31                                                                  
         CLI   XMLMODE,1           Initialize                                   
         BNE   XRRRPT20                                                         
         OC    @REFBUFF,@REFBUFF                                                
         BNZ   XRRRPT10                                                         
         L     R3,MAXREFLN         Build area for references                    
         LR    R4,R3                                                            
*                                                                               
         STORAGE OBTAIN,LENGTH=(3),LOC=ANY,BNDRY=PAGE                           
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         MVC   0(16,R1),=C'*XREF*XREF*XREF*'                                    
         LA    R1,16(R1)           Bump past label, counter  MAX#               
         ST    R1,@REFBUFF         A(Start of reference buffer)                 
         L     R4,MAXREFLN                                                      
         SHI   R4,16               Less eye catcher                             
         ST    R4,REFBUFSZ         Size of Reference buffer                     
         AR    R1,R4                                                            
         ST    R1,@REFEND          A(End of reference buffer)                   
*                                                                               
XRRRPT10 L     RE,@REFBUFF         Clear for next report                        
         L     RF,REFBUFSZ                                                      
         XCEFL                                                                  
         XC    GROUPID,GROUPID                                                  
         XC    REFCOUNT,REFCOUNT                                                
         J     EXITOK                                                           
*                                                                               
XRRRPT20 L     R0,AMSGOUT          CLEARING LIKE THIS IS LIKELY                 
         L     R1,MAXMSGLN         OVERKILL                                     
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    LASTOUT,LASTOUT                                                  
*                                                                               
         LA    R2,REFNUM           Need to give summary report a                
         USING XREFKEYD,R2         host Unique reference. Add one to            
         L     RE,@REFBUFF         the last refrence                            
         AHI   RE,L'GROUPID                                                     
         CLC   XREFVER#(XREFLOG#-XREFKEYD),0(RE)                                
         JNE   *+2                                                              
         XR    R1,R1                                                            
         ICM   R1,3,REFCOUNT                                                    
         AHI   R1,1                                                             
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XREFLOG#,DUB                                                     
         DROP  R2                                                               
*                                                                               
         L     R3,AMSGOUT          INPUT MESSAGE BUFFER                         
         GOTOR XMLTAG,XMLPARM,=C'message',STARTAG                               
         GOTOR XMLTAG,XMLPARM,=C'key',STARTAG                                   
         GOTOR XMLTAGS,XMLPARM,=C'moSystem',=C'DS'                              
         GOTOR XMLTAGS,XMLPARM,=C'hostEnvironment',(L'SYS,SYS)                  
         GOTOR XMLTAGS,XMLPARM,=C'tenant',(L'AGYALPHA,AGYALPHA)                 
         GOTOR XMLTAGS,XMLPARM,=C'client',(L'CLIENT,CLIENT)                     
*        GOTOR XMLTAGS,XMLPARM,=C'hostUniqueReference',NODATAQ                  
         GOTOR XMLTAGS,XMLPARM,=C'hostUniqueReference',                +        
               (L'REFNUM,REFNUM)                                                
         GOTOR XMLTAGS,XMLPARM,=C'trafficEnvironment',(L'ENV,ENV)               
         GOTOR XMLTAGS,XMLPARM,=C'uuid',NODATAQ                                 
         GOTOR XMLTAG,XMLPARM,=C'key',ENDTAG                                    
         GOTOR XMLTAG,XMLPARM,=C'requestPayload',STARTAG                        
         GOTOR XMLTAG,XMLPARM,=C'report',STARTAG                                
         GOTOR XMLTAGS,XMLPARM,=C'reportType',=C'instructions'                  
         GOTOR XMLTAGS,XMLPARM,=C'delivery-destination',=C'ER'                  
         GOTOR XMLTAGS,XMLPARM,=C'groupId',(L'GROUPID,GROUPID)                  
         GOTOR XMLTAG,XMLPARM,=C'groupList',STARTAG                             
*                                                                               
         LLH   R0,REFCOUNT                                                      
         L     R2,@REFBUFF                                                      
         AHI   R2,L'GROUPID                                                     
XRRRPT22 MVC   REFNUM,0(R2)                                                     
         GOTOR XMLTAGS,XMLPARM,=C'groupElementId',(L'REFNUM,REFNUM)             
         AHI   R2,XREFLNQ                                                       
         BCT   R0,XRRRPT22                                                      
*                                                                               
         GOTOR XMLTAG,XMLPARM,=C'groupList',ENDTAG                              
         GOTOR XMLTAG,XMLPARM,=C'report',ENDTAG                                 
         GOTOR XMLTAG,XMLPARM,=C'requestPayload',ENDTAG                         
         GOTOR XMLTAG,XMLPARM,=C'message',ENDTAG                                
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
         B     EXITR3                                                           
         LTORG                                                                  
                                                                                
***********************************************************************         
*  Gather information about the requestor of report                             
***********************************************************************         
PERINFO  NTR1  BASE=*,LABEL=*                                                   
         MVC   EMAIL,XSPACES                                                    
         MVC   PID,XSPACES                                                      
         MVC   FNAME,XSPACES                                                    
         MVI   FNAMELN,0                                                        
         MVC   LNAME,XSPACES                                                    
         MVI   LNAMELN,0                                                        
         MVC   USERNME,XSPACES                                                  
         MVI   USERNLN,0                                                        
*                                                                               
         LA    R4,IOKEY                                                         
         USING CTIREC,R4                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKNUM,PQUSER#                                                  
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',IOKEY,AIO,0                 
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         L     R4,AIO                                                           
         LA    R3,CTIDATA                                                       
         DROP  R4                                                               
*                                                                               
         USING CTDSCD,R3                                                        
PERIN02  CLI   0(R3),0             EOR                                          
         BE    PERIN08                                                          
         CLI   CTDSCEL,CTDSCELQ    X'02'                                        
         BE    PERIN04                                                          
         LLC   RF,CTDSCLEN                                                      
         AR    R3,RF                                                            
         B     PERIN02                                                          
*                                                                               
MVCUNME  MVC   USERNME,CTDSC                                                    
*                                                                               
PERIN04  LLC   RF,CTDSCLEN         Save off User id name                        
         SHI   RF,(CTDSC-CTDSCD)+1                                              
         EX    RF,MVCUNME          Save off User id name                        
         LA    RE,USERNME(RF)      Point to end of data                         
PERIN05  CLI   0(RE),C' '                                                       
         BH    PERIN06                                                          
         SHI   RF,1                                                             
         JNP   *+2                                                              
         BCT   RE,PERIN05                                                       
         DC    H'00'                                                            
         DROP  R3                                                               
*                                                                               
PERIN06  AHI   RF,1                                                             
         STC   RF,USERNLN          Length of user name                          
*                                                                               
                                                                                
PERIN08  LA    R4,IOKEY                                                         
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECAGY                                                   
         MVC   SA0KNUM,PQPID#                                                   
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',IOKEY,AIO,0                 
         CLI   8(R1),0                                                          
         JNE   PERIER03                                                         
         L     R4,AIO                                                           
         LA    R3,SA0DATA                                                       
         DROP  R4                                                               
*                                                                               
         USING SAPALD,R3                                                        
PERIN10  CLI   0(R3),0             EOR                                          
         JE    PERINFX                                                          
         CLI   SAPALEL,SAPALELQ    X'C3'                                        
         BE    PERIN20                                                          
         LLC   RF,SAPALLN                                                       
         AR    R3,RF                                                            
         B     PERIN10                                                          
*                                                                               
PERIN20  MVC   PID,SAPALPID        Save off PID                                 
         LA    RE,PID+L'PID-1                                                   
         LA    RF,L'PID-1                                                       
PERIN21  CLI   0(RE),C' '                                                       
         BH    PERIN21A                                                         
         SHI   RF,1                                                             
         JNP   PERIER04                                                         
         BCT   RE,PERIN21                                                       
         DC    H'00'                                                            
         DROP  R3                                                               
*                                                                               
PERIN21A AHI   RF,1                                                             
         STC   RF,PIDLN            Save length of PID                           
*                                                                               
         LA    R4,IOKEY                                                         
         USING SAPEREC,R4                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ    C'F'                                         
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECAGY      Security Agency                              
         MVC   SAPEPID,PID         PID                                          
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',IOKEY,AIO,0                 
         CLI   8(R1),0                                                          
         JNE   PERIER05                                                         
         L     R4,AIO                                                           
         LA    R3,SAPEDATA         Point to first element                       
         DROP  R4                                                               
*                                                                               
PERIN22  CLI   0(R3),0             EOR                                          
         BE    PERINFX             Done                                         
         CLI   0(R3),SANAMELQ      Name element  (X'C5')                        
         BE    PERIN25                                                          
         CLI   0(R3),SAPEEELQ      eMail element (X'E5')                        
         BE    PERIN30                                                          
*                                                                               
PERIN24  LLC   RF,1(,R3)           Next element                                 
         AR    R3,RF                                                            
         B     PERIN22                                                          
*                                                                               
         USING SANAMD,R3                                                        
PERIN25  LA    R2,SANAMES                                                       
         TM    SANAMIND,SANAMIFN   First name                                   
         BZ    PERIN26                                                          
         LLC   RF,0(,R2)           Get length of name                           
         STC   RF,FNAMELN          Save length of name                          
         SHI   RF,1                                                             
         EX    RF,MVCFNAME                                                      
         LA    R2,2(RF,R2)                                                      
*                                                                               
PERIN26  TM    SANAMIND,SANAMIMN   Middle name                                  
         BZ    PERIN28                                                          
         LLC   RF,0(,R2)           next name                                    
         LA    R2,1(RF,R2)                                                      
*                                                                               
PERIN28  TM    SANAMIND,SANAMILN   Last name                                    
         BZ    PERIN24                                                          
         LLC   RF,0(,R2)           next name                                    
         STC   RF,LNAMELN          Save length of name                          
         SHI   RF,1                                                             
         EX    RF,MVCLNAME                                                      
         B     PERIN24                                                          
         DROP  R3                                                               
*                                                                               
         USING SAPEED,R3                                                        
MVCEMAIL MVC   EMAIL(0),SAPEEID                                                 
*                                                                               
PERIN30  LLC   RF,SAPEELN                                                       
         SHI   RF,SAPEELNQ+1                                                    
         EX    RF,MVCEMAIL                                                      
         AHI   RF,1                                                             
         STC   RF,EMAILLN          Save length of email                         
         B     PERIN24                                                          
         DROP  R3                                                               
*                                                                               
PERIER03 MVI   ERROR,22                                                         
         B     PERINFX                                                          
PERIER04 MVI   ERROR,23                                                         
         B     PERINFX                                                          
PERIER05 MVI   ERROR,24                                                         
         B     PERINFX                                                          
*                                                                               
PERINFX  CLI   ERROR,0                                                          
         J     EXITOK                                                           
*                                                                               
MVCFNAME MVC   FNAME(0),1(R2)                                                   
MVCLNAME MVC   LNAME(0),1(R2)                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT LINE INTO R AND RLEN                                       *         
* NTRY: R4 = CURRENT BUFFER POINTER                                   *         
*       R3 = A(OUTPUT BUFFER) AFTER GETTING THE HDR                   *         
***********************************************************************         
GETNXTL  NTR1  BASE=*,LABEL=*                                                   
GNL10    XC    RLEN,RLEN           WE SET LENGTH OF THIS LINE HERE              
         MVC   R,XSPACES           WE MOVE INPUT A LINE AT A TIME HERE          
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
         CLI   EDIPAGE,EDIPAGEQ    ALREADY PAGE BREAK BY CC=X'89'?              
         BE    GNL10               YES - SKIP THIS /PAGE                        
*                                                                               
         CLI   GOTHDR,YES          TEST ALREADY HAVE HEADER                     
         BNE   GNL20               NO - CONTINUE                                
*                                  R3=A(OUTPUT BUFFER)                          
         AHI   R3,-9                                                            
         CLC   =X'61E6C9C4C50D25',0(R3)  /WIDE(CRLF)?                           
         BE    GNL10               YES - SKIP THIS 1ST /PAGE                    
*                                                                               
GNL20    TR    R+1(L'R-1),TRTAB    TRANSLATE OUT UNPRINTABLES                   
*                                                                               
         LHI   R0,81                                                            
         CLI   EDIWIDE,EDIWIDEQ    WIDE REPORT?                                 
         BNE   *+8                                                              
         LHI   R0,133                                                           
         CLI   EDIWIDE,EDILANDQ    LANDSCAPE REPORT?                            
         BNE   *+8                                                              
         LHI   R0,133                                                           
         CH    R0,RLEN                                                          
         BH    *+8                                                              
         STH   R0,RLEN                                                          
*                                                                               
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R4)                                                        
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS *HDR* CARD - THIS SHOULD BE THE FIRST CARD                  *         
***********************************************************************         
         USING DSTAEFTD,R7                                                      
PRCHDR   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'*HDR*',R+5       IS THIS THE HEADER                           
         BNE   EXITL               NO - LOOP UNTIL YOU GET IT                   
         MVI   GOTHDR,YES                                                       
         LH    R1,RLEN             SAVE HEADER STRIPPING OFF THE CC             
         AHI   R1,-1                                                            
         STH   R1,HDRLEN                                                        
         LA    R0,HDR                                                           
         LA    RE,R+1                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLC   =C'EDICT=',EDIEDICT OVERRIDE FOR KEY OF RECORD?                  
         BNE   *+10                NO                                           
         MVC   USERID,EDIOVKEY                                                  
         MVC   DSTAEDST,EDIDESID   SET FIRST DESTINATION                        
         MVC   DSTAEFRM,EDIFDEST   SET FORMATTED DESTINATION                    
         AHI   R7,DSTAEFTL         GO TO NEXT                                   
*                                                                               
         CLI   EDIWIDE,EDILANDQ    TREAT LANDSCAPE/WIDE THE SAME                
         BNE   *+8                   UNTIL XRR IS READY                         
         MVI   EDIWIDE,EDIWIDEQ                                                 
*                                                                               
         B     EXITR7              CONTINUE                                     
         DROP  R7                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS TRN  CARD                                                   *         
* Should build a formal DSECT for TRN card                                      
***********************************************************************         
         USING TRNCARDD,TRN                                                     
PRCTRN   NTR1  BASE=*,LABEL=*                                                   
         XC    SYSTEM,SYSTEM                                                    
         XC    MEDSYS,MEDSYS       Media system                                 
         XC    MEDIA,MEDIA                                                      
         XC    CLIENT,CLIENT                                                    
         XC    PRODUCT,PRODUCT                                                  
         XC    DAYS,DAYS                                                        
         XC    TIME,TIME                                                        
         XC    DAYPART,DAYPART                                                  
         MVC   DESTCODE,XSPACES                                                 
         MVI   IS_CABLE,NO                                                      
         MVI   IS_SYND,NO          Default                                      
         MVI   HASBXF,NO                                                        
         MVI   SKIPPROD,NO                                                      
         MVI   SUBMEDIA,C' '                                                    
         XC    PGMID,PGMID                                                      
         XC    PGMNM,PGMNM                                                      
         XC    ENDDTE,ENDDTE                                                    
*                                                                               
         CLI   HDR+67,C'D'         DARE?                                        
         BE    PRCTRN05            Yes so skip initialization                   
         MVC   SYSTEM,@EDISYST     Seems to be consistant location              
         MVC   MEDSYS,=CL5'NET'                                                 
         CLI   SYSTEM,C'N'                                                      
         BE    PRCTRN04                                                         
         MVC   MEDSYS,=CL5'SPOT'                                                
         CLI   SYSTEM,C'S'                                                      
         BE    PRCTRN04                                                         
         MVC   MEDSYS,=CL5'PRINT'                                               
         CLI   SYSTEM,C'P'                                                      
         BE    PRCTRN04                                                         
         MVC   MEDSYS,=CL5'UNKN'                                                
*                                                                               
PRCTRN04 MVC   REPTCODE,@EDIPROG   Report code / type                           
*        MVC   MEDIA,HDR+54        Media                                        
*        MVC   CLIENT,HDR+55       Client                                       
                                                                                
PRCTRN05 OC    TRNLEN,TRNLEN       Any TRN card?                                
         BZ    EXITL               No                                           
*                                                                               
         CLC   =C'TRN',TRN+11      IS THIS THE TRN card?                        
         JNE   *+2                 NO - what's up with that                     
*                                                                               
         CLI   HDR+67,C'D'         IS THIS A DARE FAX?                          
         BNE   *+10                NO                                           
         MVC   REQUESTR,TRN+35                                                  
*                                                                               
         MVC   REPTYPE,=C'AWX'                                                  
*        MVC   REPTNAME,=CL25'auto instructions'                                
         CLC   =C'AWX',RPTSUBID                                                 
         BE    PRCTRN10                                                         
         MVC   REPTYPE,=C'LET'                                                  
*        MVC   REPTNAME,=CL25'fax letters'                                      
         CLC   =C'LET',RPTSUBID                                                 
         BE    PRCTRN10                                                         
         MVC   REPTYPE,=C'SWX'                                                  
*        MVC   REPTNAME,=CL25'spot instructions'                                
         CLC   =C'SWX',RPTSUBID                                                 
         BE    PRCTRN10                                                         
         MVC   REPTYPE,=C'MWX'                                                  
*        MVC   REPTNAME,=CL25'missing invoices'                                 
         CLC   =C'VNV',REPTCODE                                                 
         BE    PRCTRN15                                                         
*        MVC   REPTNAME,=CL25'ams instructions'                                 
         CLC   =C'MWX',RPTSUBID    (advanced media systems)                     
         BE    PRCTRN20                                                         
         MVC   REPTYPE,=C'NWX'                                                  
*        MVC   REPTNAME,=CL25'net instructions'                                 
         CLC   =C'NWX',RPTSUBID                                                 
         BE    PRCTRN32                                                         
         MVC   REPTYPE,=C'CWX'                                                  
*        MVC   REPTNAME,=CL25'cable instructions'                               
         CLC   =C'CWX',RPTSUBID                                                 
         BE    PRCTRN30                                                         
         MVC   REPTYPE,=C'PWX'                                                  
*        MVC   REPTNAME,=CL25'pattern instructions'                             
         CLC   =C'PWX',RPTSUBID                                                 
         BE    PRCTRN30                                                         
         MVC   REPTYPE,=C'SHP'                                                  
*        MVC   REPTNAME,=CL25'shipping list'                                    
         CLC   =C'SHP',RPTSUBID                                                 
         BE    PRCTRN40                                                         
         MVC   REPTYPE,=C'YRY'                                                  
*        MVC   REPTNAME,=CL25'Station rotation schedule'                        
         CLC   =C'YRY',REPTCODE                                                 
         BE    PRCTRN50                                                         
         MVC   REPTYPE,=C'RRX'                                                  
*        MVC   REPTNAME,=CL25'Network rotation schedule'                        
         CLC   =C'RRX',REPTCODE                                                 
         BE    PRCTRN60                                                         
         MVC   REPTYPE,=C'COV'                                                  
*        MVC   REPTNAME,=CL25'cover letter'                                     
         CLC   =C'COV',RPTSUBID                                                 
         BE    PRCTRN70                                                         
         MVC   REPTYPE,=C'WIO'                                                  
*        MVC   REPTNAME,=CL25'Web insertion orders'                             
         CLC   =C'WIO',RPTSUBID                                                 
         BE    PRCTRN80                                                         
         MVC   REPTYPE,=C'XDX'                                                  
*        MVC   REPTNAME,=CL25'Confirmations of Purchase'                        
         CLC   =C'XDX',REPTCODE                                                 
         BE    PRCTRN90                                                         
         MVC   REPTYPE,=C'UNK'                                                  
*        MVC   REPTNAME,=CL25'Unknown'                                          
         B     PRCTRNX             Generic for now                              
***********************************************************************         
* AWX, SWX, LET                                                                 
***********************************************************************         
PRCTRN10 MVC   MEDIA,@EDISTTMD     Media                                        
         MVC   CLIENT,@EDISTTCL    Client                                       
         MVC   PRODUCT,@EDISTTPR   Product                                      
         MVC   EST,@EDISTTES       Estimate                                     
         MVC   DESTCODE(L'@EDISTTST),@EDISTTST  Station                         
         BRAS  RE,FIXDEST                                                       
         B     PRCTRNX                                                          
***********************************************************************         
* VNV (SNV)                                                                     
***********************************************************************         
PRCTRN15 MVC   MEDIA,@SPNVMED      Media                                        
         MVC   CLIENT,@SPNVCLT     Client                                       
*        MVC   PRODUCT,@SPNVPRD    Product                                      
         MVC   DESTCODE(5),@SPNVSTA   Station                                   
         CLI   DESTCODE+4,C'-'                                                  
         BNE   *+10                                                             
         MVC   DESTCODE+4(1),@SPNVSTA+5                                         
         BRAS  RE,FIXDEST                                                       
         B     PRCTRNX                                                          
***********************************************************************         
* MWX                                                                           
***********************************************************************         
PRCTRN20 MVC   MEDIA,@EDISTCMD     Media                                        
         MVC   CLIENT,@EDISTCCL    Client                                       
         MVC   PRODUCT,@EDISTCPR   Product                                      
         MVC   EST,@EDISTCES       Estimate                                     
         MVC   DESTCODE(5),HDR+9   Station                                      
         BRAS  RE,FIXDEST                                                       
         B     PRCTRNX                                                          
***********************************************************************         
* CWX, PWX                                                                      
***********************************************************************         
PRCTRN30 MVC   MEDIA,@EDINTCMD     Media                                        
         MVC   CLIENT,@EDINTCCL    Client                                       
         MVC   PRODUCT,@EDINTCPR   Product                                      
         MVC   DESTCODE(L'@EDINTNET),@EDINTNET                                  
         B     PRCTRNX                                                          
***********************************************************************         
* NWX                                                                           
***********************************************************************         
PRCTRN32 MVC   MEDIA,@EDINTNMD     Media                                        
         MVC   CLIENT,@EDINTNCL    Client                                       
         MVC   PRODUCT,@EDINTNPR   Product                                      
         MVC   DESTCODE(L'@EDINTNET),@EDINTNET                                  
         MVC   SUBMEDIA,@EDINTNSM                                               
         MVC   DAYPART,@EDINTNDP   Day part id                                  
         MVC   TIME,@EDINTPTM      program time                                 
***********************                                                         
* X'40' = Monday                                                                
* X'20' = Tuesday                                                               
* X'10' = Wednesday                                                             
* X'08' = Thursday                                                              
* X'04' = Friday                                                                
* X'02' = Saturday                                                              
* X'01' = Sunday                                                                
***********************                                                         
         MVC   DAYS,=C'NNNNNNN'    MTWTFSS (Days of the week)                   
         GOTOR =V(HEXIN),DMCB,@EDINTPDY,BYTE1,2,0                               
         LA    R0,7                R0 - Number of days in a week                
         LA    R1,X'40'            Bit mask, start with Monday                  
         LA    RE,DAYS                                                          
PRCTRN34 EX    R1,WEEKDAYS         Is this day set on?                          
         BZ    *+8                 No                                           
         MVI   0(RE),YES           Set day as a yes                             
         AHI   RE,1                Move to next day                             
         SRL   R1,1                Shift for next day                           
         BCT   R0,PRCTRN34                                                      
*                                                                               
PRCTRN39 CLI   @EDINTNSM,C'S'      Syndicatation                                
*        BNE   PRCTRNX                                                          
         BNE   *+8                 Nov22. Questionable change                   
         MVI   IS_SYND,YES                                                      
         MVC   PGMID,@EDINTNPG                                                  
*        MVC   PGMNM,TBD           Not going to be supported                    
         CLC   @EDINTNPD,SPACES                                                 
         BL    PRCTRNX                                                          
         GOTOR VDATCON,DMCB,(0,@EDINTNPD),(23,ENDDTE)                           
         B     PRCTRNX                                                          
***********************************************************************         
* SHP                                                                           
***********************************************************************         
PRCTRN40 MVC   MEDIA,@EDISTSMD     Media                                        
         MVC   CLIENT,@EDISTSCL    Client                                       
         MVC   PRODUCT,@EDISTSPR   Product                                      
         MVC   DESTCODE(L'@EDISTCHS),@EDISTCHS   SHP (Production house)         
         B     PRCTRNX                                                          
***********************************************************************         
* Station Rotation schedule                                                     
***********************************************************************         
PRCTRN50 MVC   MEDIA,@SPCPMED      Media                                        
         MVC   CLIENT,@SPCPCLT     Client                                       
         MVC   PRODUCT,@SPCPPRD    Product                                      
         MVC   EST,@SPCPEST        Estimate                                     
         MVC   DESTCODE(5),@SPCPSTA                                             
         B     PRCTRNX                                                          
***********************************************************************         
* Network Rotation schedule                                                     
***********************************************************************         
PRCTRN60 MVC   MEDIA,@SPCPMED      Media                                        
         MVC   CLIENT,@SPCPCLT     Client                                       
         MVC   PRODUCT,@SPCPPRD    Product                                      
         MVC   EST,@SPCPEST        Est                                          
         MVC   DESTCODE(5),@SPCPSTA Station                                     
         B     PRCTRNX                                                          
***********************************************************************         
* COV                                                                           
***********************************************************************         
PRCTRN70 MVC   MEDIA,@EDISTCMD     Media                                        
         MVC   CLIENT,@EDISTCCL    Client                                       
         MVC   PRODUCT,@EDISTCPR   Product                                      
         MVC   EST,@EDISTCES       Est                                          
         MVC   DESTCODE(L'@EDISTCHS),@EDISTCHS  Production House code           
         B     PRCTRNX                                                          
***********************************************************************         
* WIO                                                                           
***********************************************************************         
PRCTRN80 MVC   MEDIA,EDISTPMD      Media                                        
         MVC   CLIENT,EDISTPCL     Client                                       
         MVC   PRODUCT,EDISTPPR    Product                                      
         MVC   DESTCODE(L'EDISTPUB),EDISTPUB    Publisher code                  
         B     PRCTRNX                                                          
***********************************************************************         
* Confirmation of purchase                                                      
***********************************************************************         
PRCTRN90 CLI   @SPCPTYPE,@SPCPDATQ "C" COP                                      
         BNE   PRCTRNX                                                          
         MVC   MEDIA,@SPCPMED      Media                                        
         MVC   CLIENT,@SPCPCLT     Client                                       
         MVC   PRODUCT,@SPCPPRD    Product                                      
         MVC   EST,@SPCPEST        Estimate                                     
         MVC   DESTCODE(5),@SPCPSTA Station                                     
         B     PRCTRNX                                                          
*                                                                               
PRCTRNX  CLI   SYSTEM,C'S'         SPOT?                                        
         BNE   EXITOK                                                           
         CLC   =C'POL',PRODUCT                                                  
         BNE   EXITOK                                                           
         MVI   SKIPPROD,YES                                                     
         J     EXITOK              CONTINUE                                     
                                                                                
***********************************************************************         
* Fix destination code                                                          
***********************************************************************         
FIXDEST  CLI   MEDIA,C'T'                                                       
         BNE   FIXDESTX                                                         
         CLI   DESTCODE+4,C'T'                                                  
         BE    FIXDEST8                                                         
         CLC   =C'VNV',REPTCODE                                                 
         BNE   FIXDEST2                                                         
         CLI   DESTCODE,C'0'       Is it a number?                              
         BL    FIXDESTX            No, so leave it alone                        
         B     FIXDEST8                                                         
*                                                                               
FIXDEST2 CLI   DESTCODE+0,C'T'                                                  
         BNE   FIXDESTX                                                         
         CLI   DESTCODE+1,C'0'     Is it a number?                              
         BL    FIXDESTX            No, so leave it alone                        
         MVC   DESTCODE(4),DESTCODE+1  Shit left                                
*                                                                               
FIXDEST8 DS    0H                                                               
         MVI   DESTCODE+4,C' '     Clear last character                         
FIXDESTX BR    RE                                                               
                                                                                
WEEKDAYS TM    BYTE1,0                                                          
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ++DDS CARD(S)                                               *         
***********************************************************************         
         USING DSTAEFTD,R7                                                      
PRCDDS   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'DST',R+12        WE CAN HAVE MULTIPLE DST CARDS               
         BNE   PDDS02              THEY GET STACKED FOR USE LATER               
*********************************************************************           
* ER was supposed to handle the fax as is. This turned out to be                
* not true so we are limited to just the initial fax for now.                   
* Would need to split out mulitple destinations.                                
* Only place I found this was in Print systems, Srearched 'DST'.                
* See PPESR20, PPWIO20 <-- this may crop up.                                    
* Put PQ report on hold and continue. Warn client                               
*                                                                               
* May be easier to fix in EDIPQSCAN by sending multiple MQ's                    
* messages to EDIXRR.                                                           
*                                                                               
* Looks like R+42 for 'DST' is just a fax number.                               
*                                                                               
*********************************************************************           
         WTO   'Warnig EDIXRR does not support DST card'                        
         ABEND 911,DUMP                                                         
*                                                                               
         MVC   DSTAEDST,R+16       DESTINATION                                  
         MVC   DSTAEFRM,R+42       SET FORMATTED DESTINATION                    
         AHI   R7,DSTAEFTL         GO TO NEXT                                   
         CLI   0(R7),EOT           REACHED EOT?                                 
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
         CLI   LINEID,EOT          EOT?                                         
         BNE   PDDS04              NO - NEXT CARD                               
*                                                                               
         MVI   ERROR,20            UNKNOWN ++DDS CARD                           
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
         J     EXITOK              GET NEXT CARD                                
         LTORG                                                                  
         DROP  R2,R7                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT DRTEST HEADER ON FAX                                         *         
***********************************************************************         
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
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ADD COVERSHEET TO OUTPUT BUFFER                                     *         
***********************************************************************         
PRCCVR   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACOVSHT                                                       
         LH    R0,COVPGLNS                                                      
PCVR02   CLC   XSPACES(132),0(R2)  BLANK LINE                                   
         BE    PCVR04                                                           
*                                                                               
         MVC   00(132,R3),0(R2)    MOVE IN THIS LINE                            
         AHI   R3,131                                                           
         BRAS  RE,REMSPACE                                                      
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
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
****************************************************************                
*  Print out DATE/TIME information                                              
****************************************************************                
DATETIME NTR1  BASE=*,LABEL=*                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   HHMMSS,PRNTTIME                                                  
         GOTO1 VDATCON,DMCB,(5,1),(25,REALBDT)                                  
         GOTO1 VDATCON,DMCB,(3,REALBDT),(23,YMD)                                
         GOTO1 VDATCON,DMCB,(3,REALBDT),(20,YYYYMMDD)                           
*                                                                               
         LLC   R1,REALBHH          Binary hours                                 
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  HH,DUB                                                           
*                                                                               
         LLC   R1,REALBMM          Binary minutes                               
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MM,DUB                                                           
*                                                                               
         LLC   R1,REALBSS          Binary seconds                               
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SS,DUB                                                           
         J     EXITOK                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT 'FAX SENT' MESSAGE TO CONSOLE                     *         
***********************************************************************         
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
         MVC   PLINE+19(L'REFNUM),REFNUM                                        
         BRAS  RE,PRNT                                                          
         J     EXITOK                                                           
*                                                                               
FAXSNT   DC    C'Fax sent to XRR '                                              
LINESNT  DC    C' lines in '                                                    
PAGESNT  DC    C' pages using MQ buffer'                                        
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WAIT UNTIL POSTED OR A MESSAGE ARRIVES                   *         
***********************************************************************         
WAIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
WAIT02   XC    TIMERECB,TIMERECB   CLEAR TIMER ECB                              
         XC    GETECB,GETECB       CLEAR SIGNAL ECB                             
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
         JE    EXITOK              YOU DON'T NEED TO WAIT                       
         BH    WAIT04              SIGNAL ACCEPTED FOR MQGET, WAIT              
*                                  ERROR ON GET,                                
         CLC   MQ_RC,=A(MQRC_SIGNAL_OUTSTANDING)                                
         JE    WAIT04              ALREADY SET UP TO SIGNAL                     
*                                  ERROR ON GET,                                
         CLC   MQ_RC,=A(MQRC_CONNECTION_BROKEN)                                 
         JNE   WAIT03              DIE=BROKEN CONNECTION, RECYCLE               
*                                  OTHERWISE,                                   
         BRAS  RE,RECON            TRY RECONNECT                                
*                                  OTHERWISE,                                   
WAIT03   BRAS  RE,SETTIMER               SET TIMER, RETRY LATER                 
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
         CLI   OPERSTOP,YES        OPERATOR STOP REQUESTED?                     
         JE    EXITL               YES                                          
*                                                                               
         TM    TIMERECB,X'40'                                                   
         BO    WAIT02              TIMER POPPED, TRY GET AGAIN                  
*                                                                               
         OC    STIMER1,STIMER1                                                  
         BZ    WAIT04                                                           
         STIMERM CANCEL,ID=STIMER1                                              
         LTR   RF,RF               CANCEL TIMER                                 
         JNZ   *+2                                                              
         J     WAIT04                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*TRY TO RECONNECT TO QUEUE MANAGER                                    *         
***********************************************************************         
RECON    NTR1  BASE=*,LABEL=*                                                   
         MVC   PLINE(16),=CL16'Connectionbroken'                                
         BRAS  RE,PRNT                                                          
         XR    R3,R3                                                            
*                                                                               
         LA    R2,MQDISC                                                        
         BRAS  RE,CALLMQ           DISCONNECT MQ QMGR                           
*                                                                               
RECON1   STIMERM SET,ID=STIMER2,BINTVL=WAITR,WAIT=YES                           
         LTR   RF,RF               WAIT 30 SEC                                  
         JNZ   *+2                                                              
*                                                                               
         LA    R3,1(R3)                                                         
         CHI   R3,10                                                            
         JNL   *+2                 TOO MANY RETRIES                             
*                                                                               
         MVC   PLINE(16),=CL16'Reconnect       '                                
         EDIT  (R3),(5,PLINE+10),ALIGN=LEFT                                     
         BRAS  RE,PRNT                                                          
*                                                                               
         BRAS  RE,MQINIT                                                        
         JL    RECON1                                                           
RECONX   J     EXIT                                                             
*                                                                               
WAITR    DC    A(30*100)           WAIT 30 SEC                                  
STIMER2  DS    XL4                 FOR TIMER POPS                               
                                                                                
         LTORG                                                                  
***********************************************************************         
*SET TIMER                                                            *         
***********************************************************************         
SETTIMER NTR1  BASE=*,LABEL=*                                                   
         MVC   PLINE(16),=CL16'Set Timer'                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         STIMERM SET,ID=STIMER1,BINTVL=WAITSECS,EXIT=TIMERXIT                   
         LTR   RF,RF               WAIT 30 SEC                                  
         JZ    EXIT                                                             
         DC    H'00'                                                            
*                                                                               
WAITSECS DC    A(30*100)            WAIT 30 SEC                                 
STIMER1  DS    XL4                 FOR TIMER POPS                               
*                                                                               
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET DATA FROM EDICT RECORD FOR THIS USER                            *         
***********************************************************************         
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
         JNE   *+2                                                              
*                                                                               
BD02     L     R4,AIO                                                           
         AHI   R4,28                                                            
         XR    RF,RF                                                            
         USING EDILNKD,R4                                                       
*                                                                               
BD04     CLI   EDILNKEL,0                                                       
         JE    *+2                                                              
*                                                                               
         CLI   EDILNKEL,EDILNKEQ                                                
         BE    BD06                                                             
         IC    RF,EDILNKLN                                                      
         JXH   R4,RF,BD04                                                       
         DC    H'00'                                                            
*                                                                               
BD06     MVC   DESTNAME,EDINAME    USERID                                       
         MVC   DESTMETS,EDIMETHS   METHOD OF SENDING TRANSMISSIONS              
         MVC   DESTMETR,EDIMETHR   METHOD OF RECEIVING TRANSMISSIONS            
         MVC   DESTXRRN,EDIXRRN    XRR ACC NUMBER                               
         MVC   DESTUIDN,PQUSER#                                                 
         MVI   DESTPM360,NO                                                     
         TM    EDIOPTS,EDIPM360                                                 
         JZ    *+8                                                              
         MVI   DESTPM360,YES                                                    
BD10     TR    DESTNTRY,TRTAB                                                   
         J     EXITOK                                                           
         LTORG                                                                  
         DROP  R4,RB                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
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
         L     RF,=A(LINUKUS)      Translate table                              
         MVC   CHARTAB,0(RF)                                                    
         MVI   CHARTAB,C' '        Make Null to space                           
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
*                                                                               
INIT02   ST    R1,AMSGIN           A(INPUT BUFFER)                              
         A     R1,MAXMSGLN                                                      
         ST    R1,AMSGOUT          A(OUTPUT BUFFER)                             
*                                                                               
         GOTOR XRRRPT,1            Initialize summary report areas              
*                                                                               
         CLI   DSPACE,C'T'                                                      
         BNE   INIT10                                                           
         MVC   SYS,=C'TST'                                                      
         MVC   ENV,=C'DEV'                                                      
*                                                                               
INIT10   CLI   DSPACE,C'Q'                                                      
         BNE   INIT12                                                           
         MVC   SYS,=C'FQA'                                                      
         MVC   ENV,=C'QA1'                                                      
*                                                                               
INIT12   CLI   DSPACE,C'C'                                                      
         BNE   INIT14                                                           
         MVC   SYS,=C'CSC'                                                      
         MVC   ENV,=C'YDY'                                                      
*                                                                               
INIT14   CLI   DSPACE,C'A'                                                      
         BNE   INIT16                                                           
         MVC   SYS,=C'ADV'                                                      
         MVC   ENV,=C'PRD'                                                      
*                                                                               
INIT16   CLI   DSPACE,C'R'                                                      
         BNE   INIT20                                                           
         MVC   SYS,=C'REP'                                                      
         MVC   ENV,=C'PRD'                                                      
*                                                                               
INIT20   LHI   R0,2                                                             
         BRAS  RE,SYSMESS          COMPLETED INITIALISE                         
         J     EXITOK                                                           
*                                                                               
VCTITLE  DC    CL(L'TITLE)'Input cards to MQ ER fax receive job'                
DWTITLE  DC    CL(L'TITLE)'MQ ER fax send output log'                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE MQ QUEUES                                                *         
* NOTE: IN A DELIBERATE ATTEMPT TO SIMPLIFY THE NAMES OF THE MQ       *         
*       QUEUE MANAGER, THE MASTER QUEUE AND LOG QUEUES ARE ALL HARD   *         
***********************************************************************         
MQINIT   NTR1  BASE=*,LABEL=*                                                   
         CLI   MQINIFLG,C'Y'       MUST BE A RE-CONNECT                         
         JE    MQINI100                                                         
*                                                                               
         MVI   CONOK,NO                                                         
         LHI   R0,9                                                             
         BRAS  RE,SYSMESS          BEGINNING MQ INITIALISE                      
*                                                                               
         CLI   DSPACE,C'A'         ADV QUEUES                                   
         BNE   MQINI20                                                          
         MVC   INPQ_OBJECTNAME,QINPUTA                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTA                                         
         MVC   LOGQ_OBJECTNAME,QLOGA                                            
         B     MQINI60                                                          
*                                                                               
MQINI20  CLI   DSPACE,C'R'         REP QUEUES                                   
         BNE   MQINI30                                                          
         MVC   INPQ_OBJECTNAME,QINPUTR                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTR                                         
         MVC   LOGQ_OBJECTNAME,QLOGR                                            
         B     MQINI60                                                          
*                                                                               
MQINI30  CLI   DSPACE,C'Q'         FQA QUEUES                                   
         BNE   MQINI40                                                          
         MVC   INPQ_OBJECTNAME,QINPUTQ                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTQ                                         
         MVC   LOGQ_OBJECTNAME,QLOGQ                                            
         MVC   QMGR,QMGRQ                                                       
         B     MQINI60                                                          
*                                                                               
MQINI40  CLI   DSPACE,C'C'         CSC QUEUES                                   
         JNE   MQINI50                                                          
         MVC   INPQ_OBJECTNAME,QINPUTC                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTC                                         
         MVC   LOGQ_OBJECTNAME,QLOGC                                            
         MVC   QMGR,QMGRC                                                       
         B     MQINI60                                                          
*                                                                               
MQINI50  CLI   DSPACE,C'T'         TST QUEUES                                   
         JNE   *+2                                                              
         MVC   INPQ_OBJECTNAME,QINPUTT                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTT                                         
         MVC   LOGQ_OBJECTNAME,QLOGT                                            
         MVC   QMGR,QMGRT                                                       
*                                                                               
MQINI60  MVC   PLINE(20),=CL20'Queue Manager: '                                 
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
MQINI100 MVI   MQINIFLG,C'Y'       Set INIT FLAG to Y - Reconnect here          
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
         MVI   CONOK,YES                                                        
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* QUEUE INFORMATION - Defualt is MQ1P (Production)                    *         
***********************************************************************         
QMGRT    DC    CL48'MQ7T'                                                       
QMGRQ    DC    CL48'MQ7Q'                                                       
QMGRC    DC    CL48'MQ7C'                                                       
*                                                                               
QINPUTA  DC    CL48'MO.XRRFAX.ADV.QUEUE'                                        
QINPUTR  DC    CL48'MO.XRRFAX.REP.QUEUE'                                        
QINPUTT  DC    CL48'MO.XRRFAX.TST.QUEUE'                                        
QINPUTQ  DC    CL48'MO.XRRFAX.FQA.QUEUE'                                        
QINPUTC  DC    CL48'MO.XRRFAX.CSC.QUEUE'                                        
                                                                                
******************************                                                  
* OUT TO Extreme Reach queue *                                                  
******************************                                                  
QOUTPUTA DC    CL48'XRRFAX.ADV.QUEUE'                                           
QOUTPUTR DC    CL48'XRRFAX.REP.QUEUE'                                           
QOUTPUTT DC    CL48'XRRFAX.TST.QUEUE'                                           
QOUTPUTQ DC    CL48'XRRFAX.FQA.QUEUE'                                           
QOUTPUTC DC    CL48'XRRFAX.CSC.QUEUE'                                           
                                                                                
**********************************                                              
* OUT TO EDICTA / EDINJE Subtask *                                              
**********************************                                              
QLOGA    DC    CL48'MO.EDICT.ADV.STATUS.QUEUE'                                  
QLOGR    DC    CL48'MO.EDICT.REP.STATUS.QUEUE'                                  
QLOGT    DC    CL48'MO.EDICT.STATUS.QUEUE'                                      
QLOGQ    DC    CL48'MO.EDICT.STATUS.QUEUE'                                      
QLOGC    DC    CL48'MO.EDICT.STATUS.QUEUE'                                      
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISCONNECT FROM MQ CLEANLY                               *         
***********************************************************************         
MQCLS    NTR1  BASE=*,LABEL=*                                                   
         CLI   CONOK,YES                                                        
         JNE   EXITOK                                                           
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
         MVI   CONOK,NO                                                         
         J     EXITOK                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL MQ                                                  *         
* NTRY: R2          = A(PARAMETER STRUCTURE)                          *         
* EXIT: MQ_CC       = MQ COMPLETION CODE                              *         
*       MQ_RC       = MQ REASON CODE                                  *         
*       CC SET EQ     WHEN CALL OKAY                                  *         
***********************************************************************         
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
         CLI   TRACE,NO                                                         
         BE    EXITH                                                            
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(35),=CL35'WAITING FOR NEW MESSAGE'                      
         BRAS  RE,PRNT                                                          
         B     EXITH               FLAG THAT WE NEED TO WAIT                    
*                                                                               
CMQ02    CLI   TRACE,NO                                                         
         JE    EXITOK                                                           
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(12),=C'Completed ok'                                    
         BRAS  RE,PRNT                                                          
         J     EXITOK                                                           
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
         J     EXITL                                                            
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
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
         J     EXITOK                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A COVER PAGE FROM A FAX RECORD.                               *         
* NTRY: R7         = DSTAEFTD                                         *         
*       FAXKEY     = KEY OF FAX RECORD TO USE                         *         
* EXIT: DSTAEDST   = DESTINATION OR ZEROS (ON ERROR)                  *         
***********************************************************************         
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
         MVC   FAXKEY+4(3),XSPACES                                              
*                                                                               
CPG02    CLC   =C'NOREP',FAXKEY                                                 
         BNE   CPG04                                                            
         CLI   DSTAEDST+4,C'T'     MEDIA T?                                     
         BNE   CPG04               **SHOULD CHECK RADIO AND OTHER LATER         
         MVC   AGYALPHA,SVAGYPOW   USE AGENCY POWER CODE, NOT "DARADM"          
*                                                                               
CPG04    XC    DSTAEDST,DSTAEDST   FAX NUMBER WILL GO HERE                      
*                                                                               
         XC    KEY,KEY             READ FAX RECORD                              
X        USING CTFXREC,KEY                                                      
         MVI   X.CTFXKTYP,CTFXEQU                                               
         MVC   X.CTFXAGY,AGYALPHA                                               
         MVC   X.CTFXCODE,FAXKEY                                                
*                                                                               
         CLI   EDIDARE,EDIDARQ     IS THIS A DARE FAX?                          
         BNE   CPG06                                                            
         OC    FAXSUBC(3),FAXSUBC  1ST SUB CODE IS THERE?                       
         BZ    CPG08               NO - TRY 2ND SUB CODE                        
         MVC   X.CTFXSUBC,XSPACES                                               
         MVC   X.CTFXSUBC(3),FAXSUBC                                            
*                                                                               
CPG06    GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
*                                                                               
         CLI   EDIDARE,EDIDARQ     IS THIS A DARE FAX?                          
         BE    *+12                YES                                          
         BRAS  RE,CPGTST                                                        
         J     EXITOK                                                           
*                                                                               
         BRAS  RE,CPGTST                                                        
*                                                                               
CPG08    OC    FAXSUBC+3(3),FAXSUBC+3   2ND SUB CODE IS THERE?                  
         BZ    CPG10                    NO - TRY WITHOUT SUB CODE               
*                                                                               
         XC    KEY,KEY             READ FAX RECORD                              
         MVI   X.CTFXKTYP,CTFXEQU                                               
         MVC   X.CTFXAGY,AGYALPHA                                               
         MVC   X.CTFXCODE,FAXKEY                                                
         MVC   X.CTFXSUBC,XSPACES                                               
         MVC   X.CTFXSUBC(3),FAXSUBC+3                                          
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
*                                                                               
         BRAS  RE,CPGTST                                                        
*                                                                               
CPG10    XC    KEY,KEY             READ FAX RECORD                              
         USING CTFXREC,R4                                                       
         MVI   X.CTFXKTYP,CTFXEQU                                               
         MVC   X.CTFXAGY,AGYALPHA                                               
         MVC   X.CTFXCODE,FAXKEY                                                
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
*                                                                               
         BRAS  RE,CPGTST                                                        
         J     EXITOK              COVER PAGE NOT FOUND                         
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
         SAM24                                                                  
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX2ELQ',AIO),0,0                   
         SAM31                                                                  
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
         CLI   LONGFXPG,YES        GENERATE "LONG" FAX PAGES?                   
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
         BRAS  RE,EDITTEL                                                       
*                                                                               
CPG16    AHI   R2,132                                                           
         AHI   R2,132                                                           
         LR    R5,R2                                                            
         DROP  R4                                                               
*                                                                               
         CLI   HDR+70,NO           NO COVER PAGE?                               
         BNE   *+14                                                             
         XC    COVPGLNS,COVPGLNS                                                
         J     EXITOK                                                           
*                                                                               
         CLI   DSTTYPE,EDFDSTHQ    FAX NUMBER (HOME MARKET W/O ID)?             
         BE    CPG20                                                            
*                                                                               
         SAM24                                                                  
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX3ELQ',AIO),0,0                   
         SAM31                                                                  
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
         MVI   BYTE2,NO                                                         
         OC    REQUESTR,REQUESTR                                                
         BZ    CPG24                                                            
*                                                                               
         XC    KEY,KEY             READ FAX RECORD FOR REQUESTOR                
         MVI   X.CTFXKTYP,CTFXEQU                                               
         MVC   X.CTFXAGY,SVAGYPOW                                               
         MVC   X.CTFXCODE(2),=C'**'  LOOK FOR **RRR (REQUESTOR)                 
         MVC   X.CTFXCODE+2(3),REQUESTR                                         
         MVC   X.CTFXCODE+5(2),XSPACES                                          
*                                                                               
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
         CLI   8(R1),0                                                          
         BE    CPG22                                                            
         TM    8(R1),X'10'                                                      
         BO    CPG24               REQUESTOR DOESN'T HAVE A FAX ELEMENT         
         DC    H'0'                                                             
*                                                                               
CPG22    MVI   BYTE2,YES           SET HAVE REQUESTOR FAX DATA                  
         SAM24                                                                  
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX2ELQ',AIO),0,0                   
         SAM31                                                                  
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
         CLI   BYTE2,YES           DO WE HAVE REQUESTOR FAX DATA?               
         BNE   CPG30                                                            
         SAM24                                                                  
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX4ELQ',AIO),0,0                   
         SAM31                                                                  
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
         BRAS  RE,EDITTEL                                                       
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
         BRAS  RE,EDITTEL                                                       
         AHI   R2,132                                                           
         AHI   R2,132                                                           
         LR    R5,R2                                                            
*                                                                               
         XR    R0,R0                                                            
         SAM24                                                                  
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTFX3ELQ',AIO),0,0                   
         SAM31                                                                  
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
         J     EXITOK                                                           
         LTORG                                                                  
         DROP  X                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT TELEPHONE NUMBER TO PUT IN () & -                              *         
* NTRY:  R1    = A(CL25 TELEPHONE)                                    *         
***********************************************************************         
EDITTEL  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(25),0(R1)                                                   
         MVC   0(25,R1),XSPACES                                                 
         LA    R2,WORK                                                          
         CLC   WORK+7(4),XSPACES   IF > 7 CHARACTERS                            
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
         J     EXITOK                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* READ IN ALL INPUT CARDS                                             *         
* FORMAT INTO A SCANNER BLOCK AND SAVE FOR VALIDATION                 *         
***********************************************************************         
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
         MVI   ERROR,05            TOO MANY PARAMETERS                          
         BRAS  RE,ERRMSG                                                        
         J     EXITL                                                            
*                                                                               
GCD04    MVC   0(L'CARDIO,R3),CARDIO                                            
         AHI   R3,L'CARDIO                                                      
         B     GCD02               NEXT                                         
*                                                                               
GCDX     MVI   0(R3),X'FF'         FLAG END OF CARDS                            
         LHI   R0,4                                                             
         BRAS  RE,SYSMESS          COMPLETED READING INPUT CARDS                
         J     EXITOK                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS IN SCANNER BLOCK                           *         
***********************************************************************         
VALCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,5                                                             
         BRAS  RE,SYSMESS          STARTING VALIDATE INPUT CARDS                
         MVI   GOTERR,NO                                                        
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
         MVI   ERROR,04                                                         
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
         MVI   ERROR,01            FLAG UNKNOWN PARAMETER CARD                  
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
         CLI   DSPACE,C' '         Must have DSPACE card                        
         JH    VCDXX                                                            
         MVI   ERROR,31            Missing                                      
         BRAS  RE,ERRMSG           PRINT ERROR MESSAGE                          
         DC    H'00'                                                            
*                                                                               
VCDXX    CLI   GOTERR,YES          SET CC BASED ON ERRORS                       
         JNE   EXITOK                                                           
         J     EXITL                                                            
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DSPACE= CARD                                          *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         USING SCANBLKD,R5                                                      
VCDSPACE NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,1                                                       
         BE    VCDSP10                                                          
         MVI   ERROR,02                                                         
         J     EXITL                                                            
*                                                                               
VCDSP10  ICM   RF,15,ASSB                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),SC2NDFLD                          
*                                                                               
         MVC   DSPACE,SSODSPAC-SSOOFF(RF)                                       
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DDSIO= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCDDSIO  NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,8                                                       
         BNH   VCDDS10                                                          
         MVI   ERROR,03                                                         
         J     EXITL                                                            
*                                                                               
VCDDS10  ICM   RF,15,VDDSIO                                                     
         MVC   0(8,RF),SC2NDFLD                                                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DRTEST= CARD                                          *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCDR     NTR1  BASE=*,LABEL=*                                                   
         MVC   DRTEST,SC2NDFLD                                                  
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DEBUG= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCDEBUG  NTR1  BASE=*,LABEL=*                                                   
         MVC   DEBUG,SC2NDFLD                                                   
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF TRACE= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCTRACE  NTR1  BASE=*,LABEL=*                                                   
         MVC   TRACE,SC2NDFLD                                                   
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF QMGR= OVERRIDE CARD                                   *         
* NTRY: R3     = A(INPUT CARD)                                        *         
***********************************************************************         
VCQMGR   NTR1  BASE=*,LABEL=*                                                   
         MVC   QMGR(L'SC2NDFLD),SC2NDFLD                                        
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF LOGGING=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')  *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCLOG    NTR1  BASE=*,LABEL=*                                                   
         MVC   LOG,SC2NDFLD                                                     
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DLNS=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')     *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCDLNS   NTR1  BASE=*,LABEL=*                                                   
         MVC   SENDDLNS,SC2NDFLD                                                
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF JUNKREP=CARD  (NUMBER)                                *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VJREP    NTR1  BASE=*,LABEL=*                                                   
         OI    SC2NDVAL,SCNUMQ     IS THIS A NUMBER?                            
         JZ    EXITOK              NO - USE DEFAULT AND EXIT OK                 
         MVC   JREPMAX,SC2NDNUM                                                 
         J     EXITOK                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR MESSAGE                                      *         
***********************************************************************         
ERRMSG   NTR1  BASE=*,LABEL=*                                                   
         MVI   GOTERR,YES          SET GOT AN ERROR                             
*                                                                               
         LA    RF,ERRND                                                         
         CLI   ERROR,0             UNDEFINED ERROR                              
         BE    ERRM02                                                           
*                                                                               
         LLC   RF,ERROR            INDEX INTO ERROR TABLE                       
         BCTR  RF,0                                                             
         MHI   RF,L'ERRMSG                                                      
         A     RF,AERRMSG          RF = A(ERROR MESSAGE)                        
*                                                                               
ERRM02   MVC   PLINE,XSPACES                                                    
         MVC   PLINE(L'ERRHDR),ERRHDR                                           
         MVC   PLINE+L'ERRHDR(L'ERRMSG),0(RF)                                   
         BRAS  RE,PRNT                                                          
*                                                                               
         MVI   ERROR,0                                                          
         J     EXITOK                                                           
*                                                                               
ERRHDR   DC    C'@  *** ERROR *** '                                             
ERRND    DC    CL45'Improperly defined error'                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PLINE TO A PRINT LINE                             *         
***********************************************************************         
TRACELOG CLI   TRACE,C'F'                                                       
         BNER  RE                                                               
*                                                                               
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
         MVC   PLINE,XSPACES                                                    
         GOTO1 VPRINTER                                                         
         J     EXITOK                                                           
*                                                                               
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
SYSMESS  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         AHI   R0,-1                                                            
         BM    SYM02               R0 == 0 MEANS R1=A(MESSAGE)                  
*                                                                               
         MHI   R0,L'SYSMSGS                                                     
         L     R2,ASYSMSGS                                                      
         AR    R2,R0               R2=A(SYSMSGS ENTRY)                          
*                                                                               
SYM02    MVC   MESSAGE,XSPACES     BUILD MESSAGE                                
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
SYM16    MVC   XTRAMESS,XSPACES    CLEAR THESE OUT                              
         MVC   XTRAMES2,XSPACES                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OPEN CONTROL FILE IF REQUIRED                            *         
***********************************************************************         
OPENCTFL NTR1  BASE=*,LABEL=*                                                   
         CLI   CTOPEN,YES                                                       
         JE    EXITOK                                                           
         MVI   CTOPEN,YES                                                       
         GOTO1 VDMGR,DMCB,DMOPEN,CONTROL,CTFLIST,AIO,0                          
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DARE FAX                                                            *         
* NTRY:  R7    = DSTAEFTD                                             *         
* EXIT:  WORK  = RECEIVING FAX ID                                     *         
***********************************************************************         
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
         JNE   EXITOK                                                           
         L     RF,AIO                                                           
         CLC   KEY(STAKEFDA-STAKEYD),0(RF)                                      
         JNE   EXITOK              NOT FOUND, EXIT                              
*                                                                               
         GOTO1 VDMGR,DMCB,GETREC,GENFIL,36(RF),AIO,DMWORK                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SAM24                                                                  
         GOTO1 VHELLO,DMCB,(C'G',GENFIL),('STAHOMCQ',AIO),0,0                   
         SAM31                                                                  
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
         J     EXITOK                                                           
*                                                                               
DFX04    EQU   *                                                                
         SAM24                                                                  
         GOTO1 VHELLO,DMCB,(C'G',GENFIL),('STAREPCQ',AIO),0,0                   
         SAM31                                                                  
         CLI   12(R1),0                                                         
         JNE   EXITOK              NO REP ELEMENT (SHOULDN'T HAPPEN)            
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
         JE    EXITOK              MISSING ENTRY -- SHOULD NEVER HAPPEN         
         CLC   REPCODE,0(R3)                                                    
         BE    *+12                GOT IT                                       
         AHI   R3,L'REPIDS         SKIP TO NEXT ENTRY                           
         B     DFX06                                                            
*                                                                               
         MVC   WORK,XSPACES                                                     
         XR    R4,R4                                                            
         IC    R4,14(R3)           CONSTRUCT RECEIVING ID IN WORK               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),15(R3)      REP-ID                                       
         TM    13(R3),X'80'        APPEND OFFICE CODE?                          
         JO    EXITOK              NO                                           
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
         JO    EXITOK              NO OFFICE EXCEPTION RECORD                   
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDMGR,DMCB,GETREC,GENFIL,KEY+36,AIO,DMWORK                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SAM24                                                                  
         GOTO1 VHELLO,DMCB,(C'G',GENFIL),('AGROVRCQ',AIO),0,0                   
         SAM31                                                                  
         CLI   12(R1),0                                                         
         JNE   EXITOK              NO REP ELEMENT (SHOULDN'T HAPPEN)            
         ICM   R4,15,12(R1)                                                     
         USING AGROVRD,R4                                                       
*                                                                               
         XR    RF,RF                                                            
DFX08    CLC   REPCODE,AGROVRCR    IS THERE AN OVERRIDE FOR THIS REP?           
         BNE   *+14                YES                                          
         MVC   0(2,R3),AGROVROF    PUT OFFICE OVERRIDE IN RECEIVING ID          
         J     EXITOK                                                           
*                                                                               
         IC    RF,AGROVRLN         NEXT OVERRIDE ELEMENT                        
         AR    R4,RF                                                            
         CLI   AGROVRC,AGROVRCQ                                                 
         BE    DFX08                                                            
         J     EXITOK                                                           
         DROP  X,R4,R7                                                          
         EJECT                                                                  
***********************************************************************         
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'       *         
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.      *         
***********************************************************************         
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
         MVI   OPERSTOP,YES        YES -- SET STOP FLAG                         
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
CH04     CLI   0(R4),EOT           EOT                                          
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
         J     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
COMMTAB  DC    CL8'????????',AL1(8,0,0,0),AL4(OPDUMMY)                          
         DC    AL1(EOT)                                                         
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
EDIXRR   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SENT MQ MESSAGE TO LOG FOR THIS DESTINATION                         *         
* NTRY:  R7    = DSTAEFTD                                             *         
***********************************************************************         
         USING DSTAEFTD,R7                                                      
POSTSENT NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         CLI   DESTMETS,EDIEXRRQ   Extreme Reach?                               
         BE    POSTSN10                                                         
         CLI   DESTMETS,EDIBXFOQ   BXF only                                     
         BNE   POSTSXIT            Don't update EDICT if not R or B             
*                                                                               
POSTSN10 MVC   0(3,R3),=C'ADD'                                                  
         TM    DSTAEFLG,DSTAEFJQ   IS THIS DEST GOOD?                           
         BNO   *+10                YES                                          
         MVC   0(3,R3),=C'JNK'                                                  
         AHI   R3,3                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'REFNUM,R3),REFNUM                                            
         AHI   R3,L'REFNUM                                                      
         BRAS  RE,ADDCRLF                                                       
         MVC   0(1,R3),DESTMETS                                                 
         AHI   R3,1                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'XRRACC,R3),XRRACC                                            
         AHI   R3,L'XRRACC                                                      
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
*                                                                               
POSTSXIT J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET INFORMATION FROM USER ID RECORD                                 *         
***********************************************************************         
GETIDN   NTR1  BASE=*,LABEL=*                                                   
         XC    ROUTCODE,ROUTCODE                                                
         XC    AGYALPHA,AGYALPHA                                                
         XC    SVAGYPOW,SVAGYPOW                                                
         XC    AGYNAME,AGYNAME                                                  
         XC    AGYADDR,AGYADDR                                                  
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
X        USING CTIREC,KEY                                                       
         MVI   X.CTIKTYP,CTIKTYPQ                                               
         MVC   X.CTIKNUM,PQUSER#                                                
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
         MVC   AGYALPHA,CTAGYID-CTAGYD(R4)                                      
         MVC   SVAGYPOW,AGYALPHA                                                
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
* EXIT: REFNUM    = REFERENCE NUMBER                                  *         
***********************************************************************         
BLDXREF  NTR1  BASE=*,LABEL=*                                                   
         MVC   REFNUM,XSPACES                                                   
         USING XREFKEYD,R2                                                      
         LA    R2,REFNUM                                                        
         MVI   XREFVER#,VERSION1   VERSION 1                                    
         MVC   XREFDSPC,DSPACE     ADV/REP                                      
         CLI   DRTEST,YES                                                       
         BNE   *+8                                                              
         MVI   XREFDSPC,C'D'          DR                                        
*                                                                               
         MVC   XREFUSRD,USERIDH                                                 
         MVC   XREFSUBD,RPTSUBID                                                
         MVC   XREFRPT#,RPTPQNUM                                                
         MVC   XREFCDAT,RPTHDATE                                                
         MVC   XREFCTIM,RPTHTIME                                                
         MVC   XREFLOG#,RPTLOGNO                                                
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
         CLI   0(RF),EOT           EOT?                                         
         BE    *+14                                                             
         OC    DSTAEDST-DSTAEFTD(L'DSTAEDST,RF),DSTAEDST-DSTAEFTD(RF)           
         BNZ   *+8                                                              
         OI    RPTLDSTS,X'80'      FLAG AS LAST DESTINATION                     
*                                                                               
         GOTO1 VHEXO31,DMCB,RPTLDSTS,XREFDES#,L'RPTLDSTS,0                      
*                                                                               
         SAM31                                                                  
         L     R4,@REFBUFF                                                      
         OC    0(L'GROUPID,R4),0(R4)   Was this set?                            
         JNZ   BLDXRF10                Yes, see if they match                   
***********************************************************************         
*        BUILD GROUPID                                                          
***********************************************************************         
BLDGRPID MVC   GRPIHOST,=C'DS'     Fill out GROUPID                             
         MVI   GRPDASH1,C'-'                                                    
         MVC   GRPISYS,SYS                                                      
         MVI   GRPDASH2,C'-'                                                    
         MVC   GRPIAGY,AGYALPHA                                                 
         MVI   GRPDASH3,C'-'                                                    
         MVC   GRPICLI,CLIENT                                                   
         MVI   GRPDASH4,C'-'                                                    
*                                                                               
         MVC   0(L'GROUPID,R4),GROUPID Set new value                            
         XC    REFCOUNT,REFCOUNT       Restart the count                        
*                                                                               
BLDXRF10 CLC   0(L'GROUPID,R4),GROUPID                                          
         JNE   *+2                                                              
*                                                                               
         AHI   R4,L'GROUPID        Bump past Group Id                           
         LLH   RF,REFCOUNT         Number processed so far                      
         LR    R0,RF                                                            
         MHI   RF,XREFLNQ          Length of XREF                               
         AR    R4,RF               A(Start of table entires)                    
         MVC   0(XREFLNQ,R4),REFNUM  Save list of REFNUMs                       
         AHI   R0,1                                                             
         STH   R0,REFCOUNT                                                      
         J     EXITOK                                                           
         DROP  R2                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* COMMON STORAGE/ROUTINES                                             *         
* This area needs to be out of range of RB else you get a warning     *         
* This is why we add 4K block to the ORG below.                       *         
***********************************************************************         
         ORG   EDIXRR+(((*-EDIXRR)/4096)+2)*4096                                
COMMON   DC    CL8'*COMMON*'                                                    
SAVERD   DC    F'0'                                                             
*                                                                               
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
         ICM   R7,15,LASTOUT                                                    
         BNZ   *+8                                                              
         L     R7,AMSGOUT                                                       
         ST    R3,LASTOUT                                                       
*                                                                               
         LR    R1,R3                                                            
         SR    R1,R7                                                            
         BM    EXITOK                                                           
LINESUP2 CHI   R1,L'PLINE          Can only print so much at time               
         BNH   LINESUP4                                                         
         MVC   PLINE,0(R7)         Keep printing what is in the MSGOUT          
         BRAS  RE,PRNT                                                          
         SHI   R1,L'PLINE                                                       
         AHI   R7,L'PLINE                                                       
         B     LINESUP2                                                         
*                                                                               
LINESUP4 BCTR  R1,0                Print what is left (remander)                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),0(R7)                                                   
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'                                                            
                                                                                
VERSION1 EQU   C'1'                                                             
K        EQU   1024                                                             
*                                                                               
ARZERO   DC    16F'0'                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'HCONN==>'                                                    
HCONN    DC    F'0',F'0'           MQ QMGR CONNECTION HANDLE                    
*                                                                               
         DC    CL8'QMGR===>'                                                    
QMGR     DC    CL48'MQ1P'                                                       
*                                                                               
         DC    CL8'MSGBUFFS'                                                    
AMSGIN   DC    A(0)                INPUT MESSAGE BUFFER                         
AMSGOUT  DC    A(0)                OUTPUT MESSAGE BUFFER                        
MAXREFLN DC    A(200*K)                                                         
MAXMSGLN DC    A(4*K*K)            MAX MESSAGE LENGTH                           
*                                                                               
         LTORG                                                                  
*                                                                               
MQINIFLG DC    AL1(NO)             SET TO Y WHEN INITIALISED                    
TRACE    DC    AL1(NO)                                                          
GOTHDR   DC    AL1(NO)                                                          
GOTDATA  DC    AL1(NO)                                                          
DRTEST   DC    AL1(NO)                                                          
DEBUG    DC    AL1(NO)                                                          
SENDDLNS DC    AL1(YES)            'Y' IF WE WANT DLNS FROM XRR                 
LOG      DC    AL1(NO)             'Y' FOR FULL LOGGING                         
FACTYPE  DC    CL3'ADV'                                                         
*                                                                               
FNAMELN  DS    X                                                                
FNAME    DS    CL15                                                             
LNAMELN  DS    X                                                                
LNAME    DS    CL40                                                             
EMAILLN  DS    X                                                                
EMAIL    DS    CL52                                                             
PIDLN    DS    X                                                                
PID      DS    CL8                                                              
USERNLN  DS    X                                                                
USERNME  DS    CL10                                                             
DSPACE   DC    C' '                                                             
*                                                                               
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
VHEXO31  DC    V(HEXO31)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHEXI31  DC    V(HEXI31)                                                        
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
CHARTAB  DS    CL256                                                            
XMLREGS  DS    5F                               RE,RF,R0,R1                     
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
CTOPEN   DC    AL1(NO)                                                          
CONTROL  DC    CL8'CONTROL '                                                    
CTFLIST  DC    C'NCTFILE NGENDIR NGENFIL X'                                     
*                                                                               
LANDSCAP DC    C'LANDSCAPE'                                                     
PORTRAIT DC    C'PORTRAIT'                                                      
ORIENT   DS    C                   L or P                                       
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
XSPACES  DC    256C' '                                                          
*                                                                               
ERRMSG1  DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
ERRMSG1R DS    CL(L'JUNKRPT+L'DSTAEDST)                                         
ERRMSG1Q EQU   *-ERRMSG1                                                        
         EJECT                                                                  
***********************************************************************         
* PARAMETER LISTS TO FACILITATE MQ CALLS                              *         
* CL16 EBCDIC  ROUTINE NAME                                           *         
* A    A(ROUTINE)                                                     *         
* PARAMETERS (STANDARD IBM FORMAT)                                    *         
***********************************************************************         
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
EDIXRR   CSECT                                                                  
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
*********************************************************************           
* More tables so need CSECT                                                     
*********************************************************************           
EDIXRR   CSECT                                                                  
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
                                                                                
***********************************************************************         
* &, >, < are invalid for XML. Need to replace with                             
* (all are followed by a semicolon which I can not type in this editor          
* &amp                                                                          
* &GT                                                                           
* &LT                                                                           
***********************************************************************         
XMLTRTAB DC    XL256'00'                                                        
         ORG   XMLTRTAB+C'&&'                                                   
         DC    X'FF'                                                            
         ORG   XMLTRTAB+C'>'                                                    
         DC    X'FF'                                                            
         ORG   XMLTRTAB+C'<'                                                    
         DC    X'FF'                                                            
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* INCLUDED DATA                                                       *         
***********************************************************************         
* FACTRYXLAT                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYXLAT                                                     
         PRINT ON                                                               
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
         DS    0D                                                               
CARDTAB  DC    CL10'DSPACE    ',AL1(5,5,0,0),AL4(VCDSPACE)                      
         DC    CL10'DDSIO     ',AL1(4,4,0,0),AL4(VCDDSIO)                       
         DC    CL10'DEBUG     ',AL1(4,4,0,0),AL4(VCDEBUG)                       
         DC    CL10'DRTEST    ',AL1(5,5,0,0),AL4(VCDR)                          
         DC    CL10'TRACE     ',AL1(4,4,0,0),AL4(VCTRACE)                       
         DC    CL10'LOGGING   ',AL1(6,6,0,0),AL4(VCLOG)                         
         DC    CL10'DLNS      ',AL1(3,3,0,0),AL4(VCDLNS)                        
         DC    CL10'QMGR      ',AL1(3,3,CFBIG,0),AL4(VCQMGR)                    
         DC    CL10'JUNKREP   ',AL1(6,6,0,0),AL4(VJREP)                         
         DC    X'FF'                                                            
*                                                                               
CARDSIN  DC    15CL80' '                                                        
         DC    X'FF'                                                            
*                                                                               
CARDTABD DSECT ,                   INPUT CARD PARAMETERS                        
CARDTXT  DS    CL10                TEXT                                         
CMIN     DS    X                   MIN LEN FOR EXECUTE (-1)                     
CMAX     DS    X                   MAX LEN FOR EXECUTE (-1)                     
CFLAG    DS    X                   FLAGS                                        
CFBIG    EQU   X'80'                                                            
         DS    X                   N/D                                          
CARDVAL  DS    AL4                 A(VALIDATION ROUTINE)                        
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
EDIXRR   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                       *         
***********************************************************************         
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
EMSG21   DC    CL45'Missing ++UID card - please check           '               
EMSG22   DC    CL45'Person record not found - SA0REC            '               
EMSG23   DC    CL45'Missing PID number                          '               
EMSG24   DC    CL45'Person record not found - SAPEREC           '               
         EJECT                                                                  
***********************************************************************         
* CONSOLE MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                     *         
***********************************************************************         
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
SMSG30   DC    CL50'XXRR -- Transmissions suspended (MQ issue)     '            
SMSG31   DC    CL50'Missing DSPACE=x CARD                          '            
         EJECT                                                                  
***********************************************************************         
* W/S AREA                                                            *         
***********************************************************************         
         ORG   EDIXRR+(((*-EDIXRR)/4096)+1)*4096                                
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
XMLPARM  DS    8F                                                               
*                                                                               
EOR      DS    C                   End of report - last fax in group            
XMLMODE  DS    C                   1 - init, 2 - Summary                        
@REFBUFF DS    A                                                                
@REFEND  DS    A                                                                
REFBUFSZ DS    F                                                                
REFCOUNT DS    H                                                                
         DS    H                                                                
*                                                                               
PACKOF4B DS    PL4                 KEEP FULLWORD ALIGNED                        
WORK     DS    XL256               DITTO                                        
*                                                                               
NUMLINES DS    F                   NUMBER OF TRANSMITTED LINES/REPORT           
NUMPAGES DS    H                   PAGE COUNTER FOR FAX TRANSMISSION            
*                                                                               
PRNTDUB  DS    D                   FOR PRNT ROUTINE ONLY                        
PRNTTIME DS    CL9                 FOR PRNT ROUTINE ONLY                        
*                                                                               
REQUESTR DS    CL3                                                              
AGYALPHA DS    CL2                 AGENCY POWER CODE                            
SVAGYPOW DS    CL2                 SAVED AGENCY POWER CODE                      
AGYNAME  DS    CL33                                                             
AGYADDR  DS    CL33                                                             
XRRDATA  DS    XL21                XRR-SPECIFIC TRANSFER DATA                   
ROUTCODE DS    CL5                 DARE ROUTING CODE                            
SYSTEM   DS    C                   System                                       
MEDSYS   DS    CL5                 Media system                                 
REPTCODE DS    CL3                 Report code (type)                           
RTYCODE  DS    CL1                                                              
REPTYPE  DS    CL3                                                              
REPTNAME DS    CL25                                                             
SYS      DS    CL3                 Host                                         
ENV      DS    CL3                 Enviroment                                   
MEDIA    DS    C                   Media  Used in DARE too                      
CLIENT   DS    CL3                 Client  (Advertiser)                         
PRODUCT  DS    CL3                 Product (Brand)                              
EST      DS    CL3                 Estimate                                     
DESTCODE DS    CL7                 Station (spot), Network (net)                
SKIPPROD DS    C                   Yes/No skip product XML?                     
IS_CABLE DS    C                   YES/NO Cable?                                
IS_SYND  DS    C                   YES/NO Syndicated?                           
HASBXF   DS    C                   YES/NO Has BXF?                              
SUBMEDIA DS    C                   Sub media                                    
DAYS     DS    CL7                 Program Days MTWTFSS                         
TIME     DS    CL11                Program Time                                 
PGMID    DS    CL6                 Program id                                   
PGMNM    DS    CL20                Program Name                                 
DAYPART  DS    CL2                 Day Part id                                  
ENDDTE   DS    CL10                END DATE  (Used w/program id)                
REPCODE  DS    CL3                 DARE REP CODE                                
REFNUM   DS    CL27                VFUUUUSSSNNNNNTTTTTTTTLLLLSS                 
GROUPID  DS    CL28                DS-SYS-AA-CLT-GROUPID                        
DESTNTRY DS    XL(DESTTBLQ)        TEMP STORAGE FOR 1 DESTTBL  ENTRY            
*                                                                               
XRRACC   DS    CL8                 XRR ACC NUMBER                               
DSTNAME  DS    CL8                 DESTINATION NAME                             
DSTUIDN  DS    XL2                 USERID                                       
FAXKEY   DS    CL7                 KEY OF FAX RECORD                            
FAXSUBC  DS    CL6                 FAX SUBCODE RECORD                           
LONGFXPG DS    C                   'Y' = WE SHOULD SEND 'FAXL' COMMAND          
LASTEJCT DS    C                   'Y' = LAST LINE PRINTED WAS /PAGE            
*                                                                               
LSTSDST  DS    C                                                                
*                                                                               
YYYYMMDD DS    CL8                 DATE                                         
HHMMSS   DS    CL6                 TIME                                         
*                                                                               
REALBDT  DS    XL6                 YMDHMS binary                                
         ORG   REALBDT                                                          
REALBYY  DS    X                   Binary year                                  
REALBMO  DS    X                   Binary month                                 
REALBDD  DS    X                   Binary day                                   
*                                                                               
REALBHH  DS    X                   Binary hours                                 
REALBMM  DS    X                   Binary minutes                               
REALBSS  DS    X                   Binary seconds                               
         ORG                                                                    
YMDHMS   DS    CL19                YYYY-MM-DD HH:MM:SS                          
         ORG   YMDHMS                                                           
YMD      DS    CL10                                                             
         DC    C' '                                                             
HH       DS    CL2                                                              
         DC    C':'                                                             
MM       DS    CL2                                                              
         DC    C':'                                                             
SS       DS    CL2                                                              
         ORG                                                                    
*                                                                               
CONOK    DS    X                                                                
OPERSTOP DS    X                                                                
PLINE    DS    CL100               OUTPUT PRINT LINE                            
REPLY    DS    CL8                                                              
CARDIO   DS    CL80                                                             
COVPGLNS DS    H                   NUMBER OF LINES IN COVER PAGE                
ERROR    DS    X                                                                
GOTERR   DS    X                                                                
PARMCNT  DS    X                   NO OF I/P CARDS                              
PARMCNTQ EQU   100                 MAX NUMBER OF I/P CARDS                      
MVSNAME  DS    CL8                                                              
@DELVCDE DS    A                   A(Destination codes in MQ Msg)               
DESTLSLN DS    X                   Destination list length                      
DESTLIST DS    CL64                Destination list                             
KEY      DS    XL64                FOR CTFILE/GENDIR READS                      
IOKEY    DS    XL24                                                             
*                                                                               
EACTION  DS    X                   ACTION(S)                                    
EACTJNKQ EQU   C'J'                 UNSENDABLE                                  
*                                                                               
EERRORCD DS    X                   ERROR CODE                                   
EDAYNUM  DS    X                   PWOS DAY SENT/DLVRD/CAN                      
*                                                                               
PQUSER#  DS    H                                                                
PQRPT#   DS    H                                                                
PQPID#   DS    H                                                                
SECAGY   DS    CL2                                                              
PQDSPACE DS    C                   Report DSPACE A or R / T,C,Q                 
*                                                                               
USERID   DS    CL8                 REPORT USERID (ALPHA)                        
*SERIDOV DS    CL8                 REPORT USERID (ALPHA)                        
USERIDH  DS    CL4                 REPORT USERID (HEX AS CHARS)                 
USERIDNO DS    XL2                 REPORT USERID                                
RPTSUBID DS    CL3                 REPORT SUB-ID                                
RPTPQNUM DS    CL4                 REPORT REFERENCE NUMBER                      
RPTHDATE DS    CL4                 Report creation date                         
RPTHTIME DS    CL8                 REPORT CREATION DATE/TIME                    
RPTLOGNO DS    CL4                 LOGICAL REPORT SEQ. WITHIN PHYSICAL          
RPTLDSTS DS    XL1                 LOG. REP. DEST. SEQ. WITHIN PHYSICAL         
RPTPQTYP DS    XL2                 PQ REPORT TYPE                               
RPTGRPID DS    XL8                 YYYYDDDSSSSSTH - unique number pack          
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
         ORG   EDIXRR+(((*-EDIXRR)/4096)+1)*4096                                
***********************************************************************         
* RD CHAIN CSECT                                                      *         
***********************************************************************         
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* Notify message DSECT Version 1.                                               
* ADD<CRLF>XFRE<CRLF>DDMMMYY HH:MM<CRLF>????????<CRLF>EMAIL<CRLF>               
*                                                                               
* DLN<CRLF>XFRE<CRLF>DDMMMYY HH:MM<CRLF>TRACKING<CRLF>EMAIL<CRLF>               
* CAN<CRLF>XFRE<CRLF>DDMMMYY HH:MM<CRLF>TRACKING<CRLF>ERRNUM<CRLF>              
* REJ<CRLF>XFRE<CRLF>DDMMMYY HH:MM<CRLF>TRACKING<CRLF>ERRNUM<CRLF>              
***********************************************************************         
XREFKEYD DSECT                                                                  
XREFVER# DS    C                   Version 1                                    
XREFDSPC DS    C                   Data Space                                   
XREFUSRD DS    XL4                 User id in hex                               
XREFSUBD DS    XL3                 SUB id                                       
XREFRPT# DS    XL4                 Report number in hex                         
XREFCDAT DS    XL4                 Creation date compressed in hex              
XREFCTIM DS    XL4                 Creation time in hex                         
XREFLOG# DS    XL4                                                              
XREFDES# DS    XL2                                                              
XREFLNQ  EQU   *-XREFKEYD                                                       
***********************************************************************         
* Group Id                                                                      
***********************************************************************         
GROUPIDD DSECT                                                                  
GRPIHOST DS    CL2                 DS                                           
GRPDASH1 DS    CL1                 -                                            
GRPISYS  DS    CL3                 SYSTEM - ADV, TST, FQA, CSC                  
GRPDASH2 DS    CL1                 -                                            
GRPIAGY  DS    CL2                 Alpha Id                                     
GRPDASH3 DS    CL1                 -                                            
GRPICLI  DS    CL3                 Client                                       
GRPDASH4 DS    CL1                 -                                            
GRPICODE DS    CL14                Group Id code YYYYDDDSSSSSTH                 
GRPIDLNQ EQU   *-GROUPIDD                                                       
***********************************************************************         
* DESTINATION TABLE DSECT (CUT DOWN FROM EDICT)                       *         
***********************************************************************         
DESTTABD  DSECT                                                                 
DESTNAME  DS   CL8                 DESTINATION NAME                             
DESTUIDN  DS   XL2                 DDS USERID NUMBER                            
DESTMETS  DS   C                   METHOD OF SENDING TRANSMISSIONS              
DESTMETR  DS   C                   METHOD OF RECEVING TRANSMISSIONS             
DESTXRRN  DS   CL8                 ADV XRR MAILBOX NUMBER                       
DESTMQHD  DS   CL16                MQ Header over-ride                          
DESTPM360 DS   C                   PM360 Y/N                                    
DESTTBLQ  EQU  *-DESTTABD                                                       
         EJECT                                                                  
***********************************************************************         
* HDR and TRN card                                                    *         
***********************************************************************         
HDRCARDD DSECT                                                                  
       ++INCLUDE EDIDESTD                                                       
                                                                                
TRNCARDD DSECT                                                                  
*PREFIX=@                                                                       
       ++INCLUDE EDIDDSHD                                                       
*PREFIX=                                                                        
         ORG @EDICOMN                                                           
EDISTPMD DS  CL1                   Media                                        
EDISTPCL DS  CL1                   Client                                       
EDISTPPR DS  CL3                   Product                                      
EDISTPUB DS  CL4                   Publication                                  
         ORG @EDICOMN                                                           
*PREFIX=@                                                                       
       ++INCLUDE SPEDICT                                                        
*PREFIX=                                                                        
         ORG @EDICOMN                                                           
*PREFIX=@                                                                       
       ++INCLUDE EDILINKD                                                       
*PREFIX=                                                                        
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
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
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
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
**PAN#1  DC    CL21'040EDIXRR    05/27/20'                                      
         END                                                                    
