*          DATA SET NENAV00    AT LEVEL 146 AS OF 10/14/20                      
*PHASE T31800B                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE NETBLRDR                                                               
*INCLUDE CALCASHP                                                               
*&&ONLIN SET   Y                    ONLINE ONLY PROGRAM                         
T31800   TITLE 'NENAV00 - NET STEWARD - BASE'                                   
T31800   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,T31800,RA,RR=R2,CLEAR=YES                            
         USING WORKD,RC                                                         
         SPACE 1                                                                
*=================================================================*             
* INITIALIZATION CODE *                                                         
*=================================================================*             
         SPACE 1                                                                
         ST    RB,BASE1                                                         
         ST    RA,BASE2                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING T31800+8192,R9                                                   
*                                                                               
         ST    R2,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,ASYSPARM                                                      
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ASYSFACS,8(R1)                                                   
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
*                                                                               
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
*                                                                               
         LR    RE,RC                                                            
         AHI   RE,IOAREA1-WORKD                                                 
         ST    RE,AIO                                                           
         ST    RE,AIO1                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO2                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO3                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO4                                                          
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
*                                                                               
         LA    RE,NETREQ                                                        
         LA    RE,2048(RE)                                                      
         ST    RE,ANETIOBK                                                      
         LA    RE,1024(RE)                                                      
         ST    RE,ANETBLK                                                       
*                                                                               
         L     RE,ASYSPARM                                                      
         L     RE,8(RE)                                                         
         MVC   VRECUP,28(RE)                                                    
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VPERVERT,CPERVERT                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VWSSVRS,CWSSVR                                                   
*                                                                               
                                                                                
***************************************                                         
*                                                                               
*  MAKE NAVIGATOR IO BOUND                                                      
*                                                                               
***************************************                                         
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
*                                                                               
         L     RE,=V(NETBLRDR)                                                  
         A     RE,BASERELO                                                      
         ST    RE,VBILLRDR                                                      
*                                                                               
         L     RE,=V(CALCASHP)                                                  
         A     RE,BASERELO                                                      
         ST    RE,VCALCSHP                                                      
         DROP  R1                                                               
         SPACE 1                                                                
*===================================================================*           
* SET UP BASE PROGRAM MODULE ADDRESSES                                          
*===================================================================*           
         LA    R1,BASETAB          SET UP BASE FACILITIES                       
         LA    R0,BASETABC         COUNTER                                      
         LA    RE,BASEFACS         POINTER TO WORKING STORAGE                   
INIT02   L     RF,0(R1)            ADDRESS OF BASE FACILITY                     
         A     RF,BASERELO         RELOCATE IT                                  
         ST    RF,0(RE)                                                         
         LA    R1,4(R1)            NEXT ADDRESS                                 
         LA    RE,4(RE)            NEXT OUTPUT AREA                             
         BCT   R0,INIT02                                                        
*===================================================================*           
* SET UP ADDRESSES FOR CORE-RESIDENT PHASES                                     
*===================================================================*           
         SPACE 1                                                                
         L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
         A     R2,BASERELO                                                      
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAX NUMBER OF PHASES (CORERES)            
         SR    R0,R0                                                            
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
*                                                                               
INIT03   ICM   R0,1,0(R2)          ANY ENTRY HERE?                              
         BZ    INIT04              NONE, SKIP TO THE NEXT ENTRY                 
*                                                                               
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
*                                                                               
INIT04   LA    R2,1(R2)            BUMP TO THE NEXT ENTRY                       
         LA    R3,4(R3)                                                         
         BCT   R4,INIT03                                                        
*                                                                               
         LR    RE,RB                                                            
         AHI   RE,VCOMMON-T31800                                                
         LA    R0,VCOMBASN                                                      
         SR    RF,RF                                                            
         LA    R1,VALIBYR                                                       
INIT10   DS    0H                                                               
         ST    RE,0(R1)                                                         
         STC   RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         EJECT                                                                  
INIT20   DS    0H                                                               
         XC    SVRCVEL,SVRCVEL     CLEAR LAST RECEIVE ELEMENT                   
         XC    CMTCOUNT,CMTCOUNT   CLEAR COMMENT COUNTER                        
         XC    QDEST,QDEST                                                      
*                                                                               
         L     R3,ANETBLK                                                       
         MVI   0(R3),X'FF'          SET FIRST SWITCH IN UPLOAD RECORD           
*                                                                               
INIT25   CLI   SVXFROV,0           TEST RETURN FROM GLOBBER                     
         BNE   INIT30              NO                                           
         BRAS  RE,CHKXCTL                                                       
         BE    INIT32                                                           
*                                                                               
INIT27   GOTO1 VGLOBBER,DMCB,=C'CLEAR'                                          
*                                                                               
         MVI   SVGLOBSW,0                                                       
         MVI   SVTSRKEY,0                                                       
*                                                                               
         MVC   DPOINTER,AIO3                                                    
*                                                                               
INIT30   BRAS  RE,INIFALNK         INITIALIZE FALINK BLOCK                      
         BNE   INIT32              (CC=NEQ IF CALL OVERLAY FIRST)               
         GOTO1 VFALINK,DMCB,(X'80',FABLK)  GIVE FALINK CONTROL                  
         B     EXIT                                                             
*                                                                               
INIT32   XC    DMCB(4),DMCB                                                     
         MVC   DMCB(1),SVOLAY      MOVE OVERLAY NUMBER                          
         GOTO1 VCALLOV,DMCB,,ATWA                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         LR    R1,RC                                                            
         BASR  RE,RF                                                            
         L     R1,FABLK+(FALABLD-FALINKD)                                       
         CLI   FALCONC-FALSCRD(R1),FCZERO                                       
         BE    *+12                                                             
         CLI   FALCONC-FALSCRD(R1),FCZERO                                       
         BE    *+12                                                             
         CLI   FALCONC-FALSCRD(R1),FCDONE                                       
         BNE   EXIT                                                             
         MVI   SVOLAY,0            RESET OVERLAY IF ALL DONE                    
         B     EXIT                                                             
         EJECT                                                                  
EXIT     DS    0H                  JUST EXIT                                    
         XIT1                                                                   
BASETAB  DS    0F                                                               
         DC    A(IOCALL)                                                        
         DC    3A(0)                                                            
BASETABC EQU   (*-BASETAB)/L'BASETAB                                            
*====================================================================           
* LITERALS AND CONSTANTS                                                        
*====================================================================           
FF       EQU   X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
BREAK    NTR1  BASE=*,LABEL=*                                                   
         CR    RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* ON RETURN FROM AN OVERLAY THAT WANTS TO EXIT VIA GLOBBER,         *           
* WE RETURNED TO FALINK WITH AN FAGLB, FAGLB CALL.                  *           
* WHEN CALLED PROGRAM RETURNS TO US, THIS EXIT IS CALLED.           *           
* ON EXIT FROM HERE, AND SVRESUME IS THE OVERLAY TO BE CALLED.      *           
*====================================================================           
         SPACE 1                                                                
RESUME   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   TSACOM,ACOMFACS     RESET COMFACS FOR TSAR CALLS                 
         MVC   TSAREC,ANETBLK      RESET AREC FOR TSAR CALLS                    
         GOTO1 VGLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                          
         TM    DMCB+8,X'10'                                                     
         BZ    RSM2                NO CONTROL ELEM                              
         CLI   *,FF                SET CC LOW                                   
         BRAS  RE,EXIT                                                          
         DC    H'0'                NO RETURN HERE                               
*                                                                               
RSM2     GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
*                                                                               
G        USING GLVXFRSY,BLOCK                                                   
         CLC   =C'NETWRI',G.GLVXFRSY  FROM NET  WRI ?                           
         BE    RSM200  *****************                                        
         CLC   =C'NETNBU',G.GLVXFRSY  FROM NET  BUY ?                           
         BE    *+6     *****************                                        
         DC    H'0'    *****************                                        
         MVC   SVSESSNS,G.GLVXSESR      SAVE NNAV/NBU SESSION #'S               
         DROP  G                                                                
*                                                                               
*  CHECK FOR RETURN ERROR FROM NETBUY                                           
*                                                                               
         GOTO1 (RF),(R1),=C'GETD',WORK2,126,GLVBUY4                             
         CLI   DMCB+8,0                                                         
         BE    RSM40                                                            
*                                                                               
*  CHECK FOR DRAFT BUY                                                          
*                                                                               
         GOTO1 (RF),(R1),=C'GETD',TSARLOC,48,GLVTSAR                            
         CLI   DMCB+8,0                                                         
         BNE   RSM20                                                            
******   MVC   TSACOM,ACOMFACS                                                  
         MVC   TSARLOC+12(4),ACOMFACS   TSACOM                                  
******   MVC   TSAREC,AIO3                                                      
         MVC   TSARLOC+4(4),AIO3        TSAREC                                  
         ZIC   RE,SVSESSNS+1                                                    
         LA    RE,1(RE)                                                         
         STC   RE,TSARLOC+25    MOVE BUY SESSION INTO TSAR BLOCK                
         MVI   SVINITSW,C'D'    BYPASS TSAR HANDLING IN THE BASE                
         B     RSM30                                                            
*                                                                               
*  CHECK FOR BUY UPLOAD RETURN                                                  
*                                                                               
RSM20    GOTO1 (RF),DMCB,=C'GETD',WORK,4,GLVBUY2                                
         CLI   DMCB+8,0         CAN'T HANDLE OTHER ERRORS                       
         BE    *+6                                                              
         DC    H'0'                                                             
* DO WSSVR CALL TO GET INFO FROM THE BUY SYSTEM                                 
         XC    WORK2(100),WORK2                                                 
         LA    RE,WORK2                                                         
         USING FAWSSVRD,RE                                                      
         MVC   FAWSTOKN,WORK       TOKEN FROM NBUY                              
         OI    FAWSACTN,FAWSURST                                                
         MVC   FAWSADR,AIO3                                                     
         MVC   FAWSLEN,=H'1000'                                                 
         GOTOR VWSSVRS,FAWSSVRD                                                 
         LA    RE,WORK2                                                         
         USING FAWSSVRD,RE                                                      
         CLI   FAWSRTN,FAWSROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    FAWSACTN,FAWSUDEL                                                
         GOTOR VWSSVRS,FAWSSVRD                                                 
         DROP  RE                                                               
* READ BUY DATA RECORD FROM TEMPSTR                                             
***      L     R3,AIO3                                                          
***      XC    DMCB(24),DMCB                                                    
***      MVC   DMCB+8(4),WORK      PAGE/TERMINAL NUMBER                         
***      MVC   DMCB+20(2),=C'L='                                                
***      MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                            
***      GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R3),0                     
***      CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
***      BE    *+6                                                              
***      DC    H'0'                                                             
*                                                                               
RSM30    GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVTSAR                                 
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVNOTE                                 
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY1                                     
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY2                                     
         B     RSM400                                                           
*                                                                               
* ERROR FOUND                                                                   
* MAKE SURE DELETE ALL GLOBBER ELEMS ON ERROR                                   
RSM40    GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVTSAR                                 
         GOTO1 (RF),(R1),=C'DELE',,,GLVTSAR                                     
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY1                                     
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY2                                     
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY4                                     
         MVI   BUYERRSW,C'Y'                                                    
*        CLI   *,FF                SET CC LOW                                   
         B     RSMX                                                             
*                                                                               
*  CHECK FOR NET WRI RETURN                                                     
*                                                                               
RSM200   XC    WORK,WORK                                                        
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,4,GLVBUY2                            
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY1                                     
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY2                                     
         B     RSM400                                                           
*                                                                               
RSM400   CR    RB,RB               EXIT WITH CC =                               
*                                                                               
RSMX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
SEND     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ASETELEM,0(R1)      SAVE FALINK ROUTINE ADDRESSES                
         MVC   AADDDATA,4(R1)                                                   
*                                                                               
*  SAVE THE TSAR BLOCK IF GOING TO OVERLAY AND                                  
*  TSAR'S STATUS IS INIT.                                                       
*  NOTE: BYPASS THIS LOGIS IF GOING TO DRAFT ADD                                
*  PROGRAM AS IT DOES NOT READ TSAR RECORDS.                                    
*                                                                               
         LA    RE,NOSAVTAB         BYPASS SAVE TSAR TABLE                       
SEND02   CLI   0(RE),X'FF'                                                      
         BE    SEND03                                                           
         CLC   SVRCVEL,0(RE)                                                    
         BE    SEND05                                                           
         LA    RE,2(RE)                                                         
         B     SEND02                                                           
*                                                                               
SEND03   CLI   SVINITSW,C'D'       BYPASS TSAR HANDLING IN THE BASE             
         BE    SEND05                                                           
         MVC   TSACOM,ACOMFACS     RESET COMFACS FOR TSAR CALLS                 
         MVC   TSAREC,ANETBLK      RESET AREC FOR TSAR CALLS                    
         CLI   SVINITSW,C'Y'                                                    
         BNE   SEND05                                                           
         MVI   TSACTN,TSASAV                                                    
         BRAS  RE,CALLTSAR                                                      
         CLI   TSERRS,0            SET CC ON EXIT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SVRSTSAV,C'S'                                                    
         MVI   SVINITSW,0                                                       
*                                                                               
SEND05   CLI   SVRESUME,0          TEST XFRCTL RETURN                           
         BE    SEND10                                                           
         LA    R4,SVRESUME-2       POINT 2 BYTES BEFORE OVERLAY NUM             
         B     SEND20                                                           
*                                                                               
SEND10   LA    R4,SOVTAB                                                        
         LA    R5,(SOVTABX-SOVTAB)/L'SOVTAB                                     
SEND12   CLC   SVRCVEL,0(R4)       MATCH FIRST RCV ELCODE3                      
         BE    SEND20                                                           
         LA    R4,L'SOVTAB(R4)                                                  
         BCT   R5,SEND12                                                        
         DC    H'0'                                                             
*                                                                               
SEND20   CLI   2(R4),X'FF'         TEST NOT TO CALL ANYTHING                    
         BE    SEND40              ALAN SAYS A ZED WILL GO OUT !                
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB(1),2(R4)       MOVE OVERLAY NUMBER                          
         GOTO1 VCALLOV,DMCB,,ATWA                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         LR    R1,RC                                                            
         BASR  RE,RF                                                            
*                                                                               
         MVC   SVRESUME,SVXFROV    SAVE THIS OVERLAY NUMBER                     
         CLI   SVXFROV,0           TEST OVLY REQUESTED GLOBBER CALL             
         BE    SEND30                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,FALAGLB,FALAGLB                             
         B     SENDX                                                            
*                                                                               
SEND30   CLI   ANYDATA,C'Y'        ANY DATA IN BUFFER?                          
         BZ    SENDX               NO - DON'T CLOSE                             
*                                                                               
SEND40   GOTO1 AADDDATA,DMCB,AFABLK,FALADNE,FALADNE,0                           
*                                                                               
SENDX    CR    RB,RB                                                            
         XIT1                                                                   
         SPACE 1                                                                
*==================================================================*            
* SEND OVERLAY LOOKUP TABLE                                        *            
* ENTRIES ARE                                                      *            
*        DS    AL2(FIRST RCV EL CODE)                              *            
*        DS    XL1(SEND OVERLAY NUMBER)                            *            
*        DS    XL1(SPARE)                                          *            
*==================================================================*            
         SPACE 1                                                                
SOVTAB   DS    0AL4                                                             
         DC    XL2'10',X'01',X'00' DOWNLOAD HEADERS REQUEST                     
         DC    XL2'20',X'03',X'00' DOWNLOAD UNITS                               
         DC    XL2'24',X'06',X'00' DRAFT UNITS REQUEST                          
         DC    XL2'26',X'08',X'00' SPLIT UNITS REQUEST                          
         DC    XL2'28',X'12',X'00' COPY OR TRANU REQUEST                        
         DC    XL2'30',X'07',X'00' UNIT UPLOAD                                  
         DC    XL2'40',X'01',X'00' PROGRAM SEARCH REQUEST                       
         DC    XL2'44',X'01',X'00' PROGRAM DETAIL LOOKUP                        
         DC    XL2'48',X'05',X'00' CABLE PROGRAM ADD REQUEST                    
         DC    XL2'52',X'30',X'00' INVENTORY HEADER LOOKUP                      
         DC    XL2'53',X'32',X'00' I2/COMMENTS                                  
         DC    XL2'54',X'02',X'00' NTI PROGRAM NAME LOOKUP                      
         DC    XL2'56',X'01',X'00' IGNORE FEATURES FOR INVOICE                  
         DC    XL2'57',X'11',X'00' HISTORY RECORD LOOKUP                        
         DC    XL2'59',X'01',X'00' CABLE MATCHMAKERS (DEFAULT INFO)             
         DC    XL2'5C',X'01',X'00' CABLE MATCHMAKERS (DEFAULT INFO)             
         DC    XL2'60',X'01',X'00' WIZARD UNIT INFO                             
         DC    XL2'70',X'02',X'00' TIME PERIOD TREND (RSEARCH)                  
         DC    XL2'72',X'02',X'00' PROGRAM PERIOD TREND (RSEARCH)               
         DC    XL2'74',X'02',X'00' PROGRAM RANKER (RSEARCH)                     
         DC    XL2'76',X'02',X'00' SHARE/HUT ANALYSIS (RSEARCH)                 
         DC    XL2'78',X'09',X'00' PHEADER/EHEADER REQUEST                      
         DC    XL2'7A',X'01',X'00' HISTORY RECORD LOOKUP                        
         DC    XL2'7C',X'52',X'00' SEARCH PACAKGE                               
         DC    XL2'80',X'01',X'00' FRONTRUNNER (DEFAULT INFO)                   
         DC    XL2'82',X'50',X'00' FRONTRUNNER (PASS UNIVERSE/PROGRAM)          
         DC    XL2'84',X'50',X'00' FRONTRUNNER (PROGRAM/DEMO LOOKUP)            
*******  DC    XL2'86',X'50',X'00' FRONTRUNNER (PACKAGE ADD)                    
         DC    XL2'91',X'52',X'00' FRONTRUNNER (PACKAGE/UNIT ADD)               
         DC    XL2'92',X'52',X'00' FRONTRUNNER (PACKAGE ADD)                    
         DC    XL2'93',X'52',X'00' PACKAGE CHANGE/ADD                           
         DC    XL2'FD',X'FF',X'00' VERSION CODES                                
         DC    XL2'FE',X'FF',X'00' VERSION CODES                                
         DC    X'0100',X'04',X'00' MASTER/STATION INFO                          
         DC    X'0134',X'27',X'00' CABLE MM - CHANGE UNIT REQUEST               
         DC    X'0135',X'27',X'00' CABLE MM - DELETE UNIT REQUEST               
         DC    X'0136',X'27',X'00' CABLE MM - ADD UNIT REQUEST                  
         DC    X'0137',X'27',X'00' CABLE MM - E-MAIL RNO SPOTS                  
         DC    X'0138',X'27',X'00' CABLE MM - MATCHING COMPLETE                 
         DC    X'0142',X'01',X'00' BA RULES DOWNLOAD                            
         DC    X'0144',X'01',X'00' LIMIT RECORD DOWNLOAD                        
         DC    X'0145',X'01',X'00' PROGRAM INFO REQUEST                         
SOVTABX  EQU   *                                                                
*                                                                               
NOSAVTAB DC    XL2'52'                                                          
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* CONTROL RECEIVED HERE WHEN FALINK HAS RECEIVED DATA               *           
*===================================================================*           
         SPACE 1                                                                
RECEIVE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SVINITSW,0          RESET INIT SWITCH                            
         MVC   AGETDATA,0(R1)      SAVE FALINK ROUTINE ADDRESS                  
*                                                                               
RCV10    GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BL    RCVERR              FALINK ERROR                                 
         BH    RCVX                END OF DATA                                  
*                                                                               
         CLI   FPARMS,0            HEADER?                                      
         BNE   RCV20                                                            
         SPACE 1                                                                
*====================================================================           
* PRCHDR - PROCESS HEADER ELEMENT                                               
*====================================================================           
         SPACE 1                                                                
         L     R6,FPARMS                                                        
         USING MHELD,R6            R6=A(HEADER ENTRY)                           
*                                                                               
         OC    SVRCVEL,SVRCVEL     TEST FIRST ELEMENT                           
         BNZ   RCV10                                                            
         CLC   MHCODE,=X'00FE'     IGNORE FE/FD ELEMS                           
         BE    RCV10                                                            
         CLC   MHCODE,=X'00FD'                                                  
         BE    RCV10                                                            
         MVC   SVRCVEL,MHCODE      SAVE FIRST RECEIVE ELEMENT                   
*                                                                               
         B     RCV10                                                            
         EJECT                                                                  
*====================================================================           
* PROCESS DATA FIELD                                                            
*====================================================================           
         SPACE 1                                                                
RCV20    L     R6,FPARMS                                                        
         USING MDELD,R6            R6=A(DATA ENTRY)                             
*                                                                               
         L     R4,FPARMS+4         GET DATA ADDRESS                             
         L     R5,FPARMS+8         GET DATA LENGTH                              
*                                                                               
         LTR   R5,R5               CHECK ZERO INPUT                             
         BZ    RCV10                                                            
*                                                                               
         AHI   R5,-1               SET FOR EX                                   
*                                                                               
         ICM   RF,15,MDUSER        GET PROCESS DATA ROUTINE ADDRESS             
         A     RF,BASERELO                                                      
         BASR  RE,RF               NO RETURN EXPECTED - TRACE USE ONLY          
         DC    H'0'                                                             
*                                                                               
RCVX     CR    RB,RB                                                            
RCVEXIT  XIT1                                                                   
*                                                                               
RCVERR   GOTO1 SENDMSG             RETURN FALINK ERROR                          
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* DATA RECEIVE ROUTINES                                           *             
*=================================================================*             
         SPACE 1                                                                
DUMMY    B     RCV10               DUMMY ROUTINE - DO NOTHING                   
*                                                                               
*  CLEAR OPTIONAL FIELDS FOR LIMIT RECORD LOOKUP                                
CLRLIMIT XC    QPRD,QPRD                                                        
         MVI   BEST,0                                                           
         B     RCV10                                                            
*                                                                               
INNWEEKS L     R3,ANETBLK                                                       
         USING BUYDRFTD,R3                                                      
         EX    R5,*+4                                                           
         MVC   RDRWEEKS(0),0(R4)                                                
         OC    RDRWEEKS,SPACES                                                  
         B     RCV10                                                            
         DROP  R3                                                               
*                                                                               
INPWEEK  L     R3,ANETBLK                                                       
         USING BUYDRFTD,R3                                                      
         EX    R5,*+4                                                           
         MVC   RDRPWEEK(0),0(R4)                                                
         OC    RDRPWEEK,SPACES                                                  
         B     RCV10                                                            
         DROP  R3                                                               
*                                                                               
* INKEY TAKES THE KEY INFORMATION FROM THE BUY AND BREAK                        
* IT OUT INTO IT INDIVIDUAL COMPONENTS AS OF NOW THE DELIMETER                  
* FOR THE FIELDS C':'. THE FIELDS ARE IN THE FOLLOWING SEQUENCE.                
* CLIENT:ESTIMATE:NETWORK:PACKAGE:PROGRAM:BUY DATE:LINE NUMBER:                 
* REASON CODE                                                                   
* R3 POINTS TO FIRST OUTPUT FIELD                                               
*                                                                               
INKEYDR  L     R3,ANETBLK           DRAFT BUY KEY VALIDATION                    
         USING BUYDRFTD,R3                                                      
         MVC   RDRTYPE,=CL2'DR'                                                 
         OC    RDRSTAT,SVEDEMSW     SET DEMO OPTIONS                            
         OC    RDRSTAT2,SVSTATSW    SET DEMO OPTIONS 2                          
         LA    R3,3(R3)             R3 POINTS TO FIRST KEY FIELD                
         BRAS  RE,VALKEY                                                        
         B     RCV10                                                            
         DROP  R3                                                               
*                                                                               
INKEYDE  L     R3,ANETBLK           DELETE BUY KEY VALIDATION                   
         USING BUYUPLDD,R3                                                      
         MVC   RUPTYPE,=CL2'DE'                                                 
         LA    R3,3(R3)             R3 POINTS TO FIRST KEY FIELD                
         BRAS  RE,VALKEY                                                        
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
         DROP  R3                                                               
*                                                                               
INKEYCH  L     R3,ANETBLK           CHANGE BUY KEY VALIDATION                   
         USING BUYUPLDD,R3                                                      
         OC    RUPSTAT,SVEDEMSW     SET DEMO OPTIONS                            
         OC    RUPSTAT2,SVSTATSW    SET DEMO OPTIONS 2                          
         OC    RUPST1,SVSTEWST      SET INDICATORS FROM STEWARD                 
         MVC   RUPTYPE,=CL2'CH'                                                 
         LA    R3,3(R3)             R3 POINTS TO FIRST KEY FIELD                
         BRAS  RE,VALKEY                                                        
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
         DROP  R3                                                               
*                                                                               
INKEYBU  L     R3,ANETBLK           BUY UNIT KEY VALIDATION                     
         USING BUYUPLDD,R3                                                      
         OC    RUPSTAT,SVEDEMSW     SET DEMO OPTIONS                            
         OC    RUPSTAT2,SVSTATSW    SET DEMO OPTIONS 2                          
         OC    RUPST1,SVSTEWST      SET INDICATORS FROM STEWARD                 
         MVC   RUPTYPE,=CL2'BU'                                                 
         LA    R3,3(R3)             R3 POINTS TO FIRST KEY FIELD                
         BRAS  RE,VALKEY                                                        
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
         DROP  R3                                                               
*                                                                               
INKEYRC  L     R3,ANETBLK           REFRESH CHANGE KEY VALIDATION               
         USING BUYUPLDD,R3                                                      
         OC    RUPSTAT,SVEDEMSW     SET DEMO OPTIONS                            
         OC    RUPSTAT2,SVSTATSW    SET DEMO OPTIONS 2                          
         MVC   RUPTYPE,=CL2'RC'                                                 
         LA    R3,3(R3)             R3 POINTS TO FIRST KEY FIELD                
         BRAS  RE,VALKEY                                                        
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
         DROP  R3                                                               
*                                                                               
INKEYRB  L     R3,ANETBLK           REFRESH BUY KEY VALIDATION                  
         USING BUYUPLDD,R3                                                      
         OC    RUPSTAT,SVEDEMSW     SET DEMO OPTIONS                            
         OC    RUPSTAT2,SVSTATSW    SET DEMO OPTIONS 2                          
         MVC   RUPTYPE,=CL2'RB'                                                 
         LA    R3,3(R3)             R3 POINTS TO FIRST KEY FIELD                
         BRAS  RE,VALKEY                                                        
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
*                                                                               
INKEYHS  L     R3,ANETBLK           REFRESH BUY KEY VALIDATION                  
         USING BUYUPLDD,R3                                                      
         MVC   RUPTYPE,=CL2'HS'                                                 
         LA    R3,3(R3)             R3 POINTS TO FIRST KEY FIELD                
         BRAS  RE,VALKEY                                                        
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
*                                                                               
INKEYFR  LA    R3,SVFRCLI           R3 POINTS TO FIRST KEY FIELD                
         BRAS  RE,VALKEY                                                        
         B     RCV10                                                            
         DROP  R3                                                               
*                                                                               
*INKEYPK  L     R3,ANETBLK           BUY PACKAGE                                
*         USING PKGUPLDD,R3                                                     
*         LHI   R0,NPURRLEN                                                     
*         STCM  R0,3,TSARRCLN        SET OVERRIDE LENGTH                        
*         MVC   RPKTYPE,=CL2'PB'                                                
*         LA    R3,3(R3)             R3 POINTS TO FIRST KEY FIELD               
*         BAS   RE,VALKEY                                                       
*         MVC   SVFRCLI(13),RPKCLI                                              
*         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                  
*         B     RCV10                                                           
*         DROP  R3                                                              
*                                                                               
*INKEYFB  L     R3,ANETBLK           BUY PACKAGE                                
*         USING NPURECD,R3                                                      
*                                                                               
* SET UP FINAL RECORD SET UP                                                    
*         MVC   NPURTYPE,=CL2'FB'                                               
*         MVC   NPGDEL(2),=XL2'0112'                                            
*         MVC   NPUBEL(2),=XL2'1240'                                            
*         MVC   NPUCEL(2),=XL2'1477'                                            
*         MVC   NPUND(2),=XL2'1698'                                             
*         MVC   NPBKD(7),=XL7'5D07C5E5D5580E'                                   
*                                                                               
*         LHI   R0,NPURRLEN                                                     
*         STCM  R0,3,TSARRCLN        SET OVERRIDE LENGTH                        
*         MVC   NPURTYPE,=CL2'FB'                                               
*         LA    R3,3(R3)             R3 POINTS TO FIRST KEY FIELD               
*         BAS   RE,VALKEY                                                       
*         OC    NPUCLI(13),NPUCLI                                               
*         BNZ   *+10                                                            
*         MVC   NPUCLI(13),SVFRCLI   GET DEFAULT INFORMATION                    
*         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                  
*         B     RCV10                                                           
*         DROP  R3                                                              
*                                                                               
INKEYID  L     RE,DPOINTER         SAVE FIRST/NEXT INVOICE DETAIL KEY           
         MVC   0(L'RUPMAKEY,RE),0(R4)                                           
         AHI   RE,L'RUPMAKEY                                                    
         ST    RE,DPOINTER                                                      
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         B     RCV10                                                            
*                                                                               
* DRAFT BUY DATES                                                               
*                                                                               
INDRDATS GOTO1 VDATCON,DMCB,(4,0(R4)),(2,WORK)    MAKE COMPRESSED               
         L     RE,ANETBLK                                                       
         USING BUYDRFTD,RE                                                      
         LA    RE,RDRDATES                                                      
         LA    RF,52                                                            
*                                                                               
INDRDT20 CLI   0(RE),X'40'                                                      
         BH    INDRDT50                                                         
         MVC   0(2,RE),WORK                                                     
         B     RCV10                                                            
INDRDT50 LA    RE,2(RE)                                                         
         BCT   RF,INDRDT20                                                      
         MVC   ERROR,=AL2(115)     DRAFT BUY TABLE OVERFLOW                     
         B     RCVERR                                                           
         DROP  RE                                                               
         SPACE                                                                  
*BUY DRAFT RECORD DAY                                                           
INDRDAY  L     RE,ANETBLK                                                       
         USING BUYDRFTD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RDRDAY(0),0(R4)                                                  
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY DRAFT RECORD START TIME                                                    
INDRSTM  L     RE,ANETBLK                                                       
         USING BUYDRFTD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RDRTIME(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RDRTIMEL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY DRAFT RECORD END TIME                                                      
INDRETM  L     RE,ANETBLK                                                       
         USING BUYDRFTD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RDRTIME+2(0),0(R4)                                               
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY DRAFT RECORD LENGTH                                                        
INDRLEN  L     RE,ANETBLK                                                       
         USING BUYDRFTD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RDRLEN(0),0(R4)                                                  
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*                                                                               
* MISSED UNITS                                                                  
*                                                                               
INBYSMKG BRAS  RE,INBYSMK2                                                      
         B     RCV10                                                            
*&&DO                                                                           
* MISSED UNITS                                                                  
*                                                                               
INBYSMKG L     R6,ANETBLK                                                       
         USING BUYUPLDD,R6                                                      
         LA    RE,RUPMISS                                                       
         LA    RF,40                                                            
         CLI   0(RE),X'40'                                                      
         BNH   INMISS80                                                         
*                                                                               
INMISS20 LA    RE,1(RE)                                                         
         CLI   0(RE),X'40'                                                      
         BH    INMISS50                                                         
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         ZIC   RF,RUPMISSL          UPDATE THE LENGTH                           
         LA    RF,1(RF)                                                         
         STCM  RF,1,RUPMISSL                                                    
         B     INMISS80                                                         
INMISS50 BCT   RF,INMISS20                                                      
         DC    H'0'                                                             
*                                                                               
INMISS80 EX    R5,*+4                                                           
         MVC   0(0,RE),0(R4)        MOVE OUT INFORMATION                        
*                                                                               
         LA    R5,1(R5)                                                         
         ZIC   RF,RUPMISSL          UPDATE THE LENGTH                           
         AR    RF,R5                                                            
         STCM  RF,1,RUPMISSL                                                    
         B     RCV10                                                            
         DROP  R6                                                               
*&&                                                                             
*                                                                               
*                                                                               
*ALDATES NTR1                                                                   
*        L     RF,AIO4                                                          
*        LA    RF,8(RF)             OUTPUT FIELD                                
*        LR    R1,R5                                                            
*        LA    R1,1(1)              LENGTH OF MOVE                              
*        LR    RE,R4                INPUT FIELD                                 
*        MOVE  ((RF),(R1)),(RE)                                                 
*        L     RF,AIO4                                                          
*        LA    R5,9(R5)                                                         
*        STCM  R5,1,0(RF)                                                       
*                                                                               
*        L     R5,ANETBLK                                                       
*        USING BUYDRFTD,R5                                                      
*        LA    R5,RDRDATES                                                      
*        L     R2,AIO4                                                          
*        ST    R2,FADDR                                                         
*        XC    FTERM,FTERM                                                      
*        XC    FLAST,FLAST                                                      
*        MVI   FTERM,C':'                                                       
*                                                                               
*ALDT50  GOTO1 FVAL,0                                                           
*                                                                               
*        GOTO1 VDATCON,DMCB,(4,FLD),(2,0(R5))    MAKE COMPRESSED                
*                                                                               
*        CLI   FSTOP,X'FF'          LAST ONE                                    
*        BE    VALDTEX              EXIT                                        
*        LA    R5,2(R5)                                                         
*        B     VALDT50                                                          
*                                                                               
*ALDTEX  B     RCVEXIT                                                          
*        DROP  R5                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*  COPYU PROGRAM FIELD VALIDATIONS                                              
*                                                                               
*                                                                               
*COPYU OR TRANU                                                                 
INCOPTR  L     RE,ANETBLK                                                       
         USING CPYUPLDD,RE                                                      
         MVC   CPUTYPE,=CL2'CU'     SET UP AS COPYU                             
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*COPY TRAFFIC                                                                   
INCOCTR  L     RE,ANETBLK                                                       
         USING CPYUPLDD,RE                                                      
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    CPUSTAT1,X'80'        SET TO COPY TRAFFIC INFO                   
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*COPY BRANDS                                                                    
INCOCBR  L     RE,ANETBLK                                                       
         USING CPYUPLDD,RE                                                      
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    CPUSTAT1,X'40'        SET TO COPY TRAFFIC INFO                   
         BNE   RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*START DATE FILTER                                                              
INCOSTDT L     RE,ANETBLK                                                       
         USING CPYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPUSTDAT(0),0(R4)                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*END DATE FILTER                                                                
INCOENDT L     RE,ANETBLK                                                       
         USING CPYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPUENDAT(0),0(R4)                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*PRODUCT FILTER                                                                 
INCOPROD L     RE,ANETBLK                                                       
         USING CPYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPUPROD(0),0(R4)                                                 
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*LENGTH FILTER                                                                  
INCOLEN  L     RE,ANETBLK                                                       
         USING CPYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPULEN(0),0(R4)                                                  
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*COST FILTER                                                                    
*  C'O' - TRANSFER ONLY ZERO COST UNITS                                         
*  C'E' - EXCLUDE ZERO COST UNITS                                               
INCOZCT  L     RE,ANETBLK                                                       
         USING CPYUPLDD,RE                                                      
         CLI   0(R4),C'O'                                                       
         BNE   *+8                                                              
         OI    CPUSTAT1,X'20'        ONLY ZERO DOLLAR UNITS                     
         CLI   0(R4),C'E'                                                       
         BNE   *+8                                                              
         OI    CPUSTAT1,X'10'        EXCLUDE ZERO DOLLAR UNITS                  
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*FROM UNIT INFO                                                                 
INKCPUF  L     R3,ANETBLK           BUY UNIT KEY VALIDATION                     
         USING CPYUPLDD,R3                                                      
         LA    R3,CPUCLI                                                        
         BRAS  RE,VALKEY                                                        
         B     RCV10                                                            
         DROP  R3                                                               
         SPACE                                                                  
*TO UNIT INFO                                                                   
INKCPUT  L     R3,ANETBLK           BUY UNIT KEY VALIDATION                     
         USING CPYUPLDD,R3                                                      
         LA    R3,CPTCLI                                                        
         BRAS  RE,VALKEY                                                        
         L     R3,ANETBLK           BUY UNIT KEY VALIDATION                     
         XC    CPTBDATE(11),CPTBDATE                                            
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
         DROP  R3                                                               
         SPACE                                                                  
*                                                                               
*  BUY CABLE PROGRAM FIELD VALIDATIONS                                          
*  THE FOLLOWING VALIDATIONS ALL HAVE THE SAME INPUT FORMAT                     
*                                                                               
         SPACE                                                                  
*CABLE PROGRAM RECORD PROGRAM NAME                                              
INACPPRG L     RE,ANETBLK                                                       
         USING CPRGRECD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPRPNAM(0),0(R4)                                                 
         MVC   CPRRTYPE,=CL2'CP'                                                
         LHI   R0,CPRRLEN                                                       
         STCM  R0,3,TSARRCLN        SET OVERRIDE LENGTH                         
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*CABLE PROGRAM RECORD START TIME                                                
INACPSTM L     RE,ANETBLK                                                       
         USING CPRGRECD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPRSTIM(0),0(R4)                                                 
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*CABLE PROGRAM RECORD END TIME                                                  
INACPETM L     RE,ANETBLK                                                       
         USING CPRGRECD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPRETIM(0),0(R4)                                                 
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*CABLE PROGRAM RECORD ROTATION TIME                                             
INACPROT L     RE,ANETBLK                                                       
         USING CPRGRECD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPRROT(0),0(R4)                                                  
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*CABLE PROGRAM RECORD MIRROR CODE                                               
INACPMIR L     RE,ANETBLK                                                       
         USING CPRGRECD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPRMIR(0),0(R4)                                                  
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*CABLE PROGRAM RECORD NETWORK                                                   
INACPNET L     RE,ANETBLK                                                       
         USING CPRGRECD,RE                                                      
         EX    R5,*+4                                                           
         MVC   CPRPNET(0),0(R4)                                                 
         B     RCV10                                                            
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
*  BUY RECORD DETAIL FIELD VALIDATIONS                                          
*  THE FOLLOWING VALIDATIONS ALL HAVE THE SAME INPUT FORMAT                     
*                                                                               
         SPACE 2                                                                
*BUY NEW ACTUAL COST                                                            
INBYNACT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNACT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNACTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD ACTUAL COST                                                            
INBYOACT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOACT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOACTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW ASSIGNED COST                                                          
INBYNASS L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNASS(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNASSL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD ASSIGNED COST                                                          
INBYOASS L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOASS(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOASSL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW INTEGRATION COST                                                       
INBYNINT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNINT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNINTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD INTEGRATION COST                                                       
INBYOINT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOINT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOINTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW DAY                                                                    
INBYNDAY L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNDAY(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNDAYL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD DAY                                                                    
INBYODAY L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPODAY(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPODAYL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW ROTATION                                                               
INBYNROT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNROT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNROTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD ROTATION                                                               
INBYOROT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOROT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOROTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW SPECIAL REP                                                            
INBYNREP L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNREP(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNREPL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD SPECIAL REP                                                            
INBYOREP L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOREP(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOREPL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*                                                                               
*BUY NEW WINDOW DATE VALIDATION                                                 
*                                                                               
INBYNWIN BRAS  RE,INBNWIN                                                       
         B     RCV10                                                            
*                                                                               
*BUY OLD WINDOW DATE VALIDATION                                                 
*                                                                               
INBYOWIN BRAS  RE,INBOWIN                                                       
         B     RCV10                                                            
*                                                                               
*BUY NEW COMSCORE SERIES NUMBER VALIDATION                                      
*                                                                               
INBYNCS# BRAS  RE,INBNCS#                                                       
         B     RCV10                                                            
*                                                                               
*BUY OLD COMSCORE SERIES NUMBER VALIDATION                                      
*                                                                               
INBYOCS# BRAS  RE,INBOCS#                                                       
         B     RCV10                                                            
*                                                                               
*BUY NEW AIR NETWORK VALIDATION                                                 
*                                                                               
INBYNAIR BRAS  RE,INBNAIR                                                       
         B     RCV10                                                            
*                                                                               
*BUY OLD AIR NETWORK VALIDATION                                                 
*                                                                               
INBYOAIR BRAS  RE,INBOAIR                                                       
         B     RCV10                                                            
*                                                                               
*  BUY NEW DATE                                                                 
INBYNDAT L     R3,ANETBLK                                                       
         USING BUYUPLDD,R3                                                      
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,RUPNDAT)    MAKE COMPRESSED              
         B     RCV10                                                            
         DROP  R3                                                               
         SPACE 2                                                                
*  BUY OLD DATE                                                                 
*INBYODAT L     R3,ANETBLK                                                      
*         USING BUYUPLDD,R3                                                     
*         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                       
*         GOTO1 VDATCON,DMCB,(0,DUB),(2,RUPODAT)    MAKE COMPRESSED             
*         B     RCV10                                                           
*         DROP  R3                                                              
*         SPACE 2                                                               
*BUY NEW START TIME                                                             
INBYNSTM L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNTIM(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNTIML                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY NEW END TIME                                                               
INBYNETM L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNTIM+2(0),0(R4)                                               
         LA    R5,1(R5)                                                         
         ZIC   RF,RUPNTIML                                                      
         AR    R5,RF                                                            
         STCM  R5,1,RUPNTIML                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD TIME                                                                   
INBYOSTM L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOTIM(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOTIML                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY OLD END TIME                                                               
INBYOETM L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOTIM+2(0),0(R4)                                               
         LA    R5,1(R5)                                                         
         ZIC   RF,RUPOTIML                                                      
         AR    R5,RF                                                            
         STCM  R5,1,RUPOTIML                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW AFFID TIME                                                             
INBYNATM L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNATM(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNATML                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY OLD AFFID TIME                                                             
INBYOATM L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOATM(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOATML                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY NEW LENGTH                                                                 
INBYNLEN L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNLEN(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNLENL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD LENGTH                                                                 
INBYOLEN L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOLEN(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOLENL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW PRODUCT                                                                
INBYNPRD L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNPRD(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNPRDL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD PRODUCT                                                                
INBYOPRD L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOPRD(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOPRDL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
*BUY NEW NTI CODE                                                               
INBYNNTI L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNNTI(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNNTIL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD NTI CODE                                                               
INBYONTI L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPONTI(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPONTIL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW HUT CODE                                                               
INBYNHUT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNHUT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNHUTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD HUT CODE                                                               
INBYOHUT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOHUT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOHUTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW SHARE                                                                  
INBYNSHR L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNSHR(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNSHRL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD SHARE                                                                  
INBYOSHR L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOSHR(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOSHRL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY HOMES ACTUAL RATING                                                        
INBYAHRT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNHRA(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNHRAL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY NEW HOMES RATING                                                           
INBYNHRT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNHRT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNHRTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY OLD HOMES RATING                                                           
INBYOHRT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPOHRT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOHRTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY NEW HOMES IMPRESSION                                                       
INBYNHIM L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNHIM(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNHIML                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*SPECIAL CHARGE SEQUENCE NUMBER                                                 
INBYSSEQ L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    R5,RUPSPCHR                                                      
         LA    RF,5                 MAX NUMBER OF ENTRIES                       
INBYSQ10 CLI   0(R5),0                                                          
         BE    INBYSQ20                                                         
         LA    R5,21(R5)                                                        
         BCT   RF,INBYSQ10                                                      
         DC    H'0'                                                             
INBYSQ20 MVC   0(1,R5),0(R4)                                                    
         CLI   0(R4),0              CHECK IF ADD ACTION                         
         BNE   *+8                                                              
         MVI   0(R5),C'A'           SET ADD FLAG                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*SPECIAL CHARGE DELETE STATUS                                                   
INBYSACT BAS   RE,SPCHFIND                                                      
         MVC   1(1,R5),0(R4)                                                    
         B     RCV10                                                            
         SPACE                                                                  
*SPECIAL CHARGE TYPE FIELD                                                      
INBYSTYP BAS   RE,SPCHFIND                                                      
         MVC   2(1,R5),0(R4)                                                    
         B     RCV10                                                            
         SPACE                                                                  
*SPECIAL CHARGE AMOUNT FIELD                                                    
INBYSAMT BAS   RE,SPCHFIND                                                      
         MVC   3(4,R5),0(R4)                                                    
         B     RCV10                                                            
         SPACE                                                                  
*SPECIAL CHARGE REP FIELD                                                       
INBYSCRP BAS   RE,SPCHFIND                                                      
*!!!!    EDIT  (B2,0(R4)),(3,FULL),FILL=0                                       
         MVC   FULL(3),0(R4)       INPUT SHOULD NOT BE EDITED                   
         MVC   7(3,R5),FULL                                                     
         GOTO1 VALIREP              VALIDATE THE REP                            
         B     RCV10                                                            
         SPACE                                                                  
*SPECIAL CHARGE COMMISION FIELD                                                 
INBYSCOM BAS   RE,SPCHFIND                                                      
         MVC   10(1,R5),0(R4)                                                   
         B     RCV10                                                            
         SPACE                                                                  
*SPECIAL CHARGE STATION FIELD                                                   
INBYSSTA BAS   RE,SPCHFIND                                                      
         MVC   11(4,R5),0(R4)                                                   
         B     RCV10                                                            
         SPACE                                                                  
*SPECIAL CHARGE BILLED PRODUCT FIELD                                            
INBYSBPR BAS   RE,SPCHFIND                                                      
         MVC   15(3,R5),0(R4)                                                   
         OC    15(3,R5),SPACES                                                  
         B     RCV10                                                            
         SPACE                                                                  
*SPECIAL CHARGE TRAFFIC PRODUCT FIELD                                           
INBYSTPR BAS   RE,SPCHFIND                                                      
         MVC   18(3,R5),0(R4)                                                   
         OC    18(3,R5),SPACES                                                  
         B     RCV10                                                            
         SPACE                                                                  
*                                                                               
* ROUTINE IS USED TO FIND CORRECT POSITION IN                                   
* SPECIAL CHARGE TABLE TO ADD INFORMATION                                       
* R5=POINTS TO THE CORRECT SLOT ON RETURN                                       
*                                                                               
SPCHFIND LR    R0,RE                                                            
         L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    R5,RUPSPCHR+84       POINT TO LAST ENTRY                         
         LA    RF,4                 MAX NUMBER OF ENTRIES                       
SPCHFD10 CLI   0(R5),0                                                          
         BNE   SPCHFD20                                                         
         S     R5,=F'21'            GET PREVIOUS ENTRY                          
         BCT   RF,SPCHFD10                                                      
         CLI   0(R5),0              IS FIRST ENTRY FILLED                       
         BNE   *+6                  YES                                         
         DC    H'0'                                                             
SPCHFD20 LR    RE,R0                                                            
         BR    RE                                                               
         LTORG                                                                  
         SPACE                                                                  
*BUY PROGRAM NAME                                                               
INBYNPNM L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNPNM(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNPNML                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*BUY COMBO CODE                                                                 
* FIRST 3 POSITION ARE ALPHA                                                    
* LAST 3 POSITIONS ARE NUMERIC                                                  
INBYCMBO L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
*&&DO                                                                           
         MVC   RUPNCBO(3),0(R4)                                                 
         CLI   RUPNCBO,X'40'                                                    
         BH    *+12                                                             
         MVI   RUPNCBO,C'^'                                                     
         B     RCV10                                                            
         PACK  DUB,3(3,R4)                                                      
         CVB   R0,DUB                                                           
         STC   R0,RUPNCBO+3                                                     
         B     RCV10                                                            
         DROP  RE                                                               
*&&                                                                             
         MVC   RUPNCBO2,0(R4)      COMBO IS NOW FREEFORM (SPEC-43030)           
         CLI   RUPNCBO2,X'40'      REMOVE COMBO CODE?                           
         BH    RCV10                                                            
         MVI   RUPNCBO2,C'^'                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY POD CODE                                                                   
INBYPOD  L     RF,ANETBLK                                                       
         USING BUYUPLDD,RF                                                      
         EX    R5,*+4                                                           
         MVC   RUPNPOD(0),0(R4)                                                 
         OC    RUPNPOD,RUPNPOD                                                  
         BNZ   RCV10                                                            
         MVI   RUPNPOD,C'^'                                                     
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*BUY M/G DEMO                                                                   
INBYNMGD L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNMGD(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNMGDL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY PRE-EMPT                                                                   
INBYNPRE L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNPRE(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNPREL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY BUY TYPE                                                                   
INBYNBYT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNBYT(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNBYTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY ADU                                                                        
INBYNADU L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNADU(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNADUL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY BILLBOARD                                                                  
INBYNBLB L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNBLB(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNBLBL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SUB DAYPART                                                                    
INBYSDPT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPNSDP(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNSDPL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SPLIT LENGTH TABLE MAX ENTRIES 10                                              
INSPLLEN L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    RF,RUPSPLTB                                                      
         LA    R6,10                                                            
*                                                                               
INSPL20  CLI   0(RF),0                                                          
         BE    INSPL50                                                          
         CLI   0(RF),X'FF'                                                      
         BE    INSPL50                                                          
         LA    RF,1(RF)                                                         
         BCT   R6,INSPL20                                                       
         DC    H'0'                 TOO MANY ENTRIES                            
INSPL50  MVC   0(1,RF),0(R4)                                                    
         MVI   1(RF),X'FF'                                                      
         B     RCV10                                                            
         DROP  RE                                                               
*BUY PROGRAM RATING CODE TYPE                                                   
INBYPRTG L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPPRTGC(0),0(R4)                                                
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPPRTGL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY PROGRAM TIER                                                               
INBYTIER L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPPTIER(0),0(R4)                                                
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPPTIRL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY DEAL/CONTRACT SERIAL NUMBER                                                
*  THIS INFO WILL BE STORED RUPCOMT2                                            
*  BECAUSE OF THIS YOU CANNOT USE COMMENT2                                      
*  AND ALSO USE DEAL/CONTRACT/SERIAL NUMBER                                     
INBYDSC  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPCOMT2(6),=CL6'DSC###'                                         
         EX    R5,*+4                                                           
         MVC   RUPCOMT2+6(0),0(R4)                                              
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*  THIS INFO WILL BE STORED RUPCOMT2+40                                         
*  BECAUSE OF THIS YOU CANNOT USE COMMENT2                                      
*  AND ALSO USE FLIGHT MAINTENANCE                                              
INBYFLT  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPCOMT2+40(6),=CL6'FLT###'                                      
         EX    R5,*+4                                                           
         MVC   RUPCOMT2+46(0),0(R4)                                             
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE                                                                  
*GAP AFFID TIME                                                                 
INBYGPAF L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPGAPAF(0),0(R4)                                                
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPGAPAL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY PROGRAM CONTENT                                                            
INBYPCNT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPPCONT(0),0(R4)                                                
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPPCNTL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY PROGRAM NEW OR RETURNING                                                   
INBYNEW  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPPNEW(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPPNEWL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SET ASSIGNED COST TO UNFROZEN                                                  
INBYFRZ  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         OI    RUPSTAT,X'40'                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SET IGNORE FOR INVOICE                                                         
INBYIGF  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         NI    RUPMIGNO,FF-RUPMIGFQ                                             
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    RUPMIGNO,RUPMIGFQ                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SET IGNORE FOR INVOICE                                                         
INBYIGC  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         NI    RUPMIGNO,FF-RUPMIGCQ                                             
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    RUPMIGNO,RUPMIGCQ                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SET IGNORE FOR INVOICE                                                         
INBYIGIC L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         NI    RUPMIGNO,FF-RUPMIGIQ                                             
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    RUPMIGNO,RUPMIGIQ                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SET IGNORE FOR INVOICE                                                         
INBYIGL  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         NI    RUPMIGNO,FF-RUPMIGLQ                                             
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    RUPMIGNO,RUPMIGLQ                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SET IGNORE FOR INVOICE                                                         
INBYIGS  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         NI    RUPMIGNO,FF-RUPMIGSQ                                             
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    RUPMIGNO,RUPMIGSQ                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SET IGNORE FOR INVOICE                                                         
INBYIGT  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         NI    RUPMIGNO,FF-RUPMIGTQ                                             
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    RUPMIGNO,RUPMIGTQ                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SET MIRROR CODE TO YES                                                         
INBYMIR  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         MVI   RUPMMIRO,RUPMMIYQ                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*SET HD/CENTERCUT                                                               
INBYHCC  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   SVHDCC,0(R4)                                                     
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY EST=Y OPTION                                                               
INBYESTY L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    RUPSTAT,X'80'                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*INVOICE NUMBER                                                                 
INBYINVC L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPINV(7),=CL7'INVOICE'                                          
         MVC   RUPINV+7(10),SPACES        NETWORK SAVE                          
         EX    R5,*+4                                                           
         MVC   RUPINV+7(0),0(R4)                                                
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*INVOICE NUMBER                                                                 
INBYINTP L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPINVTP(1),0(R4)                                                
         B     RCV10                                                            
         DROP  RE                                                               
*PACKU INDICATOR                                                                
INBYOPKU L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPPKUI(5),=CL5'PACKU'                                           
         B     RCV10                                                            
         DROP  RE                                                               
*VIEWING STREAM                                                                 
INBYVTYP L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPVSTR(3),SPACES                                                
         EX    R5,*+4                                                           
         MVC   RUPVSTR(0),0(R4)                                                 
         B     RCV10                                                            
         DROP  RE                                                               
*PACKAGE FILTER                                                                 
INBYPFLT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPPKFLT(6),SPACES                                               
         EX    R5,*+4                                                           
         MVC   RUPPKFLT(0),0(R4)                                                
         B     RCV10                                                            
         DROP  RE                                                               
*UNIVERSE                                                                       
INBYUNIV L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPUNIV(0),0(R4)                                                 
         SRP   RUPUNIV(3),1,0       REMOVE PACKED SIGN                          
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPUNIVL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
*AUDIT NAME                                                                     
INBYANAM L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPANME(16),SPACES                                               
         EX    R5,*+4                                                           
         MVC   RUPANME(0),0(R4)                                                 
         B     RCV10                                                            
         DROP  RE                                                               
*AUDIT CODE                                                                     
INBYACOD L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPACDE(4),SPACES                                                
         EX    R5,*+4                                                           
         MVC   RUPACDE(0),0(R4)                                                 
         B     RCV10                                                            
         DROP  RE                                                               
*BUY HUT ADJUSTMENT                                                             
INBYHTAD L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPHTADJ(0),0(R4)                                                
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPHADJL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
*BUY UNIVERSE PERCENT                                                           
INBYUPCT L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         EX    R5,*+4                                                           
         MVC   RUPUNPCT(0),0(R4)                                                
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPUNPCL                                                    
         B     RCV10                                                            
         DROP  RE                                                               
*BUY COPYSPLIT FLAG                                                             
INBYCOPS L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         OI    RUPSTAT2,X'08'                                                   
         B     RCV10                                                            
         DROP  RE                                                               
*NO NATIONAL FLAG                                                               
INBYNATS L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         OI    RUPSTAT2,X'10'                                                   
         B     RCV10                                                            
         DROP  RE                                                               
*BUY COPYSPLIT PRODUCT                                                          
INBYCSPR L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    RF,RUPCSPCT                                                      
         LA    R1,6                                                             
INCSPR20 CLI   0(RF),0                                                          
         BE    INCSPR30                                                         
         LA    RF,CPYSPPLN(RF)                                                  
         BCT   R1,INCSPR20                                                      
INCSPR30 MVC   0(3,RF),0(R4)                                                    
         OC    0(3,RF),SPACES                                                   
         B     RCV10                                                            
         DROP  RE                                                               
*BUY COPYSPLIT 1ST PRODUCT PERCENTAGE                                           
INBY1PRD MVI   BYTE,1                                                           
         BRAS  RE,CPYSPCT                                                       
         B     RCV10                                                            
*BUY COPYSPLIT FEED PERCENTAGE                                                  
INBYFDPT MVI   BYTE,2                                                           
         BRAS  RE,CPYSPCT                                                       
         B     RCV10                                                            
*BUY COPYSPLIT FEED CODE                                                        
INBYFEED BRAS  RE,CPYSFD                                                        
         B     RCV10                                                            
*BUY COMMENT LINE 1 AND 2                                                       
INBYNCM1 SR    RF,RF                                                            
         B     INBYNCM                                                          
INBYNCM2 LA    RF,60                                                            
INBYNCM  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    R6,RUPCOMT1          FIELD                                       
         AR    R6,RF                                                            
         MVI   0(R6),X'FF'         SET CLEAR COMMENT SWITCH                     
         CLI   0(R4),0             NO INPUT                                     
         BE    RCV10                                                            
         MVC   0(60,R6),SPACES        NETWORK SAVE                              
         EX    R5,*+4                                                           
         MVC   0(0,R6),0(R4)                                                    
         B     RCV10                                                            
         DROP  RE                                                               
         SPACE 2                                                                
*BUY DEMO CATEGORY OVERIDE                                                      
INBYDDSE L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    R6,RUPDEMOS          ESTIMATE OVERRIDES                          
         B     INBYDDSP                                                         
INBYDDSA L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    R6,RUPDEMOA          ACTUAL OVERRIDES                            
         DROP  RE                                                               
*                                                                               
INBYDDSP ZIC   RE,0(R4)                                                         
         MH    RE,=H'3'                                                         
         LA    RF,QDEMOS                                                        
         AR    RF,RE                POINTS TO DEMO                              
*                                                                               
         LA    RE,75                                                            
INBYDD50 CLI   1(R6),0                                                          
         BE    INBYDD80                                                         
         LA    R6,7(R6)                                                         
         BCT   RE,INBYDD50                                                      
         DC    H'0'                                                             
*                                                                               
INBYDD80 MVC   0(3,R6),0(RF)        MOVE IN DEMO CATEGORY EST BLOCK             
         CLI   1(R6),USERMOD        CHECK FOR USER DEMO                         
         BE    RCV10                                                            
         MVI   3(R6),X'FF'          SET NO VALUE INDICATOR EST BLOCK            
         B     RCV10                                                            
         SPACE 2                                                                
*BUY DEMO VALUES                                                                
* RATING                                                                        
INBYARTG MVI   BYTE2,C'A'          ACTUAL DEMO OVERRIDE                         
INBYDRTG MVI   BYTE,C'R'                                                        
         BRAS  RE,MOVEDEM                                                       
         B     RCV10                                                            
* VPH                                                                           
INBYAVPH MVI   BYTE2,C'A'          ACTUAL DEMO OVERRIDE                         
INBYDVPH MVI   BYTE,C'V'                                                        
         BRAS  RE,MOVEDEM                                                       
         B     RCV10                                                            
* IMP                                                                           
INBYAIMP MVI   BYTE2,C'A'          ACTUAL DEMO OVERRIDE                         
INBYDIMP MVI   BYTE,C'T'                                                        
         BRAS  RE,MOVEDEM                                                       
         B     RCV10                                                            
* MATCH UNIT TO AFFID                                                           
INBYAFM  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPMAKEY,0(R4)                                                   
         MVI   RUPMFLAG,RUPMFMAQ                                                
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         B     RCV10                                                            
         SPACE 2                                                                
* UNMATCH UNIT FROM AFFID                                                       
INBYAFU  L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         MVC   RUPMAKEY,0(R4)                                                   
         MVI   RUPMFLAG,RUPMFUAQ                                                
         LHI   R0,RUPRMLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         B     RCV10                                                            
*                                                                               
* MOVE PACKAGE RECORD FIELDS OUT                                                
*                                                                               
*PACKAGE NAME 1                                                                 
INPKGNM1 L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKNAM1                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PACKAGE NAME 2                                                                 
INPKGNM2 L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKNAM2                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
*                                                                               
*BUY TYPE                                                                       
INPKGBTP L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKBTYP                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*DEMO BASE                                                                      
INPKGDTP L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKDBSE                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*INTEGRATION                                                                    
INPKGINT L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKINTG                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PACKAGE COST                                                                   
INPKGCST L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKCOST                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*MASTER PRODUCT                                                                 
INPKGPRD L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKMPRD                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*DAYPART/SET ACTION WRTE RECORD OUT                                             
INBPKDPT L     RF,ANETBLK           BUY ACTION                                  
         USING PKGUPLDD,RF                                                      
         MVC   RPKDPT,0(R4)                                                     
         MVC   RPKTYPE,=CL2'PB'                                                 
         B     PKGDAYPT                                                         
INCPKDPT L     RF,ANETBLK           CHANGE ACTION                               
         USING PKGUPLDD,RF                                                      
         MVC   RPKDPT,0(R4)                                                     
         MVC   RPKTYPE,=CL2'PC'                                                 
         B     PKGDAYPT                                                         
INDPKDPT L     RF,ANETBLK           DELETE ACTION                               
         USING PKGUPLDD,RF                                                      
         MVC   RPKDPT,0(R4)                                                     
         MVC   RPKTYPE,=CL2'PD'                                                 
         B     PKGDAYPT                                                         
*                                                                               
PKGDAYPT LHI   R0,NPURRLEN                                                      
         MVC   RPKCLI(33),SVFRCLI   MOVE KEY FIELDS TO RECORD                   
         STCM  R0,3,TSARRCLN        SET OVERRIDE LENGTH                         
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*SPECIAL REP                                                                    
INPKGSRP L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKSREP                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PACKAGE FILTER                                                                 
INPKGFLT L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKFILT                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PACKAGE V TYPE                                                                 
INPKGVTP L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKVTYPE                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*COMSCORE VIEWING TYPE                                                          
INPKGCVT L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKCVTYP                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*UNIVERSE                                                                       
INPKGUNV L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKUNIV                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
* CUNIVERSE NO LONGER PART OF COMSCORE PROJECT - SCHT 8/17                      
*                                                                               
*CUNIVERSE                                                                      
*INPKGCUN L     RF,ANETBLK                                                      
*         USING PKGUPLDD,RF                                                     
*         LA    RE,RPKCUNIV                                                     
*         B     COMMOVE                                                         
*         DROP  RF                                                              
*         SPACE                                                                 
*                                                                               
*HUT AVERAGE                                                                    
INPKGHAV L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKHAVG                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*HUT CALANDER TYPE                                                              
INPKGFLV L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKHCTYP                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*HUT ADJUSTMENT                                                                 
INPKGHPC L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         SR    RE,RE                                                            
         ICM   RE,3,0(R4)                                                       
*******  AH    RE,=H'10000'                                                     
         STCM  RE,3,RPKHADJ                                                     
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*HUT SCHEME                                                                     
INPKGHSC L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKHSCHM                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*AUDIT CODE                                                                     
INPKGACD L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKAGRP                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*AUDIT NAME                                                                     
INPKGANM L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKANAM                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*STATUS                                                                         
INPKGSTA L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKSTAT                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*OPTIONS                                                                        
INPKGOPT L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKOPTS                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*FEED PERCENTAGE                                                                
INPKGFDP L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKFPCTG                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*LENGTH                                                                         
INPKGLEN L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKLENTH                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*IMPACT PERCENTAGE                                                              
INPKGIMP L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKIMPCT                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PACKAGE CPM                                                                    
INPKGCPM L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKPKCPM                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*UNIVERSE PERCENT                                                               
INPKGUNP L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKUPCTG                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PACKAGE FIELD CHANGE INDICATORS                                                
INPKGCHG L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         LA    RE,RPKVTYPC                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
* MOVE PACKAGE /PLAN INFO OUT                                                   
*                                                                               
*                                                                               
*PERIOD TYPE                                                                    
INFRUPER XC    SVPAKGUA,SVPAKGUA                                                
         XC    SVDEMCAT,SVDEMCAT                                                
         XC    SVDEMGUA,SVDEMGUA                                                
         LA    RE,SVPERTYP                                                      
         B     COMMOVE                                                          
         SPACE                                                                  
*                                                                               
*PACKAGE GUARANTEE                                                              
INFRUGUA CLC   QAGY,=CL2'DU'       GUARANTEES NOT ALLOWED FOR MEDIAVEST         
         BE    RCV10                                                            
         LA    RE,SVPAKGUA                                                      
         B     COMMOVE                                                          
         SPACE                                                                  
*                                                                               
*DEMO GUARANTEE (NAD NUMBER)                                                    
INFRUDND CLC   QAGY,=CL2'DU'       GUARANTEES NOT ALLOWED FOR MEDIAVEST         
         BE    RCV10                                                            
         LA    RE,SVDEMCAT                                                      
         B     COMMOVE                                                          
         SPACE                                                                  
*                                                                               
*DEMO GUARANTEE (TYPE)                                                          
INFRUGTY CLC   QAGY,=CL2'DU'       GUARANTEES NOT ALLOWED FOR MEDIAVEST         
         BE    RCV10                                                            
         LA    RE,SVDEMCAT+1                                                    
         B     COMMOVE                                                          
         SPACE                                                                  
*                                                                               
*DEMO GUARANTEE (CATEGORY)                                                      
INFRUGCT CLC   QAGY,=CL2'DU'       GUARANTEES NOT ALLOWED FOR MEDIAVEST         
         BE    RCV10                                                            
         LA    RE,SVDEMCAT+2                                                    
         B     COMMOVE                                                          
         SPACE                                                                  
*                                                                               
*DEMO GUARANTEE                                                                 
INFRUDGU CLC   QAGY,=CL2'DU'       GUARANTEES NOT ALLOWED FOR MEDIAVEST         
         BE    RCV10                                                            
         LA    RE,SVDEMGUA                                                      
         B     COMMOVE                                                          
         SPACE                                                                  
*                                                                               
* GENERATE EHEADER/PHEADER REQUEST STEWARD                                      
*                                                                               
*                                                                               
*PHEADER PRINT TYPE SOON,XXX OR NOW,XXX                                         
INPHPRNT L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDPRNT                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER DESTINATION FAX=XXX                                                    
INPHDEST L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDDEST                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER CLIENT                                                                 
INPHCLI  L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDCLT                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER ESTIMATE                                                               
INPHEST  L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDEST                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER NETWORK                                                                
INPHNET  L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDNET                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER PACKAGE                                                                
INPHPACK L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDPKG                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER DEMOS                                                                  
INPHDEMS L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDDEMS                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER COMMENTS UP TO 6                                                       
INPHCOMS L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDCOM1                                                      
         LA    RF,6                                                             
INPHCM20 CLI   0(RE),X'40'          TEST FOR BLANK                              
         BNH   INPHCM30                                                         
         LA    RE,50(RE)                                                        
         BCT   RF,INPHCM20                                                      
         DC    H'0'                 TOO MANY COMMENTS PASSED                    
INPHCM30 B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER DEAL                                                                   
INPHDEAL L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDDEAL                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER CONTRACT                                                               
INPHCONT L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         LA    RE,PHEDCONT                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PHEADER REPORT TYPE C'P'=PHEADER, C'E'=EHEADER                                 
INPHTYPE L     RF,ANETBLK                                                       
         USING PHEADST,RF                                                       
         EX    R5,*+4                                                           
         MVC   PHEDTYPE(0),0(R4)                                                
         LHI   R0,PHEDLEN                                                       
         STCM  R0,3,TSARRCLN        SET OVERRIDE LENGTH                         
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
* MOVE UNIT RECORD FIELDS OUT (FRONTRUNNER)                                     
*                                                                               
*                                                                               
*START DATE OF UNIT UPLOAD(FRONTRUNNER)                                         
INFRUSPL GOTO1 VDATVAL,DMCB,(0,(R4)),SVSTPLAN                                   
         B     RCV10                                                            
         SPACE                                                                  
*                                                                               
*                                                                               
*END DATE OF UNIT UPLOAD(FRONTRUNNER)                                           
INFRUEPL GOTO1 VDATVAL,DMCB,(0,(R4)),SVENPLAN                                   
*                                                                               
*  READ ESTIMATE MAKE SURE PLAN DATES FALL WITHIN ESTIMATES                     
         GOTO1 VALIMED                                                          
         GOTO1 VCLPACK,DMCB,SVFRCLI,BCLT                                        
         MVC   FULL(3),SVFREST                                                  
*                                                                               
         GOTO1 VALIEST                                                          
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO3                                    
         L     R3,AIO3                                                          
         USING ESTHDR,R3                                                        
         MVC   ERROR,=AL2(61)      DATE BEFORE ESTIMATE START                   
         CLC   SVSTPLAN,ESTART                                                  
         BL    INFRUPLR                                                         
         MVC   ERROR,=AL2(62)      DATE AFTER ESTIMATE END                      
         CLC   SVENPLAN,EEND                                                    
         BNH   INFRUPLX                                                         
INFRUPLR B     RCVERR                                                           
INFRUPLX B     RCV10                                                            
         DROP  R3                                                               
         SPACE                                                                  
*                                                                               
* MOVE UNIT RECORD FIELDS OUT (FRONTRUNNER)                                     
*                                                                               
*                                                                               
*START DATE OF UNIT ADDS (FRONTRUNNER)                                          
INFRUSDT L     R3,ANETBLK                                                       
         USING NPURECD,R3                                                       
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,NPAKSTRT)   MAKE COMPRESSED              
         B     RCV10                                                            
         DROP  R3                                                               
         SPACE                                                                  
*                                                                               
*END DATE OF UNIT ADDS (FRONTRUNNER)                                            
INFRUEDT L     R3,ANETBLK                                                       
         USING NPURECD,R3                                                       
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,NPAKEND)   MAKE COMPRESSED               
         B     RCV10                                                            
         DROP  R3                                                               
         SPACE                                                                  
*                                                                               
*TVQ DATE (FRONTRUNNER)                                                         
INFRUTVQ L     R3,ANETBLK                                                       
         USING NPURECD,R3                                                       
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,NPAKTVQ)   MAKE COMPRESSED               
         B     RCV10                                                            
         DROP  R3                                                               
         SPACE                                                                  
*                                                                               
****QTR DATE                                                                    
***INFRUQTR L     RF,ANETBLK                                                    
***         USING NPURECD,RF                                                    
***         CLI   NPUBPER,0           IS THIS A WEEKLY PLAN                     
***         BE    *+12                                                          
***         LA    RE,NPUKPERQ                                                   
***         B     COMMOVE                                                       
***         LA    RE,NPUBPER+1                                                  
***         B     COMMOVE                                                       
***         DROP  RF                                                            
         SPACE                                                                  
*                                                                               
*MONTH                                                                          
INFRUMTH L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPUBPER+1                                                     
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*MONTH/QTR YEAR                                                                 
INFRUYER L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPUBPER                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*LENGTH                                                                         
INFRULEN L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPUBLNS                                                       
INFRLN20 CLI   0(RE),0                                                          
         BE    INFRLN40                                                         
         LA    RE,1(RE)                                                         
         B     INFRLN20                                                         
INFRLN40 B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*NUMBER OF UNITS                                                                
INFRUNUM L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPUBUNS                                                       
INFRUN20 OC    0(2,RE),0(RE)                                                    
         BZ    INFRUN40                                                         
         LA    RE,2(RE)                                                         
         B     INFRUN20                                                         
INFRUN40 B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*COST                                                                           
INFRUCST L     R3,ANETBLK                                                       
         USING NPURECD,R3                                                       
         LA    RF,NPUBUNS+2                                                     
         LA    RE,NPUBAMT                                                       
         LA    R6,8                                                             
*                                                                               
INFRCS20 OC    0(2,RF),0(RF)       IS NEXT UNIT SLOT ZERO                       
         BZ    INFRCS30                                                         
         LA    RF,2(RF)                                                         
         LA    RE,5(RE)                                                         
         BCT   R6,INFRCS20                                                      
INFRCS30 MVC   0(4,RE),0(R4)       MOVE COST OUT                                
         OI    4(RE),X'80'         SET COST INPUTTED BIT                        
         B     RCV10                                                            
         DROP  R3                                                               
         SPACE                                                                  
*                                                                               
*VPHS                                                                           
INFRUVPH L     R3,ANETBLK                                                       
         USING NPURECD,R3                                                       
         CLI   NPUCEL,C'Y'         FIRST PASS                                   
         BE    INFRVP20                                                         
         MVI   NPUCEL,C'Y'                                                      
***         MVI   NPUCLEN,119                                                   
         MVC   NPUCVPHS(2),=XL2'FFFF'                                           
*                                                                               
INFRVP20 LA    RE,NPUCVPHS                                                      
INFRVP30 CLC   0(2,RE),=XL2'FFFF'                                               
         BE    INFRVP40                                                         
         LA    RE,2(RE)                                                         
         B     INFRVP30                                                         
INFRVP40 MVC   0(2,RE),0(R4)       MOVE IN VPH                                  
         MVC   2(2,RE),=XL2'FFFF'  SET UP FOR NEXT VPH                          
         B     RCV10                                                            
         DROP  R3                                                               
         SPACE                                                                  
*                                                                               
*HUT/INITIALISE OVERRIDE SWITCH                                                 
INFRUHUT L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         MVI   NPUBOVRD,0          INIT OVERRIDE SWITCH                         
         LA    RE,NPUBHUT                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*RATING                                                                         
INFRURAT L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPUBRTG                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*SHARE                                                                          
INFRUSHR L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPUBSHR                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*SHARE OVERRIDE                                                                 
INFRUSOV L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         OI    NPUBOVRD,X'80'                                                   
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*RATING OVERRIDE                                                                
INFRUROV L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         OI    NPUBOVRD,X'20'                                                   
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*HUT OVERRIDE                                                                   
INFRUHOV L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         OI    NPUBOVRD,X'40'                                                   
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*VPH OVERRIDE NAD CATEGORY                                                      
INFRUNAD L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPUNDVPH                                                      
         LA    RF,NPUNDVPH+1                                                    
INFRND20 CLI   0(RF),0                                                          
         BE    INFRND30                                                         
         LA    RE,6(RE)                                                         
         LA    RF,6(RF)                                                         
         B     INFRND20                                                         
INFRND30 MVC   0(1,RE),0(R4)                                                    
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*VPH OVERRIDE DEMO CATEGORY                                                     
INFRUDCT L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPUNDVPH+1                                                    
INFRDC20 CLI   0(RE),0                                                          
         BE    INFRDC30                                                         
         LA    RE,6(RE)                                                         
         B     INFRDC20                                                         
INFRDC30 MVC   0(1,RE),0(R4)                                                    
         MVC   1(4,RE),=XL4'FFFFFFFF'                                           
         OI    NPUBOVRD,X'10'                                                   
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*VPH OVERRIDE DEMO AMOUNT                                                       
INFRUDEM L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPUNDVPH+2                                                    
INFRDA20 CLC   0(4,RE),=XL4'FFFFFFFF'                                           
         BE    INFRDA30                                                         
         LA    RE,6(RE)                                                         
         B     INFRDA20                                                         
INFRDA30 MVC   0(4,RE),0(R4)                                                    
         B     RCV10                                                            
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*SUB DAYPART                                                                    
INFRUSDP L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         OI    1(R4),X'40'                                                      
         LA    RE,NPGDSDPT                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PROGRAM TYPE                                                                   
INFRUPTY L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPGDTYP                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*OLD/NEW PROGRAM                                                                
INFRUNEW L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPGDNEW                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*PROGRAMS ROTATION VALUE FOR FRONTRUNNER TRANSFER                               
INFRROT  L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPGDROT                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*TIER                                                                           
INFRUTIR L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPGDTIER                                                      
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*FIRST RECORD SWITCH                                                            
INFRUFST MVI   SVFRSTSW,C'Y'                                                    
         B     RCV10                                                            
         SPACE                                                                  
*                                                                               
*PROGRAM CONTENT                                                                
INFRUCNT L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         LA    RE,NPGDCNT                                                       
         B     COMMOVE                                                          
         DROP  RF                                                               
         SPACE                                                                  
*                                                                               
*  PROGRAM CODE/SET THE RECORD UP/WRITE THE RECORD OUT                          
*                                                                               
INFRUPCD L     RF,ANETBLK                                                       
         USING NPURECD,RF                                                       
         MVC   NPURTYPE,=CL2'FB'                                                
***         MVC   NPGDEL(2),=XL2'0112'                                          
***         MVC   NPUBEL(2),=XL2'1240'                                          
***         MVC   NPUCEL(2),=XL2'1477'                                          
***         MVC   NPUND(2),=XL2'1698'                                           
***         MVC   NPAKND(2),=XL2'1813'                                          
***         MVC   NPBKEVN(5),=XL5'C5E5D5580E'                                   
         MVC   NPGDPERT,SVPERTYP                                                
         MVC   NPAKGUA,SVPAKGUA                                                 
         MVC   NPAKDCAT,SVDEMCAT                                                
         MVC   NPAKDGUA,SVDEMGUA                                                
         CLI   SVFRSTSW,C'Y'                                                    
         BNE   *+8                                                              
         OI    NPGDSTAT,X'80'                                                   
         MVI   SVFRSTSW,C'N'                                                    
         MVC   SVFRPROG,0(R4)                                                   
         MVC   NPUCLI(33),SVFRCLI   MOVE KEY FIELDS TO RECORD                   
         LHI   R0,NPURRLEN                                                      
         STCM  R0,3,TSARRCLN       SET OVERRIDE LENGTH                          
         BRAS  RE,CHKTSAR           WRITE LAST RECORD TO TSAR                   
         B     RCV10                                                            
         DROP  RE,RF                                                            
         SPACE                                                                  
*                                                                               
* FORMAT DATA TO OUTPUT RECORD                                                  
*                                                                               
*****STBUYFLD NTR1                                                              
*****         XC    WORK,WORK                                                   
*****         EX    R5,*+4                                                      
*****         MVC   WORK+8(0),0(R4)                                             
*****         LA    R5,9(R5)                                                    
*****         STCM  R5,1,WORK                                                   
*                                                                               
*****         LA    R2,WORK                                                     
*****         ST    R2,FADDR                                                    
*****         XC    FTERM,FTERM                                                 
*****         XC    FLAST,FLAST                                                 
*****         MVI   FTERM,C':'                                                  
*                                                                               
*****STBUY50  GOTO1 FVAL,0                                                      
*****         CLI   FLEN,0               CHECK FOR NO INPUT                     
*****         BE    STBUY70                                                     
*****         ZIC   R1,FLEN                                                     
*****         BCTR  R1,0                                                        
*****         EX    R1,*+4                                                      
*****         MVC   0(0,R6),FLD                                                 
*****         B     STBUY80                                                     
*****STBUY70  SR    RE,RE               IF NO INPUT                             
*****         ICM   RE,7,FLAST          MUST BUMP PAST DELIMETER                
*****         LA    RE,1(RE)            SO SCAN CAN CONTINUE                    
*****         STCM  RE,7,FLAST                                                  
*****STBUY80  ZIC   RE,BYTE              LENGTH OF STORAGE FIELD                
*****         AR    R6,RE                                                       
*****         MVC   0(1,R6),FLDH+5       LENGTH OF INPUT                        
*****         CLI   FSTOP,X'FF'                                                 
*****         BE    STBUYEX                                                     
*****         LA    R6,1(R6)                                                    
*****         B     STBUY50                                                     
*****STBUYEX  B     RCVEXIT                                                     
*****         EJECT                                                             
*                                                                               
INWIZCLT BRAS  RE,VALWZCLT    FILL SVWIZCLT TABLE                               
         B     RCV10                                                            
*                                                                               
         EJECT                                                                  
* CHECK LIST OF REQUESTED PRODUCTS AGAINST WIZ CLIENT LIST                      
* IF ONE PRODUCT MATCHES DO NOT CHECK OTHERS                                    
*                                                                               
INWIZPRD DS    0H                                                               
         XC    QPRD,QPRD                                                        
         XC    WORK,WORK                                                        
         EX    R5,*+4                                                           
         MVC   WORK+8(0),0(R4)                                                  
         LA    R5,9(R5)                                                         
         STCM  R5,1,WORK                                                        
*                                                                               
         LA    R5,SVWIZPRD                                                      
         LA    R2,WORK                                                          
         ST    R2,FADDR                                                         
         XC    FTERM,FTERM                                                      
         XC    FLAST,FLAST                                                      
         MVI   FTERM,C':'                                                       
*                                                                               
PRDWZ50  GOTO1 FVAL,0                                                           
         CLI   FLEN,0               CHECK FOR NO INPUT                          
         BE    PRDWZ70                                                          
*********************************************************                       
         MVC   QPRD,FLD                                                         
         BAS   RE,CHKWZPRD                                                      
         TM    WIZSTAT,X'02'        ERROR FOUND?                                
         BNO   PRDWZ60                                                          
         NI    WIZSTAT,X'FF'-X'02'  CLEAR IT                                    
         CLI   FSTOP,X'FF'          NO MORE ENTRIES?                            
         BE    PRDWZEX           NO                                             
         B     PRDWZ50           GET NEXT ENTRY-WITHOUT BUMPING SVTBL           
*********************************************************                       
PRDWZ60  ZIC   R1,FLEN                                                          
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R5),FLD                                                      
         OC    0(3,R5),SPACES                                                   
***      MVC   QPRD,FLD                                                         
***      BAS   RE,CHKWZPRD                                                      
         B     PRDWZ80                                                          
PRDWZ70  SR    RE,RE               IF NO INPUT                                  
         ICM   RE,7,FLAST          MUST BUMP PAST DELIMETER                     
         LA    RE,1(RE)            SO SCAN CAN CONTINUE                         
         STCM  RE,7,FLAST                                                       
PRDWZ80  CLI   FSTOP,X'FF'                                                      
         BE    PRDWZEX                                                          
         LA    R5,3(R5)                                                         
         B     PRDWZ50                                                          
*                                                                               
PRDWZEX  B     RCV10                                                            
*                                                                               
CHKWZPRD NTR1                     MATCH PROD TO CLIENT LIST                     
         LA    R2,SVWIZCLT                                                      
         LA    R3,(L'SVWIZCLT/LNWIZCLT)   # OF CLIENTS                          
         USING WIZCLTD,R2                                                       
         MVC   BCLT,WIZBCLT                                                     
CHKWZP10 MVI   READREC,C'N'        DON'T READ RECORD                            
         GOTO1 VALIPRD                                                          
         BE    CHKWP20                                                          
         LA    R2,LNWIZCLT(R2)                                                  
         MVC   BCLT,WIZBCLT        SET BCLT FOR PROD READ                       
         BCT   R3,CHKWZP10                                                      
*                                                                               
         TM    WIZSTAT,X'01'       SKIP ERRORS?                                 
         BNO   *+12                NO                                           
         OI    WIZSTAT,X'02'       YES-SET ERROR FOUND                          
         B     CHKWP20                                                          
*                                                                               
         MVC   ERROR,=AL2(109)     INVALID PRODUCT                              
         MVC   ERRORMSG(3),QPRD                                                 
         GOTO1 SENDMSG                                                          
         B     CHKWP20                                                          
CHKWP20  MVI   READREC,0           RESET FLAG                                   
         B     RCVEXIT             ONE MATCHED - THAT'S ENOUGH                  
         DROP  R2                                                               
                                                                                
*                                                                               
* CHECK LIST OF REQUESTED ESTIMATES AGAINST WIZ CLIENT LIST                     
* IF ONE EST MATCHES DO NOT CHECK OTHERS                                        
*                                                                               
INWIZEST DS    0H                                                               
         XC    QEST,QEST                                                        
         XC    WORK,WORK                                                        
         EX    R5,*+4                                                           
         MVC   WORK+8(0),0(R4)                                                  
         LA    R5,9(R5)                                                         
         STCM  R5,1,WORK                                                        
*                                                                               
         LA    R5,SVWIZEST                                                      
         LA    R2,WORK                                                          
         ST    R2,FADDR                                                         
         XC    FTERM,FTERM                                                      
         XC    FLAST,FLAST                                                      
         MVI   FTERM,C':'                                                       
*                                                                               
ESTWZ50  GOTO1 FVAL,0                                                           
         CLI   FLEN,0               CHECK FOR NO INPUT                          
         BE    ESTWZ70                                                          
         MVC   QEST,FLD                                                         
         CVB   R1,DUB              FVAL RETURNS PACKED NUMB IN DUB              
         STC   R1,0(R5)            SAVE BINARY EST                              
         STC   R1,CURREST                                                       
         BRAS  RE,CHKWZEST                                                      
         B     ESTWZ80                                                          
ESTWZ70  SR    RE,RE               IF NO INPUT                                  
         ICM   RE,7,FLAST          MUST BUMP PAST DELIMETER                     
         LA    RE,1(RE)            SO SCAN CAN CONTINUE                         
         STCM  RE,7,FLAST                                                       
ESTWZ80  CLI   FSTOP,X'FF'                                                      
         BE    ESTWZEX                                                          
         LA    R5,1(R5)                                                         
         B     ESTWZ50                                                          
*                                                                               
ESTWZEX  B     RCV10                                                            
*&&DO                                                                           
CHKWZEST NTR1                     MATCH EST TO CLIENT/PROD LIST                 
         LA    R5,(L'SVWIZCLT/LNWIZCLT)     MAX NUMBER OF CLIENTS               
         LA    R2,SVWIZCLT                                                      
         USING WIZCLTD,R2                                                       
CHKWZE10 XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),WIZBCLT                                                 
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),CURREST                                                 
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    CHKWZE20                                                         
         LA    R2,LNWIZCLT(R2)                                                  
         BCT   R5,CHKWZE10                                                      
*                                                                               
         TM    WIZSTAT,X'01'       SKIP ERRORS?                                 
         BNO   *+12                                                             
         OI    WIZSTAT,X'02'       SET ERRORS FOUND                             
         B     CHKWZE20                                                         
*                                                                               
         MVC   ERROR,=AL2(237)     EST NOT ON FILE                              
         EDIT  (B1,CURREST),(3,ERRORMSG)                                        
         GOTO1 SENDMSG                                                          
CHKWZE20 B     RCVEXIT             ONE MATCHED - THAT'S ENOUGH                  
*&&                                                                             
         EJECT                                                                  
*  NETWORK VALIDATION                                                           
*                                                                               
INNETRES MVC   BNET,SPACES        NETWORK SAVE (RESEARCH)                       
         EX    R5,*+4                                                           
         MVC   BNET(0),0(R4)                                                    
         OC    BNET,SPACES                                                      
         B     INNET10                                                          
INPKNET  XC    BNET,BNET                                                        
         CLI   0(R4),0                                                          
         BE    RCV10                                                            
         CLI   0(R4),C'0'                                                       
         BE    RCV10                                                            
INNETPRG MVC   BNET,SPACES         NETWORK SAVE (PROGRAM RECORDS)               
         EX    R5,*+4                                                           
         MVC   BNET(0),0(R4)                                                    
         OC    BNET,SPACES                                                      
*  READ STATION RECORD GET THE MARKET                                           
         GOTO1 VALISTA                                                          
*                                                                               
INNET10  LA    RE,SVNET                                                         
         LA    RF,SVMKT                                                         
         OC    SVNET,SVNET                                                      
         BZ    INNET50                                                          
         LA    R1,15                                                            
INNET20  CLI   0(RE),X'FF'                                                      
         BE    INNET50                                                          
         LA    RE,4(RE)                                                         
         LA    RF,2(RF)                                                         
         BCT   R1,INNET20                                                       
         DC    H'0'                                                             
INNET50  MVC   0(4,RE),BNET                                                     
         MVI   4(RE),X'FF'                                                      
         MVC   0(2,RF),BMKT                                                     
         MVI   2(RF),X'FF'                                                      
         B     RCV10                                                            
         SPACE 3                                                                
*                                                                               
* NETWORK - PROGRAM INFO                                                        
*                                                                               
INPINET  XC    BNET,BNET                                                        
         XC    BMKT,BMKT                                                        
         CLI   0(R4),0                                                          
         BE    RCV10                                                            
         EX    R5,*+4                                                           
         MVC   BNET(0),0(R4)                                                    
         OC    BNET,SPACES                                                      
         CLC   =C'ALL ',BNET                                                    
         BE    RCV10                                                            
         GOTO1 VALISTA                                                          
         B     RCV10                                                            
*                                                                               
* START DATE - PROGRAM INFO                                                     
*                                                                               
INPISDT  XC    SVSDATE,SVSDATE                                                  
         CLI   0(R4),0                                                          
         BE    RCV10                                                            
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SVSDATE)    MAKE COMPRESSED              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
* END DATE - PROGRAM INFO                                                       
*                                                                               
INPIEDT  XC    SVEDATE,SVEDATE                                                  
         CLI   0(R4),0                                                          
         BE    RCV10                                                            
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SVEDATE)    MAKE COMPRESSED              
         B     RCV10                                                            
*                                                                               
*  DAY2 VALIDATION                                                              
*                                                                               
INPRDAY2 EX    R5,*+4                                                           
         OC    0(0,R4),SPACES       MAKE UPPER CASE                             
         CLI   3(R4),X'40'          4 BYTE CHARACTER DAY INPUT (M-SU)           
         BNH   *+8                                                              
         LA    R5,1(R5)                                                         
         MVI   SVDY2FLT,C'N'                                                    
         GOTO1 VDAYVAL,DMCB,((R5),0(R4)),SVDAY2,SVDAY2+1                        
*                                                                               
         MVC   SVDY2DEM,SVDAY2                                                  
         LA    RE,PRDAYTB                                                       
INP2DY20 CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SVDAY2(1),0(RE)                                                  
         BE    INP2DY60                                                         
         LA    RE,2(RE)                                                         
         B     INP2DY20                                                         
*                                                                               
INP2DY60 MVC   SVDAY2,1(RE)                                                     
         MVI   SVDY2FLT,C'Y'                                                    
         B     RCV10                                                            
         SPACE 3                                                                
*                                                                               
*  DAY VALIDATION                                                               
*                                                                               
INPRROT  EX    R5,*+4                                                           
         OC    0(0,R4),SPACES       MAKE UPPER CASE                             
         LA    R5,1(R5)                                                         
         GOTO1 VDAYVAL,DMCB,((R5),0(R4)),SVROT,BYTE                             
         B     RCV10                                                            
         SPACE 3                                                                
*                                                                               
*  DAY VALIDATION                                                               
*                                                                               
INDAY    MVI   FULL,X'FF'                                                       
INPRDAY  EX    R5,*+4                                                           
         OC    0(0,R4),SPACES       MAKE UPPER CASE                             
         CLI   3(R4),X'40'          4 BYTE CHARACTER DAY INPUT (M-SU)           
         BNH   *+8                                                              
         LA    R5,1(R5)                                                         
         MVI   SVDAYFLT,C'N'                                                    
         GOTO1 VDAYVAL,DMCB,((R5),0(R4)),SVDAY,SVDAY+1                          
         CLI   FULL,X'FF'                                                       
         BE    RCV10                                                            
*                                                                               
         MVC   SVDAYDEM,SVDAY                                                   
         LA    RE,PRDAYTB                                                       
INPDY20  CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SVDAY(1),0(RE)                                                   
         BE    INPDY60                                                          
         LA    RE,2(RE)                                                         
         B     INPDY20                                                          
*                                                                               
INPDY60  MVC   SVDAY,1(RE)                                                      
         MVI   SVDAYFLT,C'Y'                                                    
         B     RCV10                                                            
*                                                                               
PRDAYTB  DC    XL19'7F087C004001200210030804040502060107FF'                     
         SPACE 1                                                                
*                                                                               
*  TIME VALIDATION 2                                                            
*                                                                               
INPRTME2 MVI   FULL,X'FF'           SET TIME2 SWITCH                            
         B     INPRTM20                                                         
*                                                                               
*  TIME VALIDATION                                                              
*                                                                               
INPRTIME MVI   FULL,0               RESET TIME2 SWITCH                          
INPRTM20 LA    R5,1(R5)             TIME SAVE                                   
         PRINT GEN                                                              
         STCM  R5,1,BYTE                                                        
         GOTO1 VTIMVAL,DMCB,(BYTE,0(R4)),DUB                                    
         PRINT NOGEN                                                            
         CLI   DMCB,X'FF'          INVALID TIME                                 
         BE    TIMERRX                                                          
         CLC   DUB(4),=C'NONE'                                                  
         BE    TIMERRX                                                          
         CLC   DUB(4),=C'VARY'                                                  
         BE    TIMERRX                                                          
         CLC   SVRCVEL,=X'0056'                                                 
         BE    INPRTM30                                                         
         CLC   DUB+2(2),=2X'00'                                                 
         BE    TIMERRX             END TIME REQUIRED                            
         CLC   DUB+2(2),=C'CC'                                                  
         BE    TIMERRX             END TIME REQUIRED                            
*        CLC   DUB(2),DUB+2        CHECK IF TIME CROSSED MIDNIGHT               
*        BNH   INPRTM50                                                         
*        LH    RE,DUB+2                                                         
*        AH    RE,=H'2400'                                                      
*        STH   RE,DUB+2                                                         
*                                                                               
*  ADD 2400 TO TIMES LESS THEN 600                                              
*                                                                               
INPRTM30 SR    RE,RE                                                            
         LH    RE,DUB                                                           
         CH    RE,=H'0559'                                                      
         BH    INPRTM40                                                         
         AH    RE,=H'2400'                                                      
         STH   RE,DUB                                                           
INPRTM40 CLC   SVRCVEL,=X'0056'                                                 
         BE    INPRTM50                                                         
         SR    RE,RE                                                            
         LH    RE,DUB+2                                                         
         CH    RE,=H'0559'                                                      
         BH    INPRTM50                                                         
         AH    RE,=H'2400'                                                      
         STH   RE,DUB+2                                                         
*                                                                               
INPRTM50 CLI   FULL,X'FF'                                                       
         BE    INPRTM60                                                         
         MVC   SVTIME,DUB                                                       
         B     RCV10                                                            
INPRTM60 MVC   SVTIME2,DUB                                                      
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  RESEARCH TIME VALIDATION                                                     
*                                                                               
INRSTIME LA    R5,1(R5)             TIME SAVE                                   
         PRINT GEN                                                              
         STCM  R5,1,BYTE                                                        
         GOTO1 VTIMVAL,DMCB,(BYTE,0(R4)),DUB                                    
         PRINT NOGEN                                                            
         CLI   DMCB,X'FF'          INVALID TIME                                 
         BE    TIMERRX                                                          
         CLC   DUB(4),=C'NONE'                                                  
         BE    TIMERRX                                                          
         CLC   DUB(4),=C'VARY'                                                  
         BE    TIMERRX                                                          
         CLC   DUB+2(2),=2X'00'                                                 
         BE    TIMERRX             END TIME REQUIRED                            
         CLC   DUB+2(2),=C'CC'                                                  
         BE    TIMERRX             END TIME REQUIRED                            
         CLC   DUB(2),DUB+2        CHECK IF TIME CROSSED MIDNIGHT               
         BNH   INRSTM50                                                         
         LH    RE,DUB+2                                                         
         AH    RE,=H'2400'                                                      
         STH   RE,DUB+2                                                         
INRSTM50 MVC   SVTIME,DUB                                                       
         B     RCV10                                                            
*                                                                               
*********** TIMERRX  DC    H'0'                                                 
TIMERRX  MVC   ERROR,=AL2(16)                                                   
         B     RCVERR                                                           
         SPACE 1                                                                
*                                                                               
* NETWORK MEDIA TYPE                                                            
*                                                                               
INNMTYP  XC    SVMEDTYP,SVMEDTYP                                                
         CLI   0(R4),0                                                          
         BE    *+10                                                             
         MVC   SVMEDTYP,0(R4)                                                   
         B     RCV10                                                            
*                                                                               
*  SET UNIVERSE ONLY LOOKUP SWITCH                                              
*                                                                               
INUNIVSW OI    SVINDS1,X'80'                                                    
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  SET NTI DEMO LOOKUP SWITCH                                                   
*                                                                               
INNTISW  OI    SVINDS1,SVI1NTIQ                                                 
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  SET VPH ONLY LOOKUP SWITCH                                                   
*                                                                               
INVPHSW  OI    SVINDS1,X'40'                                                    
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  SET HUT ONLY LOOKUP SWITCH                                                   
*                                                                               
INHUTSW  OI    SVINDS1,X'20'                                                    
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  SWITCH STOPS 86 ELEMENT FROM BEING PASSED TO THE PC ON UNIV LOOKUP           
*                                                                               
INNO86EL OI    SVINDS1,X'10'                                                    
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  JUST PASS PROFILES DONT VALIDATE UNIVERSE FOR CLIENT WIZARD                  
*                                                                               
INJSTPRF OI    SVINDS1,X'08'                                                    
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  PLAN TVQ BOOK                                                                
*                                                                               
INTVQBK  GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SVTVQBK)    MAKE COMPRESSED              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  UNIVERSE DATE VALIDATION                                                     
*                                                                               
INUNIVDT GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SVUNIVDT)   MAKE COMPRESSED              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  MONTH OF SERVICE VALIDATION                                                  
*                                                                               
INPRMOS  MVC   DUB(3),0(R4)                                                     
         MVC   DUB+3(2),=C'01'                                                  
         MVC   DUB+5(3),3(R4)                                                   
         GOTO1 VDATVAL,DMCB,(0,DUB),WORK                                        
         GOTO1 VDATCON,DMCB,(0,WORK),(3,DUB)                                    
         MVC   BMOS(2),DUB            YYMM                                      
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  START DATE VALIDATION                                                        
*                                                                               
INPRSDAT GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         MVC   SVEESDAT,DUB                                                     
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SVSDATE)    MAKE COMPRESSED              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  END DATE VALIDATION                                                          
*                                                                               
INPREDAT GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         MVC   SVEEEDAT,DUB                                                     
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SVEDATE)    MAKE COMPRESSED              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
INWIZTST NI    WIZSTAT,X'FF'-X'01'                                              
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    WIZSTAT,X'01'     SET SKIP ERRORS                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  BUY DATE VALIDATION                                                          
*                                                                               
INBUYDAT GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         MVC   SVEEBDAT,DUB                                                     
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SVBDATE)    MAKE COMPRESSED              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  MISSED DATE VALIDATION                                                       
*                                                                               
INMSBDAT GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SVMSBDAT)   MAKE COMPRESSED              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  REP CODE VALIDATION                                                          
*                                                                               
INREP    XC    QMMREP,QMMREP        REP CODE                                    
         EX    R5,*+4                                                           
         MVC   QMMREP(0),0(R4)                                                  
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  NTI CODE VALIDATION                                                          
*                                                                               
INNTI    XC    SVNTI,SVNTI          NTI CODE                                    
         EX    R5,*+4                                                           
         MVC   SVNTI(0),0(R4)                                                   
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  PROGRAM TYPE                                                                 
*                                                                               
INPRTYPE EX    R5,*+4                                                           
         MVC   SVPRTYPE(0),0(R4)                                                
         OC    SVPRTYPE,SPACES                                                  
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  2 CHARACTER DAYPART                                                          
*                                                                               
INPRDPT  LA    RE,SV2DAYPT         2 CHARACTER DAYPART                          
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  UNIVERSE NUMBER                                                              
*                                                                               
INUNIV   XC    DUB,DUB                                                          
         MVC   DUB+5(3),0(R4)                                                   
         SRP   DUB+5(3),1,0                                                     
         MVC   SVUNIV,DUB+5                                                     
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  AUDIT TRAIL GROUP CODE                                                       
*                                                                               
INAUDCDE L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    RE,RUPNCBO                                                       
         B     COMMOVE                                                          
         DROP  RE                                                               
         SPACE 1                                                                
*                                                                               
*  PROGRAM TIER                                                                 
*                                                                               
INPRTIER LA    RE,SVPRTIER         PROGRAM TIER                                 
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  PROGRAM NEW/RETURNING                                                        
*                                                                               
INPRNEW  LA    RE,SVPRNEW          PROGRAM NEW/RETURNING                        
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  PLAN HUT YEAR                                                                
*                                                                               
INHUTYR  LA    RE,PLANHTYR         HUT YEAR                                     
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  PLAN HUT NUMBER OF YEARS BACK                                                
*                                                                               
INHUTNO  LA    RE,PLANHTNO         NUMBER OF YEARS BACK                         
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  PLAN HUT SCHEME                                                              
*                                                                               
INHUTSC  LA    RE,PLANHTSC         PLAN HUT SCHEME                              
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  PLAN HUT AVERAGE                                                             
*                                                                               
INHUTAV  LA    RE,PLANHTAV         PLAN HUT AVERAGE                             
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  PLAN HUT PERCENT OVERRIDE                                                    
*                                                                               
INHUTPO  LA    RE,PLANHTPO         HUT PERCENT OVERRIDE                         
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  PLAN PERIOD W/M/Q                                                            
*                                                                               
INHUTPE  LA    RE,PLANPERT         PLAN PERIOD                                  
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  PLAN HUT FLAVOR                                                              
*                                                                               
INHUTFL  LA    RE,PLANHTFL         HUT FLAVOR                                   
         B     COMMOVE                                                          
         SPACE 1                                                                
*                                                                               
*  PROGRAM CONTENT                                                              
*                                                                               
INPRCONT EX    R5,*+4                                                           
         MVC   SVPRCONT(0),0(R4)                                                
         OC    SVPRCONT,SPACES                                                  
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  RUNNING FRONTRUNNER                                                          
*                                                                               
INFRNTSW MVC   SVFRSW,0(R4)        SET MATCHMAKER                               
         CLI   0(R4),C'M'          IS THIS A MATCHMAKER REQUEST                 
         BE    RCV10                                                            
         CLI   0(R4),C'F'          IS THIS A FRONTRUNNER REQUEST                
         BE    RCV10                                                            
INFRNT10 MVI   SVFRSW,C'Y'         JUST SET TO YES                              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  BRAND OWNERSHIP REPORT REQUEST                                               
*                                                                               
INBRNDOW MVI   SVBRNDOW,C'Y'       JUST SET TO YES                              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  BARRULES RECORD INFO TO BE PASSED DURING THE WIZARD DOWNLOAD                 
*                                                                               
INBARRLS OI    WIZSTAT,X'08'       SET STATUS ON                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  COMMON MOVE                                                                  
*                                                                               
COMMOVE  EX    R5,*+4                                                           
         MVC   0(0,RE),0(R4)                                                    
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  DAYPART VALIDATION                                                           
*                                                                               
INDAYPRT XC    SVDAYPT,SVDAYPT      DAYPART CODE                                
         EX    R5,*+4                                                           
         MVC   SVDAYPT(0),0(R4)                                                 
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  NAD DEFINITION VALIDATION                                                    
*                                                                               
INNADDEF XC    SVPRNAD,SVPRNAD                                                  
         EX    R5,*+4                                                           
         MVC   SVPRNAD(0),0(R4)                                                 
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  PROGRAM FILTER VALIDATION                                                    
*                                                                               
INPRFLT  XC    SVPRFILT,SVPRFILT                                                
         EX    R5,*+4                                                           
         MVC   SVPRFILT(0),0(R4)                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  I2 REQUEST CLIENT                                                            
*                                                                               
INI2CLT  XC    QCLT,QCLT                                                        
         EX    R5,*+4                                                           
         MVC   QCLT(0),0(R4)                                                    
*                                                                               
         GOTO1 VALIMED                                                          
         GOTO1 VCLPACK,DMCB,QCLT,BCLT                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         JNE   VCLTERR                                                          
                                                                                
         L     R6,AIO4                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
         USING CLTHDRD,R6                                                       
         MVC   SVOFFC,COFFICE          CLIENT OFFICE                            
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  I2 REQUEST ESTIMATE                                                          
*                                                                               
INI2EST  XC    I2EST,I2EST                                                      
         XC    I2ESTB,I2ESTB                                                    
         EX    R5,*+4                                                           
         MVC   I2EST(0),0(R4)                                                   
*                                                                               
         CLC   I2EST,=C'000'                                                    
         BE    RCV10                                                            
         PACK  DUB,I2EST                                                        
         CVB   RE,DUB                                                           
         STC   RE,I2ESTB                                                        
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  I2 REQUEST ESTIMATE FILTER                                                   
*                                                                               
INI2EFL  XC    I2ESTFLT,I2ESTFLT                                                
         EX    R5,*+4                                                           
         MVC   I2ESTFLT(0),0(R4)                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  I2 REQUEST INTEGRATION MATCH REQUEST                                         
*                                                                               
INI2INT  XC    I2INTREQ,I2INTREQ                                                
         EX    R5,*+4                                                           
         MVC   I2INTREQ(0),0(R4)                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  I2 REQUEST CALANDER OPTION BROADCAST/CALANDER                                
*                                                                               
INI2CAL  XC    I2CALREQ,I2CALREQ                                                
         EX    R5,*+4                                                           
         MVC   I2CALREQ(0),0(R4)                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  I2 REQUEST PRODUCT GROUP                                                     
*                                                                               
INI2PGR  XC    I2PRDGRP,I2PRDGRP                                                
         EX    R5,*+4                                                           
         MVC   I2PRDGRP(0),0(R4)                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  I2 AFFID POSTING OPTION (DRAFT OR FILEMARK)                                  
*                                                                               
INI2DRF  XC    I2DRFTSW,I2DRFTSW                                                
         EX    R5,*+4                                                           
         MVC   I2DRFTSW(0),0(R4)                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  VERSION CONTROL                                                              
*                                                                               
INVERSN  MVC   VERSION,0(R4)       VERSION DATA                                 
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  SET TSAR RECORD LENGTH                                                       
*                                                                               
INSTRUC  BRAS  RE,INSTRUCR                                                      
         B     RCV10                                                            
*        *************                                                          
*        *************                                                          
*                                                                               
*  PACKAGE GUARANTEE DEMO                                                       
*                                                                               
INPKGDEM L     RF,ANETBLK                                                       
         USING PKGUPLDD,RF                                                      
         XC    RPKGDEM,RPKGDEM                                                  
         BRAS  RE,INDEMO                                                        
         MVC   RPKGDEM,QDEMOS                                                   
         B     RCV10                                                            
*  DEMO VALIDATION                                                              
*                                                                               
INDEMOS  BRAS  RE,INDEMO                                                        
INDEMEX  B     RCV10                                                            
         DROP  R6                                                               
         SPACE 1                                                                
*        *************                                                          
*        *************                                                          
*                                                                               
*  DEMO VALIDATION                                                              
*                                                                               
**INDEMOS  XC    BLOCK(256),BLOCK      DEMOS EXIST ...                          
**         XC    QDEMOS,QDEMOS                                                  
**         LA    R6,BLOCK                                                       
**         USING DBLOCK,R6             SET UP CALL TO DEMOCON                   
**         MVC   DBCOMFCS,ACOMFACS                                              
**         MVC   DBFILE,=C'TPT'                                                 
**         MVI   DBSELMED,C'T'                                                  
*                                                                               
**         XC    WORK,WORK                                                      
**         LA    RE,WORK2                                                       
**         LA    RF,260                                                         
**         XCEF                                                                 
*  BUILD OUTPUT FIELD FOR DEMOVAL                                               
**         LA    RE,WORK2+8                                                     
**         SR    RF,RF                                                          
**         LR    R1,R4                                                          
**INDEM100 CLI   0(R1),X'40'                                                    
**         BNH   INDEM160                                                       
**         MVC   0(1,RE),0(R1)                                                  
**         LA    RF,1(RF)                                                       
**         LA    RE,1(RE)                                                       
**         LA    R1,1(R1)                                                       
**         B     INDEM100                                                       
**INDEM160 STCM  RF,1,WORK2+5                                                   
**         LA    RF,8(RF)                                                       
**         STCM  RF,1,WORK2                                                     
*                                                                               
**         GOTO1 VDEMOVAL,DMCB,(1,WORK2),(25,QDEMOS),(C'S',BLOCK),WORK          
*                                                                               
*  CONVERT IMPRESSIONS TO A "T" PREFIX                                          
                                                                                
**         LA    RE,QDEMOS                                                      
**         MVI   BYTE,C'T'            SET IMPS TO THOUSANDS                     
**         CLI   SVSOURC,C'N'                                                   
**         BE    INDEM180                                                       
**         CLI   SVSOURC,C'S'                                                   
**         BE    INDEM180                                                       
**         MVI   BYTE,C'H'            SET IMPS TO HUNDREDS                      
**INDEM180 CLI   0(RE),X'FF'                                                    
**         BE    INDEMEX                                                        
**         CLI   1(RE),C'I'                                                     
**         BE    INDEM200                                                       
**         CLI   1(RE),C'T'                                                     
**         BE    INDEM200                                                       
**         CLI   1(RE),C'H'                                                     
**         BNE   *+10                                                           
**INDEM200 MVC   1(1,RE),BYTE                                                   
**         LA    RE,3(RE)                                                       
**         B     INDEM180                                                       
*                                                                               
**INDEMEX  B     RCV10                                                          
**         DROP  R6                                                             
         SPACE 1                                                                
*                                                                               
*  DEMO TYPE SWITCH                                                             
*  N = ESTIMATED NO GUARANTEE, A = ACTUAL                                       
*  BLANK = ESTIMATED WITH GUARANYEE                                             
*                                                                               
INACTGUA NI    SVEDEMSW,X'CF'       CLEAR THE SWITCHES                          
         CLC   0(2,R4),=C'EY'      EST WITH GUARANTEES                          
         BE    INACTG20                                                         
         CLC   0(2,R4),=C'EN'       NO GUARANTEES APPLIED                       
         BE    INACTG10                                                         
         CLI   0(R4),C'N'           NO GUARANTEES APPLIED                       
         BNE   *+8                                                              
INACTG10 OI    SVEDEMSW,X'20'                                                   
         CLI   0(R4),C'A'           RETURN ACTUALS                              
         BNE   *+8                                                              
         OI    SVEDEMSW,X'10'                                                   
INACTG20 B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  DEMO COLUMN 2 TYPE SWITCH                                                    
*  N = ESTIMATED NO GUARANTEE                                                   
*  Y = ESTIMATED WITH GUARANTEE                                                 
*  A = ACTUALS                                                                  
*  BLANK = ESTIMATED WITH GUARANYEE                                             
*                                                                               
INACGUA2 NI    SVSTATSW,X'8F'       CLEAR THE SWITCHES                          
         CLI   0(R4),C'A'           ACTUALS                                     
         BNE   *+8                                                              
         OI    SVSTATSW,X'01'                                                   
         CLC   0(2,R4),=C'EN'          EST NO GUARANTEES                        
         BNE   *+8                                                              
         OI    SVSTATSW,X'02'                                                   
         CLC   0(2,R4),=C'EY'          EST WITH GUARANTEES                      
         BNE   *+8                                                              
         OI    SVSTATSW,X'04'                                                   
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  SET INDICATOR THAT THIS IS COMING FROM THE HUB                               
*                                                                               
INSHUBSW NI    SVSTEWST,X'7F'       CLEAR THE INDICATOR                         
         CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    SVSTEWST,X'80'       SET HUB INDICATOR                           
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  SET PRISMA ID                                                                
*                                                                               
INPRSMID MVC   SVPRSMID,0(R4)                                                   
         B     RCV10                                                            
*                                                                               
*  RTG EQUIVALENCE SWITCH                                                       
*  E = 30 SECOND EQUIVALENCE, R = RAW NO EQUIVALENCE                            
*  BLANK = USE PROFILE                                                          
*                                                                               
INRTGEQU NI    SVEDEMSW,X'F3'       CLEAR THE SWITCHES                          
         CLI   0(R4),C'E'           EQUIVALENCE AT 30 SECONDS                   
         BNE   *+8                                                              
         OI    SVEDEMSW,X'08'                                                   
         CLI   0(R4),C'R'           NO EQUIVALENCING                            
         BNE   *+8                                                              
         OI    SVEDEMSW,X'04'                                                   
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  IMP EQUIVALENCE SWITCH                                                       
*  E = 30 SECOND EQUIVALENCE, R = RAW NO EQUIVALENCE                            
*  BLANK = USE PROFILE                                                          
*                                                                               
INIMPEQU NI    SVEDEMSW,X'FC'       CLEAR THE SWITCHES                          
         CLI   0(R4),C'E'           EQUIVALENCE AT 30 SECONDS                   
         BNE   *+8                                                              
         OI    SVEDEMSW,X'02'                                                   
         CLI   0(R4),C'R'           NO EQUIVALENCING                            
         BNE   *+8                                                              
         OI    SVEDEMSW,X'01'                                                   
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  SOURCE VALIDATION                                                            
*                                                                               
INSOURCE XC    SVSOURC,SVSOURC      SOURCE    --                                
         EX    R5,*+4                                                           
         MVC   SVSOURC(0),0(R4)                                                 
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  MISSED PROGRAM CODE VALIDATION                                               
*                                                                               
INMSPRG  MVC   SVMSPROG,SVMSPROG    PROGRAM CODE                                
         EX    R5,*+4                                                           
         MVC   SVMSPROG(0),0(R4)                                                
         OC    SVMSPROG,SPACES                                                  
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  PROGRAM CODE TABLE (FRONTRUNNER)                                             
*                                                                               
INFRPRCD LA    R6,SVWIZCLT                                                      
         LA    RF,89                                                            
*                                                                               
INFRPR20 CLI   0(R6),0                                                          
         BE    INFRPR50                                                         
         LA    R6,6(R6)                                                         
         BCT   RF,INFRPR20                                                      
*                                                                               
INFRPR50 EX    R5,*+4                                                           
         MVC   0(0,R6),0(R4)                                                    
         OC    0(6,R6),SPACES                                                   
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  PROGRAM CODE VALIDATION                                                      
*                                                                               
INPRGCD  MVC   SVPRGCD,SPACES       PROGRAM CODE                                
         EX    R5,*+4                                                           
         MVC   SVPRGCD(0),0(R4)                                                 
         OC    SVPRGCD,SPACES                                                   
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  PROGRAM NAME                                                                 
*                                                                               
INPRNAM  MVC   SVPRNAM,SPACES       PROGRAM NAME                                
         EX    R5,*+4                                                           
         MVC   SVPRNAM(0),0(R4)                                                 
         OC    SVPRNAM,SPACES                                                   
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  CLIENT VALIDATION                                                            
*                                                                               
INCLIENT XC    QCLT,QCLT                                                        
         EX    R5,*+4                                                           
         MVC   QCLT(0),0(R4)                                                    
         GOTO1 VALICLT                                                          
*                                                                               
         LA    RE,SVCLI                                                         
         OC    SVCLI,SVCLI                                                      
         BZ    INCLT50                                                          
         LA    RF,5                                                             
INCLT20  CLI   0(RE),X'FF'                                                      
         BE    INCLT50                                                          
         CLC   BCLT,0(RE)          ALREADY IN THERE?                            
         BE    RCV10                                                            
         LA    RE,2(RE)                                                         
         BCT   RF,INCLT20                                                       
         DC    H'0'                                                             
INCLT50  MVC   0(2,RE),BCLT                                                     
         MVI   2(RE),X'FF'                                                      
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  ESTIMATE                                                                     
*                                                                               
INPKEST  MVI   BEST,0                                                           
         CLI   0(R4),0                                                          
         BE    RCV10                                                            
INEST    MVC   BEST,0(R4)                                                       
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  SEARCH PACKAGE VALIDATIONS                                                   
*                                                                               
INPKDPT  MVI   SPKDPT,0             DAYPART CODE                                
         EX    R5,*+4                                                           
         MVC   SPKDPT(0),0(R4)                                                  
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
INPKSTAT MVC   SPKSTAT,0(R4)        PACKAGE STATUS                              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
INPKESTD XC    SPKESTD,SPKESTD                                                  
         CLI   0(R4),0                                                          
         BE    RCV10                                                            
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB      PACKAGE ESTIMATE START            
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SPKESTD)    MAKE COMPRESSED              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
INPKEEND XC    SPKEEND,SPKEEND                                                  
         CLI   0(R4),0                                                          
         BE    RCV10                                                            
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB      PACKAGE ESTIMATE END              
         GOTO1 VDATCON,DMCB,(0,DUB),(2,SPKEEND)    MAKE COMPRESSED              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  PRODUCT VALIDATION                                                           
*                                                                               
INPROD2  LA    R6,QPRD2                                                         
         MVI   MYFLAG,2                                                         
         B     INPROD10                                                         
*                                                                               
INPROD1  LA    R6,QPRD                                                          
         MVI   MYFLAG,1                                                         
*                                                                               
INPROD10 XC    0(3,R6),0(R6)                                                    
         EX    R5,*+4                                                           
         MVC   0(0,R6),0(R4)                                                    
         OC    0(3,R6),SPACES                                                   
         CLI   MYFLAG,2                                                         
         BE    INPRD20                                                          
         GOTO1 VALIPRD                                                          
         B     INPRD30                                                          
*                                                                               
INPRD20  GOTO1 VALIPR2                                                          
*                                                                               
INPRD30  LA    RE,SVPRD                                                         
         OC    SVPRD,SVPRD                                                      
         BZ    INPRD50                                                          
         LA    RF,25                                                            
INPRD40  CLI   0(RE),X'00'                                                      
         BE    INPRD50                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,INPRD40                                                       
         DC    H'0'                                                             
INPRD50  MVC   0(1,RE),BPRD                                                     
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  LENGTH VALIDATION                                                            
*                                                                               
INLENGTH XC    SVLENGTH,SVLENGTH                                                
         EX    R5,*+4                                                           
         MVC   SVLENGTH,0(R4)                                                   
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  PACKAGE                                                                      
*                                                                               
INPKPKGE MVI   BPKG,0                                                           
         CLI   0(R4),0                                                          
         BE    RCV10                                                            
INPACKGE MVC   BPKG,0(R4)           PACKAGE NUMBER                              
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  MISSED PACKAGE VALIDATION                                                    
*                                                                               
INMSPACK XC    SVMSPAKG,SVMSPAKG                                                
         EX    R5,*+4                                                           
         MVC   SVMSPAKG,0(R4)                                                   
         OC    SVMSPAKG,SPACES                                                  
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  BUY TYPE                                                                     
*                                                                               
INBUYTYP XC    SVBUYTYP,SVBUYTYP                                                
         EX    R5,*+4                                                           
         MVC   SVBUYTYP,0(R4)                                                   
         OC    SVBUYTYP,SPACES                                                  
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  ACTUAL COST                                                                  
*                                                                               
INACTCST XC    SVACTCST,SVACTCST                                                
         EX    R5,*+4                                                           
         MVC   SVACTCST,0(R4)                                                   
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  LINE NUMBER                                                                  
*                                                                               
INPRSLIN XC    SVLINE,SVLINE                                                    
         EX    R5,*+4                                                           
         MVC   SVLINE,0(R4)                                                     
         OC    SVLINE,SPACES                                                    
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  MISSED LINE NUMBER                                                           
*                                                                               
INMSSLIN XC    SVMSLINE,SVMSLINE                                                
         EX    R5,*+4                                                           
         MVC   SVMSLINE,0(R4)                                                   
         OC    SVMSLINE,SPACES                                                  
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  FILTERS VALIDATION                                                           
*                                                                               
INNOBILL MVI   BYTE,X'80'                                                       
         B     FILT1SET                                                         
*                                                                               
INNODEMO MVI   BYTE,X'40'                                                       
         B     FILT1SET                                                         
*                                                                               
INNOLOCK MVI   BYTE,X'20'                                                       
         B     FILT1SET                                                         
*                                                                               
INNOMGD  MVI   BYTE,X'10'                                                       
         B     FILT1SET                                                         
*                                                                               
INNOMISS MVI   BYTE,X'08'                                                       
         B     FILT1SET                                                         
*                                                                               
INNOPAID MVI   BYTE,X'04'                                                       
         B     FILT1SET                                                         
*                                                                               
INNOPRMP MVI   BYTE,X'02'                                                       
         B     FILT1SET                                                         
*                                                                               
FILT1SET CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OC    SVFILT1,BYTE                                                     
         B     RCV10                                                            
*                                                                               
*  CABLE MATCHMAKER VALIDATIONS                                                 
*                                                                               
         SPACE 2                                                                
*                                                                               
*  GROUP CODE                                                                   
*                                                                               
INGRP    BRAS  RE,GETPROF           GET AGENCY PROFILES                         
         MVC   QGRP,SPACES                                                      
         EX    R5,*+4                                                           
         MVC   QGRP(0),0(R4)                                                    
         B     RCV10                                                            
*                                                                               
*  PASSED MATHED HEADER INFORMATION                                             
*                                                                               
INMATCH  NI    SVHDFLAG,X'FF'-FLSNDMAT   TURN BIT OFF                           
         CLI   0(R4),C'Y'          SEND MATCHED DATA?                           
         BNE   RCV10                NO                                          
         OI    SVHDFLAG,FLSNDMAT                                                
         B     RCV10                                                            
*                                                                               
*  BUYER CODE                                                                   
*                                                                               
INBYR    MVC   QBYR,SPACES                                                      
         EX    R5,*+4                                                           
         MVC   QBYR(0),0(R4)                                                    
         OC    QBYR,SPACES                                                      
         XC    QSPV,QSPV                                                        
         B     RCV10                                                            
*                                                                               
*  SUPERVISOR CODE                                                              
*                                                                               
INSPV    MVC   QSPV,SPACES                                                      
         EX    R5,*+4                                                           
         MVC   QSPV(0),0(R4)                                                    
         OC    QSPV,SPACES                                                      
         XC    QBYR,QBYR                                                        
         B     RCV10                                                            
*                                                                               
*  MEDIA CODE                                                                   
*                                                                               
*INMED    MVI   INVHMED,X'40'                                                   
*         EX    R5,*+4                                                          
*         MVC   INVHMED(0),0(R4)                                                
*         B     RCV10                                                           
*                                                                               
* I2 REQUEST REQUESTOR                                                          
*                                                                               
INQSTR   XC    QUESTOR,QUESTOR                                                  
         LA    R1,QUESTOR                                                       
         B     INHMOV                                                           
*                                                                               
* I2 REQUEST WHEN                                                               
*                                                                               
INQWHEN  MVC   QWHEN,0(R4)                                                      
         B     RCV10                                                            
*                                                                               
* I2 REQUEST DESTINATION                                                        
*                                                                               
INQDEST  LA    R1,QDEST                                                         
         B     INHMOV                                                           
*                                                                               
* I2 REQUEST BOOK                                                               
*                                                                               
INQBOOK  XC    WORK,WORK           CREATE DUMMY FLDHDR                          
         EX    R5,*+4                                                           
         MVC   WORK+8(0),0(R4)                                                  
*                                                                               
         MVC   QBOOK,WORK+8                                                     
         CLC   =C'ACT',WORK+8                                                   
         BE    IQB20                                                            
*                                                                               
IQB10    AHI   R5,1                                                             
         STC   R5,WORK+5                                                        
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A00'   GET BOOKVAL ADDRESS                 
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(C'N',WORK),(1,DUB),VSCANNER                           
*                                                                               
         TM    DUB,X'BF'           TEST ANY GARBAGE OPTIONS INPUT               
         BNZ   INBKERR                                                          
         CLI   4(R1),0                                                          
         BE    INBKERR                                                          
         GOTO1 VDATCON,DMCB,(3,DUB+1),WORK+32                                   
         MVC   QBOOK,WORK+32       SAVE EBCDIC YYMM                             
*                                                                               
IQB20    MVC   QHUT,=C'NO'         SET DEFAULT HUT VALUE                        
         B     RCV10                                                            
*                                                                               
INBKERR  MVI   ERROR+1,BADBOOK                                                  
         GOTO1 SENDMSG                                                          
*                                                                               
* I2 REQUEST HUT                                                                
*                                                                               
INQHUT   CLC   =C'NO',0(R4)                                                     
         BE    INQHUTX                                                          
*                                                                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB(0),0(0,R4)                                                   
         CVD   R0,DUB                                                           
         UNPK  QHUT,DUB            SAVE NUMERIC MONTH                           
INQHUTX  B     RCV10                                                            
*                                                                               
* COMMENTS                                                                      
*                                                                               
INQCOM   DS    0H                  I2 REQUEST COMMENTS                          
         L     R6,AIO2                                                          
         CLI   CMTCOUNT,0                                                       
         BE    INQCOM4                                                          
* FIND END OF BUFFER                                                            
         SR    R0,R0                                                            
INQCOM2  ICM   R0,1,0(R6)                                                       
         BZ    INQCOM4                                                          
         AR    R6,R0                                                            
         B     INQCOM2                                                          
*                                                                               
INQCOM4  IC    R0,CMTCOUNT                                                      
         AHI   R0,1                                                             
         STC   R0,CMTCOUNT                                                      
*                                                                               
         LTR   R5,R5               ANY INPUT?                                   
         BNM   INQCOM6             YES                                          
         MVI   0(R6),2             CREATE COMMENT WITH NO DATA                  
         MVI   1(R6),0                                                          
         MVI   2(R6),0                                                          
         B     INQCOMX                                                          
*                                                                               
INQCOM6  LA    R0,2(R5)            GET L'COMMENT + 1 (FOR LENGTH)               
         STC   R0,0(R6)                                                         
         EX    R5,*+4                                                           
         MVC   1(0,R6),0(R4)       MOVE COMMENT DATA                            
         LA    RE,2(R5,R6)         POINT PAST COMMENT                           
         MVI   0(RE),0             AND SET EOD FLAG                             
*                                                                               
INQCOMX  B     RCV10                                                            
         EJECT                                                                  
*                                                                               
INHMOV   EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R4)                                                    
         B     RCV10                                                            
*                                                                               
*  DELETE COMMENTS?                                                             
*                                                                               
INDELCOM NI    SVCOMFLG,X'FF'-SVCMDELQ                                          
         CLI   0(R4),C'Y'                                                       
         BNE   *+8                                                              
         OI    SVCOMFLG,SVCMDELQ                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  INVOICE #                                                                    
*                                                                               
ININV    MVC   SVINV,0(R4)                                                      
         OC    SVINV,SPACES                                                     
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
*  IGNORE FEATURES                                                              
*                                                                               
INIGFILM NI    SVIGNORE,X'FF'-SVIFILMQ                                          
         CLI   0(R4),C'Y'                                                       
         BNE   *+8                                                              
         OI    SVIGNORE,SVIFILMQ                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
INIGCOST NI    SVIGNORE,X'FF'-SVICOSTQ                                          
         CLI   0(R4),C'Y'                                                       
         BNE   *+8                                                              
         OI    SVIGNORE,SVICOSTQ                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
INIGICST NI    SVIGNORE,X'FF'-SVIICSTQ                                          
         CLI   0(R4),C'Y'                                                       
         BNE   *+8                                                              
         OI    SVIGNORE,SVIICSTQ                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
INIGTIME NI    SVIGNORE,X'FF'-SVITIMEQ                                          
         CLI   0(R4),C'Y'                                                       
         BNE   *+8                                                              
         OI    SVIGNORE,SVITIMEQ                                                
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
INIGSEP  NI    SVIGNORE,X'FF'-SVISEPQ                                           
         CLI   0(R4),C'Y'                                                       
         BNE   *+8                                                              
         OI    SVIGNORE,SVISEPQ                                                 
         B     RCV10                                                            
         SPACE 1                                                                
*                                                                               
INIGLEN  NI    SVIGNORE,X'FF'-SVILENQ                                           
         CLI   0(R4),C'Y'                                                       
         BNE   *+8                                                              
         OI    SVIGNORE,SVILENQ                                                 
         B     RCV10                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
INDEMO   NTR1  BASE=*,LABEL=*                                                   
         LR    R6,R4                                                            
*                                                                               
         OC    SVCSLIC,SVCSLIC     ALREADY HAVE COMSCORE LICENSE?               
         JNZ   INDEMO06                                                         
         MVC   HALF,TWAAGY         SET SECURITY AGY                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING CT5KEY,RF                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,TWAAGY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO4               
         CLC   KEY(L'CT5KEY),KEYSAVE                                            
         JNE   INDEMO04                                                         
*                                                                               
         L     RF,AIO4                                                          
         LA    R5,CT5DATA          POINT TO THE FIRST ELEMENT IN REC            
         USING CTSEAD,R5                                                        
INDEMO02 CLI   0(R5),0                                                          
         JE    INDEMO04                                                         
         CLI   0(R5),CTSEAELQ      ALREADY ON SECURITY ALPHA ELEM?              
         JE    *+16                YES                                          
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         J     INDEMO02                                                         
         MVC   HALF,CTSEAAID                                                    
*                                                                               
INDEMO04 XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING TOKRECD,RF          BUILD TOKEN RECORD KEY                       
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
         MVC   TOKKSAGY,HALF       SECURITY AGENCY ALPHA                        
         MVC   TOKKAAGY,TWAAGY     AND AGENCY ALPHA                             
         MVI   TOKKSYS,X'03'       NET SYSTEM                                   
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                
         CLC   KEY(L'TOKKEY),KEYSAVE                                            
         JNE   INDEMO06                                                         
                                                                                
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,AIO4,WORK            
                                                                                
         L     R5,AIO4                                                          
         AHI   R5,TOKFIRST                                                      
         USING RTAUTHD,R5                                                       
         CLI   0(R5),RTAUTELQ      X'0A' ELEMENT                                
         JNE   INDEMO06                                                         
         OC    RTAUTID(L'RTAUTID+L'RTAUTSEC),RTAUTID                            
         JZ    INDEMO06                                                         
         MVC   SVCSLIC,RTAUTID     SAVE USER TOKEN FOR DEMOVAL                  
         DROP  R5,RF                                                            
*                                                                               
INDEMO06 XC    QDEMOS,QDEMOS                                                    
         MVI   QDEMOS,X'FF'                                                     
*                                                                               
         L     RF,ATWA             COMSCORE DEMO NAME LIST                      
         AHI   RF,QNTDMS-TWAD                                                   
         XC    0(L'QNTDMS,RF),0(RF)                                             
         MVI   0(RF),X'FF'                                                      
         MVI   BYTE,0              USED FOR COMSCORE DEMO INDEX                 
*                                                                               
INDEMO08 XC    BLOCK(256),BLOCK    DEMOS EXIST ...                              
         LA    RF,BLOCK                                                         
         USING DBLOCK,RF           SET UP CALL TO DEMOCON                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         TM    SVINDS1,SVI1NTIQ    NTI DEMOS?                                   
         JZ    *+14                                                             
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         DROP  RF                                                               
*                                                                               
* INDEMOS DOES ONE DEMO CATEGORY AT A TIME THEREFORE THE COMSCORE               
* DEMO INDEX FROM DEMOVAL WILL ALWAYS BE 1.  WE USE BYTE TO KEEP THE            
* REAL INDEX BASED ON THE # OF COMSCORE DEMOS WE HAVE.                          
*                                                                               
         CLI   0(R6),C'X'          COMSCORE DEMO?                               
         JE    *+12                                                             
         CLI   1(R6),C'X'          COMSCORE DEMO?                               
         JNE   INDEMO10                                                         
         ZIC   RF,BYTE             SET INDEX FOR COMSCORE DEMO                  
         AHI   RF,1                                                             
         STC   RF,BYTE                                                          
*                                                                               
INDEMO10 XC    WORK,WORK                                                        
         XC    WORK2(50),WORK2                                                  
*  BUILD OUTPUT FIELD FOR DEMOVAL                                               
         LA    RE,WORK2+8                                                       
         SR    RF,RF                                                            
INDEMO12 CLI   0(R6),0              CHECK FOR END                               
         BE    INDEMO14                                                         
         CLI   0(R6),C','           CHECK FOR COMMA                             
         BE    INDEMO14                                                         
         MVC   0(1,RE),0(R6)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    R6,1(R6)                                                         
         B     INDEMO12                                                         
INDEMO14 STCM  RF,1,WORK2+5                                                     
         LA    RF,8(RF)                                                         
         STCM  RF,1,WORK2                                                       
*                                                                               
         XC    WORK2+50(P5XDLNQ),WORK2+50                                       
         LA    RF,WORK2+50                                                      
         USING P5XD,RF                                                          
         MVC   P5XID,=C'P5X '                                                   
         LA    RE,SVCSLIC                                                       
         ST    RE,P5XLICNS         A(32 BYTE COMSCORE LICENSE)                  
         ST    RF,DMCB+16          EXTENDED PARAM5                              
         OI    DMCB+16,X'80'       SO DEMOVAL KNOWS EXTENDED BLOCK              
*                                                                               
         TM    SVINDS1,SVI1NTIQ    NTI DEMOS?                                   
         JZ    INDEMO16                                                         
         GOTO1 VDEMOVAL,DMCB,(1,WORK2),(1,FULL),BLOCK,WORK,,BLOCK               
         J     INDEMO18                                                         
*                                                                               
INDEMO16 GOTO1 VDEMOVAL,DMCB,(1,WORK2),(1,FULL),(C'S',BLOCK),WORK,,    *        
               BLOCK                                                            
INDEMO18 CLI   DMCB+4,0             CHECK FOR ERROR                             
         BNE   INDEMO20                                                         
*                                                                               
* MOVE ERROR OUT                                                                
         MVC   ERROR,=AL2(2)       INVALID INPUT                                
         ZIC   RE,WORK2+5          SKIP CLIENT CONVERSION ALREADY SET           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ERRORMSG(0),WORK2+8                                              
         GOTO1 SENDMSG                                                          
         B     INDEMOX                                                          
*                                                                               
*  MOVE DEMO INTO QDEMOS LIST                                                   
*                                                                               
INDEMO20 LA    RE,QDEMOS                                                        
         LA    RF,24                                                            
INDEMO22 CLI   0(RE),X'FF'                                                      
         BE    INDEMO24                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,INDEMO22                                                      
         DC    H'0'                 TOO MANY DEMOS PASSED                       
INDEMO24 MVC   0(3,RE),FULL         DEMO NUMBER                                 
         MVI   3(RE),X'FF'          SET END OF TABLE                            
         XC    FULL,FULL                                                        
*                                                                               
         CLI   2(RE),0              COMSCORE DEMO?                              
         BNE   INDEMO28                                                         
         MVC   1(1,RE),BYTE         SET REAL COMSCORE INDEX                     
*                                                                               
         L     RF,ATWA              SAVE COMSCORE DEMO NAME IN LIST             
         AHI   RF,QNTDMS-TWAD                                                   
INDEMO26 CLI   0(RF),X'FF'          FIND FIRST AVAILABLE SLOT                   
         JE    *+12                                                             
         AHI   RF,CDEMLNQ                                                       
         J     INDEMO26                                                         
*                                                                               
         MVC   0(CDEMLNQ,RF),BLOCK  SAVE IT                                     
         MVI   CDEMLNQ(RF),X'FF'                                                
*                                                                               
*  SEE IF ANY MORE DEMOS                                                        
*                                                                               
INDEMO28 CLI   0(R6),0                                                          
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     INDEMO08                                                         
*                                                                               
*  CONVERT IMPRESSIONS TO A "T" PREFIX/MOVE INTO QDEMOS                         
*                                                                               
         LA    RE,QDEMOS                                                        
         MVI   BYTE,C'T'            SET IMPS TO THOUSANDS                       
         CLI   SVSOURC,C'N'                                                     
         BE    INDEMO30                                                         
         CLI   SVSOURC,C'S'                                                     
         BE    INDEMO30                                                         
         MVI   BYTE,C'H'            SET IMPS TO HUNDREDS                        
INDEMO30 CLI   0(RE),X'FF'                                                      
         BE    INDEMOX                                                          
         CLI   1(RE),C'I'                                                       
         BE    INDEMO32                                                         
         CLI   1(RE),C'T'                                                       
         BE    INDEMO32                                                         
         CLI   1(RE),C'H'                                                       
         BNE   *+10                                                             
INDEMO32 MVC   1(1,RE),BYTE                                                     
         LA    RE,3(RE)                                                         
         B     INDEMO30                                                         
*                                                                               
INDEMOX  J     EXIT                                                             
         LTORG                                                                  
*                                                                               
INBNWIN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ANETBLK                                                       
         USING BUYUPLDD,R6                                                      
         CLI   0(R4),0                                                          
         JE    EXIT                                                             
         MVI   RUPNWINL,L'RUPNWIN                                               
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,RUPNWIN)    MAKE COMPRESSED              
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
INBOWIN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ANETBLK                                                       
         USING BUYUPLDD,R6                                                      
         CLI   0(R4),0                                                          
         JE    EXIT                                                             
         MVI   RUPOWINL,L'RUPOWIN                                               
         GOTO1 VDATVAL,DMCB,(0,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,(0,DUB),(2,RUPOWIN)    MAKE COMPRESSED              
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
INBNCS#  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ANETBLK                                                       
         USING BUYUPLDD,R6                                                      
         EX    R5,*+4                                                           
         MVC   RUPNCS#(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNCS#L       COMSCORE SERIES NUMBER                       
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
INBOCS#  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ANETBLK                                                       
         USING BUYUPLDD,R6                                                      
         EX    R5,*+4                                                           
         MVC   RUPOCS#(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOCS#L       COMSCORE SERIES NUMBER                       
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
INBNAIR  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ANETBLK                                                       
         USING BUYUPLDD,R6                                                      
         EX    R5,*+4                                                           
         MVC   RUPNAIR(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPNAIRL       AIR NETWORK INFO                             
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
INBOAIR  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ANETBLK                                                       
         USING BUYUPLDD,R6                                                      
         EX    R5,*+4                                                           
         MVC   RUPOAIR(0),0(R4)                                                 
         LA    R5,1(R5)                                                         
         STCM  R5,1,RUPOAIRL       AIR NETWORK INFO                             
         J     EXIT                                                             
         DROP  R6                                                               
*====================================================================*          
* INITIALIZE FALINK                                                             
*====================================================================*          
         SPACE 1                                                                
INIFALNK NTR1  BASE=*,LABEL=*                                                   
         CLI   SVXFROV,0           TEST RETURN FROM GLOBBER                     
         BNE   INI2                                                             
         LA    R0,FABLK                                                         
         LA    R1,FALINKDL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SVOLDRCV,SVOLDRCV                                                
         XC    SVREASON,SVREASON                                                
*                                                                               
INI2     MVC   SVRESUME,SVXFROV    SAVE THE GLOBBING OVERLAY NUMBER             
         MVI   SVXFROV,0           AND CLEAR THIS FLAG NOW !                    
*                                                                               
         OI    NAVSERVH+1,X'01'    SERVICE REQUEST ALWAYS MODIFIED              
         OI    NAVSERVH+6,X'80'                                                 
*                                                                               
                                                                                
         LA    R2,FABLK                                                         
         ST    R2,AFABLK           FOR OTHER OVERLAYS                           
         USING FALINKD,R2                                                       
         LA    R1,NAVINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R1,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
*                                                                               
         L     R1,ACOMFACS             A(SWITCH)                                
         L     R1,CSWITCH-COMFACSD(R1)                                          
         ST    R1,FALASWCH                                                      
*                                                                               
         L     R0,=A(RECEIVE)                                                   
         A     R0,BASERELO                                                      
         ST    R0,FALARCV                                                       
*                                                                               
         L     R0,=A(SEND)                                                      
         A     R0,BASERELO                                                      
         ST    R0,FALASND                                                       
*                                                                               
         L     R0,=A(BREAK)                                                     
         A     R0,BASERELO                                                      
         ST    R0,FALASTP                                                       
*                                                                               
         L     R0,=A(RESUME)                                                    
         A     R0,BASERELO                                                      
         ST    R0,FALARSM                                                       
*                                                                               
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R1,FAMSGBLK             A(MESSAGE BLOCK)                         
         ST    R1,FALAMSG                                                       
*                                                                               
         LA    R1,FACON                A(CONTROL FIELD BUFFER)                  
         ST    R1,FALACON                                                       
         L     R1,ATWA                                                          
         AH    R1,=Y(SVFALINK-TWAD) A(FALINK SAVED STORAGE)                     
         ST    R1,FALASVE                                                       
*                                                                               
         L     R0,=A(FAMAP)            A(MAP TABLE)                             
         A     R0,BASERELO                                                      
         ST    R0,FALAMAP                                                       
         ST    R0,AMAPTAB          FOR OTHER OVERLAYS                           
*                                                                               
         MVC   FALAPGS,=AL4(FALATMSA)                                           
*                                                                               
*  EACH TRACK = 3 PAGES                                                         
         MVI   FALUNCIS,12         TEMPEST TRACKS 30 PAGES FOR UPLOAD           
*                                                                               
         L     R1,FALABLD          POINT TO CONTROL FIELD HEADER                
         USING FALSCRD,R1                                                       
         CLI   FALCONA,C' '                                                     
         BNH   INIX                                                             
         CLI   FALCONA,FADOWN      TEST FOR FALINK ACTIONS                      
         BE    INI10                                                            
         CLI   FALCONA,FAUPLD                                                   
         BE    INI10                                                            
         CLI   FALCONA,FAVER       TEST FOR VERSION(S)                          
         BE    INI10                                                            
         CLI   FALCONA,FAVERE                                                   
         BE    INI10                                                            
         CLI   FALCONA,FANVER                                                   
         BE    INI10                                                            
         CLI   FALCONA,FANVERE                                                  
         BNE   INIGO3              NOT FALINK CALL 03 OVERLAY                   
                                                                                
INI10    CLI   FALCONA,FANVER      C'P' - TEST 1ST PAGE OF NEW UPLD             
         BNE   INIX                NO, JUST USE LAST SVOLAY                     
                                                                                
         MVI   SVOLAY,0                                                         
         GOTO1 VGETFACT,DMCB,(X'80',0),F#FAL1ST                                 
*                                                                               
         L     R1,0(,R1)           GET RETURN BLOCK                             
*                                                                               
         CLC   DDLKMAP,0(R1)       ONE SPECIAL TEST!                            
         JE    INIGO3                                                           
*                                                                               
         LA    RE,BEQUS                                                         
         LA    RF,BEQUX                                                         
         CLI   0(R1),C'B'                                                       
         JNE   INI30                                                            
*                                                                               
INI20    CLC   0(4,R1),0(RE)                                                    
         JE    INIGO3                                                           
         LA    RE,4(RE)                                                         
         CR    RE,RF                                                            
         JL    INI20                                                            
         J     INIX                                                             
*                                                                               
INI30    LA    RE,CEQUS                                                         
         LA    RF,CEQUX                                                         
         CLI   0(R1),C'C'                                                       
         JNE   INI40                                                            
*                                                                               
INI32    CLC   0(5,R1),0(RE)                                                    
         JE    INIGO3                                                           
         LA    RE,5(RE)                                                         
         CR    RE,RF                                                            
         JL    INI32                                                            
         J     INIX                                                             
*                                                                               
INI40    LA    RE,DEQUS                                                         
         LA    RF,DEQUX                                                         
         CLI   0(R1),C'D'                                                       
         JNE   INIX                                                             
*                                                                               
INI42    CLC   0(6,R1),0(RE)                                                    
         JE    INIGO3                                                           
         LA    RE,6(RE)                                                         
         CR    RE,RF                                                            
         JL    INI42                                                            
         J     INIX                                                             
                                                                                
INIGO3   MVI   SVOLAY,3            SET TO CALL 03 OVERLAY                       
*                                                                               
INIX     CLI   SVOLAY,0            SET CC=NEQ FOR GO TO OVERLAY                 
         XIT1                                                                   
         DROP  R1,R2                                                            
*                                                                               
DDLKMAP  DC    C'D=F'              DDLINK MAPS                                  
*                                                                               
BEQUS    EQU   *                                                                
UNTDLDL  DC    C'B=20'             UNIT DOWNLOAD IDENTIFIER                     
ILGDLDL  DC    C'B=21'             ILOG DOWNLOAD IDENTIFIER                     
NPRO_DL  DC    C'B=92'             NET PROFILE DOWNLOAD                         
NCML_DL2 DC    C'B=97'             COMMERCIAL DOWNLOAD                          
NCTX_DL  DC    C'B=98'             COMTEXT DOWNLOAD                             
NCCL_DL  DC    C'B=99'             COMCLASS DOWNLOAD                            
NFED_DL  DC    C'B=9A'             FEED DOWNLOAD                                
NPAT_DL  DC    C'B=9B'             PATTERN DOWNLOAD                             
NPGR_DL  DC    C'B=9C'             PRODUCT GROUP DOWNLOAD                       
BEQUX    EQU   *                                                                
*                                                                               
CEQUS    EQU   *                                                                
CFMCUPL  DC    C'C=121'            CFM CLIENT UPLOAD                            
CFMPUPL  DC    C'C=122'            CFM PRODUCT UPLOAD                           
QNIBLD   DC    C'C=125'            NINS/GEN DOWNLOAD (BEFORE XFRCTL)            
QNIDOWN  DC    C'C=126'            NINS/GEN DOWNLOAD (AFTER XFRCTL)             
QNIGEN   DC    C'C=127'            NINS/GEN                                     
QNTULST  DC    C'C=150'            NET UNIT LIST                                
QNTASGNL DC    C'C=152'            NET ASGN LIST                                
QNTASGNU DC    C'C=154'            NET ASGN UPLD                                
NPATG    DC    C'C=499'            NET PATTERN GEN UPLOAD                       
NCML_UP  DC    C'C=500'            NET COMMERCIAL UPLOAD                        
NPAT_UP  DC    C'C=501'            NET PATTERN UPLOAD                           
CEQUX    EQU   *                                                                
*                                                                               
DEQUS    EQU   *                                                                
NCLT_DL  DC    C'D=0082'           NET CLIENT DOWNLOAD                          
NCML_DL  DC    C'D=0093'           NET COMML DOWNLOAD                           
NHSE_DL  DC    C'D=0094'           NET HOUSE DOWNLOAD                           
NRCP_DL  DC    C'D=0095'           NET RECAP DOWNLOAD                           
NCPH_DL  DC    C'D=0096'           NET CLIENT PRODUCTION HOUSE DL               
*                                                                               
         DC    C'D=0125'           NINS/GEN DOWNLOAD (BEFORE XFRCTL)            
         DC    C'D=0126'           NINS/GEN DOWNLOAD (AFTER XFRCTL)             
QNISUB   DC    C'D=0127'           NINS/GEN UPLOAD   (BEFORE XFRCTL)            
QNIEND   DC    C'D=0128'           NINS/GEN UPLOAD   (AFTER XFRCTL)             
*                                                                               
CMMDLDL  DC    C'D=0132'           CABLE MATCHMAKER DOWNLOAD                    
UONDLDL  DC    C'D=0133'           UNITS ORDERED NOT RUN                        
FUA      DC    C'D=0139'           FUA                                          
BARUPL   DC    C'D=0140'           BA RULES UPLOAD                              
LIMUPL   DC    C'D=0145'           LIMIT UPLOAD                                 
*                                                                               
QNLBLD   DC    C'D=0150'           NET/LIST DOWNLOAD (BEFORE XFRCTL)            
QNLDOWN  DC    C'D=0151'           NET/LIST DOWNLAD  (AFTER XFRCTL)             
QNASUB   DC    C'D=0152'           NET/ASS SUBMIT (BEFORE XFRCTL)               
QNAEND   DC    C'D=0153'           NET/ASS END    (AFTER XFRCTL)                
QRNTGS   DC    C'D=0161'           RATING SOURCE DOWNLOAD                       
DEQUX    EQU   *                                                                
*                                                                               
TWAPGS   DC    AL4(FALATMS)                                                     
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
INSTRUCR NTR1  BASE=*,LABEL=*                                                   
         XC    TSARRCLN,TSARRCLN                                                
         CLI   0(R4),C'A'                                                       
         BE    INSTRUCX                                                         
         CLI   0(R4),C'B'                                                       
         BNE   *+12                                                             
         LHI   R0,RUPPALEN                                                      
         STCM  R0,3,TSARRCLN                                                    
         CLI   0(R4),C'C'                                                       
         BNE   *+12                                                             
         LHI   R0,RUPSPLEN                                                      
         STCM  R0,3,TSARRCLN                                                    
         CLI   0(R4),C'D'                                                       
         BNE   INSTRUCX                                                         
         LHI   R0,RUPASLEN                                                      
         STCM  R0,3,TSARRCLN                                                    
INSTRUCX XIT1                                                                   
         LTORG                                                                  
         SPACE 1                                                                
*                                                                               
* MISSED UNITS                                                                  
*                                                                               
INBYSMK2 NTR1  BASE=*,LABEL=*                                                   
         L     R6,ANETBLK                                                       
         USING BUYUPLDD,R6                                                      
         LA    RE,RUPMISS                                                       
         LA    RF,40                                                            
         CLI   0(RE),X'40'                                                      
         BNH   INMISS80                                                         
*                                                                               
INMISS20 LA    RE,1(RE)                                                         
         CLI   0(RE),X'40'                                                      
         BH    INMISS50                                                         
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         ZIC   RF,RUPMISSL          UPDATE THE LENGTH                           
         LA    RF,1(RF)                                                         
         STCM  RF,1,RUPMISSL                                                    
         B     INMISS80                                                         
INMISS50 BCT   RF,INMISS20                                                      
         DC    H'0'                                                             
*                                                                               
INMISS80 EX    R5,*+4                                                           
         MVC   0(0,RE),0(R4)        MOVE OUT INFORMATION                        
*                                                                               
         LA    R5,1(R5)                                                         
         ZIC   RF,RUPMISSL          UPDATE THE LENGTH                           
         AR    RF,R5                                                            
         STCM  RF,1,RUPMISSL                                                    
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
* MOVE DEMO OVERRIDE AMOUNT TO RECORD                                           
*                                                                               
MOVEDEM  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    R6,RUPDEMOS          ESTIMATE OVERRIDE TABLE                     
         CLI   BYTE2,C'A'                                                       
         BNE   *+8                                                              
         LA    R6,RUPDEMOA          ACTUAL OVERRIDE TABLE                       
         MVI   BYTE2,0                                                          
*                                                                               
         LA    RF,75                                                            
         CLI   1(R6),USERMOD        CHECK USER DEMO                             
         BNE   MOVDM50                                                          
         CLI   0(R6),0                                                          
         BE    MOVDM80                                                          
         B     *+12                                                             
MOVDM50  CLI   3(R6),X'FF'          CHECK NO VALUE INDICATOR                    
         BE    MOVDM90              MOVE VALUE OUT                              
         CLI   1(R6),0              CHECK NO DEMO INPUTTED                      
         BE    MOVDM80              MOVE PREVIOUS DEMO OUT                      
         LA    R6,7(R6)                                                         
         BCT   RF,MOVDM50                                                       
*                                                                               
MOVDM80  OC    0(3,R6),0(R6)        WAS CATEGORY DEFINED                        
         BNZ   MOVDM90                                                          
         LR    R5,R6                                                            
         SH    R5,=H'7'             GOT TO PREVIOUS ENTRY                       
         MVC   0(3,R6),0(R5)        USE PREVIOUS CATEGORY                       
MOVDM90  MVC   3(4,R6),0(R4)        MOVE VALUE OUT                              
         CLI   2(R6),0              COMSCORE DEMO?                              
         JE    MOVDMEX                                                          
         CLI   1(R6),USERMOD        CHECK FOR USER DEMO                         
         BNE   MOVDM100                                                         
         MVC   0(1,R6),BYTE         MOVE DEMO TYPE IN 1ST POSITION              
         B     MOVDMEX                                                          
*                                                                               
* SET UP FOR PERSONAL LANGUAGE                                                  
MOVDM100 LA    R5,BLOCK                                                         
         USING DBLOCK,R5             SET UP CALL TO DEMOCON                     
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 VDEMOCON,DMCB,(1,0(R6)),('DEMOCON_16',DUB),DBLOCK,DUB+3          
         MVC   DUB+1(1),BYTE                                                    
         GOTO1 VDEMOCON,DMCB,(1,DUB),('DEMOCON_17',0(R6)),DBLOCK,DUB+3          
*                                                                               
******   MVC   1(1,R6),BYTE         MOVE DEMO TYPE IN                           
MOVDMEX  XIT1                                                                   
         DROP  R5,RE                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CHKWZEST NTR1  BASE=*,LABEL=*     MATCH EST TO CLIENT/PROD LIST                 
         LA    R5,(L'SVWIZCLT/LNWIZCLT)     MAX NUMBER OF CLIENTS               
         LA    R2,SVWIZCLT                                                      
         USING WIZCLTD,R2                                                       
CHKWZE10 XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),WIZBCLT                                                 
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),CURREST                                                 
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    CHKWZE20                                                         
         LA    R2,LNWIZCLT(R2)                                                  
         BCT   R5,CHKWZE10                                                      
*                                                                               
         TM    WIZSTAT,X'01'       SKIP ERRORS?                                 
         BNO   *+12                                                             
         OI    WIZSTAT,X'02'       SET ERRORS FOUND                             
         B     CHKWZE20                                                         
*                                                                               
         MVC   ERROR,=AL2(237)     EST NOT ON FILE                              
         EDIT  (B1,CURREST),(3,ERRORMSG)                                        
         GOTO1 SENDMSG                                                          
CHKWZE20 J     RCVEXIT             ONE MATCHED - THAT'S ENOUGH                  
         LTORG                                                                  
                                                                                
*                                                                               
* MOVE FEED PCT AND 1ST PRODUCT PCT TO TABLE                                    
* R4 = INPUT VALUE                                                              
* BYTE = 1 1ST PRODUCT PERCENT                                                  
* BYTE = 2 FEED PERCENT                                                         
*                                                                               
CPYSPCT  NTR1  BASE=*,LABEL=*                                                   
         L     R1,ANETBLK                                                       
         USING BUYUPLDD,R1                                                      
         LA    RE,RUPCSPCT                                                      
         USING CPYSPPCT,RE                                                      
*                                                                               
         LA    RF,6                 MAX PRODUCTS                                
*                                                                               
* CHECK NEXT ENTRY PAST CURRENT POINTER                                         
* IF X'0' I AM AT LAST PRODUCT ADDED                                            
CPYSPC20 CLI   CPYSPPLN(RE),0       CHECK 1 ENTRY AHEAD                         
         JE    CPYSPC40                                                         
         LA    RE,CPYSPPLN(RE)                                                  
         JCT   RF,CPYSPC20                                                      
*                                                                               
CPYSPC40 CLI   BYTE,1                                                           
         JNE   CPYSPC60                                                         
         MVC   CPYSP1PT,0(R4)       MOVE OUT 1ST PROD PERCENT                   
         J     CPYSPCEX                                                         
*                                                                               
CPYSPC60 MVC   CPYSPFPT,0(R4)       MOVE OUT FEED PCT                           
*                                                                               
CPYSPCEX XIT1                                                                   
         LTORG                                                                  
         DROP  R1,RE                                                            
         EJECT                                                                  
*                                                                               
* MOVE FEED INDEX AND FEED TO TABLE                                             
* R4 = INPUT VALUE                                                              
*                                                                               
CPYSFD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,ANETBLK                                                       
         USING BUYUPLDD,R1                                                      
         LA    RE,RUPCSPCT                                                      
         USING CPYSPPCT,RE                                                      
*                                                                               
         LA    RF,6                 MAX PRODUCTS                                
         SR    R6,R6                                                            
*                                                                               
* GET PRODUCT INDEX NUMBER                                                      
* IF X'0' I AM AT LAST PRODUCT ADDED                                            
CPYSFD20 CLI   CPYSPPLN(RE),0       CHECK 1 ENTRY AHEAD                         
         JE    CPYSFD40                                                         
         LA    RE,CPYSPPLN(RE)                                                  
         LA    R6,1(R6)                                                         
         JCT   RF,CPYSFD20                                                      
         DROP  RE                                                               
*                                                                               
* GET NEXT SLOT IN FEED CODE TABLE                                              
CPYSFD40 LA    RE,RUPCSFD                                                       
         USING CPYSFEED,RE                                                      
*                                                                               
         LA    RF,6                 MAX PRODUCTS                                
CPYSFD60 OC    CPYFFEED,CPYFFEED                                                
         JZ    CPYSFD80                                                         
         LA    RE,CPYSPFLN(RE)                                                  
         JCT   RF,CPYSFD60                                                      
*                                                                               
CPYSFD80 STCM  R6,1,CPYFPROD        MOVE OUT PRODUCT INDEX                      
         MVC   CPYFFEED,0(R4)       FEED CODE                                   
         OC    CPYFFEED,SPACES      BLANK FILL                                  
*                                                                               
CPYSFDEX XIT1                                                                   
         LTORG                                                                  
         DROP  R1,RE                                                            
         EJECT                                                                  
*                                                                               
VALKEY   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         EX    R5,*+4                                                           
         MVC   WORK+8(0),0(R4)                                                  
         LA    R5,9(R5)                                                         
         STCM  R5,1,WORK                                                        
*                                                                               
         MVC   0(19,R3),SPACES  SPACE FILL CLI,EST,NET,PKG,PROG FIELDS          
         LR    R5,R3                                                            
         LA    R4,HEDLNTAB                                                      
         LA    R2,WORK                                                          
         ST    R2,FADDR                                                         
         XC    FTERM,FTERM                                                      
         XC    FLAST,FLAST                                                      
         MVI   FTERM,C':'                                                       
*                                                                               
VALKY50  GOTO1 FVAL,0                                                           
         CLI   FLEN,0               CHECK FOR NO INPUT                          
         BE    VALKY70                                                          
         CLC   FLEN,0(R4)          TEST FIELD AT MAXIMUM LENGTH                 
         BE    VALKY52                                                          
         SR    R1,R1               PRE-FILL WITH SPACES OR ZEROES               
         IC    R1,0(R4)                                                         
         BCTR  R1,0                                                             
         MVI   0(R5),C' '                                                       
         TM    1(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(R5),C'0'                                                       
         SHI   R1,1                                                             
         BM    VALKY52                                                          
         EX    R1,*+4                                                           
         MVC   1(0,R5),0(R5)                                                    
VALKY52  LR    RE,R5                                                            
         SR    RF,RF                                                            
         IC    RF,FLEN                                                          
         TM    1(R4),X'80'         TEST NUMERIC FIELD                           
         BZ    VALKY54                                                          
         CLC   FLEN,0(R4)          TEST FIELD AT MAXIMUM LENGTH                 
         BE    VALKY54                                                          
         SR    RE,RE                                                            
         IC    RE,0(R4)                                                         
         SR    RE,RF                                                            
         LA    RE,0(RE,R5)                                                      
VALKY54  BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),FLD                                                      
         B     VALKY80                                                          
VALKY70  SR    RE,RE               IF NO INPUT                                  
         ICM   RE,7,FLAST          MUST BUMP PAST DELIMETER                     
         LA    RE,1(RE)            SO SCAN CAN CONTINUE                         
         STCM  RE,7,FLAST                                                       
VALKY80  CLI   FSTOP,X'FF'                                                      
         BE    VALKYEX                                                          
VALKY90  ZIC   RE,0(R4)                                                         
         AR    R5,RE                                                            
         AHI   R4,L'HEDLNTAB                                                    
         B     VALKY50                                                          
*                                                                               
VALKYEX  XIT1                                                                   
*                                                                               
HEDLNTAB DS    0XL2                                                             
         DC    X'0300'              CLIENT FIELD LENGTH                         
         DC    X'0380'              ESTIMATE FIELD LENGTH                       
         DC    X'0400'              NETWORK FIELD LENGTH                        
         DC    X'0380'              PACKAGE FIELD LENGTH                        
         DC    X'0600'              PROG CODE FIELD LENGTH                      
         DC    X'0800'              DATE FIELD LENGTH                           
         DC    X'0380'              SUB-LINE FIELD LENGTH                       
         DC    X'0300'              REASON FIELD LENGTH                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************                                 
*                                                                               
CHKTSAR  NTR1  BASE=*,LABEL=*                                                   
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         CLI   RUPSEQ,X'FF'                                                     
         BNE   CHKTSR20                                                         
         BAS   RE,TSARINIT                                                      
*  WRITE THE RECORD OUT                                                         
CHKTSR20 ZIC   RE,UPLSEQNM                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,1,UPLSEQNM                                                    
         MVC   RUPSEQ,UPLSEQNM      MOVE IN SEQUENCE NUMBER                     
         MVI   TSACTN,TSAADD                                                    
         MVC   TSAREC,ANETBLK                                                   
         BAS   RE,CALLTSAR                                                      
         BE    CHKTSR40                                                         
         MVC   ERROR,=AL2(66)       TSAR BUFFER BLOWN                           
         B     TSARERR                                                          
*  CLEAR THE I/O AREA                                                           
CHKTSR40 LR    RE,R5                                                            
         SR    RF,RF                                                            
*!!!     LHI   RF,RUPRCLEN                                                      
*!!!     LHI   RF,RUPLEN2                                                       
         LHI   RF,RUPLEN3          SPEC-43030                                   
         XCEF                                                                   
CHKTSREX XIT1                                                                   
*                                                                               
TSARERR  GOTO1 SENDMSG             RETURN FALINK ERROR                          
         XIT1                                                                   
*                                                                               
TSARINIT NTR1                                                                   
         CLI   SVTSARIN,C'Y'                                                    
         BNE   *+12                                                             
         OI    TSINDS,TSIREUSE                                                  
         B     TSARIN40                                                         
         XC    TSACTN(TSARDL),TSACTN                                            
* EACH TRACK = 3 PAGES                                                          
         MVI   TSPAGN,4+TSPAGNCI       REQUEST 12 PAGES                         
         OI    TSINDS,TSIALLOC     CALL FOR TEMEPST                             
*****    OI    TSIND2,TSI2BUF2     BUFFER B                                     
         OI    TSINDS,TSIXTTWA     AND IT HAS BIG PAGES !                       
*!!!TSARIN40 LHI   R0,RUPRCLEN                                                  
*!!!TSARIN40 LHI   R0,RUPLEN2                                                   
TSARIN40 LHI   R0,RUPLEN3          SPEC-43030                                   
         STH   R0,TSRECL                                                        
         MVI   TSKEYL,1                                                         
         OC    TSARRCLN,TSARRCLN    CHECK FOR OVERRIDE LENGTH                   
         JZ    *+10                                                             
         MVC   TSRECL,TSARRCLN                                                  
         MVC   TSAREC,ANETBLK                                                   
         MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,ACOMFACS                                                  
         BAS   RE,CALLTSAR                                                      
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    TSINDS,TSIREUSE                                                  
         MVI   SVTSARIN,C'Y'                                                    
         MVI   SVINITSW,C'Y'                                                    
         MVI   SVRSTSAV,C'I'                                                    
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
CALLTSAR LR    R0,RE                                                            
********************                                                            
***      LA    RE,SVTSSVRS                                                      
***      CLI   0(RE),0                                                          
***      JE    *+12                                                             
***      LA    RE,1(RE)                                                         
***      J     *-12                                                             
***      MVC   0(1,RE),TSACTN                                                   
********************                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                 *              
* ON ENTRY, CALLER MUST HAVE RC = A(WORK)                        *              
*================================================================*              
         SPACE 1                                                                
         DS    0D                                                               
VCOMMON  NTR1  BASE=*,LABEL=*                                                   
         L     RA,BASE2                                                         
*                                                                               
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
*                                                                               
         SRL   RF,24                                                            
         L     RF,VBRANCH(RF)                                                   
         AR    RF,RB                                                            
         BASR  RE,RF               *** NO RETURN EXPECTED HERE ***              
         DC    H'0'                                                             
VCOMMONX XIT1                                                                   
*                                                                               
*VCOMERR  DC    H'00'                                                           
VCOMERR  DS    0H                                                               
*                                                                               
VMSG     LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         MVI   FAMSGSYS,7          TALENT SYSTEM                                
         CLI   MSGTYPE,0                                                        
         JE    *+8                                                              
         MVI   FAMSGTYP,C'E'       DEFAULT IS ERROR                             
         OC    ERROR,ERROR         FAMSGNO CAN BE SET BY FALINK                 
         JZ    *+10                                                             
         MVC   FAMSGNO,ERROR                                                    
         OC    ERRORMSG,ERRORMSG   XTRA ERROR MESSAGE?                          
         JZ    *+10                                                             
         MVC   FAMSGXTR,ERRORMSG                                                
         CLI   *,FF                SET CC LOW                                   
         L     RD,BASERD           MONITOR TO SPMAK00                           
         L     RD,8(RD)            SPMAK00 TO FALINK                            
         L     RD,8(RD)            FALINK TO SPMAK00                            
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 1                                                                
VBRANCH  DS    0A                                                               
         DC    A(VBYR-VCOMMON)                                                  
         DC    A(VMED-VCOMMON)                                                  
         DC    A(VCLT-VCOMMON)                                                  
         DC    A(VPRD-VCOMMON)                                                  
         DC    A(VEST-VCOMMON)                                                  
         DC    A(VSTA-VCOMMON)                                                  
         DC    A(VPR2-VCOMMON)                                                  
         DC    A(VGETHDR-VCOMMON)                                               
         DC    A(VGETDATA-VCOMMON)                                              
         DC    A(VMSG-VCOMMON)     ERROR MESSAGES USE THIS                      
         DC    A(VFVAL-VCOMMON)     FIELD SCANNER AND VALIDATER                 
         DC    A(VREP-VCOMMON)                                                  
         DC    16A(0)              SPARE                                        
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* RETURN ADDRESS OF MAP HEADER ELEMENT IN R1                     *              
*================================================================*              
         SPACE 1                                                                
VGETHDR  L     RE,=A(FAMAP)            A(MAP TABLE)                             
         A     RE,BASERELO                                                      
*****    LA    RE,FAMAP                                                         
         USING MHELD,RE                                                         
         SR    RF,RF                                                            
*                                                                               
GHDR2    CLM   R1,3,MHCODE         MATCH EL                                     
         BE    GHDRX                                                            
         ICM   RF,3,MHDISP                                                      
         AR    RE,RF                                                            
         CLI   MHLEN,0                                                          
         BNE   GHDR2                                                            
         DC    H'0'                                                             
*                                                                               
GHDRX    ST    RE,HDRADDR                                                       
         B     VCOMMONX                                                         
         DROP  RE                                                               
         EJECT                                                                  
*================================================================*              
* RETURN DATA ITEM ADDRESS FOR HEADER  IN HDRADDR                *              
* R1 CONTAINS DATA ITEM NUMBER                                   *              
*================================================================*              
         SPACE 1                                                                
VGETDATA ICM   RE,15,HDRADDR                                                    
         JNZ   *+6                                                              
         DC    H'0'                TAKING NO PRISONERS                          
         USING MDELD,RE                                                         
         SR    RF,RF                                                            
         IC    RF,MHLEN-MHELD(RE)  DSPL TO FIRST DATA ITEM                      
         AR    RE,RF                                                            
*                                                                               
GDAT2    CLM   R1,3,MDCODE         MATCH EL                                     
         BE    GDATX                                                            
         ICM   RF,1,MDLEN                                                       
         AR    RE,RF                                                            
         CLI   MDLEN,0                                                          
         BNE   GDAT2                                                            
         DC    H'0'                                                             
*                                                                               
GDATX    ST    RE,DATADDR                                                       
         B     VCOMMONX                                                         
         DROP  RE                                                               
         EJECT                                                                  
*================================================================*              
* FVAL - FIELD VALIDATION AND SCANNING ROUTINE                                  
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER)                                                
*        FMAX  = MAXIMUM SCAN LENGTH (OPTIONALLY SET BY USER)                   
*        FLAST = A(LAST STRING) SET BY FVAL                                     
*              = ZERO (FORCES EDIT TO START AT FADDR+8)                         
*        FLEN  = LENGTH OF LAST STRING - SET BY FVAL                            
*              = ZERO TO FORCE EDIT TO START AT FLAST                           
*        FTERM = LIST OF UP TO 6 SCAN TERMINATORS ENDED BY X'00'                
*                                                                               
* ON EXIT                                                                       
*        FLDH  = FIELD HEADER BUILT BY FVAL - CONTAINS DATA LENGTH              
*                AND VALIDITY BITS                                              
*        FLD   = EXTRACTED DATA STRING IN SPACE FILLED FIELD                    
*        FSTOP = STOP CHARACTER FOUND BY FVAL OR X'FF' FOR NO MORE DATA         
*        DUB   = CONTAINS PACKED VALUE OF DATA STRING FOR NUMERIC FIELD         
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
*        R1    = 0 FOR NO PARAMETER LIST                                        
*                                                                               
*================================================================*              
VFVAL    XC    FLDH,FLDH           CLEAR OUTPUT FIELD HEADER                    
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL OUTPUT FIELD WITH SPACES                
         MVI   FLDH,L'FLDH+L'FLD   SET DUMMY HEADER LENGTH                      
         MVI   FSTOP,X'FF'         SET FSTOP TO NO DATA FOUND                   
         L     R2,FADDR                                                         
         ZIC   R3,0(R2)                                                         
         SH    R3,=H'8'            R3 CONTAINS FIELD DATA LENGTH                
         LA    R2,8(R2)            POINT TO DATA START                          
         OC    FLAST,FLAST         TEST FOR LAST STRING                         
         BNZ   FVAL2                                                            
*                                                                               
* CODE BELOW TO FVAL8 ASSUMES -                                                 
* R1 POINTS TO SCAN INPUT  R2 POINTS TO SCREEN FIELD START                      
* R3 CONTAINS BYTE REMAINING TO BE SCANNED                                      
*                                                                               
FVAL1    STCM  R2,7,FLAST          SAVE SCAN START POINT                        
         LR    R1,R2               SET R1 AS SCAN INPUT POINTER                 
         MVI   FLEN,0              CLEAR LAST LENGTH TO BE SAFE                 
         MVI   FNDX,0                                                           
         B     FVAL4                                                            
*                                                                               
FVAL2    SR    R1,R1                                                            
         ICM   R1,7,FLAST          SET R1 TO POINT TO SCAN START                
         ZIC   R0,FLEN             LENGTH OF LAST STRING                        
         AR    R1,R0               POINT TO LAST STOP CHARACTER                 
         CLI   FLEN,0              TEST IF RE-EDITING                           
         JE    *+8                 YES-NO NEED TO JUMP OVER STOP CHAR.          
         LA    R1,1(R1)            INCREMENT LENGTH FOR STOP CHARACTER          
         LR    R0,R1               START POINT FOR SCAN                         
         SR    R0,R2               BYTES ALREADY SCANNED                        
         SR    R3,R0               BYTES LEFT TO SCAN                           
         BNP   FVALX               NOTHING TO SCAN - EXIT                       
         CLI   FMAX,0              TEST FOR USER SCAN LIMIT                     
         JE    *+8                 NO                                           
         IC    R3,FMAX             YES-USE IT IN PLACE OF DERIVED LIMIT         
         STCM  R1,7,FLAST          SAVE SCAN START                              
         MVI   FLEN,0              CLEAR LAST LENGTH                            
*                                                                               
FVAL4    LA    R0,L'FTERM                                                       
         LA    RE,FTERM            POINT AT SCAN TERMINATORS                    
         SPACE 1                                                                
FVAL5    CLI   0(RE),X'00'         TEST FOR END-OF-LIST                         
         BE    FVAL6                                                            
         CLC   0(1,R1),0(RE)       TEST FOR TERMINATOR                          
         BE    FVAL7               FOUND ONE                                    
         LA    RE,1(RE)            NEXT LIST ENTRY                              
         BCT   R0,FVAL5                                                         
*                                                                               
FVAL6    LA    R1,1(R1)            NEXT DATA BYTE                               
         BCT   R3,FVAL4                                                         
         B     FVAL8               SEARCH WAS FRUITLESS                         
*                                                                               
FVAL7    MVC   FSTOP,0(R1)         SET STOP CHARACTER                           
*                                                                               
FVAL8    LR    R3,R1               COMPUTE DATA LENGTH                          
         SR    RE,RE                                                            
         ICM   RE,7,FLAST                                                       
         SR    R3,RE                                                            
         BZ    FVALX               ONLY FOUND A TERMINATOR                      
         STC   R3,FLEN                                                          
         STC   R3,FLDH+5                                                        
         BCTR  R3,0                SET TO EXECUTE                               
         EX    R3,*+8              EXTRACT DATA STRING                          
         B     FVAL10                                                           
         MVC   FLD(0),0(RE)                                                     
*                                                                               
FVAL10   LA    RE,FLD(R3)          ADJUST LENGTH FOR TRAILING BLANKS            
         LA    R3,1(R3)            COUNTER                                      
         LR    R0,R3                                                            
*                                                                               
FVAL11   CLI   0(RE),0             TEST FOR TRAILING ZERO                       
         JNE   *+8                                                              
         MVI   0(RE),C' '          CHANGE IT TO A BLANK                         
         CLI   0(RE),C' '          TEST FOR A BLANK                             
         JNE   *+10                NO                                           
         BCTR  RE,0                YES-BACK UP FIELD POINTER                    
         BCT   R0,FVAL11                                                        
         STC   R0,FLDH+5                                                        
         LTR   R0,R0               TEST FOR ZERO REAL LENGTH                    
         BZ    FVALX               EXIT FOR EMPTY FIELD                         
         LR    R3,R0               SET REAL DATA LENGTH                         
         LA    RE,FLD              POINT TO START OF DATA                       
         MVI   FLDH+4,X'0C'        VALID NUMERIC AND ALPHA DATA                 
*                                                                               
FVAL12   CLI   0(RE),C'A'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'I'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'J'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'R'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'S'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'Z'                                                       
         BNH   FVAL14              VALID ALPHA                                  
*                                                                               
FVAL13   NI    FLDH+4,X'FF'-X'04'  NOT ALPHA                                    
*                                                                               
FVAL14   CLI   0(RE),C'0'          TEST IF NUMERIC                              
         JL    *+12                                                             
         CLI   0(RE),C'9'                                                       
         BNH   FVAL15                                                           
         NI    FLDH+4,X'FF'-X'08'  NOT NUMERIC                                  
*                                                                               
FVAL15   LA    RE,1(RE)            NEXT BYTE IN DATA STRING                     
         BCT   R3,FVAL12                                                        
*                                                                               
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BZ    FVALX                                                            
         CLI   FLDH+5,15                                                        
         JNH   *+12                                                             
         NI    FLDH+4,X'FF'-X'08'                                               
         B     FVALX                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     FVALX                                                            
         PACK  DUB,FLD(0)          EXECUTED                                     
*                                                                               
FVALX    MVI   FMAX,0              ALWAYS CLEARED BY FVAL                       
         B     EXXMOD                                                           
         EJECT                                                                  
*================================================================*              
* VALIDATE MEDIA CODE                                            *              
*================================================================*              
         SPACE 1                                                                
VMED     XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,QAGY                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+READ                                        
*                                                                               
         L     R6,AIO                                                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAGYNAM,AGYNAME                                                 
         MVC   SVAGYADR,AGYADDR                                                 
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
         MVC   SVAGYFL2,AGYFLAG2   SAVE AGENCY FLAG 2                           
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     VMED4                                                            
         SPACE 1                                                                
VMED2    BAS   RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
VMED4    CLI   2(R6),C'N'          MATCH MEDIA CODE                             
         BNE   VMED2                                                            
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   SVMEDNM,4(R6)       MEDIA NAME                                   
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
*================================================================*              
* VALIDATE BUYER                                                 *              
*================================================================*              
         SPACE 1                                                                
VBYR     B     VCOMMONX                                                         
         EJECT                                                                  
*================================================================*              
* VALIDATE CLIENT                                                *              
*================================================================*              
         SPACE 1                                                                
VCLT     MVC   ERROR,=AL2(236)     CLIENT NOT ON FILE                           
         TM    WIZSTAT,X'04'       SKIP CLIENT CONVERSION ALREADY SET           
         BO    VCLT01              YES                                          
         OI    QCLT+2,X'40'        CHANGE SPACE TO BLENK                        
         MVC   ERRORMSG(3),QCLT                                                 
         GOTO1 VCLPACK,DMCB,QCLT,BCLT                                           
         CLI   0(R1),0                                                          
         BE    VCLT01                                                           
*->      BRAS  RE,VCOMERR                                                       
         B     VCLTERR                                                          
***********************************************************                     
VCLTERR  TM    WIZSTAT,X'01'       SKIP ERRORS IN WIZARD?                       
         JO    *+8                 YES                                          
         BRAS  RE,VCOMERR          NO-STOP AT ERROR                             
         OI    WIZSTAT,X'02'       SET ERROR FOUND                              
         B     VCOMMONX            AND EXIT                                     
***********************************************************                     
         SPACE 1                                                                
VCLT01   CLI   BAGYMD,0            IS AGY/MEDIA SET?                            
         BNE   VCLT02                                                           
         GOTO1 VALIMED                                                          
VCLT02   XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         JE    *+8                                                              
*->      BRAS  RE,VCOMERR                                                       
         B     VCLTERR                                                          
                                                                                
         L     R6,AIO4                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
         USING CLTHDRD,R6                                                       
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   SVCNAME,CNAME       AND CLIENT NAME                              
         MVC   SVOFFC,COFFICE          CLIENT OFFICE                            
* MOVE ECOST LEVEL                                                              
         MVI   SVECST,0                                                         
         TM    COPT3,COP3T                                                      
         JZ    *+8                                                              
         MVI   SVECST,C'T'                                                      
         TM    COPT3,COP3TI                                                     
         JZ    *+8                                                              
         MVI   SVECST,C'I'                                                      
         TM    COPT4,COP4TIS                                                    
         JZ    *+8                                                              
         MVI   SVECST,C'S'                                                      
*                                                                               
*  CHECK CLIENT SECURITY                                                        
*                                                                               
         CLC   QCLT,=CL3'ALL'                                                   
         BE    CLT05                                                            
*                                                                               
         BAS   RE,BLDSECRT                                                      
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2(200),WORK2                                                 
         LA    R1,WORK2                                                         
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,QCLT                                                      
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),TWAACCS                                                
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         L     RE,ATWA                                                          
         AH    RE,=Y(SVSECRET-TWAD)                                             
         ST    RE,OFCSECD                                                       
****     MVC   OFCSECD,ASECBLK                                                  
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 VOFFICER,DMCB,(C'N',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         BE    VCOMMONX                                                         
*                                                                               
         MVC   ERROR,=AL2(178)      ACCESS TO CLIENT NOT AUTHORIZED             
         XC    ERRORMSG,ERRORMSG                                                
         B     VCLTERR                                                          
*                                                                               
* SECURITY CHECK IF CLIENT IS C'ALL'                                            
*                                                                               
CLT05    MVC   ERROR,=AL2(55)      SECURITY LOCKOUT                             
         OC    TWAACCS(2),TWAACCS  TEST ANY SECURITY LIMIT                      
         BZ    VCOMMONX                                                         
         CLI   TWAACCS,C'*'        TEST OFFICE LOCKOUT                          
         BE    CLT20               YES                                          
         CLI   TWAACCS,C'+'        TEST MKT LOCKOUT                             
         BE    CLT20               YES                                          
         CLI   TWAACCS,C'$'        TEST OFFICE LIST                             
         BE    CLT20               YES                                          
*                                                                               
CLT10    CLC   TWAACCS(2),BCLT                                                  
         JE    *+8                                                              
****->   BRAS  RE,VCOMERR                                                       
         B     VCLTERR                                                          
*                                                                               
CLT20    CLI   TWAACCS,C'$'                                                     
         BE    CLT30                                                            
         CLI   TWAACCS,C'*'                                                     
         BNE   VCOMMONX                                                         
         LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         JH    *+12                                                             
         LA    R1,COFFICE                                                       
         LA    R0,1                                                             
*                                                                               
CLT25    CLC   TWAACCS+1(1),0(R1)                                               
         BE    VCOMMONX                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,CLT25                                                         
****->   BRAS  RE,VCOMERR                                                       
         B     VCLTERR                                                          
*                                                                               
CLT30    CLI   TWAACCS,C'$'        TEST OFFICE LIST                             
         BNE   VCOMMONX                                                         
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,DUB,ACOMFACS                                       
         CLI   0(R1),0                                                          
         JE    *+8                                                              
****->   BRAS  RE,VCOMERR                                                       
         B     VCLTERR                                                          
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*  ROUTINE SETS UP SECRET BLOCK FOR FUTURE SECURITY CALLS                       
*                                                                               
BLDSECRT NTR1                                                                   
*                                                                               
* INITIALIZE SECRET                                                             
*                                                                               
         L     RE,ATWA                                                          
         AH    RE,=Y(SVSECRET-TWAD)                                             
         ST    RE,ASECBLK                                                       
*                                                                               
         SPACE 1                                                                
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         JNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS       OR HAVE LIMIT ACCESS                    
         BZ    BLDSECEX                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         PRINT GEN                                                              
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         PRINT NOGEN                                                            
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDSECEX XIT1                                                                   
         EJECT                                                                  
*===============================================================*               
* VALIDATE PRODUCT CODE                                         *               
*===============================================================*               
* RETURN CC EQUAL IF ALL OK                                                     
*                                                                               
VPR2     MVI   MYFLAG,2            INDICATE PRD2                                
         B     VPRD2                                                            
*                                                                               
VPRD     MVI   MYFLAG,1            INDICATE PRD1                                
*                                                                               
VPRD2    XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         CLI   MYFLAG,1                                                         
         JE    *+10                                                             
         MVC   KEY+4(3),QPRD2                                                   
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCOMMONX                   RETURN CC NOT EQUAL                   
         CLI   READREC,C'N'        DON'T GET RECORD?                            
         BE    VPRDX               EXIT WIT CC EQUAL                            
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         CLI   MYFLAG,2                                                         
         BE    VPRD10                                                           
         MVC   BPRD,PCODE+1                                                     
         MVC   SVPNAME,PNAME                                                    
         B     VPRDX                                                            
*                                                                               
VPRD10   MVC   BPRD2,PCODE+1                                                    
         MVC   SVPNAME2,PNAME                                                   
         B     VPRDX                                                            
VPRDX    CLC   BPRD,BPRD           SET CC EQUAL                                 
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
*===============================================================*               
* VALIDATE EBCDIC ESTIMATE NUMBER IN FULL(3)                    *               
* RETURN BINARY VALUE IN FULL+3(1)                              *               
*===============================================================*               
         SPACE 1                                                                
VEST     MVC   ERROR,=AL2(237)     ESTIMATE NOT ON FILE                         
         PACK  DUB,FULL(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,BEST             SET BINARY ESTIMATE                          
         CHI   R0,255                                                           
         JNH   *+8                                                              
         BRAS  RE,VCOMERR                                                       
*                                                                               
         OC    BCLT,BCLT                                                        
         BZ    VESTX                                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),=CL3'POL'                                               
         MVC   KEY+7(1),BEST                                                    
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(8),KEYSAVE                                                   
         BE    VESTX                                                            
         BRAS  RE,VCOMERR                                                       
VESTX    B     VCOMMONX                                                         
         EJECT                                                                  
*===============================================================*               
* VALIDATE EBCDIC REP NUMBER IN FULL(3)                         *               
*===============================================================*               
         SPACE 1                                                                
VREP     MVC   ERROR,=AL2(232)     REP NOT FOUND                                
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY             NOW VALIDATE THE REP                         
         LA    R3,KEY                                                           
         USING REPRECD,R3                                                       
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(L'REPKEY-1),REPKEY                                      
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,FULL                                                     
         MVC   REPKAGY,QAGY                                                     
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO                                    
         CLC   KEY(L'REPKEY),KEYSAVE  TEST IF REP FOUND                         
         BE    VREPX                  YES                                       
         BRAS  RE,VCOMERR                                                       
VREPX    B     VCOMMONX                                                         
         DROP  R3                                                               
         EJECT                                                                  
*==================================================================*            
* LOOK UP MARKET NUMBER AND GET 3 BYTE PACKED STA                  *            
*==================================================================*            
         SPACE 1                                                                
VSTA     MVC   ERROR,=AL2(238)     NETWORK NOT ON FILE                          
*  READ STATION RECORD GET THE MARKET                                           
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),BNET                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,QAGY                                                     
         MVC   STAKCLT,=XL3'F0F0F0'                                             
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO                                    
         CLC   KEY(9),KEYSAVE                                                   
         BE    VSTA30                                                           
         BRAS  RE,VCOMERR                                                       
VSTA30   L     R3,AIO                                                           
         MVC   SVNTISTA,SNTISTA                                                 
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STH   R1,BMKT                                                          
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,BMKTSTA                               
         B     VCOMMONX                                                         
         DROP  R3                                                               
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* GETPROF - READ REQUIRED PROFILES *                                            
*=====================================================================*         
         SPACE 1                                                                
GETPROF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,=C'S0A0'         READ A0 PROFILE                              
         LA    R5,SVA0PRF                                                       
         BAS   RE,GETIT                                                         
         SPACE 1                                                                
         LA    R4,=C'S0A0'         READ AGY LEVEL A0 PROFILE                    
         O     R4,=X'80000000'     SET FLAG FOR GETIT                           
         LA    R5,WORK2                                                         
         BAS   RE,GETIT                                                         
         MVC   SVA0PRF(1),WORK2   SET GROSS/NET FIELD ONLY                      
*                                                                               
         LA    R4,=C'S0MK'                                                      
         LA    R5,SVMKPRF                                                       
         BAS   RE,GETIT                                                         
         CLI   SVMKPRF+1,C'A'      TEST PROFILE VALUE SET                       
         JNL   *+8                                                              
         MVI   SVMKPRF+1,C'Y'      SET DEFAULT VALUE                            
*                                                                               
         LA    R4,=C'S000'                                                      
         LA    R5,SV00PRF                                                       
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL,=C'SA0A'                                                    
         NI    FULL,X'BF'          CHANGE 'S' TO LOWER CASE                     
         LA    R4,FULL                                                          
         LA    R5,SVA0APRF                                                      
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL+1(3),=C'I2X'                                                
         LA    R5,SVI2XPRF                                                      
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL+1(3),=C'I2Y'                                                
         LA    R5,SVI2YPRF                                                      
         BAS   RE,GETIT                                                         
*                                                                               
* CONVERT PROFILE DATES TO Y2K FORMAT                                           
         OC    SVA0APRF+5(2),SVA0APRF+5 SPECIAL OFFICE START                    
         BZ    GETPRF10                                                         
         MVC   DUB(2),SVA0APRF+5                                                
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),WORK    MAKE EBCDIC                         
         GOTO1 (RF),(R1),WORK,(3,DUB)       MAKE BINARY                         
         MVC   SVA0APRF+5(2),DUB                                                
*                                                                               
GETPRF10 OC    SVA0APRF+7(3),SVA0APRF+7 MAKEGOOD AS MISSED                      
         BZ    GETPRFX                                                          
         MVC   DUB(3),SVA0APRF+7                                                
         GOTO1 VDATCON,DMCB,(3,DUB),WORK                                        
         GOTO1 (RF),(R1),WORK,(3,SVA0APRF+7)                                    
*                                                                               
GETPRFX  BRAS  RE,EXIT                                                          
         DC    H'0'                NO RETURN EXPECTED HERE                      
         XIT1                                                                   
*                                                                               
GETIT    NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),0(R4)                                                    
         MVC   WORK+4(2),QAGY                                                   
         LTR   R4,R4               IF NEGATIVE, STOP AT AGY                     
         BM    GETIT2                                                           
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
*                                                                               
GETIT2   XC    0(16,R5),0(R5)                                                   
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,(R5),VDATAMGR                                     
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* VALWZCLT  -  FILL SVWIZCLT TABLE                                              
*=====================================================================*         
         SPACE 1                                                                
VALWZCLT NTR1  BASE=*,LABEL=*                                                   
         CLC   0(3,R4),=CL3'ALL'                                                
         BNE   VALWZ20                                                          
         MVC   SVWIZCLT(3),=CL3'ALL'                                            
         B     VALWZEX                                                          
*                                                                               
VALWZ20  XC    WORK,WORK                                                        
         EX    R5,*+4                                                           
         MVC   WORK+8(0),0(R4)                                                  
         LA    R5,9(R5)                                                         
         STCM  R5,1,WORK                                                        
*                                                                               
         LA    R5,SVWIZCLT                                                      
         LA    R2,WORK                                                          
         ST    R2,FADDR                                                         
         XC    FTERM,FTERM                                                      
         XC    FLAST,FLAST                                                      
         MVI   FTERM,C':'                                                       
*                                                                               
VALWZ50  GOTO1 FVAL,0                                                           
         CLI   FLEN,0               CHECK FOR NO INPUT                          
         BE    VALWZ70                                                          
****************************************************************                
         MVC   QCLT,FLD                                                         
         GOTO1 VALICLT                                                          
         TM    WIZSTAT,X'02'       ERROR FOUND IN SKIP-ERROR MODE?              
         BNO   VALWZ60             NO                                           
         NI    WIZSTAT,X'FF'-X'02' YES/CLEAR IT                                 
         CLI   FSTOP,X'FF'         ..LAST ENTRY?                                
         BE    VALWZEX             ..YES- EXIT                                  
         B     VALWZ50             ..NO-GO GET NEXT ENTRY WITHOUT               
*                                       BUMPING CLIENT SV TABLE                 
*******************************************************************             
VALWZ60  ZIC   R1,FLEN                                                          
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R5),FLD                                                      
         CLI   2(R5),0             TWO BYTE CLIENT CODE?                        
         BNE   *+8                                                              
         MVI   2(R5),X'40'         YES - SPACE FILL                             
***      MVC   QCLT,FLD                                                         
***      GOTO1 VALICLT                                                          
         MVC   3(2,R5),BCLT        SAVE BINARY CLT                              
         MVC   5(20,R5),SVCNAME    CLIENT NAME                                  
         MVC   25(1,R5),SVOFFC     OFFICE                                       
         MVC   26(1,R5),SVECST     ECOST LEVEL                                  
*                                                                               
         B     VALWZ80                                                          
VALWZ70  SR    RE,RE               IF NO INPUT                                  
         ICM   RE,7,FLAST          MUST BUMP PAST DELIMETER                     
         LA    RE,1(RE)            SO SCAN CAN CONTINUE                         
         STCM  RE,7,FLAST                                                       
VALWZ80  CLI   FSTOP,X'FF'                                                      
         BE    VALWZEX                                                          
         LA    R5,LNWIZCLT(R5)                                                  
         B     VALWZ50                                                          
*                                                                               
VALWZEX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* IO ALL PURPOSE DATAMGR HANDLER                                                
*=====================================================================*         
*          DATA SET NEBUY00    AT LEVEL 215 AS OF 11/08/99                      
* I/O CONTROLLER                                                                
*     ON ENTRY   KEY      = CONTAINS KEY FOR READ                               
*                NDXDA    = CONTAINS DISK ADDRESS FOR D/A FILE READ             
*                                                                               
*     PARAMETERS                                                                
*                P1       = I/O MASK - COMMAND/FILE/DIRECTORY OR                
*                           FILE/CONTROL SETTINGS                               
*                P2       = BYTES 1-3 - A(I/O AREA) FOR FILE READ               
*                                                                               
*     AFTER I/O                                                                 
*                IOFLAG   = 0 OR ERROR SETTING                                  
*                KEYSAVE  = KEY PASSED TO ROUTINE                               
*                KEY      = CONTAINS KEY RETURNED BY DATAMGR                    
*                AIO      = A(I/O AREA) FOR RECORD                              
*                                                                               
**IOCALL NTR1  BASE=BASE1,WORK=(R8,IOWORKX-IOWORKD)                             
IOCALL   NTR1  BASE=*,LABEL=*,WORK=(R8,IOWORKX-IOWORKD)                         
         L     RA,BASE2                                                         
         USING IOWORKD,R8          LOCAL STORAGE                                
         LM    R2,R3,0(R1)         I/O MASK AND I/O AREA                        
         STC   R2,IOWORK1          LOW ORDER BYTE OF I/O MASK                   
         MVC   IOWORK2,IOWORK1     SAVE LOW ORDER BYTE                          
         STCM  R2,2,IOWORK3        SAVE THIRD BYTE                              
         NI    IOWORK2,X'0F'       ISOLATE COMMAND NUMBER                       
*                                                                               
         ZIC   R1,IOWORK2                                                       
         SLL   R1,3                MULTIPLY BY 8 TO DEVELOP INDEX               
         LA    RE,CMNDTAB-L'CMNDTAB(R1) INDEX INTO COMMAND TABLE                
         MVC   IOCMND,0(RE)        EXTRACT COMMAND NAME                         
         SPACE 1                                                                
IO2      MVC   IOWORK2,IOWORK1     REFRESH FLAG VALUES                          
         NI    IOWORK2,X'70'       ISOLATE FILE NUMBER                          
         ZIC   R1,IOWORK2          FILE NUMBER                                  
         SRL   R1,4                DIVIDE BY 8 TO DEVELOP INDEX                 
         LA    R0,L'FILTAB                                                      
         MR    R0,R0                                                            
         LA    RE,FILTAB-L'FILTAB(R1)                                           
         MVC   IODIR,0(RE)         EXTRACT DIRECTORY NAME                       
         MVC   IOFILE,7(RE)        FILE NAME                                    
         MVC   IOVALS,14(RE)                                                    
         BAS   RE,SETCTL           SET CONTROL BIT                              
         BAS   RE,SETSTAT          SET LAST STATUS LOG                          
         ZIC   R1,IOWORK1          TEST IOFLAG VS. EXCEPTION VALS               
         EX    R1,*+8                                                           
         J     *+8                                                              
         TM    IOEXCPT,0                                                        
         JZ    *+6                                                              
         DC    H'0'                YES-BLOW UP - CHECK IOFLAG                   
         TM    IOWORK3,X'01'       TEST FOR PASS DELETES                        
         JZ    *+8                 NO                                           
         OI    DMINBITS,X'08'                                                   
         TM    IOWORK3,X'02'       TEST FOR READ FOR UPDATE                     
         JZ    *+8                 NO                                           
         OI    DMINBITS,X'80'                                                   
         TM    IOWORK1,DIR         TEST FOR DIRECTORY READ                      
         BZ    IO6                                                              
         SPACE 1                                                                
IO4      MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,IOPARM,(DMINBITS,IOCMND),IODIR,KEY,KEY                  
         LA    RE,KEY                                                           
         ZIC   R0,IODADSP                                                       
         AR    RE,R0               POINT TO DISK ADDRESS                        
         MVC   NDXDA,0(RE)                                                      
         MVC   IOFLAG,8(R1)                                                     
         B     IOX                                                              
         SPACE 1                                                                
IO6      ST    R3,AIO                                                           
         LA    R2,NDXDA            POINT AT DISK ADDRESS                        
         TM    IOEXCPT,DIR         TEST FOR FILE W DIRECTORY                    
         BZ    IO7                                                              
         LA    R2,KEY              FOR IS FILE, POINT TO KEY                    
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
IO7      GOTO1 VDATAMGR,IOPARM,(DMINBITS,IOCMND),IOFILE,(R2),AIO,DMWORK         
         MVC   IOFLAG,8(R1)                                                     
         TM    IOEXCPT,DIR         TEST FOR IS FILE                             
         BZ    IOX                 NO                                           
         L     RF,AIO                                                           
         ZIC   R1,IOKEYL                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     IOX                                                              
         MVC   KEY(0),0(RF)        EXTRACT KEY FROM RECORD                      
         SPACE 1                                                                
IOX      MVI   DMINBITS,0                                                       
         TM    IOFLAG,X'FD'        TEST FOR DATAMGR ERROR                       
         BZ    IOXX                NONE                                         
         XC    ERROR,ERROR                                                      
* IT WOULD DIE ANYWAY                                                           
*NOP     TM    IOFLAG,X'20'        TEST FOR DUPLICATE KEY ON ADD                
*        BZ    ERROR               NO-LET GETMSG SORT IT OUT                    
         DC    H'0'                YES-DUMP TO INSURE PROPER RECOVERY           
*                                                                               
IOXX     MVI   IOFLAG,0                                                         
         B     EXXMOD                                                           
         SPACE 4                                                                
*--SET POSTING TYPE IN THE STATUS FIELD                                         
SETCTL   NTR1                                                                   
*--CHECK IF WE ARE UPDATING A UNIT RECORD                                       
         CLC   IOFILE(3),=CL3'UNT'                                              
         BNE   SETCTLEX                                                         
         CLC   IOCMND(6),=CL6'ADDREC'                                           
         BE    SETCTL10                                                         
         CLC   IOCMND(6),=CL6'PUTREC'                                           
         BNE   SETCTLEX                                                         
SETCTL10 CLI   0(R3),X'04'                                                      
         BNE   SETCTLEX                                                         
*                                                                               
*--TEST FOR UPDATE COMMAND                                                      
         L     R3,AIO              SET TO FILE AREA                             
         L     R4,AIO              SET TO FILE AREA                             
         LA    R3,2(R3)       ADJUST SO KEY AND FILE AT THE SAME PLACE          
         CLI   IOWORK2,PUT                                                      
         BE    SETCTL20                                                         
         CLI   IOWORK2,ADDREC                                                   
         BE    SETCTL20                                                         
         LA    R3,KEY                                                           
         CLI   IOWORK2,WRITE                                                    
         BE    SETCTL20                                                         
         CLI   IOWORK2,ADD                                                      
         BNE   SETCTLEX                                                         
*                                                                               
SETCTL20 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R4)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
         USING NUSDRD,R4                                                        
*                                                                               
         CLI   NUPOSTYP,C'N'       NETWORK                                      
         BNE   *+12                                                             
         NI    20(R3),X'FC'                                                     
         B     SETCTLEX                                                         
         CLI   NUPOSTYP,C'C'       CABLE                                        
         BNE   *+16                                                             
         NI    20(R3),X'FC'                                                     
         OI    20(R3),X'01'                                                     
         B     SETCTLEX                                                         
         CLI   NUPOSTYP,C'S'       SYNDICATION                                  
         BNE   *+16                                                             
         NI    20(R3),X'FC'                                                     
         OI    20(R3),X'02'                                                     
         B     SETCTLEX                                                         
         CLI   NUPOSTYP,C'O'       OTHER                                        
         BNE   *+12                                                             
         NI    20(R3),X'FC'        OTHER                                        
         OI    20(R3),X'03'                                                     
SETCTLEX B     EXXMOD                                                           
         SPACE 4                                                                
*--SET LAST ACTIVITY BIT SETTINGS IN NEW LOG ELEMENT                            
*--R3 POINTS TO RECORD                                                          
SETSTAT  NTR1                                                                   
         USING NURECD,R3                                                        
*--CHECK IF WE ARE UPDATING A UNIT RECORD                                       
         CLC   IOFILE(3),=CL3'UNT'                                              
         BNE   SETSTEX                                                          
         CLC   IOCMND(6),=CL6'ADDREC'                                           
         BE    SETST040                                                         
         CLC   IOCMND(6),=CL6'PUTREC'                                           
         BNE   SETSTEX                                                          
SETST040 CLI   0(R3),X'04'                                                      
         BNE   SETSTEX                                                          
*                                                                               
*          DATA SET NEBUY00    AT LEVEL 215 AS OF 11/08/99                      
SETST050 GOTO1 VDATCON,DMCB,(5,0),(2,HALF)                                      
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
         USING NUSDRD,R4                                                        
*--CHECK IF ANY CHANGES WERE MADE                                               
         CLI   NUACTWHY,0                                                       
         BNE   SETST060                                                         
         CLI   NUACT2WY,0                                                       
         BE    SETSTEX                                                          
*                                                                               
SETST060 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'68',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    SETST070                                                         
         CLI   12(R1),X'06'        ELEMENT NOT FOUND                            
         BE    SETST200                                                         
         DC    H'0'                                                             
*                                                                               
SETST070 L     R2,12(R1)                                                        
         USING NUATV,R2                                                         
         XC    WORK(62),WORK                                                    
         MVC   WORK(2),0(R2)                                                    
         MVC   WORK+8(54),NUATVINF                                              
         MVC   0(62,R2),WORK                                                    
         MVC   NUATVINF(2),HALF                                                 
         MVC   NUATVINF+2(1),NUACTWHY                                           
         MVC   NUATVINF+3(1),NUACT2WY                                           
         B     SETSTEX                                                          
         DROP  R2                                                               
*                                                                               
SETST200 XC    WORK(62),WORK                                                    
         LA    R2,WORK                                                          
         USING NUATV,R2                                                         
         MVC   NUATVEL(2),=XL2'683E'                                            
         MVC   NUATVINF(2),HALF                                                 
         MVC   NUATVINF+2(1),NUACTWHY                                           
         MVC   NUATVINF+3(1),NUACT2WY                                           
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'68',(R3)),(R2),0                   
         B     SETSTEX                                                          
*                                                                               
*--FIRST TWO POSITIONS OF SPACES MUST BE RESET,BECUASE OF                       
*--OVERFLOW IN THE FIELD "WORK".                                                
SETSTEX  MVC   SPACES(2),=XL2'4040'                                             
         B     EXXMOD                                                           
         EJECT                                                                  
* RETURN FROM MODULE AND ROUTINES                                               
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* CHECK FOR RETURN FROM NET TRAFFIC                                             
* IF SO, EXIT WITH CC EQ                                                        
* NOTE - SVXFROV NOT USED BY NENAV40 (DDLINK APP)                               
*=============================================================                  
                                                                                
CHKXCTL  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,GLVXLENQ,GLVXCTL                     
         CLI   8(R1),0                                                          
         JNE   CHKXCNEQ                                                         
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRSY(6),=C'NETTRA' RETURN FROM NET/TRAFFIC                   
         JNE   CHKXCX                 NO                                        
         DROP  R1                                                               
* GET SPFMT GLOBAL TO GET RETURN ROUTING                                        
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,24,GLVSPTRF                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         LA    R1,WORK                                                          
         USING GLVTRFD,R1                                                       
         MVI   NAVINPH+5,13                                                     
         MVC   NAVINP(13),=C'P06 D=0XXXAAT'                                     
         MVC   NAVINP+7(3),TRFROUT                                              
         MVI   SVOLAY,3                                                         
         CR    RB,RB               SET CC EQ                                    
         J     CHKXCX                                                           
CHKXCNEQ LTR   RB,RB                                                            
*                                                                               
CHKXCX   XIT1                                                                   
         LTORG                                                                  
         DROP  R2,R3,R4,RC                                                      
         EJECT                                                                  
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(QFALINK)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QDEMOUT)                                                     
         DC    AL1(QDAYUNPK)                                                    
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QSTAVAL)                                                     
         DC    AL1(QTSAR)                                                       
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QGETRATE)                                                    
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QNETWEEK)                                                    
         DC    AL1(QDEFINE)                                                     
         DC    AL1(QGETNUN)                                                     
         DC    AL1(QGETHUT)                                                     
         DC    AL1(QNETVALU)                                                    
         DC    AL1(QMSPACK)                                                     
PHASESN  EQU   *-PHASES                                                         
         EJECT                                                                  
* TABLE OF I/O COMMANDS                                                         
*                                                                               
CMNDTAB  DS    0CL8                                                             
         DC    CL8'DMREAD'                                                      
         DC    CL8'DMRSEQ'                                                      
         DC    CL8'DMRDHI'                                                      
         DC    CL8'DMWRT'                                                       
         DC    CL8'DMADD'                                                       
         DC    CL8'GETREC'                                                      
         DC    CL8'PUTREC'                                                      
         DC    CL8'ADDREC'                                                      
         SPACE 2                                                                
* TABLE OF FILES/DIRECTORIES AND THEIR ATTRIBUTES                               
*                                                                               
FILTAB   DS    0XL17                                                            
         DC    CL7'SPTDIR',CL7'SPTFILE',AL1(0,13,14)                            
         DC    CL7'UNTDIR',CL7'UNTFIL',AL1(0,20,21)                             
         DC    CL7'STATION',CL7'STATION',AL1(DIR,17,0)                          
         DC    CL7'TRFDIR',CL7'TRFFILE',AL1(0,13,14)                            
         DC    CL7'NTIDIR',CL7'NTIFILE',AL1(0,18,19)                            
         DC    CL7'CTFILE',CL7'CTFILE',AL1(DIR,25,0)                            
         DC    CL7'XSPDIR',CL7'XSPFIL',AL1(0,32,36)                             
*                                                                               
*  CONSTATNTS                                                                   
*                                                                               
SPTFILE  DC    CL8'SPTFILE'                                                     
UNTFILE  DC    CL8'UNTFILE'                                                     
         EJECT                                                                  
*====================================================================*          
* FALINK MAP TABLE                                                              
*====================================================================*          
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**FAMAP*'         EYE CATCHER                                  
FAMAP    DS    0D                                                               
         SPACE 1                                                                
*====================================================================*          
* RECEIVE HEADERS (FROM PC)                                          *          
*====================================================================*          
*                                                                               
*  INIT RECORD                                                                  
*                                                                               
         SPACE 1                                                                
H10      DC   AL1(H10X-H10)           HEADER LENGTH                             
         DC   XL2'0010'               HEADER CODE                               
         DC   AL2(H10XX-H10)          DISP TO NEXT HEADER                       
H10X     EQU  *                                                                 
         DC    X'00'                                                            
H10XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  PROGRAM SEARCH                                                               
*                                                                               
H40      DC   AL1(H40X-H40)           HEADER LENGTH                             
         DC   XL2'0040'               HEADER CODE                               
         DC   AL2(H40XX-H40)          DISP TO NEXT HEADER                       
H40X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'4001 ',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETPRG)                                                    
         DC    AL1(14),AL2(02),CL5'4002 ',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPRDAY)                                                     
         DC    AL1(14),AL2(03),CL5'4003 ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPRTIME)                                                    
         DC    AL1(14),AL2(04),CL5'4004 ',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(05),CL5'4005 ',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPREDAT)                                                    
         DC    AL1(14),AL2(06),CL5'4006 ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INNTI)                                                       
         DC    AL1(14),AL2(07),CL5'4007 ',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INNADDEF)                                                    
         DC    AL1(14),AL2(08),CL5'4008 ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPRFLT)                                                     
         DC    AL1(14),AL2(09),CL5'4009 ',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPRDAY2)                                                    
         DC    AL1(14),AL2(10),CL5'4010 ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPRTME2)                                                    
         DC    AL1(14),AL2(11),CL5'4011 ',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INPRTYPE)                                                    
         DC    AL1(14),AL2(12),CL5'4012 ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INPRDPT)                                                     
         DC    AL1(14),AL2(13),CL5'4013 ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INPRTIER)                                                    
         DC    AL1(14),AL2(14),CL5'4014 ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INPRNEW)                                                     
         DC    AL1(14),AL2(15),CL5'4015 ',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INPRCONT)                                                    
         DC    AL1(14),AL2(16),CL5'4016 ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRNTSW)                                                    
         DC    AL1(14),AL2(17),CL5'4017 ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPRROT)                                                     
         DC    AL1(14),AL2(18),CL5'4018 ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INNMTYP)                                                     
         DC    X'00'                                                            
H40XX    EQU   *                                                                
*                                                                               
*  DRAFT WIZARD INFORMATION                                                     
*                                                                               
H60      DC   AL1(H60X-H60)           HEADER LENGTH                             
         DC   XL2'0060'               HEADER CODE                               
         DC   AL2(H60XX-H60)          DISP TO NEXT HEADER                       
H60X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'60CLI',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INWIZCLT)                                                    
         DC    AL1(14),AL2(02),CL5'60PRD',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INWIZPRD)                                                    
         DC    AL1(14),AL2(03),CL5'60EST',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INWIZEST)                                                    
         DC    AL1(14),AL2(04),CL5'60SDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(05),CL5'60EDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPREDAT)                                                    
         DC    AL1(14),AL2(06),CL5'60TST',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INWIZTST)                                                    
         DC    AL1(14),AL2(07),CL5'60FRS',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRNTSW)                                                    
         DC    AL1(14),AL2(08),CL5'60REP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBRNDOW)                                                    
         DC    AL1(14),AL2(09),CL5'60BAR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBARRLS)                                                    
         DC    X'00'                                                            
H60XX    EQU   *                                                                
*  DRAFT WIZARD REPLY DATA                                                      
*                                                                               
H62      DC   AL1(H62X-H62)           HEADER LENGTH                             
         DC   XL2'0062'               HEADER CODE                               
         DC   AL2(H62XX-H62)          DISP TO NEXT HEADER                       
H62X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'CLTCD',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(02),CL5'CLTNM',AL1(MDTCHQ),AL1(0)                    
         DC    AL1(10),AL2(03),CL5'CLTOF',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'CLTN0',AL1(MDTCHQ),AL1(0)                    
         DC    AL1(10),AL2(05),CL5'CLTN1',AL1(MDTCHQ),AL1(0)                    
         DC    AL1(10),AL2(06),CL5'CLTN2',AL1(MDTCHQ),AL1(0)                    
         DC    AL1(10),AL2(07),CL5'PRDCD',AL1(MDTCHQ),AL1(0)                    
         DC    AL1(10),AL2(08),CL5'ESTCD',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(09),CL5'ESTNM',AL1(MDTCHQ),AL1(0)                    
         DC    AL1(10),AL2(10),CL5'DEMOS',AL1(MDTCHQ),AL1(0)                    
         DC    AL1(10),AL2(11),CL5'ESTDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(12),CL5'EENDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(13),CL5'ELOCK',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(14),CL5'DPCDE',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(15),CL5'DPNME',AL1(MDTCHQ),AL1(14)                   
         DC    AL1(10),AL2(16),CL5'DPEQU',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(17),CL5'CLTSL',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(18),CL5'APPRO',AL1(MDTCHQ),AL1(0)                    
         DC    AL1(10),AL2(19),CL5'ECTIS',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(20),CL5'OPPRD',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(21),CL5'PREST',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(22),CL5'PRTRG',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(23),CL5'PRGID',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(24),CL5'PRGTI',AL1(MDTCHQ),AL1(12)                   
         DC    AL1(10),AL2(25),CL5'PRGLN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(26),CL5'PRGG#',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(27),CL5'PRGNM',AL1(MDTCHQ),AL1(24)                   
         DC    AL1(10),AL2(28),CL5'PRDID',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(29),CL5'PRDG#',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(30),CL5'HOMTG',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(31),CL5'FLPRD',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(32),CL5'FLTYP',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(33),CL5'FLDPT',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(34),CL5'FLSDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(35),CL5'FLEDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(36),CL5'FLFID',AL1(MDTCHQ),AL1(10)                   
         DC    AL1(10),AL2(37),CL5'FLIID',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(38),CL5'FEDAG',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(39),CL5'FEDNT',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(40),CL5'FEDFD',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(41),CL5'FEDDS',AL1(MDTCHQ),AL1(60)                   
         DC    AL1(10),AL2(42),CL5'CLROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(43),CL5'PRLOK',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(44),CL5'EDICN',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(45),CL5'EDIUI',AL1(MDTCHQ),AL1(0)                    
*  FOLLOWING FIELDS ARE THE OPTIONAL BARRULES RECORD                            
         DC    AL1(10),AL2(50),CL5'BRUAD',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(51),CL5'BRRSD',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(52),CL5'BRNTR',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(53),CL5'BRCTR',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(54),CL5'BRMDS',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(55),CL5'BRDWP',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(56),CL5'BRTBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(57),CL5'BRTBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(58),CL5'BRGBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(59),CL5'BRGBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(60),CL5'BRWBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(61),CL5'BRWBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(62),CL5'BRI0U',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(70),CL5'BRNTP',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(71),CL5'BRPGP',AL1(MDTBIQ),AL1(4)                    
*  DEMO PERCENT INFORMATION                                                     
         DC    AL1(10),AL2(63),CL5'BRDTW',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(64),CL5'BRDTL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(65),CL5'BRDTU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(66),CL5'BRBGL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(67),CL5'BRBGU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(68),CL5'BRDBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(69),CL5'BRDBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(72),CL5'6272 ',AL1(MDTCHQ),AL1(1)                    
         DC    X'00'                                                            
H62XX    EQU   *                                                                
*                                                                               
*  PROGRAM DETAIL                                                               
*                                                                               
H44      DC   AL1(H44X-H44)           HEADER LENGTH                             
         DC   XL2'0044'               HEADER CODE                               
         DC   AL2(H44XX-H44)          DISP TO NEXT HEADER                       
H44X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'44NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETPRG)                                                    
         DC    AL1(14),AL2(02),CL5'44COD',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INPRGCD)                                                     
         DC    AL1(14),AL2(03),CL5'44DAT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    X'00'                                                            
H44XX    EQU   *                                                                
*                                                                               
*  NTI PROGRAM NAME LOOKUP                                                      
*                                                                               
H54      DC   AL1(H54X-H54)           HEADER LENGTH                             
         DC   XL2'0054'               HEADER CODE                               
         DC   AL2(H54XX-H54)          DISP TO NEXT HEADER                       
H54X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'54UDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(02),CL5'54TIM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPRTIME)                                                    
         DC    AL1(14),AL2(03),CL5'54DAY',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPRDAY)                                                     
         DC    AL1(14),AL2(04),CL5'54NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETPRG)                                                    
         DC    X'00'                                                            
H54XX    EQU   *                                                                
*                                                                               
*  NTI PROGRAM NAME LOOKUP                                                      
*                                                                               
H55      DC   AL1(H55X-H55)           HEADER LENGTH                             
         DC   XL2'0055'               HEADER CODE                               
         DC   AL2(H55XX-H55)          DISP TO NEXT HEADER                       
H55X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'55PGM',AL1(MDTCHQ),AL1(25)                   
         DC    AL1(10),AL2(02),CL5'55BOK',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(03),CL5'55SRC',AL1(MDTCHQ),AL1(1)                    
         DC    X'00'                                                            
H55XX    EQU   *                                                                
*                                                                               
*  IGNORE FEATURES FOR INVOICE                                                  
*                                                                               
H56      DC   AL1(H56X-H56)           HEADER LENGTH                             
         DC   XL2'0056'               HEADER CODE                               
         DC   AL2(H56XX-H56)          DISP TO NEXT HEADER                       
H56X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'54UDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(02),CL5'56PRD',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPROD1)                                                     
         DC    AL1(14),AL2(03),CL5'56NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETPRG)                                                    
         DC    AL1(14),AL2(04),CL5'56INV',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(ININV)                                                       
         DC    AL1(14),AL2(05),CL5'56EST',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INEST)                                                       
         DC    AL1(14),AL2(06),CL5'56DAY',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPRDAY)                                                     
         DC    AL1(14),AL2(07),CL5'56IFL',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INIGFILM)                                                    
         DC    AL1(14),AL2(08),CL5'56ICO',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INIGCOST)                                                    
         DC    AL1(14),AL2(09),CL5'56ITI',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INIGTIME)                                                    
         DC    AL1(14),AL2(10),CL5'56ILE',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INIGLEN)                                                     
         DC    AL1(14),AL2(14),CL5'56ICI',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INIGICST)                                                    
         DC    AL1(14),AL2(11),CL5'56CLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCLIENT)                                                    
         DC    AL1(14),AL2(12),CL5'56TIM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPRTIME)                                                    
         DC    AL1(14),AL2(13),CL5'56CST',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INACTCST)                                                    
         DC    AL1(14),AL2(15),CL5'56SEP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INIGSEP)                                                     
         DC    X'00'                                                            
H56XX    EQU   *                                                                
*                                                                               
*                                                                               
*  CABLE PROGRAM ADD REQUEST                                                    
*                                                                               
H48      DC   AL1(H48X-H48)           HEADER LENGTH                             
         DC   XL2'0048'               HEADER CODE                               
         DC   AL2(H48XX-H48)          DISP TO NEXT HEADER                       
H48X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'48NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INACPNET)                                                    
         DC    AL1(14),AL2(02),CL5'48STM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INACPSTM)                                                    
         DC    AL1(14),AL2(03),CL5'48ETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INACPETM)                                                    
         DC    AL1(14),AL2(04),CL5'48ROT',AL1(MDTCHQ),AL1(7)                    
         DC    AL4(INACPROT)                                                    
         DC    AL1(14),AL2(05),CL5'48MIR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INACPMIR)                                                    
*-- PROGRAM NAME MUST BE THE LAST ELEMENT                                       
         DC    AL1(14),AL2(08),CL5'48NAM',AL1(MDTCHQ),AL1(16)                   
         DC    AL4(INACPPRG)                                                    
         DC    X'00'                                                            
H48XX    EQU   *                                                                
         SPACE 3                                                                
*                                                                               
*  CABLE PROGRAM ADD REQUEST REPLY                                              
*                                                                               
H49      DC   AL1(H49X-H49)           HEADER LENGTH                             
         DC   XL2'0049'               HEADER CODE                               
         DC   AL2(H49XX-H49)          DISP TO NEXT HEADER                       
H49X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'31NUM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'31PCD',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(57),CL5'31ENM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(58),CL5'31EMS',AL1(MDTCHQ),AL1(64)                   
         DC    AL1(10),AL2(59),CL5'31EXT',AL1(MDTCHQ),AL1(54)                   
         DC    X'00'                                                            
H49XX    EQU   *                                                                
         SPACE 3                                                                
*                                                                               
*  HISTORY RECORD LOOKUP                                                        
*                                                                               
H7A      DC   AL1(H7AX-H7A)           HEADER LENGTH                             
         DC   XL2'007A'               HEADER CODE                               
         DC   AL2(H7AXX-H7A)          DISP TO NEXT HEADER                       
H7AX     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'7ACLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCLIENT)                                                    
         DC    AL1(14),AL2(01),CL5'7AEST',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INEST)                                                       
         DC    AL1(14),AL2(01),CL5'7ANET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETPRG)                                                    
         DC    AL1(14),AL2(02),CL5'7ACOD',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INPRGCD)                                                     
         DC    AL1(14),AL2(03),CL5'7ADAT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(03),CL5'7ASLN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INPRSLIN)                                                    
         DC    X'00'                                                            
H7AXX    EQU   *                                                                
         SPACE 3                                                                
*                                                                               
*  SEARCH PACKAGE                                                               
*                                                                               
H7C      DC   AL1(H7CX-H7C)           HEADER LENGTH                             
         DC   XL2'007C'               HEADER CODE                               
         DC   AL2(H7CXX-H7C)          DISP TO NEXT HEADER                       
H7CX     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'7CCLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCLIENT)                                                    
         DC    AL1(14),AL2(02),CL5'7CNET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPKNET)                                                     
         DC    AL1(14),AL2(03),CL5'7CEST',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INPKEST)                                                     
         DC    AL1(14),AL2(04),CL5'7CPKG',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INPKPKGE)                                                    
         DC    AL1(14),AL2(05),CL5'7CDPT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INPKDPT)                                                     
         DC    AL1(14),AL2(06),CL5'7CSTA',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INPKSTAT)                                                    
         DC    AL1(14),AL2(07),CL5'7CESD',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPKESTD)                                                    
         DC    AL1(14),AL2(08),CL5'7CEED',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPKEEND)                                                    
         DC    X'00'                                                            
H7CXX    EQU   *                                                                
         SPACE 3                                                                
*                                                                               
*  RETURN PACKAGE                                                               
*                                                                               
H7D      DC   AL1(H7DX-H7D)           HEADER LENGTH                             
         DC   XL2'007D'              HEADER CODE                                
         DC   AL2(H7DXX-H7D)          DISP TO NEXT HEADER                       
H7DX     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'7DCLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(02),CL5'7DEST',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(03),CL5'7DNET',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(04),CL5'7DPKG',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(05),CL5'7DPNM',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(06),CL5'7DCST',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(07),CL5'7DDPT',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(08),CL5'7DDBS',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(09),CL5'7DVEW',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(10),CL5'7DPN2',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(11),CL5'7DSTA',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(12),CL5'7DEST',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(13),CL5'7DEND',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(14),CL5'7DPRD',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(15),CL5'7DCPM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(16),CL5'7DINT',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(17),CL5'7DFLT',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(18),CL5'7DBTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(19),CL5'7DUNI',AL1(MDTPKQ),AL1(3)                    
         DC    AL1(10),AL2(20),CL5'7DAUD',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(21),CL5'7DANM',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(22),CL5'7DACD',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(23),CL5'7DHAV',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(24),CL5'7DHAD',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(25),CL5'7DHCA',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(26),CL5'7DUN%',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(27),CL5'7DREP',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(28),CL5'7DFD%',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(29),CL5'7DIM%',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(30),CL5'7DLEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(31),CL5'7DOPT',AL1(MDTCHQ),AL1(60)                   
         DC    AL1(10),AL2(32),CL5'7DINN',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(33),CL5'7DDEM',AL1(MDTCHQ),AL1(13)                   
         DC    AL1(10),AL2(34),CL5'7DPID',AL1(MDTCHQ),AL1(20)                   
         DC    AL1(10),AL2(35),CL5'7DPLC',AL1(MDTCHQ),AL1(8)                    
         DC    AL1(10),AL2(36),CL5'7DCSV',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(37),CL5'7DVRS',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(38),CL5'7DVTY',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(39),CL5'7DGDM',AL1(MDTCHQ),AL1(10)                   
         DC    X'00'                                                            
H7DXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
********- DRAFT UNIT TABLES                                          *          
*====================================================================*          
*                                                                               
*  DRAFT UNITS REQUEST                                                          
*                                                                               
H24      DC   AL1(H24X-H24)           HEADER LENGTH                             
         DC   XL2'0024'               HEADER CODE                               
         DC   AL2(H24XX-H24)          DISP TO NEXT HEADER                       
H24X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'24KEY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYDR)                                                     
         DC    AL1(14),AL2(02),CL5'24DEM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INDEMOS)                                                     
         DC    AL1(14),AL2(03),CL5'24WDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INDRDATS)                                                    
         DC    AL1(14),AL2(04),CL5'24NWK',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INNWEEKS)                                                    
         DC    AL1(14),AL2(05),CL5'24PWK',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INPWEEK)                                                     
         DC    AL1(14),AL2(06),CL5'24STM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INDRSTM)                                                     
         DC    AL1(14),AL2(07),CL5'24ETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INDRETM)                                                     
         DC    AL1(14),AL2(08),CL5'24DAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INDRDAY)                                                     
         DC    AL1(14),AL2(09),CL5'24LEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INDRLEN)                                                     
         DC    AL1(14),AL2(10),CL5'24GOA',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INACTGUA)                                                    
         DC    AL1(14),AL2(11),CL5'24REQ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INRTGEQU)                                                    
         DC    AL1(14),AL2(12),CL5'24IEQ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INIMPEQU)                                                    
         DC    AL1(14),AL2(13),CL5'24STA',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INACGUA2)                                                    
         DC    X'00'                                                            
H24XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  DRAFT UNIT REPLY                                                             
*                                                                               
H25      DC   AL1(H25X-H25)           HEADER LENGTH                             
         DC   XL2'0025'               HEADER CODE                               
         DC   AL2(H25XX-H25)          DISP TO NEXT HEADER                       
H25X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'25NUM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'25DAT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(03),CL5'25DAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'25STM',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(05),CL5'25ETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(06),CL5'25LEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(07),CL5'25INT',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(08),CL5'25NAM',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(09),CL5'25NTI',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(10),CL5'25BTP',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(11),CL5'25VPH',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(12),CL5'25GRP',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(13),CL5'25IMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(14),CL5'25HSH',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(15),CL5'25HHT',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(16),CL5'25HRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(17),CL5'25HIM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(98),CL5'25EMS',AL1(MDTCHQ),AL1(64)                   
         DC    AL1(10),AL2(99),CL5'25EXT',AL1(MDTCHQ),AL1(54)                   
         DC    X'00'                                                            
H25XX    EQU   *                                                                
         SPACE 3                                                                
*====================================================================*          
********- SPLIT UNITS SECTION                                        *          
*====================================================================*          
*                                                                               
*  DEFAULT UPLOAD INFORMATION                                                   
*                                                                               
H26      DC   AL1(H26X-H26)           HEADER LENGTH                             
         DC   XL2'0026'               HEADER CODE                               
         DC   AL2(H26XX-H26)          DISP TO NEXT HEADER                       
H26X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'26DEM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INDEMOS)                                                     
         DC    AL1(14),AL2(02),CL5'26REC',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSTRUC)                                                     
         DC    AL1(14),AL2(03),CL5'26GOA',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INACTGUA)                                                    
         DC    AL1(14),AL2(04),CL5'26REQ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INRTGEQU)                                                    
         DC    AL1(14),AL2(05),CL5'26IEQ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INIMPEQU)                                                    
         DC    AL1(14),AL2(06),CL5'26STA',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INACGUA2)                                                    
         DC    X'00'                                                            
H26XX    EQU   *                                                                
         SPACE 3                                                                
*                                                                               
*  SPLIT UNITS REPLY                                                            
*                                                                               
H27      DC   AL1(H27X-H27)           HEADER LENGTH                             
         DC   XL2'0027'               HEADER CODE                               
         DC   AL2(H27XX-H27)          DISP TO NEXT HEADER                       
H27X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'27NUM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'27OAC',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(03),CL5'27OAS',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(04),CL5'27LIN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(05),CL5'27NAC',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(06),CL5'27NAS',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(07),CL5'27OVP',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(08),CL5'27ORT',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(09),CL5'27OIM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(10),CL5'27NVP',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(11),CL5'27NRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(12),CL5'27NIM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(13),CL5'27RES',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(14),CL5'7OVP2',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(15),CL5'7ORT2',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(16),CL5'7OIM2',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(17),CL5'7NVP2',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(18),CL5'7NRT2',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(19),CL5'7NIM2',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(57),CL5'31ENM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(58),CL5'31EMS',AL1(MDTCHQ),AL1(64)                   
         DC    AL1(10),AL2(59),CL5'31EXT',AL1(MDTCHQ),AL1(6)                    
         DC    X'00'                                                            
H27XX    EQU   *                                                                
         SPACE 3                                                                
*====================================================================*          
********- COPYU SECTION                                              *          
*====================================================================*          
*                                                                               
*  DEFAULT UPLOAD INFORMATION                                                   
*                                                                               
H28      DC   AL1(H28X-H28)           HEADER LENGTH                             
         DC   XL2'0028'               HEADER CODE                               
         DC   AL2(H28XX-H28)          DISP TO NEXT HEADER                       
H28X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'28TRA',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INCOPTR)                                                     
         DC    AL1(14),AL2(02),CL5'28CTR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INCOCTR)                                                     
         DC    AL1(14),AL2(03),CL5'28CBR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INCOCBR)                                                     
         DC    AL1(14),AL2(04),CL5'28SDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INCOSTDT)                                                    
         DC    AL1(14),AL2(05),CL5'28EDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INCOENDT)                                                    
         DC    AL1(14),AL2(06),CL5'28PRD',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCOPROD)                                                    
         DC    AL1(14),AL2(07),CL5'28LEN',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCOLEN)                                                     
         DC    AL1(14),AL2(08),CL5'28ZCT',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INCOZCT)                                                     
         DC    AL1(14),AL2(20),CL5'28FKY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKCPUF)                                                     
         DC    AL1(14),AL2(21),CL5'28TKY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKCPUT)                                                     
         DC    X'00'                                                            
H28XX    EQU   *                                                                
         SPACE 3                                                                
*                                                                               
*  COPYU REPLY                                                                  
*                                                                               
H29      DC   AL1(H29X-H29)           HEADER LENGTH                             
         DC   XL2'0029'               HEADER CODE                               
         DC   AL2(H29XX-H29)          DISP TO NEXT HEADER                       
H29X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'29NUM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'29TRN',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(57),CL5'29ENM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(58),CL5'29EMS',AL1(MDTCHQ),AL1(64)                   
         DC    AL1(10),AL2(59),CL5'29EXT',AL1(MDTCHQ),AL1(54)                   
         DC    X'00'                                                            
H29XX    EQU   *                                                                
         SPACE 3                                                                
*====================================================================*          
********- UPLOAD UNITS SECTION                                       *          
*====================================================================*          
*                                                                               
*  DEFAULT UPLOAD INFORMATION                                                   
*                                                                               
H30      DC   AL1(H30X-H30)           HEADER LENGTH                             
         DC   XL2'0030'               HEADER CODE                               
         DC   AL2(H30XX-H30)          DISP TO NEXT HEADER                       
H30X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'30DEM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INDEMOS)                                                     
         DC    AL1(14),AL2(02),CL5'30REC',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSTRUC)                                                     
         DC    AL1(14),AL2(03),CL5'30GOA',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INACTGUA)                                                    
         DC    AL1(14),AL2(04),CL5'30REQ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INRTGEQU)                                                    
         DC    AL1(14),AL2(05),CL5'30IEQ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INIMPEQU)                                                    
         DC    AL1(14),AL2(06),CL5'30STA',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INACGUA2)                                                    
         DC    AL1(14),AL2(07),CL5'30HUB',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSHUBSW)                                                    
         DC    AL1(14),AL2(08),CL5'30PRI',AL1(MDTCHQ),AL1(20)                   
         DC    AL4(INPRSMID)                                                    
         DC    X'00'                                                            
H30XX    EQU   *                                                                
*                                                                               
*  UPLOAD REPLY                                                                 
*                                                                               
H31      DC   AL1(H31X-H31)           HEADER LENGTH                             
         DC   XL2'0031'               HEADER CODE                               
         DC   AL2(H31XX-H31)          DISP TO NEXT HEADER                       
H31X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'31NUM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'31DAT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(03),CL5'31DAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'31STM',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(05),CL5'31ETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(06),CL5'31LEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(07),CL5'31INT',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(08),CL5'31NAM',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(09),CL5'31NTI',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(10),CL5'31BTP',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(11),CL5'31VPH',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(12),CL5'31GRP',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(13),CL5'31IMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(14),CL5'31SLN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(15),CL5'31HSH',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(16),CL5'31HHT',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(17),CL5'31HRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(18),CL5'31HIM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(19),CL5'31MPT',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(20),CL5'31ROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(21),CL5'31DPR',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(22),CL5'31RES',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(23),CL5'31VP2',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(24),CL5'31GR2',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(25),CL5'31IM2',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(26),CL5'31HS2',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(27),CL5'31HH2',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(28),CL5'31HR2',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(29),CL5'31HI2',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(30),CL5'31DBS',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(31),CL5'31OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(32),CL5'32OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(33),CL5'33OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(35),CL5'35OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(36),CL5'36OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(37),CL5'37OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(38),CL5'38OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(43),CL5'43OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(44),CL5'44OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(45),CL5'45OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(46),CL5'46OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(47),CL5'47OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(48),CL5'48OVR',AL1(MDTCHQ),AL1(1)                    
*        DC    AL1(10),AL2(49),CL5'49OVR',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(57),CL5'31ENM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(58),CL5'31EMS',AL1(MDTCHQ),AL1(64)                   
         DC    AL1(10),AL2(59),CL5'31EXT',AL1(MDTCHQ),AL1(54)                   
         DC    X'00'                                                            
H31XX    EQU   *                                                                
         SPACE 3                                                                
*====================================================================*          
********- CHANGE UNIT TABLES                                         *          
*====================================================================*          
*                                                                               
*  CHANGE UNITS REQUEST                                                         
*                                                                               
H32      DC   AL1(H32X-H32)           HEADER LENGTH                             
         DC   XL2'0032'               HEADER CODE                               
         DC   AL2(H32XX-H32)          DISP TO NEXT HEADER                       
H32X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'2NACT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNACT)                                                    
         DC    AL1(14),AL2(02),CL5'2OACT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYOACT)                                                    
         DC    AL1(14),AL2(03),CL5'2NASS',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNASS)                                                    
         DC    AL1(14),AL2(04),CL5'2OASS',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYOASS)                                                    
         DC    AL1(14),AL2(05),CL5'2NINT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNINT)                                                    
         DC    AL1(14),AL2(06),CL5'2OINT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYOINT)                                                    
         DC    AL1(14),AL2(07),CL5'2NDAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNDAY)                                                    
         DC    AL1(14),AL2(08),CL5'2ODAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYODAY)                                                    
         DC    AL1(14),AL2(09),CL5'2NROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNROT)                                                    
         DC    AL1(14),AL2(10),CL5'2OROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYOROT)                                                    
         DC    AL1(14),AL2(11),CL5'2NSTM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSTM)                                                    
         DC    AL1(14),AL2(12),CL5'2OSTM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOSTM)                                                    
         DC    AL1(14),AL2(13),CL5'2NETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNETM)                                                    
         DC    AL1(14),AL2(14),CL5'2OETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOETM)                                                    
         DC    AL1(14),AL2(15),CL5'2NLEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNLEN)                                                    
         DC    AL1(14),AL2(16),CL5'2OLEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYOLEN)                                                    
         DC    AL1(14),AL2(17),CL5'2NPRD',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYNPRD)                                                    
         DC    AL1(14),AL2(18),CL5'2OPRD',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYOPRD)                                                    
         DC    AL1(14),AL2(19),CL5'2NNTI',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNNTI)                                                    
         DC    AL1(14),AL2(20),CL5'2ONTI',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYONTI)                                                    
         DC    AL1(14),AL2(21),CL5'32PNM',AL1(MDTCHQ),AL1(16)                   
         DC    AL4(INBYNPNM)                                                    
         DC    AL1(14),AL2(22),CL5'32MGD',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNMGD)                                                    
         DC    AL1(14),AL2(23),CL5'32PRE',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNPRE)                                                    
         DC    AL1(14),AL2(24),CL5'32BTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBYT)                                                    
         DC    AL1(14),AL2(25),CL5'32ADU',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNADU)                                                    
         DC    AL1(14),AL2(26),CL5'32BLB',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBLB)                                                    
         DC    AL1(14),AL2(27),CL5'32CM1',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM1)                                                    
         DC    AL1(14),AL2(28),CL5'32CM2',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM2)                                                    
         DC    AL1(14),AL2(29),CL5'2DDSP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYDDSE)                                                    
         DC    AL1(14),AL2(30),CL5'22RTG',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDRTG)                                                    
         DC    AL1(14),AL2(31),CL5'32VPH',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDVPH)                                                    
         DC    AL1(14),AL2(32),CL5'32IMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDIMP)                                                    
         DC    AL1(14),AL2(33),CL5'2NHUT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHUT)                                                    
         DC    AL1(14),AL2(34),CL5'2OHUT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOHUT)                                                    
         DC    AL1(14),AL2(35),CL5'2NSHR',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSHR)                                                    
         DC    AL1(14),AL2(36),CL5'2OSHR',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOSHR)                                                    
         DC    AL1(14),AL2(37),CL5'2NHRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHRT)                                                    
         DC    AL1(14),AL2(38),CL5'2OHRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOHRT)                                                    
         DC    AL1(14),AL2(39),CL5'2NIMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNHIM)                                                    
         DC    AL1(14),AL2(40),CL5'2NATM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNATM)                                                    
         DC    AL1(14),AL2(41),CL5'2OATM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOATM)                                                    
         DC    AL1(14),AL2(42),CL5'32SDP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYSDPT)                                                    
         DC    AL1(14),AL2(44),CL5'2NREP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNREP)                                                    
         DC    AL1(14),AL2(45),CL5'2OREP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOREP)                                                    
         DC    AL1(14),AL2(46),CL5'2NDAT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INBYNDAT)                                                    
         DC    AL1(14),AL2(47),CL5'32CBO',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INBYCMBO)                                                    
         DC    AL1(14),AL2(48),CL5'2PRTG',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INBYPRTG)                                                    
         DC    AL1(14),AL2(49),CL5'2TIER',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYTIER)                                                    
         DC    AL1(14),AL2(50),CL5'2PCNT',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INBYPCNT)                                                    
         DC    AL1(14),AL2(51),CL5'32NEW',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNEW)                                                     
         DC    AL1(14),AL2(52),CL5'2ESTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYESTY)                                                    
         DC    AL1(14),AL2(53),CL5'32FRZ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYFRZ)                                                     
         DC    AL1(14),AL2(54),CL5'32POD',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYPOD)                                                     
         DC    AL1(14),AL2(56),CL5'32FLT',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INBYFLT)                                                     
         DC    AL1(14),AL2(57),CL5'32GAF',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYGPAF)                                                    
         DC    AL1(14),AL2(69),CL5'32NWD',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INBYNWIN)                                                    
         DC    AL1(14),AL2(70),CL5'32OWD',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INBYOWIN)                                                    
         DC    AL1(14),AL2(71),CL5'32NCS',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INBYNCS#)                                                    
         DC    AL1(14),AL2(72),CL5'32OCS',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INBYOCS#)                                                    
         DC    AL1(14),AL2(73),CL5'32NAI',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INBYNAIR)                                                    
         DC    AL1(14),AL2(74),CL5'32OAI',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INBYOAIR)                                                    
         DC    AL1(14),AL2(75),CL5'32SPL',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INSPLLEN)                                                    
         DC    AL1(14),AL2(76),CL5'2ADSP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYDDSA)                                                    
         DC    AL1(14),AL2(77),CL5'2ARTG',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYARTG)                                                    
         DC    AL1(14),AL2(78),CL5'3AVPH',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYAVPH)                                                    
         DC    AL1(14),AL2(79),CL5'3AIMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYAIMP)                                                    
         DC    AL1(14),AL2(80),CL5'2AHRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYAHRT)                                                    
*-- SPECIAL CHARGE MAP CODES                                                    
         DC    AL1(14),AL2(81),CL5'2SSEQ',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYSSEQ)                                                    
         DC    AL1(14),AL2(82),CL5'2STYP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYSTYP)                                                    
         DC    AL1(14),AL2(83),CL5'2SAMT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYSAMT)                                                    
         DC    AL1(14),AL2(84),CL5'2SREP',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYSCRP)                                                    
         DC    AL1(14),AL2(85),CL5'2SCOM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYSCOM)                                                    
         DC    AL1(14),AL2(86),CL5'2SSTA',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INBYSSTA)                                                    
         DC    AL1(14),AL2(87),CL5'2SBPR',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYSBPR)                                                    
         DC    AL1(14),AL2(88),CL5'2STPR',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYSTPR)                                                    
         DC    AL1(14),AL2(89),CL5'2SDEL',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYSACT)                                                    
*                                                                               
         DC    AL1(14),AL2(59),CL5'32KEY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYCH)                                                     
*-- PACKU FUNCTIONS MAPS 60-74                                                  
         DC    AL1(14),AL2(60),CL5'32PKU',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYOPKU)                                                    
         DC    AL1(14),AL2(61),CL5'32VTY',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYVTYP)                                                    
         DC    AL1(14),AL2(62),CL5'32PFL',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INBYPFLT)                                                    
         DC    AL1(14),AL2(68),CL5'32UNI',AL1(MDTPKQ),AL1(3)                    
         DC    AL4(INBYUNIV)                                                    
         DC    AL1(14),AL2(64),CL5'32ANM',AL1(MDTCHQ),AL1(16)                   
         DC    AL4(INBYANAM)                                                    
         DC    AL1(14),AL2(65),CL5'32ACD',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INBYACOD)                                                    
         DC    AL1(14),AL2(66),CL5'32HAD',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYHTAD)                                                    
         DC    AL1(14),AL2(67),CL5'32UPC',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYUPCT)                                                    
*-- COPY SPLIT MAP CODES 92-97                                                  
         DC    AL1(14),AL2(92),CL5'32CSS',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYCOPS)                                                    
         DC    AL1(14),AL2(93),CL5'32NTS',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNATS)                                                    
         DC    AL1(14),AL2(94),CL5'32CSP',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYCSPR)                                                    
         DC    AL1(14),AL2(95),CL5'32CS1',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBY1PRD)                                                    
         DC    AL1(14),AL2(96),CL5'32CS2',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYFDPT)                                                    
         DC    AL1(14),AL2(97),CL5'32CSF',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INBYFEED)                                                    
         DC    X'00'                                                            
H32XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
********- DELETE UNIT TABLES                                         *          
*====================================================================*          
*                                                                               
*  DELETE UNITS REQUEST                                                         
*                                                                               
H34      DC   AL1(H34X-H34)           HEADER LENGTH                             
         DC   XL2'0034'               HEADER CODE                               
         DC   AL2(H34XX-H34)          DISP TO NEXT HEADER                       
H34X     EQU  *                                                                 
         DC    AL1(14),AL2(59),CL5'34KEY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYDE)                                                     
         DC    X'00'                                                            
H34XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
********- ADD UNIT TABLES                                            *          
*====================================================================*          
*                                                                               
*  BUY UNITS REQUEST                                                            
*                                                                               
H36      DC   AL1(H36X-H36)           HEADER LENGTH                             
         DC   XL2'0036'               HEADER CODE                               
         DC   AL2(H36XX-H36)          DISP TO NEXT HEADER                       
H36X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'6NACT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNACT)                                                    
         DC    AL1(14),AL2(03),CL5'6NASS',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNASS)                                                    
         DC    AL1(14),AL2(05),CL5'6NINT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNINT)                                                    
         DC    AL1(14),AL2(07),CL5'6NDAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNDAY)                                                    
         DC    AL1(14),AL2(09),CL5'6NROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNROT)                                                    
         DC    AL1(14),AL2(11),CL5'6NSTM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSTM)                                                    
         DC    AL1(14),AL2(13),CL5'6NETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNETM)                                                    
         DC    AL1(14),AL2(15),CL5'6NLEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNLEN)                                                    
         DC    AL1(14),AL2(17),CL5'6NPRD',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INBYNPRD)                                                    
         DC    AL1(14),AL2(19),CL5'6NNTI',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNNTI)                                                    
         DC    AL1(14),AL2(21),CL5'36PNM',AL1(MDTCHQ),AL1(16)                   
         DC    AL4(INBYNPNM)                                                    
         DC    AL1(14),AL2(22),CL5'36MGD',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNMGD)                                                    
         DC    AL1(14),AL2(23),CL5'36PRE',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNPRE)                                                    
         DC    AL1(14),AL2(24),CL5'36BTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBYT)                                                    
         DC    AL1(14),AL2(25),CL5'36ADU',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNADU)                                                    
         DC    AL1(14),AL2(26),CL5'36BLB',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBLB)                                                    
         DC    AL1(14),AL2(27),CL5'36CM1',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM1)                                                    
         DC    AL1(14),AL2(28),CL5'36CM2',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM2)                                                    
         DC    AL1(14),AL2(29),CL5'6DDSP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYDDSE)                                                    
         DC    AL1(14),AL2(30),CL5'36RTG',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDRTG)                                                    
         DC    AL1(14),AL2(31),CL5'36VPH',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDVPH)                                                    
         DC    AL1(14),AL2(32),CL5'36IMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDIMP)                                                    
         DC    AL1(14),AL2(33),CL5'6NHUT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHUT)                                                    
         DC    AL1(14),AL2(35),CL5'6NSHR',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSHR)                                                    
         DC    AL1(14),AL2(37),CL5'6NHRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHRT)                                                    
         DC    AL1(14),AL2(39),CL5'6NIMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNHIM)                                                    
         DC    AL1(14),AL2(40),CL5'6NATM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNATM)                                                    
         DC    AL1(14),AL2(42),CL5'36SDP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYSDPT)                                                    
         DC    AL1(14),AL2(43),CL5'36MKG',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYSMKG)                                                    
         DC    AL1(14),AL2(44),CL5'6NREP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNREP)                                                    
         DC    AL1(14),AL2(45),CL5'36REP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOREP)                                                    
         DC    AL1(14),AL2(47),CL5'36CBO',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INBYCMBO)                                                    
         DC    AL1(14),AL2(48),CL5'6PRTG',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INBYPRTG)                                                    
         DC    AL1(14),AL2(49),CL5'6TIER',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYTIER)                                                    
         DC    AL1(14),AL2(50),CL5'6PCNT',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INBYPCNT)                                                    
         DC    AL1(14),AL2(51),CL5'36NEW',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNEW)                                                     
         DC    AL1(14),AL2(53),CL5'36FRZ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYFRZ)                                                     
         DC    AL1(14),AL2(54),CL5'36POD',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYPOD)                                                     
         DC    AL1(14),AL2(55),CL5'36DCS',AL1(MDTCHQ),AL1(32)                   
         DC    AL4(INBYDSC)                                                     
         DC    AL1(14),AL2(56),CL5'36FLT',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INBYFLT)                                                     
         DC    AL1(14),AL2(57),CL5'36GAF',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYGPAF)                                                    
         DC    AL1(14),AL2(69),CL5'36NWD',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INBYNWIN)                                                    
         DC    AL1(14),AL2(71),CL5'36NCS',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INBYNCS#)                                                    
         DC    AL1(14),AL2(73),CL5'36NAI',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INBYNAIR)                                                    
*-- SPECIAL CHARGE MAP CODES                                                    
         DC    AL1(14),AL2(81),CL5'6SSEQ',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYSSEQ)                                                    
         DC    AL1(14),AL2(82),CL5'6STYP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYSTYP)                                                    
         DC    AL1(14),AL2(83),CL5'6SAMT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYSAMT)                                                    
         DC    AL1(14),AL2(84),CL5'6SREP',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYSCRP)                                                    
         DC    AL1(14),AL2(85),CL5'6SCOM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYSCOM)                                                    
         DC    AL1(14),AL2(86),CL5'6SSTA',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INBYSSTA)                                                    
         DC    AL1(14),AL2(87),CL5'6SBPR',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYSBPR)                                                    
         DC    AL1(14),AL2(88),CL5'6STPR',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYSTPR)                                                    
         DC    AL1(14),AL2(89),CL5'6SDEL',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYSACT)                                                    
*-- COPY SPLIT MAP CODES 92-97                                                  
         DC    AL1(14),AL2(92),CL5'32CSS',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYCOPS)                                                    
         DC    AL1(14),AL2(93),CL5'32NTS',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNATS)                                                    
         DC    AL1(14),AL2(94),CL5'32CSP',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INBYCSPR)                                                    
         DC    AL1(14),AL2(95),CL5'32CS1',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBY1PRD)                                                    
         DC    AL1(14),AL2(96),CL5'32CS2',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYFDPT)                                                    
         DC    AL1(14),AL2(97),CL5'32CSF',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INBYFEED)                                                    
         DC    AL1(14),AL2(59),CL5'36KEY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYBU)                                                     
         DC    X'00'                                                            
H36XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
********- REFRESH DEMO CHANGE                                        *          
*====================================================================*          
*                                                                               
*  REFRESH DEMO CHANGE                                                          
*                                                                               
H38      DC   AL1(H38X-H38)           HEADER LENGTH                             
         DC   XL2'0038'               HEADER CODE                               
         DC   AL2(H38XX-H38)          DISP TO NEXT HEADER                       
H38X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'8NACT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNACT)                                                    
         DC    AL1(14),AL2(02),CL5'8OACT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYOACT)                                                    
         DC    AL1(14),AL2(03),CL5'8NASS',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNASS)                                                    
         DC    AL1(14),AL2(04),CL5'8OASS',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYOASS)                                                    
         DC    AL1(14),AL2(05),CL5'8NINT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNINT)                                                    
         DC    AL1(14),AL2(06),CL5'8OINT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYOINT)                                                    
         DC    AL1(14),AL2(07),CL5'8NDAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNDAY)                                                    
         DC    AL1(14),AL2(08),CL5'8ODAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYODAY)                                                    
         DC    AL1(14),AL2(09),CL5'8NROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNROT)                                                    
         DC    AL1(14),AL2(10),CL5'8OROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYOROT)                                                    
         DC    AL1(14),AL2(11),CL5'8NSTM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSTM)                                                    
         DC    AL1(14),AL2(12),CL5'8OSTM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOSTM)                                                    
         DC    AL1(14),AL2(13),CL5'8NETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNETM)                                                    
         DC    AL1(14),AL2(14),CL5'8OETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOETM)                                                    
         DC    AL1(14),AL2(15),CL5'8NLEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNLEN)                                                    
         DC    AL1(14),AL2(16),CL5'8OLEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYOLEN)                                                    
         DC    AL1(14),AL2(17),CL5'8NPRD',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYNPRD)                                                    
         DC    AL1(14),AL2(18),CL5'8OPRD',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYOPRD)                                                    
         DC    AL1(14),AL2(19),CL5'8NNTI',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNNTI)                                                    
         DC    AL1(14),AL2(20),CL5'8ONTI',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYONTI)                                                    
         DC    AL1(14),AL2(21),CL5'38PNM',AL1(MDTCHQ),AL1(16)                   
         DC    AL4(INBYNPNM)                                                    
         DC    AL1(14),AL2(22),CL5'38MGD',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNMGD)                                                    
         DC    AL1(14),AL2(23),CL5'38PRE',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNPRE)                                                    
         DC    AL1(14),AL2(24),CL5'38BTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBYT)                                                    
         DC    AL1(14),AL2(25),CL5'38ADU',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNADU)                                                    
         DC    AL1(14),AL2(26),CL5'38BLB',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBLB)                                                    
         DC    AL1(14),AL2(27),CL5'38CM1',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM1)                                                    
         DC    AL1(14),AL2(28),CL5'38CM2',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM2)                                                    
         DC    AL1(14),AL2(29),CL5'8DDSP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYDDSE)                                                    
         DC    AL1(14),AL2(30),CL5'38RTG',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDRTG)                                                    
         DC    AL1(14),AL2(31),CL5'38VPH',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDVPH)                                                    
         DC    AL1(14),AL2(32),CL5'38IMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDIMP)                                                    
         DC    AL1(14),AL2(33),CL5'8NHUT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHUT)                                                    
         DC    AL1(14),AL2(34),CL5'8OHUT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOHUT)                                                    
         DC    AL1(14),AL2(35),CL5'8NSHR',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSHR)                                                    
         DC    AL1(14),AL2(36),CL5'8OSHR',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOSHR)                                                    
         DC    AL1(14),AL2(37),CL5'8NHRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHRT)                                                    
         DC    AL1(14),AL2(38),CL5'8OHRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOHRT)                                                    
         DC    AL1(14),AL2(39),CL5'8NIMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNHIM)                                                    
         DC    AL1(14),AL2(40),CL5'8NATM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNATM)                                                    
         DC    AL1(14),AL2(41),CL5'8OATM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOATM)                                                    
         DC    AL1(14),AL2(42),CL5'38SDP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYSDPT)                                                    
         DC    AL1(14),AL2(44),CL5'8NREP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNREP)                                                    
         DC    AL1(14),AL2(45),CL5'8OREP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOREP)                                                    
         DC    AL1(14),AL2(46),CL5'8NDAT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INBYNDAT)                                                    
         DC    AL1(14),AL2(52),CL5'2ESTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYESTY)                                                    
         DC    AL1(14),AL2(75),CL5'38SPL',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INSPLLEN)                                                    
         DC    AL1(14),AL2(76),CL5'8ADSP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYDDSA)                                                    
         DC    AL1(14),AL2(77),CL5'8ARTG',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYARTG)                                                    
         DC    AL1(14),AL2(78),CL5'8AVPH',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYAVPH)                                                    
         DC    AL1(14),AL2(79),CL5'8AIMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYAIMP)                                                    
         DC    AL1(14),AL2(80),CL5'8AHRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYAHRT)                                                    
         DC    AL1(14),AL2(59),CL5'38KEY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYRC)                                                     
         DC    X'00'                                                            
H38XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
********- REFRESH DEMO ADD (MAKEGOOD DRAFT BUY)                      *          
*====================================================================*          
*                                                                               
*  REFRESH DEMO BUY                                                             
*                                                                               
H39      DC   AL1(H39X-H39)           HEADER LENGTH                             
         DC   XL2'0039'               HEADER CODE                               
         DC   AL2(H39XX-H39)          DISP TO NEXT HEADER                       
H39X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'9NACT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNACT)                                                    
         DC    AL1(14),AL2(03),CL5'9NASS',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNASS)                                                    
         DC    AL1(14),AL2(05),CL5'9NINT',AL1(MDTBCQ),AL1(4)                    
         DC    AL4(INBYNINT)                                                    
         DC    AL1(14),AL2(07),CL5'9NDAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNDAY)                                                    
         DC    AL1(14),AL2(09),CL5'9NROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNROT)                                                    
         DC    AL1(14),AL2(11),CL5'9NSTM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSTM)                                                    
         DC    AL1(14),AL2(13),CL5'9NETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNETM)                                                    
         DC    AL1(14),AL2(15),CL5'9NLEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNLEN)                                                    
         DC    AL1(14),AL2(17),CL5'9NPRD',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INBYNPRD)                                                    
         DC    AL1(14),AL2(19),CL5'9NNTI',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNNTI)                                                    
         DC    AL1(14),AL2(21),CL5'39PNM',AL1(MDTCHQ),AL1(16)                   
         DC    AL4(INBYNPNM)                                                    
         DC    AL1(14),AL2(22),CL5'39MGD',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNMGD)                                                    
         DC    AL1(14),AL2(23),CL5'39PRE',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNPRE)                                                    
         DC    AL1(14),AL2(24),CL5'39BTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBYT)                                                    
         DC    AL1(14),AL2(25),CL5'39ADU',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNADU)                                                    
         DC    AL1(14),AL2(26),CL5'39BLB',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBLB)                                                    
         DC    AL1(14),AL2(27),CL5'39CM1',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM1)                                                    
         DC    AL1(14),AL2(28),CL5'39CM2',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM2)                                                    
         DC    AL1(14),AL2(29),CL5'9DDSP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYDDSE)                                                    
         DC    AL1(14),AL2(30),CL5'39RTG',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDRTG)                                                    
         DC    AL1(14),AL2(31),CL5'39VPH',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDVPH)                                                    
         DC    AL1(14),AL2(32),CL5'39IMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDIMP)                                                    
         DC    AL1(14),AL2(33),CL5'9NHUT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHUT)                                                    
         DC    AL1(14),AL2(35),CL5'9NSHR',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSHR)                                                    
         DC    AL1(14),AL2(37),CL5'9NHRT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHRT)                                                    
         DC    AL1(14),AL2(39),CL5'9NIMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNHIM)                                                    
         DC    AL1(14),AL2(40),CL5'9NATM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNATM)                                                    
         DC    AL1(14),AL2(42),CL5'39SDP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYSDPT)                                                    
         DC    AL1(14),AL2(43),CL5'39MKG',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYSMKG)                                                    
         DC    AL1(14),AL2(44),CL5'9NREP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNREP)                                                    
         DC    AL1(14),AL2(48),CL5'9PRTG',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INBYPRTG)                                                    
         DC    AL1(14),AL2(49),CL5'9TIER',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYTIER)                                                    
         DC    AL1(14),AL2(50),CL5'9PCNT',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INBYPCNT)                                                    
         DC    AL1(14),AL2(51),CL5'39NEW',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNEW)                                                     
         DC    AL1(14),AL2(56),CL5'39FLT',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INBYFLT)                                                     
         DC    AL1(14),AL2(57),CL5'39GAF',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYGPAF)                                                    
         DC    AL1(14),AL2(59),CL5'39KEY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYRB)                                                     
         DC    X'00'                                                            
H39XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
********- HISTORY RECORD LOOKUP                                      *          
*====================================================================*          
*                                                                               
*  HISTORY RECORD REQUEST                                                       
*                                                                               
H57      DC   AL1(H57X-H57)           HEADER LENGTH                             
         DC   XL2'0057'               HEADER CODE                               
         DC   AL2(H57XX-H57)          DISP TO NEXT HEADER                       
H57X     EQU  *                                                                 
         DC    AL1(14),AL2(59),CL5'57KEY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYHS)                                                     
         DC    X'00'                                                            
H57XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* RESEARCH REQUESTS (FROM PC)                                        *          
*====================================================================*          
*                                                                               
*  TIME PERIOD TREND                                                            
*                                                                               
H70      DC   AL1(H70X-H70)           HEADER LENGTH                             
         DC   XL2'0070'               HEADER CODE                               
         DC   AL2(H70XX-H70)          DISP TO NEXT HEADER                       
H70X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'70NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETRES)                                                    
         DC    AL1(14),AL2(02),CL5'70DAY',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPRDAY)                                                     
         DC    AL1(14),AL2(03),CL5'70TIM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INRSTIME)                                                    
         DC    AL1(14),AL2(04),CL5'70SDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(05),CL5'70EDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPREDAT)                                                    
         DC    AL1(14),AL2(07),CL5'70DEM',AL1(MDTCHQ),AL1(200)                  
         DC    AL4(INDEMOS)                                                     
         DC    AL1(14),AL2(08),CL5'70SRC',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSOURCE)                                                    
         DC    X'00'                                                            
H70XX    EQU   *                                                                
*                                                                               
*                                                                               
*  PROGRAM PERIOD TREND                                                         
*                                                                               
H72      DC   AL1(H72X-H72)           HEADER LENGTH                             
         DC   XL2'0072'               HEADER CODE                               
         DC   AL2(H72XX-H72)          DISP TO NEXT HEADER                       
H72X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'72NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETRES)                                                    
         DC    AL1(14),AL2(02),CL5'72NTI',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INNTI)                                                       
         DC    AL1(14),AL2(04),CL5'72SDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(05),CL5'72EDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPREDAT)                                                    
         DC    AL1(14),AL2(07),CL5'72DEM',AL1(MDTCHQ),AL1(200)                  
         DC    AL4(INDEMOS)                                                     
         DC    AL1(14),AL2(08),CL5'72SRC',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSOURCE)                                                    
         DC    X'00'                                                            
H72XX    EQU   *                                                                
*                                                                               
*                                                                               
*  PROGRAM RANKER                                                               
*                                                                               
H74      DC   AL1(H74X-H74)           HEADER LENGTH                             
         DC   XL2'0074'               HEADER CODE                               
         DC   AL2(H74XX-H74)          DISP TO NEXT HEADER                       
H74X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'74NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETRES)                                                    
         DC    AL1(14),AL2(02),CL5'74DAY',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPRDAY)                                                     
         DC    AL1(14),AL2(03),CL5'74TIM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INRSTIME)                                                    
         DC    AL1(14),AL2(04),CL5'74SDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(05),CL5'74EDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPREDAT)                                                    
         DC    AL1(14),AL2(07),CL5'74DEM',AL1(MDTCHQ),AL1(200)                  
         DC    AL4(INDEMOS)                                                     
         DC    AL1(14),AL2(08),CL5'74SRC',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSOURCE)                                                    
         DC    X'00'                                                            
H74XX    EQU   *                                                                
*                                                                               
*                                                                               
*  SHARE/HUT ANALYSIS                                                           
*                                                                               
H76      DC   AL1(H76X-H76)           HEADER LENGTH                             
         DC   XL2'0076'               HEADER CODE                               
         DC   AL2(H76XX-H76)          DISP TO NEXT HEADER                       
H76X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'76NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETRES)                                                    
         DC    AL1(14),AL2(02),CL5'76DAY',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPRDAY)                                                     
         DC    AL1(14),AL2(03),CL5'76TIM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INRSTIME)                                                    
         DC    AL1(14),AL2(04),CL5'76SDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(05),CL5'76EDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPREDAT)                                                    
         DC    AL1(14),AL2(08),CL5'76SRC',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSOURCE)                                                    
         DC    X'00'                                                            
H76XX    EQU   *                                                                
*                                                                               
*                                                                               
*  PHEADER/EHEADER REQUEST                                                      
*                                                                               
H78      DC   AL1(H78X-H78)           HEADER LENGTH                             
         DC   XL2'0078'               HEADER CODE                               
         DC   AL2(H78XX-H78)          DISP TO NEXT HEADER                       
H78X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'78PNT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPHPRNT)                                                    
         DC    AL1(14),AL2(02),CL5'78DST',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPHDEST)                                                    
         DC    AL1(14),AL2(03),CL5'78CLI',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPHCLI)                                                     
         DC    AL1(14),AL2(04),CL5'78EST',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPHEST)                                                     
         DC    AL1(14),AL2(05),CL5'78NET',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPHNET)                                                     
         DC    AL1(14),AL2(06),CL5'78PKG',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPHPACK)                                                    
         DC    AL1(14),AL2(07),CL5'78DEM',AL1(MDTCHQ),AL1(64)                   
         DC    AL4(INPHDEMS)                                                    
         DC    AL1(14),AL2(08),CL5'78COM',AL1(MDTCHQ),AL1(50)                   
         DC    AL4(INPHCOMS)                                                    
         DC    AL1(14),AL2(09),CL5'78DEL',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INPHDEAL)                                                    
         DC    AL1(14),AL2(10),CL5'78CNT',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INPHCONT)                                                    
*  MAP CODE 1 MUST BE PASSED LAST FROM STEWARD                                  
         DC    AL1(14),AL2(20),CL5'78TYP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INPHTYPE)                                                    
         DC    X'00'                                                            
H78XX    EQU   *                                                                
*                                                                               
*====================================================================*          
* 12-15 - INITIAL DOWNLOAD DATA                                      *          
*====================================================================*          
         SPACE 1                                                                
H11      DC   AL1(H11X-H11)           HEADER LENGTH                             
         DC   XL2'0011'               HEADER CODE                               
         DC   AL2(H11XX-H11)          DISP TO NEXT HEADER                       
H11X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'SRHST',AL1(MDTCHQ),AL1(35)                   
         DC    AL1(10),AL2(02),CL5'SRPRT',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(03),CL5'SRSEC',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'SRCRT',AL1(MDTCHQ),AL1(25)                   
         DC    AL1(10),AL2(05),CL5'SRUSR',AL1(MDTCHQ),AL1(25)                   
         DC    AL1(10),AL2(06),CL5'SRPSW',AL1(MDTCHQ),AL1(25)                   
         DC    AL1(10),AL2(07),CL5'ALURL',AL1(MDTCHQ),AL1(70)                   
         DC    AL1(10),AL2(08),CL5'ALSUR',AL1(MDTCHQ),AL1(70)                   
         DC    AL1(10),AL2(09),CL5'ALTOK',AL1(MDTCHQ),AL1(50)                   
         DC    AL1(10),AL2(10),CL5'ALHTP',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(11),CL5'ALACC',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(12),CL5'SNDHB',AL1(MDTCHQ),AL1(20)                   
         DC    AL1(10),AL2(13),CL5'1113 ',AL1(MDTCHQ),AL1(71)                   
         DC    AL1(10),AL2(14),CL5'1114 ',AL1(MDTCHQ),AL1(52)                   
         DC    AL1(10),AL2(15),CL5'SRRED',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(16),CL5'1116 ',AL1(MDTCHQ),AL1(36)                   
         DC    X'00'                                                            
H11XX    EQU   *                                                                
         SPACE 1                                                                
H12      DC   AL1(H12X-H12)           HEADER LENGTH                             
         DC   XL2'0012'               HEADER CODE                               
         DC   AL2(H12XX-H12)          DISP TO NEXT HEADER                       
H12X     EQU  *                                                                 
         DC    AL1(10),AL2(03),CL5'DEMOC',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(04),CL5'DAY  ',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(05),CL5'LENGT',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(06),CL5'TRANM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(07),CL5'TRANP',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(08),CL5'RESTR',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(09),CL5'CNTNT',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(10),CL5'DEMNM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(11),CL5'SWSDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(12),CL5'SWEDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(13),CL5'MAXUN',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(14),CL5'AGFL2',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(15),CL5'TRANS',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(16),CL5'MGMAX',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(17),CL5'SC1BT',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(18),CL5'SC2BT',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(19),CL5'SCLIT',AL1(MDTCHQ),AL1(20)                   
         DC    AL1(10),AL2(20),CL5'SCDIS',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(21),CL5'SCPRO',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(22),CL5'HISCD',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(23),CL5'HISDS',AL1(MDTCHQ),AL1(14)                   
         DC    AL1(10),AL2(24),CL5'REPCD',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(25),CL5'REPNM',AL1(MDTCHQ),AL1(8)                    
         DC    AL1(10),AL2(26),CL5'AGYID',AL1(MDTCHQ),AL1(2)                    
         DC    X'00'                                                            
H12XX    EQU   *                                                                
         SPACE 1                                                                
H13      DC   AL1(H13X-H13)           HEADER LENGTH                             
         DC   XL2'0013'               HEADER CODE                               
         DC   AL2(H13XX-H13)          DISP TO NEXT HEADER                       
H13X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'DAYPT',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(02),CL5'DAYPD',AL1(MDTCHQ),AL1(14)                   
         DC    AL1(10),AL2(03),CL5'DAYPE',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'CLIEN',AL1(MDTCHQ),AL1(3)                    
         DC    X'00'                                                            
H13XX    EQU   *                                                                
         SPACE 1                                                                
H14      DC   AL1(H14X-H14)           HEADER LENGTH                             
         DC   XL2'0014'               HEADER CODE                               
         DC   AL2(H14XX-H14)          DISP TO NEXT HEADER                       
H14X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'REASC',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(02),CL5'REASD',AL1(MDTCHQ),AL1(50)                   
         DC    X'00'                                                            
H14XX    EQU   *                                                                
         SPACE 1                                                                
H15      DC   AL1(H15X-H15)           HEADER LENGTH                             
         DC   XL2'0015'               HEADER CODE                               
         DC   AL2(H15XX-H15)          DISP TO NEXT HEADER                       
H15X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'NTIST',AL1(MDTCHQ),AL1(5)                    
         DC    AL1(10),AL2(02),CL5'STYPE',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(03),CL5'PTYPE',AL1(MDTCHQ),AL1(1)                    
         DC    X'00'                                                            
H15XX    EQU   *                                                                
         SPACE 1                                                                
H16      DC   AL1(H16X-H16)           HEADER LENGTH                             
         DC   XL2'0016'               HEADER CODE                               
         DC   AL2(H16XX-H16)          DISP TO NEXT HEADER                       
H16X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'NADPR',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'NADCT',AL1(MDTCHQ),AL1(7)                    
         DC    X'00'                                                            
H16XX    EQU   *                                                                
         SPACE 1                                                                
H17      DC   AL1(H17X-H17)           HEADER LENGTH                             
         DC   XL2'0017'               HEADER CODE                               
         DC   AL2(H17XX-H17)          DISP TO NEXT HEADER                       
H17X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'AGSTA',AL1(MDTCHQ),AL1(1)                    
         DC    X'00'                                                            
H17XX    EQU   *                                                                
         SPACE 1                                                                
H18      DC   AL1(H18X-H18)           HEADER LENGTH                             
         DC   XL2'0018'               HEADER CODE                               
         DC   AL2(H18XX-H18)          DISP TO NEXT HEADER                       
H18X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'PRTYP',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(02),CL5'PRTDS',AL1(MDTCHQ),AL1(60)                   
         DC    X'00'                                                            
H18XX    EQU   *                                                                
         SPACE 1                                                                
*  MAP 1 X'80' = (FRONTRUNNER) NO DEMO CHANGES ALLOWED                          
*  MAP 1 X'40' = (FRONTRUNNER) REASON CODE REQUIRED                             
*  MAP 1 X'20' = (FRONTRUNNER) NO DAYPART CHANGES ALLOWED                       
*  MAP 1 X'10' = (FRONTRUNNER) PKG2 NAME FIELD REQUIRED                         
*  MAP 1 X'08' = (FRONTRUNNER) TVQ DEMOS ALLOWED                                
*  MAP 1 X'04' = (FRONTRUNNER) OPTICON DEMOS ALLOWED                            
*  MAP 1 X'02' = (STEWARD)     FROZEN ASSIGNED COST                             
*  MAP 1 X'01' = (STEWARD)     CREATE UPLOAD LOG                                
H19      DC   AL1(H19X-H19)           HEADER LENGTH                             
         DC   XL2'0019'               HEADER CODE                               
         DC   AL2(H19XX-H19)          DISP TO NEXT HEADER                       
H19X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'FRSW1',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'FRPR2',AL1(MDTCHQ),AL1(1)                    
         DC    X'00'                                                            
H19XX    EQU   *                                                                
         SPACE 1                                                                
**********************************************************************          
* COMSCORE WEB SERVICE SECURITY DOWNLOAD                                        
**********************************************************************          
H20      DC   AL1(H20X-H20)           HEADER LENGTH                             
         DC   XL2'0020'               HEADER CODE                               
         DC   AL2(H20XX-H20)          DISP TO NEXT HEADER                       
H20X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'FRCST',AL1(MDTCHQ),AL1(200)                  
         DC    AL1(10),AL2(02),CL5'FRCSC',AL1(MDTCHQ),AL1(200)                  
         DC    AL1(10),AL2(03),CL5'FRCSE',AL1(MDTCHQ),AL1(200)                  
         DC    AL1(10),AL2(04),CL5'FRCSM',AL1(MDTCHQ),AL1(200)                  
         DC    AL1(10),AL2(05),CL5'FRCSD',AL1(MDTBIQ),AL1(1)                    
         DC    X'00'                                                            
H20XX    EQU   *                                                                
         SPACE 1                                                                
**********************************************************************          
* COMSCORE STREAMS (VIEWING TYPES)                                              
**********************************************************************          
H21      DC   AL1(H21X-H21)           HEADER LENGTH                             
         DC   XL2'0021'               HEADER CODE                               
         DC   AL2(H21XX-H21)          DISP TO NEXT HEADER                       
H21X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'FRCSS',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(02),CL5'FRCSD',AL1(MDTCHQ),AL1(15)                   
         DC    AL1(10),AL2(03),CL5'FRCSR',AL1(MDTCHQ),AL1(8)                    
         DC    X'00'                                                            
H21XX    EQU   *                                                                
         SPACE 1                                                                
**********************************************************************          
* COMSCORE SOURCES                                                              
**********************************************************************          
H22      DC   AL1(H22X-H22)           HEADER LENGTH                             
         DC   XL2'0022'               HEADER CODE                               
         DC   AL2(H22XX-H22)          DISP TO NEXT HEADER                       
H22X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'FRCSS',AL1(MDTCHQ),AL1(20)                   
         DC    X'00'                                                            
H22XX    EQU   *                                                                
         SPACE 1                                                                
**********************************************************************          
* UNIVERSE CUT OFF DATES                                                        
**********************************************************************          
H23      DC   AL1(H23X-H23)           HEADER LENGTH                             
         DC   XL2'0023'               HEADER CODE                               
         DC   AL2(H23XX-H23)          DISP TO NEXT HEADER                       
H23X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'UNVYR',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(02),CL5'UNVDT',AL1(MDTCHQ),AL1(8)                    
         DC    X'00'                                                            
H23XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 46    - PROGRAMS SEARCH REQUEST REPLY                              *          
*====================================================================*          
         SPACE 1                                                                
H42      DC   AL1(H42X-H42)           HEADER LENGTH                             
         DC   XL2'0042'               HEADER CODE                               
         DC   AL2(H42XX-H42)          DISP TO NEXT HEADER                       
H42X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'42DAY',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(02),CL5'42TIM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(03),CL5'42NAM',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(04),CL5'42PCD',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(05),CL5'42SDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(06),CL5'42EDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(07),CL5'42NTI',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(08),CL5'42RTG',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(09),CL5'42SHR',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(10),CL5'42NEW',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(11),CL5'42TYP',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(12),CL5'42CNT',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(13),CL5'42TER',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(14),CL5'42ROT',AL1(MDTBIQ),AL1(1)                    
*******  DC    AL1(10),AL2(15),CL5'42DPT',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(16),CL5'42DPR',AL1(MDTBIQ),AL1(1)                    
         DC    X'00'                                                            
H42XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 46    - PROGRAMS DETAILS REQUEST REPLY                             *          
*====================================================================*          
         SPACE 1                                                                
H46      DC   AL1(H46X-H46)           HEADER LENGTH                             
         DC   XL2'0046'               HEADER CODE                               
         DC   AL2(H46XX-H46)          DISP TO NEXT HEADER                       
H46X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'46DAY',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(02),CL5'46TIM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(03),CL5'46NAM',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(04),CL5'46NTI',AL1(MDTBIQ),AL1(2)                    
         DC    X'00'                                                            
H46XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 7B    - HISTORY RECORD REQUEST REPLY                               *          
*====================================================================*          
         SPACE 1                                                                
H7B      DC   AL1(H7BX-H7B)           HEADER LENGTH                             
         DC   XL2'007B'               HEADER CODE                               
         DC   AL2(H7BXX-H7B)          DISP TO NEXT HEADER                       
H7BX     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'7BDAT',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(02),CL5'7BTIM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(03),CL5'7BNAM',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(04),CL5'7BNTI',AL1(MDTBIQ),AL1(2)                    
         DC    X'00'                                                            
H7BXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 46    - RESEARCH REPLY                                             *          
*====================================================================*          
         SPACE 1                                                                
*                                                                               
*  TIME PERIOD TREND                                                            
*                                                                               
H71      DC   AL1(H71X-H71)           HEADER LENGTH                             
         DC   XL2'0071'               HEADER CODE                               
         DC   AL2(H71XX-H71)          DISP TO NEXT HEADER                       
H71X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'71WEK',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(03),CL5'71DAY',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(04),CL5'71TIM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(05),CL5'71NAM',AL1(MDTCHQ),AL1(25)                   
         DC    AL1(10),AL2(06),CL5'71DEM',AL1(MDTBIQ),AL1(4)                    
         DC    X'00'                                                            
H71XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  PROGRAM PERIOD TREND                                                         
*                                                                               
H73      DC   AL1(H73X-H73)           HEADER LENGTH                             
         DC   XL2'0073'               HEADER CODE                               
         DC   AL2(H73XX-H73)          DISP TO NEXT HEADER                       
H73X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'73WEK',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(03),CL5'73DAY',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(04),CL5'73TIM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(05),CL5'75NAM',AL1(MDTCHQ),AL1(25)                   
         DC    AL1(10),AL2(06),CL5'73DEM',AL1(MDTBIQ),AL1(4)                    
         DC    X'00'                                                            
H73XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  PROGRAM RANKER                                                               
*                                                                               
H75      DC   AL1(H75X-H75)           HEADER LENGTH                             
         DC   XL2'0075'               HEADER CODE                               
         DC   AL2(H75XX-H75)          DISP TO NEXT HEADER                       
H75X     EQU  *                                                                 
         DC    AL1(10),AL2(02),CL5'75NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(03),CL5'75DAY',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(04),CL5'75TIM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(05),CL5'75NAM',AL1(MDTCHQ),AL1(25)                   
         DC    AL1(10),AL2(06),CL5'75DEM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(08),CL5'75NTI',AL1(MDTCHQ),AL1(4)                    
         DC    X'00'                                                            
H75XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  SHARE/HUT ANALYSIS                                                           
*                                                                               
H77      DC   AL1(H77X-H77)           HEADER LENGTH                             
         DC   XL2'0077'               HEADER CODE                               
         DC   AL2(H77XX-H77)          DISP TO NEXT HEADER                       
H77X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'77WEK',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(03),CL5'77DAY',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(04),CL5'77TIM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(06),CL5'77DEM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(07),CL5'77IMP',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(09),CL5'77HUT',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(10),CL5'77UNI',AL1(MDTBIQ),AL1(4)                    
         DC    X'00'                                                            
H77XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 99 - ERROR REPLY                                                              
*====================================================================*          
*                                                                               
*  ERROR REPLY RECORD                                                           
*                                                                               
H99      DC   AL1(H99X-H99)           HEADER LENGTH                             
         DC   XL2'0099'               HEADER CODE                               
         DC   AL2(H99XX-H99)          DISP TO NEXT HEADER                       
H99X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'99ERR',AL1(MDTCHQ),AL1(54)                   
         DC    X'00'                                                            
H99XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* ALL MAP CODES BETWEEN 50-6- ARE FOR CABLE MATCHMAKER                          
* EXCEPT FOR THE CHANGE UNIT MAP CODE WHICH WILL REMAIN THE SAME                
* AS STEWARDS.                                                                  
*====================================================================*          
         SPACE 1                                                                
*====================================================================*          
* 50 (OFFICE/BUYER NAMES)                                                       
*====================================================================*          
         SPACE 1                                                                
H50      DC    AL1(H50X-H50)       HEADER LENGTH                                
         DC    XL2'0050'           HEADER CODE                                  
         DC    AL2(H50XX-H50)      DISP TO NEXT HEADER                          
H50X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'OFCNM',AL1(MDTCHQ),AL1(24)                   
         DC    AL1(10),AL2(02),CL5'BYRNM',AL1(MDTCHQ),AL1(24)                   
         DC    AL1(10),AL2(03),CL5'SPVNM',AL1(MDTCHQ),AL1(24)                   
         DC    X'00'                                                            
H50XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*====================================================================*          
* 58 - HISTORY RECORD UPLOAD                                                    
*                                                                               
*====================================================================*          
         SPACE 1                                                                
H58      DC    AL1(H58X-H58)       HEADER LENGTH                                
         DC    XL2'0058'           HEADER CODE                                  
         DC    AL2(H58XX-H58)      DISP TO NEXT HEADER                          
H58X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'58SEQ',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'58ADT',AL1(MDTBDQ),AL1(3)                    
         DC    AL1(10),AL2(03),CL5'58ATM',AL1(MDTCHQ),AL1(8)                    
         DC    AL1(10),AL2(04),CL5'58BDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(05),CL5'58STA',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(06),CL5'58RSN',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(07),CL5'58PRN',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(08),CL5'58ROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(09),CL5'58LEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(10),CL5'58PRD',AL1(MDTCHQ),AL1(18)                   
         DC    AL1(10),AL2(11),CL5'5ACTO',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(12),CL5'5USRO',AL1(MDTCHQ),AL1(8)                    
         DC    AL1(10),AL2(13),CL5'58GRP',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(14),CL5'58BTM',AL1(MDTCHQ),AL1(11)                   
*  ORIGINAL MISSED MAKEGOOD INFO                                                
         DC    AL1(10),AL2(16),CL5'58MTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(17),CL5'58MPC',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(18),CL5'58MPN',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(19),CL5'58MDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(20),CL5'58MLN',AL1(MDTBIQ),AL1(1)                    
*  CHANGED INFO                                                                 
         DC    AL1(10),AL2(32),CL5'58USR',AL1(MDTCHQ),AL1(8)                    
         DC    AL1(10),AL2(33),CL5'58RSN',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(34),CL5'58CTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(35),CL5'58CPC',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(36),CL5'58CPN',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(37),CL5'58CDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(38),CL5'58CLI',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(39),CL5'58CLN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(40),CL5'58CST',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(41),CL5'58CDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(42),CL5'58CTM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(43),CL5'58CPN',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(44),CL5'58CPR',AL1(MDTCHQ),AL1(18)                   
         DC    AL1(10),AL2(45),CL5'58COM',AL1(MDTCHQ),AL1(60)                   
         DC    AL1(10),AL2(46),CL5'58CPE',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(47),CL5'58CRT',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(55),CL5'58CDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(56),CL5'58CTM',AL1(MDTCHQ),AL1(8)                    
         DC    AL1(10),AL2(57),CL5'58ENM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(58),CL5'58EMS',AL1(MDTCHQ),AL1(64)                   
         DC    AL1(10),AL2(59),CL5'58EXT',AL1(MDTCHQ),AL1(54)                   
         DC    X'00'                                                            
H58XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*====================================================================*          
* 51 - MATCH HEADER DATA                                                        
*                                                                               
*====================================================================*          
         SPACE 1                                                                
H51      DC    AL1(H51X-H51)       HEADER LENGTH                                
         DC    XL2'0051'           HEADER CODE                                  
         DC    AL2(H51XX-H51)      DISP TO NEXT HEADER                          
H51X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(03),CL5'STA  ',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(04),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(05),CL5'PRD  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(06),CL5'PR2  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(07),CL5'MOS  ',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(08),CL5'ESRNG',AL1(MDTCHQ),AL1(7)                    
         DC    AL1(10),AL2(10),CL5'CNAME',AL1(MDTCHQ),AL1(20)                   
         DC    AL1(10),AL2(11),CL5'PNAME',AL1(MDTCHQ),AL1(20)                   
         DC    AL1(10),AL2(14),CL5'FLMSQ',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(15),CL5'FILM ',AL1(MDTCHQ),AL1(8)                    
         DC    AL1(10),AL2(16),CL5'FLAGS',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(17),CL5'CALBR',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(18),CL5'PRGGR',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(19),CL5'COSTP',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(20),CL5'BUYER',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(21),CL5'PRGNM',AL1(MDTCHQ),AL1(24)                   
         DC    AL1(10),AL2(22),CL5'FLMAN',AL1(MDTBIQ),AL1(1)                    
         DC    X'00'                                                            
H51XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 52 - DOWNLOAD HEADERS REQUEST                                      *          
*====================================================================*          
         SPACE 1                                                                
H52      DC    AL1(H52X-H52)       HEADER LENGTH                                
         DC    XL2'0052'           HEADER CODE                                  
         DC    AL2(H52XX-H52)      DISP TO NEXT HEADER                          
H52X     EQU   *                                                                
         DC    AL1(14),AL2(01),CL5'GROUP',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INGRP)                                                       
         DC    AL1(14),AL2(02),CL5'BUYER',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYR)                                                       
***      DC    AL1(14),AL2(03),CL5'MEDIA',AL1(MDTCHQ),AL1(1)                    
***      DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(04),CL5'MATCH',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INMATCH)                                                     
         DC    AL1(14),AL2(05),CL5'SUPVR',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INSPV)                                                       
         DC    AL1(14),AL2(06),CL5'STDAT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(07),CL5'ENDAT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPREDAT)                                                    
         DC    X'00'                                                            
H52XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 53 - REQUEST OLREPORT                                                         
*====================================================================*          
         SPACE 1                                                                
H53      DC    AL1(H53X-H53)       HEADER LENGTH                                
         DC    XL2'0053'           HEADER CODE                                  
         DC    AL2(H53XX-H53)      DISP TO NEXT HEADER                          
H53X     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'53STR',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQSTR)                                                      
         DC    AL1(14),AL2(02),CL5'53COM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQCOM)                                                      
         DC    AL1(14),AL2(03),CL5'53WHN',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQWHEN)                                                     
         DC    AL1(10),AL2(04),CL5'53TID',AL1(MDTCHQ),AL1(9)                    
         DC    AL1(14),AL2(05),CL5'53DST',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQDEST)                                                     
         DC    AL1(14),AL2(06),CL5'53CLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INI2CLT)                                                     
         DC    AL1(14),AL2(07),CL5'53EST',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INI2EST)                                                     
         DC    AL1(14),AL2(08),CL5'53FLT',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INI2EFL)                                                     
         DC    AL1(14),AL2(09),CL5'53NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETPRG)                                                    
         DC    AL1(14),AL2(10),CL5'53PR1',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPROD1)                                                     
         DC    AL1(14),AL2(11),CL5'53PR2',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPROD2)                                                     
         DC    AL1(14),AL2(12),CL5'53SDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(13),CL5'53INT',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INI2INT)                                                     
         DC    AL1(14),AL2(14),CL5'53CAL',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INI2CAL)                                                     
         DC    AL1(14),AL2(15),CL5'53PGR',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INI2PGR)                                                     
         DC    AL1(14),AL2(16),CL5'53DRF',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INI2DRF)                                                     
         DC    AL1(14),AL2(17),CL5'53MOS',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INPRMOS)                                                     
         DC    AL1(14),AL2(18),CL5'53DCM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INDELCOM)                                                    
         DC    X'00'                                                            
H53XX EQU      *                                                                
         SPACE 1                                                                
*                                                                               
*  INIT RECORD (CABLE MATCHMAKER)                                               
*                                                                               
         SPACE 1                                                                
H59      DC   AL1(H59X-H59)           HEADER LENGTH                             
         DC   XL2'0059'               HEADER CODE                               
         DC   AL2(H59XX-H59)          DISP TO NEXT HEADER                       
H59X     EQU  *                                                                 
         DC    X'00'                                                            
H59XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 5C - PACKAGE INFORMATION WIZARD                                    *          
*====================================================================*          
         SPACE 1                                                                
H5C      DC    AL1(H5CX-H5C)       HEADER LENGTH                                
         DC    XL2'005C'           HEADER CODE                                  
         DC    AL2(H5CXX-H5C)      DISP TO NEXT HEADER                          
H5CX     EQU   *                                                                
         DC    AL1(14),AL2(01),CL5'5CCLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCLIENT)                                                    
         DC    AL1(14),AL2(02),CL5'5CNET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETRES)                                                    
         DC    AL1(14),AL2(03),CL5'5CEST',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INEST)                                                       
         DC    AL1(14),AL2(04),CL5'5CPKG',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INPACKGE)                                                    
         DC    AL1(14),AL2(05),CL5'5CREP',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INREP)                                                       
         DC    AL1(14),AL2(16),CL5'5CFRS',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRNTSW)                                                    
         DC    X'00'                                                            
H5CXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 60 - PACKAGE INFORMATION REPLY                                                
*====================================================================*          
         SPACE 1                                                                
H5D      DC    AL1(H5DX-H5D)       HEADER LENGTH                                
         DC    XL2'005D'           HEADER CODE                                  
         DC    AL2(H5DXX-H5D)      DISP TO NEXT HEADER                          
H5DX     EQU   *                                                                
         DC    AL1(10),AL2(01),CL5'5DAUD',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'5DSTA',AL1(MDTBIQ),AL1(1)                    
         DC    X'00'                                                            
H5DXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 79 - PHEADER/EHEADER REPLY                                                    
*====================================================================*          
         SPACE 1                                                                
H79      DC    AL1(H79X-H79)       HEADER LENGTH                                
         DC    XL2'0079'           HEADER CODE                                  
         DC    AL2(H79XX-H79)      DISP TO NEXT HEADER                          
H79X     EQU   *                                                                
         DC    AL1(10),AL2(01),CL5'79PHD',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'79PST',AL1(MDTBIQ),AL1(1)                    
         DC    X'00'                                                            
H79XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* ALL MAP CODES BETWEEN 80-99 ARE FOR FRONTRUNNER                               
* EXCEPT FOR THE CHANGE UNIT MAP CODE WHICH WILL REMAIN THE SAME                
* AS STEWARDS.                                                                  
*====================================================================*          
*                                                                               
*  INIT RECORD (FRONTRUNNER)                                                    
*                                                                               
         SPACE 1                                                                
H80      DC   AL1(H80X-H80)           HEADER LENGTH                             
         DC   XL2'0080'               HEADER CODE                               
         DC   AL2(H80XX-H80)          DISP TO NEXT HEADER                       
H80X     EQU  *                                                                 
         DC    X'00'                                                            
H80XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  CLIENT/UNIVERSE REQUEST                                                      
*                                                                               
*  NOTE: IF MAPCODE 9 EXISTS, IT MUST COME BEFORE MAPCODE 4                     
*                                                                               
         SPACE 1                                                                
H82      DC   AL1(H82X-H82)           HEADER LENGTH                             
         DC   XL2'0082'               HEADER CODE                               
         DC   AL2(H82XX-H82)          DISP TO NEXT HEADER                       
H82X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'82CLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCLIENT)                                                    
         DC    AL1(14),AL2(02),CL5'82UNI',AL1(MDTPKQ),AL1(3)                    
         DC    AL4(INUNIV)                                                      
         DC    AL1(14),AL2(03),CL5'82UDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INUNIVDT)                                                    
         DC    AL1(14),AL2(09),CL5'82NTI',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INNTISW)                                                     
         DC    AL1(14),AL2(04),CL5'82DEM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INDEMOS)                                                     
         DC    AL1(14),AL2(05),CL5'82USW',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INUNIVSW)                                                    
         DC    AL1(14),AL2(06),CL5'82N86',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INNO86EL)                                                    
         DC    AL1(14),AL2(07),CL5'82JPR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INJSTPRF)                                                    
         DC    AL1(14),AL2(08),CL5'82FRS',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRNTSW)                                                    
         DC    X'00'                                                            
H82XX    EQU   *                                                                
         SPACE 1                                                                
*  UNIVERSE DEMO INFORMATION                                                    
*                                                                               
H83      DC   AL1(H83X-H83)           HEADER LENGTH                             
         DC   XL2'0083'               HEADER CODE                               
         DC   AL2(H83XX-H83)          DISP TO NEXT HEADER                       
H83X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'UNIVN',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(02),CL5'UNADN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(03),CL5'UNADC',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'UNADV',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(05),CL5'UNDEM',AL1(MDTBIQ),AL1(4)                    
*                                                                               
* CUNIVERSE NO LONGER PART OF COMSCORE PROJECT - SCHT 8/17                      
*                                                                               
*        DC    AL1(10),AL2(06),CL5'UNCAT',AL1(MDTCHQ),AL1(8)                    
*        DC    AL1(10),AL2(07),CL5'UNDEM',AL1(MDTBIQ),AL1(4)                    
         DC    X'00'                                                            
H83XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  PROGRAM LOOKUP                                                               
*                                                                               
H84      DC   AL1(H84X-H84)           HEADER LENGTH                             
         DC   XL2'0084'               HEADER CODE                               
         DC   AL2(H84XX-H84)          DISP TO NEXT HEADER                       
H84X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'84NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INNETPRG)                                                    
         DC    AL1(14),AL2(02),CL5'84PCD',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INFRPRCD)                                                    
         DC    AL1(14),AL2(03),CL5'84SDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPRSDAT)                                                    
         DC    AL1(14),AL2(04),CL5'84EDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPREDAT)                                                    
         DC    AL1(14),AL2(05),CL5'84UNI',AL1(MDTPKQ),AL1(3)                    
         DC    AL4(INUNIV)                                                      
         DC    AL1(14),AL2(06),CL5'84UDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INUNIVDT)                                                    
         DC    AL1(14),AL2(07),CL5'84DEM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INDEMOS)                                                     
         DC    AL1(14),AL2(08),CL5'84HYR',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INHUTYR)                                                     
         DC    AL1(14),AL2(09),CL5'84HNO',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INHUTNO)                                                     
         DC    AL1(14),AL2(10),CL5'84HSC',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INHUTSC)                                                     
         DC    AL1(14),AL2(11),CL5'84HAV',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INHUTAV)                                                     
         DC    AL1(14),AL2(12),CL5'84HPO',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INHUTPO)                                                     
         DC    AL1(14),AL2(13),CL5'84HPE',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INHUTPE)                                                     
         DC    AL1(14),AL2(14),CL5'84HFL',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INHUTFL)                                                     
         DC    AL1(14),AL2(15),CL5'84TVQ',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INTVQBK)                                                     
         DC    AL1(14),AL2(16),CL5'84CLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCLIENT)                                                    
         DC    AL1(14),AL2(17),CL5'84VSW',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INVPHSW)                                                     
         DC    AL1(14),AL2(18),CL5'84HSW',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INHUTSW)                                                     
         DC    X'00'                                                            
H84XX    EQU   *                                                                
         SPACE 1                                                                
*  PROGRAM RECORD KEY INFORMATION                                               
*                                                                               
H85      DC   AL1(H85X-H85)           HEADER LENGTH                             
         DC   XL2'0085'               HEADER CODE                               
         DC   AL2(H85XX-H85)          DISP TO NEXT HEADER                       
H85X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'VPPCD',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(02),CL5'VPDAT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(03),CL5'VPROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'VPDAY',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(05),CL5'VPTIM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(06),CL5'VPNAM',AL1(MDTCHQ),AL1(16)                   
         DC    X'00'                                                            
H85XX    EQU   *                                                                
         SPACE 1                                                                
*  PROGRAM RECORD DATA INFORMATION                                              
*                                                                               
H86      DC   AL1(H86X-H86)           HEADER LENGTH                             
         DC   XL2'0086'               HEADER CODE                               
         DC   AL2(H86XX-H86)          DISP TO NEXT HEADER                       
H86X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'VPSDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(02),CL5'VPEDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(03),CL5'VPNTI',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(04),CL5'VPNEW',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(05),CL5'VPTYP',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(06),CL5'VPCNT',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(07),CL5'VPTER',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(08),CL5'VPRTG',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(09),CL5'VPSHR',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(10),CL5'VPDEM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(11),CL5'VPPRG',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(12),CL5'VPNDN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(13),CL5'VPNDC',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(14),CL5'VPNDV',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(15),CL5'VPDPT',AL1(MDTCHQ),AL1(2)                    
         DC    AL1(10),AL2(16),CL5'VPDPR',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(17),CL5'VPCDC',AL1(MDTCHQ),AL1(8)                    
         DC    AL1(10),AL2(18),CL5'VPCDA',AL1(MDTBIQ),AL1(4)                    
         DC    X'00'                                                            
H86XX    EQU   *                                                                
*  PROGRAM HUT REPLY                                                            
*                                                                               
H87      DC   AL1(H87X-H87)           HEADER LENGTH                             
         DC   XL2'0087'               HEADER CODE                               
         DC   AL2(H87XX-H87)          DISP TO NEXT HEADER                       
H87X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'87PCD',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(02),CL5'87DAT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(03),CL5'87HUT',AL1(MDTBIQ),AL1(2)                    
         DC    X'00'                                                            
H87XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  QUARTERLY INFO                                                               
*                                                                               
*H90      DC   AL1(H90X-H90)           HEADER LENGTH                            
*         DC   XL2'0091'               HEADER CODE                              
*         DC   AL2(H90XX-H90)          DISP TO NEXT HEADER                      
*H90X     EQU  *                                                                
*         DC    AL1(14),AL2(01),CL5'91PGU',AL1(MDTBIQ),AL1(4)                   
*         DC    AL4(INPKGGUA)                                                   
*         DC    AL1(14),AL2(02),CL5'91DGU',AL1(MDTBIQ),AL1(4)                   
*         DC    AL4(INDEMGUA)                                                   
*         DC    X'00'                                                           
*H90XX    EQU   *                                                               
*                                                                               
*  PACKAGE/UNIT KEY INFO                                                        
*                                                                               
H91      DC   AL1(H91X-H91)           HEADER LENGTH                             
         DC   XL2'0091'               HEADER CODE                               
         DC   AL2(H91XX-H91)          DISP TO NEXT HEADER                       
H91X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'91KEY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYFR)                                                     
         DC    AL1(14),AL2(02),CL5'91SPL',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INFRUSPL)                                                    
         DC    AL1(14),AL2(03),CL5'91EPL',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INFRUEPL)                                                    
         DC    AL1(14),AL2(04),CL5'95FST',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INFRUFST)                                                    
         DC    AL1(14),AL2(05),CL5'95PRI',AL1(MDTCHQ),AL1(20)                   
         DC    AL4(INPRSMID)                                                    
         DC    X'00'                                                            
H91XX    EQU   *                                                                
*                                                                               
*  PACKAGE/PLAN SUPPORT INFO                                                    
*                                                                               
H92      DC   AL1(H92X-H92)           HEADER LENGTH                             
         DC   XL2'0092'               HEADER CODE                               
         DC   AL2(H92XX-H92)          DISP TO NEXT HEADER                       
H92X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'92PTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRUPER)                                                    
         DC    AL1(14),AL2(02),CL5'92GUA',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INFRUGUA)                                                    
         DC    AL1(14),AL2(03),CL5'92GND',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INFRUDND)                                                    
         DC    AL1(14),AL2(04),CL5'92GTY',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRUGTY)                                                    
         DC    AL1(14),AL2(05),CL5'92GCT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INFRUGCT)                                                    
         DC    AL1(14),AL2(06),CL5'92DGU',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INFRUDGU)                                                    
         DC    X'00'                                                            
H92XX    EQU   *                                                                
*                                                                               
*  PACKAGE ADD                                                                  
*                                                                               
H93      DC   AL1(H93X-H93)           HEADER LENGTH                             
         DC   XL2'0093'               HEADER CODE                               
         DC   AL2(H93XX-H93)          DISP TO NEXT HEADER                       
H93X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'93NM1',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPKGNM1)                                                    
         DC    AL1(14),AL2(02),CL5'93NM2',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPKGNM2)                                                    
         DC    AL1(14),AL2(03),CL5'93DTP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INPKGDTP)                                                    
         DC    AL1(14),AL2(04),CL5'93BTP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INPKGBTP)                                                    
         DC    AL1(14),AL2(05),CL5'93INT',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INPKGINT)                                                    
         DC    AL1(14),AL2(06),CL5'93ACT',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INPKGCST)                                                    
         DC    AL1(14),AL2(07),CL5'93PRD',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPKGPRD)                                                    
         DC    AL1(14),AL2(08),CL5'93UNI',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPKGUNV)                                                    
         DC    AL1(14),AL2(09),CL5'93HAV',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INPKGHAV)                                                    
         DC    AL1(14),AL2(10),CL5'93HSC',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INPKGHSC)                                                    
         DC    AL1(14),AL2(11),CL5'93HFL',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INPKGFLV)                                                    
         DC    AL1(14),AL2(12),CL5'93HPO',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INPKGHPC)                                                    
         DC    AL1(14),AL2(13),CL5'93ACD',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPKGACD)                                                    
         DC    AL1(14),AL2(14),CL5'93ANM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPKGANM)                                                    
         DC    AL1(14),AL2(16),CL5'93SRP',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPKGSRP)                                                    
         DC    AL1(14),AL2(17),CL5'93FLT',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INPKGFLT)                                                    
         DC    AL1(14),AL2(18),CL5'93VTP',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPKGVTP)                                                    
******                                                                          
         DC    AL1(14),AL2(19),CL5'93STA',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPKGSTA)                                                    
         DC    AL1(14),AL2(20),CL5'93OPT',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INPKGOPT)                                                    
         DC    AL1(14),AL2(21),CL5'93FDP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INPKGFDP)                                                    
         DC    AL1(14),AL2(22),CL5'93LEN',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPKGLEN)                                                    
         DC    AL1(14),AL2(23),CL5'93IMP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INPKGIMP)                                                    
         DC    AL1(14),AL2(24),CL5'93CPM',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INPKGCPM)                                                    
         DC    AL1(14),AL2(25),CL5'93UNP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INPKGUNP)                                                    
*                                                                               
* CUNIVERSE NO LONGER PART OF COMSCORE PROJECT - SCHT 8/17                      
*                                                                               
*        DC    AL1(14),AL2(28),CL5'93CUN',AL1(MDTCHQ),AL1(4)                    
*        DC    AL4(INPKGCUN)                                                    
         DC    AL1(14),AL2(29),CL5'93CVT',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INPKGCVT)                                                    
         DC    AL1(14),AL2(30),CL5'93GDM',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(INPKGDEM)                                                    
*-- DAYPART MUST BE THE LAST ELEMENT                                            
         DC    AL1(14),AL2(15),CL5'93DPA',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INBPKDPT)                                                    
         DC    AL1(14),AL2(26),CL5'93DPC',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INCPKDPT)                                                    
         DC    AL1(14),AL2(27),CL5'93DPD',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INDPKDPT)                                                    
******   DC    AL1(14),AL2(59),CL5'93KEY',AL1(MDTCHQ),AL1(0)                    
******   DC    AL4(INKEYPK)                                                     
         DC    X'00'                                                            
H93XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  PACKAGE ERROR/ADD REPLY                                                      
*                                                                               
H94      DC   AL1(H94X-H94)           HEADER LENGTH                             
         DC   XL2'0094'               HEADER CODE                               
         DC   AL2(H94XX-H94)          DISP TO NEXT HEADER                       
H94X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'94SEQ',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'94NMB',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(03),CL5'94EST',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'94CLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(05),CL5'94NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(06),CL5'94PKG',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(07),CL5'94PRG',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(08),CL5'94LEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(09),CL5'94DAT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(57),CL5'94ENM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(58),CL5'94EMS',AL1(MDTCHQ),AL1(64)                   
         DC    AL1(10),AL2(59),CL5'94EXT',AL1(MDTCHQ),AL1(54)                   
         DC    X'00'                                                            
H94XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  UNIT ADD (FRONTRUNNER)                                                       
*                                                                               
H95      DC   AL1(H95X-H95)           HEADER LENGTH                             
         DC   XL2'0095'               HEADER CODE                               
         DC   AL2(H95XX-H95)          DISP TO NEXT HEADER                       
H95X     EQU  *                                                                 
         DC    AL1(14),AL2(01),CL5'95SDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INFRUSDT)                                                    
         DC    AL1(14),AL2(02),CL5'95EDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INFRUEDT)                                                    
******   DC    AL1(14),AL2(03),CL5'95QTR',AL1(MDTBIQ),AL1(1)                    
******   DC    AL4(INFRUQTR)                                                    
******   DC    AL1(14),AL2(04),CL5'95MTH',AL1(MDTBIQ),AL1(1)                    
******   DC    AL4(INFRUMTH)                                                    
******   DC    AL1(14),AL2(05),CL5'95YER',AL1(MDTBIQ),AL1(1)                    
******   DC    AL4(INFRUYER)                                                    
         DC    AL1(14),AL2(06),CL5'95LEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INFRULEN)                                                    
         DC    AL1(14),AL2(07),CL5'95NUM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INFRUNUM)                                                    
         DC    AL1(14),AL2(08),CL5'95CST',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INFRUCST)                                                    
         DC    AL1(14),AL2(09),CL5'95VPH',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INFRUVPH)                                                    
         DC    AL1(14),AL2(10),CL5'95HUT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INFRUHUT)                                                    
         DC    AL1(14),AL2(11),CL5'95HOV',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRUHOV)                                                    
         DC    AL1(14),AL2(12),CL5'95SHR',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INFRUSHR)                                                    
         DC    AL1(14),AL2(13),CL5'95SOV',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRUSOV)                                                    
         DC    AL1(14),AL2(14),CL5'95RAT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INFRURAT)                                                    
         DC    AL1(14),AL2(15),CL5'95ROV',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRUROV)                                                    
         DC    AL1(14),AL2(16),CL5'95NAD',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INFRUNAD)                                                    
         DC    AL1(14),AL2(17),CL5'95DCT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INFRUDCT)                                                    
         DC    AL1(14),AL2(18),CL5'95DEM',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INFRUDEM)                                                    
         DC    AL1(14),AL2(19),CL5'95SDP',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INFRUSDP)                                                    
         DC    AL1(14),AL2(20),CL5'95PTY',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INFRUPTY)                                                    
         DC    AL1(14),AL2(21),CL5'95NEW',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRUNEW)                                                    
         DC    AL1(14),AL2(22),CL5'95TIR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INFRUTIR)                                                    
         DC    AL1(14),AL2(23),CL5'95CNT',AL1(MDTCHQ),AL1(2)                    
         DC    AL4(INFRUCNT)                                                    
         DC    AL1(14),AL2(24),CL5'95TVQ',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INFRUTVQ)                                                    
*******  DC    AL1(14),AL2(25),CL5'95FST',AL1(MDTCHQ),AL1(8)                    
*******  DC    AL4(INFRUFST)                                                    
         DC    AL1(14),AL2(27),CL5'95ROT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INFRROT)                                                     
*                                                                               
**  THESE MAPS MUST BE SENT LAST BY THE PC                                      
*                                                                               
         DC    AL1(14),AL2(25),CL5'95PCD',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INFRUPCD)                                                    
         DC    AL1(14),AL2(26),CL5'95PCD',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INFRUPCD)                                                    
         DC    X'00'                                                            
H95XX    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*  UNIT ERROR REPLY                                                             
*                                                                               
H96      DC   AL1(H96X-H96)           HEADER LENGTH                             
         DC   XL2'0096'               HEADER CODE                               
         DC   AL2(H96XX-H96)          DISP TO NEXT HEADER                       
H96X     EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'94SEQ',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'94NMB',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(03),CL5'94EST',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'94CLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(05),CL5'94NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(06),CL5'94PKG',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(07),CL5'94PRG',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(08),CL5'94LEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(09),CL5'94DAT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(57),CL5'94ENM',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(58),CL5'94EMS',AL1(MDTCHQ),AL1(64)                   
         DC    AL1(10),AL2(59),CL5'94EXT',AL1(MDTCHQ),AL1(54)                   
         DC    X'00'                                                            
H96XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* FD - VERSION DATA                                                             
*====================================================================*          
         SPACE 1                                                                
HFD      DC   AL1(HFDX-HFD)           HEADER LENGTH                             
         DC   XL2'00FD'               HEADER CODE                               
         DC   AL2(HFDXX-HFD)          DISP TO NEXT HEADER                       
HFDX     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'VERSN',AL1(MDTHXQ),AL1(4)                    
         DC    AL4(INVERSN)                                                     
         DC    X'00'                                                            
HFDXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* FE - VERSION DATA                                                             
*====================================================================*          
         SPACE 1                                                                
HFE      DC   AL1(HFEX-HFE)           HEADER LENGTH                             
         DC   XL2'00FE'               HEADER CODE                               
         DC   AL2(HFEXX-HFE)          DISP TO NEXT HEADER                       
HFEX     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'VERSN',AL1(MDTHXQ),AL1(4)                    
         DC    AL4(INVERSN)                                                     
         DC    X'00'                                                            
HFEXX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* MASTER/STATION INFO REQUEST                                         *         
***********************************************************************         
                                                                                
H100     DC    AL1(H100X-H100)     HEADER LENGTH                                
         DC    XL2'0100'           HEADER CODE                                  
         DC    AL2(H100XX+1-H100)  DISP TO NEXT HEADER                          
H100X    DS    0X                                                               
                                                                                
         DC    AL1(14),AL2(01),CL5'01NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPINET)                                                     
H100XX   DC    X'00'                                                            
*                                                                               
*  MASTER/STATION INFO REPLY                                                    
*                                                                               
H101     DC   AL1(H101X-H101)         HEADER LENGTH                             
         DC   XL2'0101'               HEADER CODE                               
         DC   AL2(H101XX-H101)        DISP TO NEXT HEADER                       
H101X    EQU  *                                                                 
         DC    AL1(10),AL2(01),CL5'01NET',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(02),CL5'02NTI',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(03),CL5'03MED',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(04),CL5'04POS',AL1(MDTCHQ),AL1(1)                    
         DC    X'00'                                                            
H101XX   EQU   *                                                                
                                                                                
***********************************************************************         
* CABLE MATCHMAKER - CHANGE UNITS REQUEST                             *         
***********************************************************************         
                                                                                
H134     DC    AL1(H134X-H134)     HEADER LENGTH                                
         DC    XL2'0134'           HEADER CODE                                  
         DC    AL2(H134XX+1-H134)  DISP TO NEXT HEADER                          
H134X    DS    0X                                                               
                                                                                
         DC    AL1(14),AL2(01),CL5'NCOST',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNACT)                                                    
         DC    AL1(14),AL2(02),CL5'OCOST',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYOACT)                                                    
         DC    AL1(14),AL2(03),CL5'NASGN',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNASS)                                                    
         DC    AL1(14),AL2(04),CL5'OASGN',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYOASS)                                                    
         DC    AL1(14),AL2(05),CL5'NINTG',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNINT)                                                    
         DC    AL1(14),AL2(06),CL5'OINTG',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYOINT)                                                    
         DC    AL1(14),AL2(07),CL5'NDAYS',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNDAY)                                                    
         DC    AL1(14),AL2(08),CL5'ODAYS',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYODAY)                                                    
         DC    AL1(14),AL2(09),CL5'NROTN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNROT)                                                    
         DC    AL1(14),AL2(10),CL5'OROTN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYOROT)                                                    
         DC    AL1(14),AL2(11),CL5'NSTIM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSTM)                                                    
         DC    AL1(14),AL2(12),CL5'OSTIM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOSTM)                                                    
         DC    AL1(14),AL2(13),CL5'NETIM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNETM)                                                    
         DC    AL1(14),AL2(14),CL5'OETIM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOETM)                                                    
         DC    AL1(14),AL2(15),CL5'NSLEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNLEN)                                                    
         DC    AL1(14),AL2(16),CL5'OSLEN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYOLEN)                                                    
         DC    AL1(14),AL2(17),CL5'NPRDC',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYNPRD)                                                    
         DC    AL1(14),AL2(18),CL5'OPRDC',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYOPRD)                                                    
         DC    AL1(14),AL2(19),CL5'NNTI ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNNTI)                                                    
         DC    AL1(14),AL2(20),CL5'ONTI ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYONTI)                                                    
         DC    AL1(14),AL2(21),CL5'PRGNM',AL1(MDTCHQ),AL1(16)                   
         DC    AL4(INBYNPNM)                                                    
         DC    AL1(14),AL2(22),CL5'MAKGD',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNMGD)                                                    
         DC    AL1(14),AL2(23),CL5'PEMPT',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNPRE)                                                    
         DC    AL1(14),AL2(24),CL5'BTYPE',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBYT)                                                    
         DC    AL1(14),AL2(25),CL5'ADU  ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNADU)                                                    
         DC    AL1(14),AL2(26),CL5'BILBD',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBLB)                                                    
         DC    AL1(14),AL2(27),CL5'COMM1',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM1)                                                    
         DC    AL1(14),AL2(28),CL5'COMM2',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM2)                                                    
         DC    AL1(14),AL2(29),CL5'DDISP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYDDSE)                                                    
         DC    AL1(14),AL2(30),CL5'DRTNG',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDRTG)                                                    
         DC    AL1(14),AL2(31),CL5'DVPH ',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDVPH)                                                    
         DC    AL1(14),AL2(32),CL5'DIMP ',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDIMP)                                                    
         DC    AL1(14),AL2(33),CL5'NHUT ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHUT)                                                    
         DC    AL1(14),AL2(34),CL5'OHUT ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOHUT)                                                    
         DC    AL1(14),AL2(35),CL5'NSHR ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSHR)                                                    
         DC    AL1(14),AL2(36),CL5'OSHR ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOSHR)                                                    
         DC    AL1(14),AL2(37),CL5'NHRTG',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHRT)                                                    
         DC    AL1(14),AL2(38),CL5'OHRTG',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOHRT)                                                    
         DC    AL1(14),AL2(39),CL5'HIMP ',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNHIM)                                                    
         DC    AL1(14),AL2(40),CL5'NAFFT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNATM)                                                    
         DC    AL1(14),AL2(41),CL5'OAFFT',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOATM)                                                    
         DC    AL1(14),AL2(42),CL5'DPRT ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYSDPT)                                                    
         DC    AL1(14),AL2(44),CL5'NSREP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNREP)                                                    
         DC    AL1(14),AL2(45),CL5'OSREP',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYOREP)                                                    
         DC    AL1(14),AL2(46),CL5'NDATE',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INBYNDAT)                                                    
         DC    AL1(14),AL2(47),CL5'COMBO',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INBYCMBO)                                                    
         DC    AL1(14),AL2(48),CL5'INVCE',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYINVC)                                                    
         DC    AL1(14),AL2(49),CL5'INVTP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYINTP)                                                    
         DC    AL1(14),AL2(57),CL5'MAKEY',AL1(MDTHXQ),AL1(L'RUPMAKEY)           
         DC    AL4(INBYAFM)                                                     
         DC    AL1(14),AL2(58),CL5'UAKEY',AL1(MDTHXQ),AL1(L'RUPMAKEY)           
         DC    AL4(INBYAFU)                                                     
         DC    AL1(14),AL2(60),CL5'MIROR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYMIR)                                                     
         DC    AL1(14),AL2(59),CL5'KEY  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYCH)                                                     
         DC    AL1(14),AL2(61),CL5'IGFLM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGF)                                                     
         DC    AL1(14),AL2(62),CL5'IGCST',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGC)                                                     
         DC    AL1(14),AL2(63),CL5'IGLEN',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGL)                                                     
         DC    AL1(14),AL2(64),CL5'IGTIM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGT)                                                     
         DC    AL1(14),AL2(65),CL5'IGICO',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGIC)                                                    
         DC    AL1(14),AL2(66),CL5'IGSEP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGS)                                                     
         DC    AL1(14),AL2(67),CL5'HDCUT',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYHCC)                                                     
H134XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* CABLE MATCHMAKER - DELETE UNIT REQUEST                              *         
***********************************************************************         
                                                                                
H135     DC    AL1(H135X-H135)     HEADER LENGTH                                
         DC    XL2'0135'           HEADER CODE                                  
         DC    AL2(H135XX+1-H135)  DISP TO NEXT HEADER                          
H135X    DS    0X                                                               
                                                                                
         DC    AL1(14),AL2(58),CL5'UAKEY',AL1(MDTHXQ),AL1(L'RUPMAKEY)           
         DC    AL4(INBYAFU)                                                     
         DC    AL1(14),AL2(59),CL5'34KEY',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYDE)                                                     
H135XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* CABLE MATCHMAKER - ADD UNITS REQUEST                                *         
***********************************************************************         
                                                                                
H136     DC    AL1(H136X-H136)     HEADER LENGTH                                
         DC    XL2'0136'           HEADER CODE                                  
         DC    AL2(H136XX+1-H136)  DISP TO NEXT HEADER                          
H136X    DS    0X                                                               
                                                                                
         DC    AL1(14),AL2(01),CL5'COST ',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNACT)                                                    
         DC    AL1(14),AL2(03),CL5'ACOST',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNASS)                                                    
         DC    AL1(14),AL2(05),CL5'ICOST',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNINT)                                                    
         DC    AL1(14),AL2(07),CL5'DAY  ',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNDAY)                                                    
         DC    AL1(14),AL2(09),CL5'ROTN ',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNROT)                                                    
         DC    AL1(14),AL2(11),CL5'STIME',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSTM)                                                    
         DC    AL1(14),AL2(13),CL5'ETIME',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNETM)                                                    
         DC    AL1(14),AL2(15),CL5'SENLN',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYNLEN)                                                    
         DC    AL1(14),AL2(17),CL5'PRDCD',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INBYNPRD)                                                    
         DC    AL1(14),AL2(19),CL5'NTICD',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNNTI)                                                    
         DC    AL1(14),AL2(21),CL5'PRGNM',AL1(MDTCHQ),AL1(16)                   
         DC    AL4(INBYNPNM)                                                    
         DC    AL1(14),AL2(22),CL5'MAKGD',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNMGD)                                                    
         DC    AL1(14),AL2(23),CL5'PEMPT',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNPRE)                                                    
         DC    AL1(14),AL2(24),CL5'BTYPE',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBYT)                                                    
         DC    AL1(14),AL2(25),CL5'ADU  ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNADU)                                                    
         DC    AL1(14),AL2(26),CL5'BILBD',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYNBLB)                                                    
         DC    AL1(14),AL2(27),CL5'COMM1',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM1)                                                    
         DC    AL1(14),AL2(28),CL5'COMM2',AL1(MDTCHQ),AL1(60)                   
         DC    AL4(INBYNCM2)                                                    
         DC    AL1(14),AL2(29),CL5'DDISP',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYDDSE)                                                    
         DC    AL1(14),AL2(30),CL5'DRTG ',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDRTG)                                                    
         DC    AL1(14),AL2(31),CL5'DVPH ',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDVPH)                                                    
         DC    AL1(14),AL2(32),CL5'DIMP ',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYDIMP)                                                    
         DC    AL1(14),AL2(33),CL5'HUT  ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHUT)                                                    
         DC    AL1(14),AL2(35),CL5'SHR  ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNSHR)                                                    
         DC    AL1(14),AL2(37),CL5'HRTG ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNHRT)                                                    
         DC    AL1(14),AL2(39),CL5'HIMP ',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INBYNHIM)                                                    
         DC    AL1(14),AL2(40),CL5'AFFTM',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNATM)                                                    
         DC    AL1(14),AL2(42),CL5'DAYPT',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INBYSDPT)                                                    
         DC    AL1(14),AL2(43),CL5'MAKGD',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYSMKG)                                                    
         DC    AL1(14),AL2(44),CL5'REPCD',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBYNREP)                                                    
         DC    AL1(14),AL2(45),CL5'FROZ',AL1(MDTCHQ),AL1(1)                     
         DC    AL4(INBYFRZ)                                                     
         DC    AL1(14),AL2(48),CL5'INVCE',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYINVC)                                                    
         DC    AL1(14),AL2(49),CL5'INVTP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYINTP)                                                    
         DC    AL1(14),AL2(57),CL5'MAKEY',AL1(MDTHXQ),AL1(L'RUPMAKEY)           
         DC    AL4(INBYAFM)                                                     
         DC    AL1(14),AL2(60),CL5'MIROR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYMIR)                                                     
         DC    AL1(14),AL2(59),CL5'KEY  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INKEYBU)                                                     
         DC    AL1(14),AL2(61),CL5'IGFLM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGF)                                                     
         DC    AL1(14),AL2(62),CL5'IGCST',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGC)                                                     
         DC    AL1(14),AL2(63),CL5'IGLEN',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGL)                                                     
         DC    AL1(14),AL2(64),CL5'IGTIM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGT)                                                     
         DC    AL1(14),AL2(65),CL5'IGICO',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGIC)                                                    
         DC    AL1(14),AL2(66),CL5'IGSEP',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYIGS)                                                     
         DC    AL1(14),AL2(67),CL5'HDCUT',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INBYHCC)                                                     
H136XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* CABLE MATCHMAKER - E-MAIL RNO UNITS REQUEST                         *         
***********************************************************************         
                                                                                
H137     DC    AL1(H137X-H137)     HEADER LENGTH                                
         DC    XL2'0137'           HEADER CODE                                  
         DC    AL2(H137XX+1-H137)  DISP TO NEXT HEADER                          
H137X    DS    0X                                                               
                                                                                
         DC    AL1(14),AL2(01),CL5'IDKEY',AL1(MDTHXQ),AL1(L'RUPMAKEY)           
         DC    AL4(INKEYID)                                                     
H137XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* CABLE MATCHMAKER - INVOICE MATCHING COMPLETE                        *         
***********************************************************************         
                                                                                
H138     DC    AL1(H138X-H138)     HEADER LENGTH                                
         DC    XL2'0138'           HEADER CODE                                  
         DC    AL2(H138XX+1-H138)  DISP TO NEXT HEADER                          
H138X    DS    0X                                                               
H138XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* STEWARD - BA RULES DOWNLOAD REQUEST                                           
***********************************************************************         
                                                                                
H142     DC    AL1(H142X-H142)     HEADER LENGTH                                
         DC    XL2'0142'           HEADER CODE                                  
         DC    AL2(H142XX+1-H142)  DISP TO NEXT HEADER                          
H142X    DS    0X                                                               
                                                                                
         DC    AL1(14),AL2(01),CL5'BAREQ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(DUMMY)                                                       
H142XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* STEWARD - BA RULES DOWNLOAD FIELDS                                            
***********************************************************************         
                                                                                
H141     DC    AL1(H141X-H141)     HEADER LENGTH                                
         DC    XL2'0141'           HEADER CODE                                  
         DC    AL2(H141XX+1-H141)  DISP TO NEXT HEADER                          
H141X    DS    0X                                                               
                                                                                
         DC    AL1(10),AL2(01),CL5'BRAD ',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(03),CL5'BRCLT',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(04),CL5'BRUAC',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(05),CL5'BRRSD',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(06),CL5'BR0UN',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(07),CL5'BRDWT',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(08),CL5'BRTBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(09),CL5'BRTBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(10),CL5'BRGBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(11),CL5'BRGBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(12),CL5'BRWBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(13),CL5'BRWBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(14),CL5'BRNSM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(15),CL5'BRCSM',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(16),CL5'BRMDS',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(17),CL5'BR1TW',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(18),CL5'B1TBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(19),CL5'B1TBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(20),CL5'B1GBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(21),CL5'B1GBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(22),CL5'B1WBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(23),CL5'B1WBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(24),CL5'BR2TW',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(25),CL5'B2TBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(26),CL5'B2TBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(27),CL5'B2GBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(28),CL5'B2GBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(29),CL5'B2WBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(30),CL5'B2WBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(31),CL5'BR3TW',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(32),CL5'B3TBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(33),CL5'B3TBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(34),CL5'B3GBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(35),CL5'B3GBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(36),CL5'B3WBL',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(37),CL5'B3WBU',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(38),CL5'BRNWT',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(39),CL5'BRPWT',AL1(MDTBIQ),AL1(4)                    
H141XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* STEWARD - LIMIT RECORD DOWNLOAD REQUEST                                       
***********************************************************************         
                                                                                
H144     DC    AL1(H144X-H144)     HEADER LENGTH                                
         DC    XL2'0144'           HEADER CODE                                  
         DC    AL2(H144XX+1-H144)  DISP TO NEXT HEADER                          
H144X    DS    0X                                                               
                                                                                
         DC    AL1(14),AL2(01),CL5'LRCLI',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCLIENT)                                                    
         DC    AL1(14),AL2(02),CL5'LRCLR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(CLRLIMIT)                                                    
         DC    AL1(14),AL2(03),CL5'LRPRD',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPROD1)                                                     
         DC    AL1(14),AL2(04),CL5'7AEST',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INEST)                                                       
H144XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* STEWARD - LIMIT RECORD DOWNLOAD FIELDS                                        
***********************************************************************         
                                                                                
H143     DC    AL1(H143X-H143)     HEADER LENGTH                                
         DC    XL2'0143'           HEADER CODE                                  
         DC    AL2(H143XX+1-H143)  DISP TO NEXT HEADER                          
H143X    DS    0X                                                               
                                                                                
         DC    AL1(10),AL2(01),CL5'LMAD ',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'LMSTA',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(03),CL5'LMPRD',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(04),CL5'LMEST',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(05),CL5'LMI/E',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(06),CL5'LMLST',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(07),CL5'LMNET',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(08),CL5'LMDAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(09),CL5'LMTIM',AL1(MDTCHQ),AL1(11)                   
         DC    AL1(10),AL2(10),CL5'LMPRG',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(10),AL2(11),CL5'LMSDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(12),CL5'LMEDT',AL1(MDTCDQ),AL1(2)                    
H143XX   DC    X'00'                                                            
***********************************************************************         
* STEWARD - PROGRAM INFO REQUEST                                                
***********************************************************************         
                                                                                
H145     DC    AL1(H145X-H145)     HEADER LENGTH                                
         DC    XL2'0145'           HEADER CODE                                  
         DC    AL2(H145XX+1-H145)  DISP TO NEXT HEADER                          
H145X    DS    0X                                                               
                                                                                
         DC    AL1(14),AL2(01),CL5'PINET',AL1(MDTCHQ),AL1(4)                    
         DC    AL4(INPINET)                                                     
         DC    AL1(14),AL2(02),CL5'PIPRG',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INPRGCD)                                                     
         DC    AL1(14),AL2(03),CL5'PISTD',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPISDT)                                                     
         DC    AL1(14),AL2(04),CL5'PIETD',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INPIEDT)                                                     
H145XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* STEWARD - PROGRAM INFO REQUEST                                                
***********************************************************************         
                                                                                
H146     DC    AL1(H146X-H146)     HEADER LENGTH                                
         DC    XL2'0146'           HEADER CODE                                  
         DC    AL2(H146XX+1-H146)  DISP TO NEXT HEADER                          
H146X    DS    0X                                                               
                                                                                
         DC    AL1(10),AL2(01),CL5'PIMKT',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(02),CL5'PIPRG',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(03),CL5'PIEDT',AL1(MDTCHQ),AL1(8)                    
         DC    AL1(10),AL2(04),CL5'PISER',AL1(MDTCHQ),AL1(10)                   
*        DC    AL1(10),AL2(05),CL5'PIDEM',AL1(MDTCHQ),AL1(8)                    
*        DC    AL1(10),AL2(06),CL5'PIDVA',AL1(MDTBIQ),AL1(4)                    
H146XX   DC    X'00'                                                            
                                                                                
***********************************************************************         
* STEWARD - NETWORK/MARKET RECORDS                                              
***********************************************************************         
                                                                                
H147     DC    AL1(H147X-H147)     HEADER LENGTH                                
         DC    XL2'0147'           HEADER CODE                                  
         DC    AL2(H147XX+1-H147)  DISP TO NEXT HEADER                          
H147X    DS    0X                                                               
                                                                                
         DC    AL1(10),AL2(01),CL5'IDNET',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(02),CL5'IDMKT',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(03),CL5'IDCSS',AL1(MDTCHQ),AL1(10)                   
H147XX   DC    X'00'                                                            
                                                                                
         DC    X'00'               EOFT                                         
         EJECT                                                                  
* DSECT TO COVER I/O ROUTINE LOCAL STORAGE                                      
*                                                                               
IOWORKD  DSECT                                                                  
IOWORK   DS    CL96                FOR GETREC, PUTREC, AND ADDREC               
IOPARM   DS    6F                  PARAMETER LIST                               
IOWORK1  DS    X                                                                
IOWORK2  DS    X                                                                
IOWORK3  DS    X                                                                
IOCMND   DS    CL7                 DATA MANAGER COMMAND                         
IODIR    DS    CL7                 DIRECTORY NAME                               
IOFILE   DS    CL7                 FILE NAME                                    
IOVALS   DS    0XL3                                                             
IOEXCPT  DS    X                   EXCEPTION IOFLAG VALUES FOR FILE             
IOKEYL   DS    X                   KEY LENGTH                                   
IODADSP  DS    X                   DISPLACEMENT TO DISK ADDRESS                 
*                                                                               
*  EQUATES                                                                      
*                                                                               
USERMOD  EQU   33                                                               
IOWORKX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
******   PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLVSPTRF                                                     
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE NAVDSECTS                                                      
       ++INCLUDE NENAVMMD                                                       
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DEDEMEQUS2                                                     
       ++INCLUDE FAWSSVRD                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE GEGENTOK                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DEDEMOVALD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'146NENAV00   10/14/20'                                      
         END                                                                    
