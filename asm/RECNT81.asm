*          DATA SET RECNT81    AT LEVEL 016 AS OF 04/25/14                      
*PHASE T80281A                                                                  
*                                                                               
         TITLE 'RECNT81 - T80281 - REPPAK STX MQ'                               
*                                                                               
T80281   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 RECNT81X-RECNT81D,T80281,RR=RE,CLEAR=YES                         
*                                                                               
         BASR  R8,0                                                             
         AHI   R8,GLOBALS-*                                                     
         USING GLOBALS,R8          R8=A(Global literals)                        
*                                                                               
         LR    R9,RC                                                            
         USING RECNT81D,R9                                                      
*                                                                               
         ST    RE,RELO81                                                        
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         BRAS  RE,INIWKST          Init working storage                         
         JNE   EXXMODX             Not equal = no need to process MQ            
*                                                                               
         CLC   =C'ER/',CONMSG      Error message displayed?                     
         JE    EXXMODX                                                          
         CLC   =C'ER\',CONMSG      Error message displayed?                     
         JE    EXXMODX                                                          
         CLC   =C'ER#',CONMSG      Error message displayed?                     
         JE    EXXMODX                                                          
         CLC   =C'WR/',CONMSG      Warning message displayed?                   
         JE    EXXMODX                                                          
         CLC   =C'WR\',CONMSG      Warning message displayed?                   
         JE    EXXMODX                                                          
         CLC   =C'WR#',CONMSG      Warning message displayed?                   
         JE    EXXMODX                                                          
*                                                                               
         CLC   =C'CFX ',CONACT     CFX action?                                  
         JE    EXXMODX             No need to process MQ msg for CFX            
         CLC   =C'CF* ',CONACT     CF* action (takeover confirmation)?          
         JE    EXXMODX             No need to process MQ msg for CFX            
*                                                                               
         CLC   =C'LIN',TWAGENCP    StationToolKit?                              
         JNE   PRCMQ20                                                          
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'GETD',WKELEM02,GLCONLNQ,GLRKACT                     
         TM    DMCB+8,X'10'                                                     
         JNZ   PRCMQ20                                                          
         LA    RE,WKELEM02                                                      
         USING GLCONNUM,RE                                                      
         OC    GLCONERR,GLCONERR   Error occurred?                              
         JNZ   EXXMODX                                                          
         DROP  RE                                                               
*                                                                               
PRCMQ20  BRAS  RE,INIMQPA          Init MQ/contract/e-mail parameters           
*                                                                               
         CLI   WKMQMODE,MQMACTSQ   MQ triggered by CON actions?                 
         JE    PRCMQ30                                                          
*                                                                               
         CLI   WKMQMODE,MQMWIPSQ   MQ triggered by WIPS?                        
         JNE   *+12                                                             
         BRAS  RE,PRCWIPS          Process MQ message for WIPS                  
         J     EXXMODX                                                          
*                                                                               
         CLI   WKMQMODE,MQMTRF#Q   MQ triggered by traffic order# upd?          
         JNE   *+12                                                             
         BRAS  RE,PRCTRF#          Process MQ message for traffic ord#          
         J     EXXMODX                                                          
*                                                                               
         J     EXXMODX             No need to process MQ message                
*                                                                               
PRCMQ30  CLC   =C'SEND',CONACT     MQ for send?                                 
         JE    MQSEND                                                           
*                                                                               
         CLC   =C'CF',CONACT       MQ for confirmation?                         
         JE    *+14                                                             
         CLC   =C'CONX',CONACT     MQ for confirmation?                         
         JNE   PRCMQ32             CONX=CF (used to suppress worksheet)         
         CLC   MQMSGSTA(L'PARTCFTX),PARTCFTX                                    
         JE    MQPCF_              Process partial confirmation                 
         J     MQCF__              Process confirmation                         
*                                                                               
PRCMQ32  CLC   =C'EC',CONACT       MQ for EC?                                   
         JE    MQEC__                                                           
*                                                                               
         CLC   =C'MGS',CONACT      Makegood offer send?                         
         JNE   PRCMQ40                                                          
         MVI   WKEMAILM,EMLMGS_Q   E-mail triggered by MGS                      
         BRAS  RE,PRCEMAL          Process e-mail                               
*                                                                               
PRCMQ40  TM    TWACONFG,TW_MGOMQ   Process MQ mesg for MGO updated?             
         JNZ   MQMGOUPD                                                         
*                                                                               
         J     EXXMODX             No need to process MQ message                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MQSEND   BRAS  RE,PRCSEND          Process send                                 
         JNE   EXXMODX                                                          
         MVI   WKEMAILM,EMLSENDQ   E-mail triggered by SEND                     
         BRAS  RE,PRCEMAL          Process e-mail                               
         J     EXXMODX                                                          
*                                                                               
MQCF__   BRAS  RE,PRCCF__          Process confirmation                         
         MVI   WKEMAILM,EMLCONFQ   E-mail triggered by CONFIRM                  
         BRAS  RE,PRCEMAL          Process e-mail                               
         J     EXXMODX                                                          
*                                                                               
MQEC__   BRAS  RE,PRCEC__          Process EC                                   
         J     EXXMODX                                                          
*                                                                               
MQPCF_   BRAS  RE,PRCPCF_          Process partial confirmation                 
         MVI   WKEMAILM,EMLPCONQ   E-mail triggered by PARTIAL CONFIRM          
         BRAS  RE,PRCEMAL          Process e-mail                               
         J     EXXMODX                                                          
*                                                                               
MQMGOUPD BRAS  RE,PRCMGOU          Process MGO updated                          
         J     EXXMODX                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EXXMODX  XMOD1 1                                                                
*                                                                               
EXITY    CR    RB,RB               Equal                                        
         J     *+6                                                              
EXITN    LTR   RB,RB               Not equal                                    
EXITX    XIT1                                                                   
*                                                                               
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC is equal                                  
         CLI   0(R5),0                                                          
         JNE   NXTEL                                                            
         LTR   R5,R5               CC is not equal                              
NXTELX   BR    RE                                                               
*                                                                               
STARTTAG MVI   0(R2),OPENBRKQ      Build start tag <....>                       
         AHI   R2,1                                                             
         BCTR  R3,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R2),0(R4)                                                    
         EX    R3,0(RF)                                                         
         AHI   R3,1                                                             
         AR    R2,R3                                                            
         MVI   0(R2),CLOSBRKQ                                                   
         AHI   R2,1                                                             
         BR    RE                                                               
*                                                                               
END__TAG BCTR  R2,0                Check trailing spaces/nulls                  
         CLI   0(R2),0             Null?                                        
         JE    END__TAG                                                         
         CLI   0(R2),C' '          Space?                                       
         JE    END__TAG                                                         
         AHI   R2,1                Bump to build end tag                        
         MVI   0(R2),OPENBRKQ      Build end tag </...>                         
         MVI   1(R2),BACKSLAQ                                                   
         AHI   R2,1+1                                                           
         BCTR  R3,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R2),0(R4)                                                    
         EX    R3,0(RF)                                                         
         AHI   R3,1                                                             
         AR    R2,R3                                                            
         MVI   0(R2),CLOSBRKQ                                                   
         AHI   R2,1                                                             
         BR    RE                                                               
*                                                                               
TRALNULL BCTR  RF,0                Check trailing spaces/nulls                  
         CLI   0(RF),0             Null?                                        
         JE    TRALNULL                                                         
         CLI   0(RF),C' '          Space?                                       
         JE    TRALNULL                                                         
         AHI   RF,1                Bump to available char                       
         BR    RE                                                               
*                                                                               
CKSPCHAR CLI   0(R1),AMP____Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XML_AMPQ,R2),XML_AMPQ                                        
         AHI   R2,L'XML_AMPQ                                                    
         J     CKSPCH60                                                         
         CLI   0(R1),LT_____Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XML_LT_Q,R2),XML_LT_Q                                        
         AHI   R2,L'XML_LT_Q                                                    
         J     CKSPCH60                                                         
         CLI   0(R1),GT_____Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XML_GT_Q,R2),XML_GT_Q                                        
         AHI   R2,L'XML_GT_Q                                                    
         J     CKSPCH60                                                         
         CLI   0(R1),QUOT___Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XMLQUOTQ,R2),XMLQUOTQ                                        
         AHI   R2,L'XMLQUOTQ                                                    
         J     CKSPCH60                                                         
         CLI   0(R1),APOS___Q                                                   
         JNE   *+18                                                             
         MVC   0(L'XMLAPOSQ,R2),XMLAPOSQ                                        
         AHI   R2,L'XMLAPOSQ                                                    
         J     CKSPCH60                                                         
         MVC   0(1,R2),0(R1)       Save input field one char at a time          
         AHI   R2,1                Bump to next char in MQ message              
CKSPCH60 AHI   R1,1                Bump to next char in input field             
         JCT   R0,CKSPCHAR         Loop for all chars in input field            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                  ** Global literlas **                        
*                                                                               
NOQ      EQU   C'N'                                                             
YESQ     EQU   C'Y'                                                             
EOR      EQU   0                   End of record element code                   
FRSTELEM EQU   RREPELEM-RREPREC    Displacement to first Rep rec elem           
*                                                                               
OPENBRKQ EQU   C'<'                                                             
CLOSBRKQ EQU   C'>'                                                             
BACKSLAQ EQU   C'/'                                                             
*                                                                               
AMP____Q EQU   C'&&'               Ampersand                                    
LT_____Q EQU   C'<'                Less than                                    
GT_____Q EQU   C'>'                Greater than                                 
QUOT___Q EQU   C'"'                Quotation mark                               
APOS___Q EQU   C''''               Apostrophe                                   
*                                                                               
XML_AMPQ DC    C'&&amp;'           XML equivalent of &                          
XML_LT_Q DC    C'&&lt;'            XML equivalent of <                          
XML_GT_Q DC    C'&&gt;'            XML equivalent of >                          
XMLQUOTQ DC    C'&&quot;'          XML equivalent of "                          
XMLAPOSQ DC    C'&&apos;'          XML equivalent of '                          
*                                                                               
CRLF     DC    X'0D25'             CRLF                                         
QULABEL  DC    CL16'REPCON2STX******'                                           
SPACE150 DC    150C' '                                                          
*                                                                               
PARTCFTX DC    C'PARTIALLY CONFIRMED'                                           
CONFIRTX DC    C'CONFIRMED'                                                     
RETURNTX DC    C'RETURNED'                                                      
REVISDTX DC    C'REVISED'                                                       
NEW___TX DC    C'NEW'                                                           
*                                                                               
OFFERDTX DC    C'OFFERED'                                                       
*                                                                               
REPORDSN DC    C'rep-order-send'                                                
STAORDSN DC    C'sta-order-return'                                              
REPORDCF DC    C'rep-order-confirm'                                             
STAORDCF DC    C'sta-order-confirm'                                             
REPORDEC DC    C'rep-order-ec'                                                  
STAORDEC DC    C'sta-order-ec'                                                  
STAORDPC DC    C'sta-order-partial-confirm'                                     
UPDWIPST DC    C'update-wip-status'                                             
STATRF#U DC    C'station-traffic-order-update'                                  
MGOUPDTE DC    C'offer-update'                                                  
*                                                                               
MQMSGFRM DC    C'MQ-msg-info'                                                   
MASTREPC DC    C'masterrepcode'                                                 
REPCODE_ DC    C'repcode'                                                       
STATION_ DC    C'station'                                                       
REPORDR# DC    C'rep-order-number'                                              
TRAFORD# DC    C'traffic-order-number'                                          
VERSION_ DC    C'version'                                                       
MOD_____ DC    C'mod'                                                           
WIP_____ DC    C'wip'                                                           
STATUS__ DC    C'status'                                                        
LASSNDON DC    C'last-sent-on'                                                  
LASSNDTM DC    C'last-sent-time'                                                
CONTYPCD DC    C'contract-type-code'                                            
CONTYPNM DC    C'contract-type-name'                                            
AGYCODE_ DC    C'agency-code'                                                   
AGYNAME_ DC    C'agency-name'                                                   
BUYERNAM DC    C'buyer-name'                                                    
ADVTSRCD DC    C'advertiser-code'                                               
ADVTSRNM DC    C'advertiser-name'                                               
AGYCONNT DC    C'agency-connection'                                             
CPE_____ DC    C'cpe'                                                           
PRDNAME_ DC    C'product-name'                                                  
SALPERCD DC    C'salesperson-code'                                              
SALPERNM DC    C'salesperson-name'                                              
OFFICE__ DC    C'office'                                                        
FLSTRDAT DC    C'flight-start'                                                  
FLENDDAT DC    C'flight-end'                                                    
FLWEEKS_ DC    C'flight-weeks'                                                  
PRIMDCOD DC    C'primary-demographic-code'                                      
ECDATE__ DC    C'ec-date'                                                       
ORDTOTAL DC    C'order-total'                                                   
OFPENREP DC    C'offers-pending-rep'                                            
STASHARE DC    C'station-share'                                                 
*                                                                               
REPORDCM DC    C'rep-ord-cmt'                                                   
STAORDCM DC    C'station-ord-cmt'                                               
*                                                                               
AUDREPID DC    C'audit-rep-pid'                                                 
AUDREPNM DC    C'audit-asst-name'                                               
*                                                                               
E_RETURN DC    C'Returned order'                                                
E_CONFIR DC    C'Confirmed order'                                               
E_PARTCF DC    C'Partially Confirmed order'                                     
E_MGSEND DC    C'Sent an Offer for order'                                       
*                                                                               
EMALFROM DC    CL150'StationToolKitSupport@mediaocean.com'                      
EMALBCC_ DC    C'StationToolKitelog@mediaocean.com'                             
*                                                                               
* Do not change sequence of below 4 fields                                      
*                                                                               
SMTPMAIL DC    CL10'*SMTPMAIL*'                                                 
JMOPTION DS    0H                                                               
FROMLONG DC    CL10'*FROMLONG*'                                                 
SENDER   DC    CL10'*SENDER***'                                                 
REPLYTO  DC    CL10'*REPLYTO**'                                                 
         DC    X'FF'                                                            
JMOPTNQ  EQU   *-JMOPTION                                                       
*                                                                               
* Do not change sequence of above 4 fields                                      
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INIWKST  NTR1  BASE=*,LABEL=*      Init working storages                        
*                                                                               
         L     RE,4(R1)            Point to calling parameter                   
         MVI   WKMQMODE,0          Init MQ processing mode                      
         CLC   =C'MQMSG',0(RE)                                                  
         JNE   *+8                                                              
         MVI   WKMQMODE,MQMACTSQ   Set to MQ triggered by CON actions           
         CLC   =C'MQWIP',0(RE)                                                  
         JNE   *+8                                                              
         MVI   WKMQMODE,MQMWIPSQ   Set to MQ triggered by WIPS                  
         CLC   =C'MQTR#',0(RE)                                                  
         JNE   *+8                                                              
         MVI   WKMQMODE,MQMTRF#Q   Set to MQ by traffic order# update           
*                                                                               
         CLC   =C'ADD',CONACT      Adding contract?                             
         JE    EXITN                                                            
         CLI   RCONREC,X'0C'       Have contract record?                        
         JNE   EXITN                                                            
*                                                                               
         LA    R5,RCONREC          Point to contract record                     
         LA    R5,FRSTELEM(R5)                                                  
         USING RCONELEM,R5                                                      
         CLI   RCONCODE,X'01'      Have contract description element?           
         JNE   EXITN                                                            
         CLC   RCONSAL,SPACE150    Have Salesperson code?                       
         JNH   EXITN                                                            
         DROP  R5                                                               
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(WKAIO1_-RECNT81D)                                          
         ST    RE,AWKAIO1                                                       
*                                                                               
         LR    RE,R9                                                            
         A     RE,=A(MQMSG-RECNT81D)                                            
         ST    RE,AMQMSG_                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   VMQIO,CMQIO         Save DDMQIO address                          
         MVC   VJESMAIL,CJESMAIL   Save JESMAIL address                         
         DROP  RF                                                               
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000AE0',0                                      
         CLI   DMCB+4,X'FF'        ERROR?                                       
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOCON,0(R1)                                                   
*                                                                               
         XC    MSSAGEID,MSSAGEID   Init message ID                              
         MVI   WKEMAILM,0          Init e-mail mode                             
*                                                                               
         MVC   SVCONREP,RCONKREP   Contract key - rep code                      
         MVC   SVCONSTA,RCONKSTA   Contract key - station call letters          
         MVC   SVCONCON,RCONKCON   Contract key - contract number               
*                                                                               
         GOTOR (RFCONNUM,VREPFACS),DMCB,(1,SVCONCON),(2,SVCON9CM)               
         GOTOR (RFCONNUM,VREPFACS),DMCB,(1,SVCONCON),(5,CON#CHAR)               
*                                                                               
         MVC   SVMQMFRM(3),=C'CON' Init MQ messaage from CON program            
         MVC   SVMQMFRM+3(8),CON#CHAR                                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R5,KEY                                                           
         USING RSTAKEY,R5                                                       
         MVI   RSTAKTYP,X'02'      Station record type                          
         MVC   RSTAKREP,SVCONREP   Rep code                                     
         MVC   RSTAKSTA,SVCONSTA   Station call letters                         
         GOTO1 VHIGH                                                            
         CLC   RSTAKEY,KEYSAVE     Found station record?                        
         JNE   EXITN                                                            
         GOTO1 VGETREC,DMCB,AWKAIO1                                             
         TM    DMCB+8,X'FD'                                                     
         JZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AWKAIO1                                                       
         CLI   RSTACODE,X'01'      Have station main element?                   
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVRSTAP6,RSTAP6     Save STK MQ message profile                  
         DROP  R5                                                               
*                                                                               
         CLI   SVRSTAP6,C'Y'       To process MQ message?                       
         JNE   EXITN                                                            
*                                                                               
         J     EXITY               CC equal = to process MQ                     
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INIMQPA  NTR1  BASE=*,LABEL=*      Init MQ parameters                           
*                                                                               
         L     R0,AMQMSG_                                                       
         LHI   R1,MQMSGLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               Clear MQ message                             
*                                                                               
         L     R2,AMQMSG_                                                       
         MVC   0(L'QULABEL,R2),QULABEL                                          
         LA    R2,L'QULABEL(R2)                                                 
         MVC   0(L'CRLF,R2),CRLF   Add CRLF                                     
         LA    R2,L'CRLF(R2)       Bump past CRLF                               
         ST    R2,WKAMQMST                                                      
*                                                                               
         MVC   MSSAGEID+00(2),SVCONREP                                          
         MVC   MSSAGEID+02(8),CON#CHAR                                          
         MVC   MSSAGEID+10(06),=C'REPCON'                                       
*                                                                               
         BRAS  RE,CKSIDE                                                        
*                                                                               
         XC    SVTRORD#,SVTRORD#                                                
         XC    SVORDTOT,SVORDTOT                                                
         XC    SVAGYOFF,SVAGYOFF                                                
         MVI   SVCONFSW,0                                                       
*                                                                               
         LA    R5,RCONREC                                                       
         USING RCONKEY,R5                                                       
         MVC   SVAGYOFF(L'RCONKAGY),RCONKAGY                                    
         LA    RE,SVAGYOFF                                                      
         AHI   RE,L'SVAGYOFF-1                                                  
         LA    RF,L'SVAGYOFF                                                    
         CLI   0(RE),C' '                                                       
         JH    *+10                                                             
         BCTR  RE,0                                                             
         JCT   RF,*-10                                                          
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'-'                                                       
         MVC   1(L'RCONKOFF,RE),RCONKOFF                                        
         DROP  R5                                                               
*                                                                               
         LA    R5,FRSTELEM(R5)                                                  
         USING RCONXEL,R5                                                       
         MVI   ELCODE,X'1F'        Extended description elem                    
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ20                                                          
         MVC   SVCONFSW,RCONCONF   Save confirmation switch                     
         MVC   SVTRORD#,RCONTRF    Save traffic order number                    
         MVC   SVORDTOT,RCONTOT    Save order total                             
         DROP  R5                                                               
*                                                                               
INIMQ20  MVI   SVMODNUM,0                                                       
         XC    SVCABNAM,SVCABNAM                                                
         XC    SVFLSTDT,SVFLSTDT                                                
         XC    SVFLENDT,SVFLENDT                                                
         MVI   SV#WEEKS,0                                                       
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         USING RCONELEM,R5                                                      
         MVI   ELCODE,X'01'        Contract description elem                    
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ22                                                          
         MVC   SVCONSAL,RCONSAL    Save contract salesperson code               
         MVC   SVFLSTDT,RCONDATE+0 Save flight start date                       
         MVC   SVFLENDT,RCONDATE+3 Save flight end date                         
         MVC   SVCABNAM,RCONBUYR   Save contract agency buyer name              
         MVC   SVMODNUM,RCONMOD    Save contract mod number                     
         MVC   SV#WEEKS,RCONWKS    Save number of weeks                         
         MVC   SVCONTYP,RCONTYPE   Save contract type                           
         DROP  R5                                                               
*                                                                               
INIMQ22  MVI   WIPSFLAG,C'N'       Set WIP=false                                
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         USING RCONXEL,R5                                                       
         MVI   ELCODE,X'1F'        Extended description elem                    
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   *+12                                                             
         TM    RCONCONF,X'40'      Confirmed now?                               
         JO    INIMQ30             Yes - not WIP                                
         DROP  R5                                                               
*                                                                               
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         USING RCONSEND,R5                                                      
         MVI   ELCODE,X'20'        Send info elem                               
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ24                                                          
         TM    RCONSENF,X'30'      REP/STA version not advanced?                
         JO    INIMQ24                                                          
         MVI   WIPSFLAG,C'Y'       Set WIP=true                                 
         DROP  R5                                                               
*                                                                               
INIMQ24  MVC   SVPRDNAM,SPACE150   Init expanded product name                   
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         USING RCONEXEL,R5                                                      
         MVI   ELCODE,X'05'        Contract expansion element                   
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ30                                                          
         MVC   SVPRDNAM,RCONEXPR   Expanded product name                        
         DROP  R5                                                               
*                                                                               
INIMQ30  XC    SVLASSDT,SVLASSDT   Init last send date                          
         XC    SVSNDTIM,SVSNDTIM   Init last send time                          
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         USING RCONSEND,R5                                                      
         MVI   ELCODE,X'20'        Send info elem                               
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ32                                                          
         TM    RCONSENF,X'80'      Last send by rep?                            
         JZ    *+16                                                             
         MVC   SVLASSDT,RCONSRDT                                                
         MVC   SVSNDTIM,RCONSRTI                                                
         TM    RCONSENF,X'40'      Last send by station?                        
         JZ    *+16                                                             
         MVC   SVLASSDT,RCONSSDT                                                
         MVC   SVSNDTIM,RCONSSTI                                                
*                                                                               
INIMQ32  MVC   SVAGYCON,=C'MANUAL' Default to manual                            
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         MVI   ELCODE,X'1D'        DARE Agency Order elem                       
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ34                                                          
         USING RCONDREL,R5                                                      
         TM    RCONDRFG,X'80'                                                   
         JZ    *+10                                                             
         MVC   SVAGYCON,=C'DARE  ' Set to Dare                                  
         TM    RCONDRF2,X'01'                                                   
         JZ    *+10                                                             
         MVC   SVAGYCON,=C'XML   ' Set to XML                                   
         DROP  R5                                                               
*                                                                               
INIMQ34  MVC   SVCPE___,SPACE150                                                
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         MVI   ELCODE,X'A2'        EASI code elem                               
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ36                                                          
         USING RCONIEL,R5                                                       
         LA    RF,SVCPE___                                                      
         LR    R1,RF               For expanded estimate descriptions           
         CLC   RCONIADV,SPACE150                                                
         JNH   INIMQ34D                                                         
         MVC   0(L'RCONIADV,RF),RCONIADV                                        
         LA    RF,SVCPE___+(L'SVCPE___-1)                                       
         BRAS  RE,TRALNULL                                                      
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
*                                                                               
INIMQ34D CLC   RCONIPRD,SPACE150                                                
         JNH   INIMQ34H                                                         
         MVC   0(L'RCONIPRD,RF),RCONIPRD                                        
         LA    RF,SVCPE___+(L'SVCPE___-1)                                       
         BRAS  RE,TRALNULL                                                      
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         LR    R1,RF                                                            
*                                                                               
INIMQ34H CLC   RCONIEST,SPACE150                                                
         JNH   INIMQ34M                                                         
         MVC   0(L'RCONIEST,RF),RCONIEST                                        
*                                                                               
INIMQ34M CLC   RCONXEST,SPACE150                                                
         JNH   INIMQ36                                                          
         LR    RF,R1                                                            
         MVC   0(L'RCONXEST,RF),RCONXEST                                        
         DROP  R5                                                               
*                                                                               
INIMQ36  XC    SVPRIDEM,SVPRIDEM                                                
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         MVI   ELCODE,X'12'        Expanded SAR elem                            
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ38                                                          
         USING RSARXEL,R5                                                       
         LA    RE,WKELEM02                                                      
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         DROP  RE                                                               
         XC    WKELEM01,WKELEM01                                                
         MVC   WKELEM01(L'RSARXDEM),RSARXDEM                                    
         DROP  R5                                                               
         LA    R2,8                Eight set of demo to process                 
         LA    R3,WKELEM01                                                      
INIMQ36M OC    0(3,R3),0(R3)       Have demo?                                   
         JZ    INIMQ38                                                          
         CLI   1(R3),C'T'          Set input for DEMOCON                        
         JNE   *+8                                                              
         MVI   1(R3),C'I'                                                       
         GOTOR VDEMOCON,DMCB,(0,(R3)),(6,WORK),(0,WKELEM02)                     
         TM    0(R3),X'40'         Primary demo?                                
         JZ    *+14                                                             
         MVC   SVPRIDEM,WORK       Save primary demo and done                   
         J     INIMQ38                                                          
         LA    R3,3(R3)                                                         
         JCT   R2,INIMQ36M                                                      
*                                                                               
INIMQ38  XC    SVECDATE,SVECDATE                                                
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         MVI   ELCODE,X'15'        EC control elem                              
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ40                                                          
         USING RCONECEL,R5                                                      
         MVC   SVECDATE,RCONECDT   Save last send date                          
         DROP  R5                                                               
*                                                                               
INIMQ40  DS    0H                  Logic is revamped in CKSTAOFR                
*                                                                               
* * * *  MVI   SVOFRPND,NOQ        Set makegood offer pending to NO             
* * * *  LA    R5,RCONREC                                                       
* * * *  LA    R5,FRSTELEM(R5)                                                  
* * * *  MVI   ELCODE,X'1E'        Random flag element                          
* * * *  CLC   ELCODE,0(R5)                                                     
* * * *  JE    *+12                                                             
* * * *  BRAS  RE,NXTEL                                                         
* * * *  JNE   INIMQ42                                                          
* * * *  USING RCONRFEL,R5                                                      
* * * *  CLI   RCONR#MO,0          Any makegood offers?                         
* * * *  JNH   *+8                                                              
* * * *  MVI   SVOFRPND,YESQ       Set makegood offer pending to YES            
* * * *  DROP  R5                                                               
*                                                                               
INIMQ42  XC    SVSTASHR,SVSTASHR   Init station share                           
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         MVI   ELCODE,X'06'        SPL(TV)/EPL(Radio) element                   
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ44                                                          
         USING RCONSPEL,R5                                                      
         EDIT  RCONSPAM,SVSTASHR,2,ALIGN=LEFT                                   
         DROP  R5                                                               
*                                                                               
INIMQ44  MVI   SVCURMOD,0          Init most recent mod number                  
         MVI   SVCURVER,0          Init most recent version number              
         MVI   SVMODELM,NOQ        Init mod date/time element switch            
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         MVI   ELCODE,X'22'        Mod date/time element                        
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ46                                                          
         USING RMODELEM,R5                                                      
         MVC   SVCURMOD,RMODEL1M   Save most recent mod number                  
         MVC   SVCURVER,RMODEL1V   Save most recent version number              
         MVI   SVMODELM,YESQ       Set mod date/time element switch             
         DROP  R5                                                               
*                                                                               
INIMQ46  XC    SVSTAUEM,SVSTAUEM                                                
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         MVI   ELCODE,RCONSEEQ     Station user e-mail element code             
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ48                                                          
         USING RCONSEEL,R5                                                      
         LLC   RE,RCONSELN                                                      
         CHI   RE,RCONSEOQ+1       Empty element?                               
         JNH   INIMQ48                                                          
         JH    *+12                                                             
         CLI   RCONSEML,C' '                                                    
         JNH   INIMQ48                                                          
         AHI   RE,-(RCONSEOQ+1)    Length for EX instruction                    
         BASR  RF,0                                                             
         MVC   SVSTAUEM(0),RCONSEML                                             
         EX    RE,0(RF)                                                         
         DROP  R5                                                               
*                                                                               
INIMQ48  XC    SVSALEML,SVSALEML   Init saved S/A e-mail (override)             
         LA    R5,RCONREC                                                       
         LA    R5,FRSTELEM(R5)                                                  
         MVI   ELCODE,X'9E'        S/A e-mail override                          
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   INIMQ50                                                          
         USING RCONXOEL,R5                                                      
         LLC   RE,RCONXOEL+1                                                    
         CHI   RE,3                Empty element?                               
         JNH   INIMQ50                                                          
         AHI   RE,-3               Length for EX instruction                    
         BASR  RF,0                                                             
         MVC   SVSALEML(0),RCONXOSA                                             
         EX    RE,0(RF)                                                         
         DROP  R5                                                               
*                                                                               
INIMQ50  DS    0H                                                               
*                                                                               
         BRAS  RE,INISTATS         Initialize status for MQ message             
         BRAS  RE,GETADVSR         Get advertiser data                          
         BRAS  RE,GETSALPR         Get salesperson data                         
         BRAS  RE,GETOFFCR         Get office data                              
         BRAS  RE,GETCTYPE         Get contract type data                       
         BRAS  RE,GETAUDIT         Get audit data                               
         BRAS  RE,GETREPRC         Get rep record for profile values            
         BRAS  RE,CKSTAOFR         Check station created offers                 
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INISTATS NTR1  BASE=*,LABEL=*      Initialize status for MQ message             
*                                                                               
         XC    MQMSGSTA,MQMSGSTA                                                
*                                                                               
         CLI   SVVERSN#,1          Version at 1?                                
         JNE   INISTA20                                                         
         TM    SVCONFSW,X'40'      Confirmed?                                   
         JNZ   INISTA22                                                         
         MVC   MQMSGSTA(L'NEW___TX),NEW___TX                                    
         J     INISTA_X                                                         
*                                                                               
INISTA20 TM    SVCONFSW,X'40'      Confirmed?                                   
         JZ    INISTA30                                                         
INISTA22 XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R5,KEY                                                           
         USING RCFCKEY,R5                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,SVCONREP                                                
         MVC   RCFCKCON,SVCONCON                                                
         GOTO1 VHIGH                                                            
         CLC   RCFCKEY,KEYSAVE     Have partial confirmation comment?           
         JNE   INISTA26                                                         
         TM    RCFCCNTL-2,X'80'    Deleted?                                     
         JNZ   INISTA26                                                         
         MVC   MQMSGSTA(L'PARTCFTX),PARTCFTX                                    
         J     INISTA_X                                                         
INISTA26 MVC   MQMSGSTA(L'CONFIRTX),CONFIRTX                                    
         J     INISTA_X                                                         
         DROP  R5                                                               
*                                                                               
INISTA30 CLI   CONWSIDE,C'R'       Rep side?                                    
         JNE   INISTA40                                                         
         MVC   MQMSGSTA(L'REVISDTX),REVISDTX                                    
         J     INISTA_X                                                         
INISTA40 CLI   CONWSIDE,C'S'       Station side?                                
         JNE   INISTA50                                                         
         MVC   MQMSGSTA(L'RETURNTX),RETURNTX                                    
         J     INISTA_X                                                         
*                                                                               
INISTA50 DS    0H                                                               
*                                                                               
INISTA_X J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETADVSR NTR1  BASE=*,LABEL=*      Get advertiser data                          
*                                                                               
         XC    SVADVNAM,SVADVNAM                                                
         XC    KEY,KEY             Init advertiser key                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    RE,RCONREC                                                       
         USING RCONKEY,RE                                                       
         LA    R5,KEY                                                           
         USING RADVKEY,R5                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,RCONKADV                                                
         MVC   RADVKREP,SVCONREP                                                
         DROP  RE                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   RADVKEY,KEYSAVE                                                  
         JNE   EXITX                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,AWKAIO1                                             
*                                                                               
         L     R5,AWKAIO1                                                       
         USING RADVREC,R5                                                       
         MVC   SVADVNAM,RADVNAME   Save advertiser name                         
         DROP  R5                                                               
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETSALPR NTR1  BASE=*,LABEL=*      Get salesperson data                         
*                                                                               
         XC    SVSALNAM,SVSALNAM                                                
         XC    KEY,KEY             Init salesperson key                         
         XC    KEYSAVE,KEYSAVE                                                  
         LA    RE,RCONREC                                                       
         USING RCONKEY,RE                                                       
         LA    R5,KEY                                                           
         USING RSALKEY,R5                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,SVCONREP                                                
         MVC   RSALKSAL,SVCONSAL                                                
         DROP  RE                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   RSALKEY,KEYSAVE                                                  
         JNE   EXITX                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,AWKAIO1                                             
*                                                                               
         L     R5,AWKAIO1                                                       
         USING RSALREC,R5                                                       
         MVC   SVSALNAM,RSALNAME   Save salesperson name                        
         DROP  R5                                                               
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETOFFCR NTR1  BASE=*,LABEL=*      Get office data                              
*                                                                               
         XC    SVOFFNAM,SVOFFNAM                                                
         XC    KEY,KEY             Init office key                              
         XC    KEYSAVE,KEYSAVE                                                  
         LA    RE,RCONREC                                                       
         USING RCONKEY,RE                                                       
         LA    R5,KEY                                                           
         USING ROFFKEY,R5                                                       
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,SVCONREP                                                
         MVC   ROFFKOFF,RCONKOFF                                                
         DROP  RE                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   ROFFKEY,KEYSAVE                                                  
         JNE   EXITX                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,AWKAIO1                                             
*                                                                               
         L     R5,AWKAIO1                                                       
         USING ROFFREC,R5                                                       
         MVC   SVOFFNAM,ROFFNAME   Save office name                             
         DROP  R5                                                               
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCTYPE NTR1  BASE=*,LABEL=*      Get audit data                               
*                                                                               
         XC    SVCTYDES,SVCTYDES   Init contract type name                      
         CLI   SVCONTYP,0          Have contract type?                          
         JNH   EXITX                                                            
*                                                                               
         XC    KEY,KEY             Init contract type key                       
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R5,KEY                                                           
         USING RCTYKEY,R5                                                       
         MVI   RCTYKTYP,X'32'                                                   
         MVC   RCTYKCTY,SVCONTYP                                                
         MVC   RCTYKREP,SVCONREP                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   RCTYKEY,KEYSAVE                                                  
         JNE   EXITX                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,AWKAIO1                                             
*                                                                               
         L     R5,AWKAIO1                                                       
         USING RCTYREC,R5                                                       
         MVC   SVCTYDES,RCTYDESC   Save contract type name                      
         DROP  R5                                                               
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETAUDIT NTR1  BASE=*,LABEL=*      Get audit data                               
*                                                                               
         XC    SVAUDSAG,SVAUDSAG                                                
         XC    SVAUDPID,SVAUDPID                                                
         XC    SVAUDANM,SVAUDANM                                                
         XC    SVPIDCL8,SVPIDCL8                                                
*                                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R5,KEY                                                           
         USING RAUDKEY,R5                                                       
         MVI   RAUDKTYP,X'4D'                                                   
         MVC   RAUDKREP,SVCONREP                                                
         MVC   RAUDKCON,SVCONCON                                                
         MVC   RAUDKVER,SVVERSN#                                                
         XI    RAUDKVER,X'FF'      Complement version number                    
*                                                                               
         MVI   RAUDKMOD,X'FF'      Default to zero mode number                  
         CLI   SVMODELM,YESQ       Have mod date/time element?                  
         JNE   GETAU10                                                          
         MVC   WKBYTE01,SVCURVER   Copy latest version number                   
         XI    WKBYTE01,X'FF'      Complement                                   
         CLC   =C'CFX ',CONACT     CFX action?                                  
         JNE   *+10                                                             
         MVC   RAUDKVER,WKBYTE01   Use mod date/time elem version #             
         CLC   RAUDKVER,WKBYTE01   Version modified?                            
         JNE   GETAU10                                                          
         LLC   RF,SVCURMOD         Get lastest mod number                       
         AHI   RF,1                Bump up by one                               
         STC   RF,RAUDKMOD                                                      
         XI    RAUDKMOD,X'FF'      Complement mod number                        
*                                                                               
GETAU10  GOTO1 VHIGH                                                            
         CLC   RAUDKEY,KEYSAVE     Found contract audit comment record?         
         JNE   GETAU50                                                          
         DROP  R5                                                               
         GOTO1 VGETREC,DMCB,AWKAIO1                                             
         L     R5,AWKAIO1                                                       
         USING RAUDREC,R5                                                       
         CLI   RAUDTHEL,RAUDTHQ    Have audit comment text header elem?         
         JNE   GETAU50                                                          
         MVC   SVAUDSAG,RAUDTHAG                                                
         MVC   SVAUDPID,RAUDTHPD                                                
         MVC   SVAUDANM,RAUDTHAS                                                
         DROP  R5                                                               
*                                                                               
         OC    SVAUDPID,SVAUDPID   Have PID?                                    
         JNZ   *+6                                                              
         DC    H'0'                Why no PID?                                  
* * * *  JZ    GETAU50                                                          
*                                                                               
         L     R5,AWKAIO1                                                       
         USING SA0REC,R5                                                        
         XC    SA0KEY,SA0KEY       Read person password record                  
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KNUM,SVAUDPID                                                 
         MVC   SA0KAGY,SVAUDSAG                                                 
         CLC   SA0KAGY,SPACE150    Have security agency?                        
         JH    *+10                                                             
         MVC   SA0KAGY,SVCONREP    Use contract rep                             
         MVC   KEYSAVE,SA0KEY                                                   
         GOTO1 DATAMGR,DMCB,=CL7'DMRDHI',=CL7'CTFILE',SA0KEY,SA0KEY,0,0         
         CLC   SA0KEY,KEYSAVE                                                   
         JE    *+14                                                             
GETAU24  MVC   SVPIDCL8,=C'Bad PID*'                                            
         J     GETAU50                                                          
*                                                                               
         LA    R1,SA0DATA                                                       
         USING SAPALD,R1                                                        
GETAU26  CLI   SAPALEL,EOR         Test end of record                           
         JE    GETAU24                                                          
         CLI   SAPALEL,SAPALELQ    Test new security person element             
         JE    GETAU28                                                          
         LLC   R0,SAPALLN          Bump to next element                         
         AR    R1,R0                                                            
         J     GETAU26                                                          
*                                                                               
GETAU28  MVC   SVPIDCL8,SAPALPID   Save character PID                           
*                                                                               
GETAU50  DS    0H                                                               
*                                                                               
GETAU_X  J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREPRC NTR1  BASE=*,LABEL=*      Get Rep record data                          
*                                                                               
         XC    R_SOMPRF,R_SOMPRF   Init SOM Profile values                      
*                                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R5,KEY                                                           
         USING RREPKEY,R5                                                       
         MVI   RREPKTYP,X'01'      Rep record tpye                              
         MVC   RREPKREP,SVCONREP                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   RREPKEY,KEYSAVE     Found rep record?                            
         JNE   EXITX                                                            
         DROP  R5                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,AWKAIO1                                             
         L     R5,AWKAIO1                                                       
         LA    R5,FRSTELEM(R5)                                                  
*                                                                               
         XC    REPPRGPF(RREPPGML*15),REPPRGPF                                   
         USING RREPPGMP,R5                                                      
         MVI   ELCODE,X'04'        Program profile element                      
         BRAS  RE,NXTEL                                                         
         JNE   *+10                                                             
         MVC   REPPRGPF(RREPPGML*15),RREPPGM1                                   
         GOTOR BITOUT,DMCB,REPPRGPF+SOMPBITQ,R_SOMPRF                           
         DROP  R5                                                               
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* Display 64 Y/N bytes                                                          
*        P1    A(Double word data bits)                                         
*        P2    A(output)                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BITOUT   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,64               64 bits to display                           
         L     RE,0(R1)            A(Data bits)                                 
         L     R2,4(R1)            A(Output)                                    
         LM    R4,R5,0(RE)                                                      
         J     BOUT30                                                           
*                                                                               
BOUT20   SLDL  R4,1                                                             
BOUT30   MVI   0(R2),C'Y'                                                       
         LTR   R4,R4               High order bit on?                           
         JM    BOUT40                                                           
         MVI   0(R2),C'N'                                                       
BOUT40   LA    R2,1(R2)            Next entry in output                         
         JCT   R0,BOUT20                                                        
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCSEND  NTR1  BASE=*,LABEL=*      Process send                                 
*                                                                               
         L     R2,WKAMQMST         Point to start of MQ message                 
*                                                                               
         CLI   CONWSIDE,C'R'       Rep side?                                    
         JNE   *+12                                                             
         LA    R3,L'REPORDSN                                                    
         LA    R4,REPORDSN                                                      
         CLI   CONWSIDE,C'S'       Station side?                                
         JNE   *+12                                                             
         LA    R3,L'STAORDSN                                                    
         LA    R4,STAORDSN                                                      
         BRAS  RE,STARTTAG                                                      
*                                                                               
         BRAS  RE,BLDMSG01         Build message (common fields)                
         BRAS  RE,BLDORDCM         Build order comments                         
         BRAS  RE,BLDAUDIT         Build audit fields                           
*                                                                               
         BRAS  RE,END__TAG                                                      
*                                                                               
         L     R4,AMQMSG_                                                       
         LR    R5,R2                                                            
         SR    R5,R4               Length of MQ message                         
*                                                                               
         GOTOR VMQIO,DMCB,=CL8'PUT',(R4),(R5),0,0,DUB,C'UNIM',MSSAGEID          
*                                                                               
         J     EXITY                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCCF__  NTR1  BASE=*,LABEL=*      Process confirmation                         
*                                                                               
         L     R2,WKAMQMST         Point to start of MQ message                 
*                                                                               
         CLI   CONWSIDE,C'R'       Rep side?                                    
         JNE   *+12                                                             
         LA    R3,L'REPORDCF                                                    
         LA    R4,REPORDCF                                                      
         CLI   CONWSIDE,C'S'       Station side?                                
         JNE   *+12                                                             
         LA    R3,L'STAORDCF                                                    
         LA    R4,STAORDCF                                                      
         BRAS  RE,STARTTAG                                                      
*                                                                               
         BRAS  RE,BLDMSG01         Build message (common fields)                
         BRAS  RE,BLDAUDIT         Build audit fields                           
*                                                                               
         BRAS  RE,END__TAG                                                      
*                                                                               
         L     R4,AMQMSG_                                                       
         LR    R5,R2                                                            
         SR    R5,R4               Length of MQ message                         
*                                                                               
         GOTOR VMQIO,DMCB,=CL8'PUT',(R4),(R5),0,0,DUB,C'UNIM',MSSAGEID          
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCEC__  NTR1  BASE=*,LABEL=*      Process EC                                   
*                                                                               
         L     R2,WKAMQMST         Point to start of MQ message                 
*                                                                               
         CLI   CONWSIDE,C'R'       Rep side?                                    
         JNE   *+12                                                             
         LA    R3,L'REPORDEC                                                    
         LA    R4,REPORDEC                                                      
         CLI   CONWSIDE,C'S'       Station side?                                
         JNE   *+12                                                             
         LA    R3,L'STAORDEC                                                    
         LA    R4,STAORDEC                                                      
         BRAS  RE,STARTTAG                                                      
*                                                                               
         BRAS  RE,BLDMSG02         Build message (common fields)                
*                                                                               
         BRAS  RE,END__TAG                                                      
*                                                                               
         L     R4,AMQMSG_                                                       
         LR    R5,R2                                                            
         SR    R5,R4               Length of MQ message                         
*                                                                               
         GOTOR VMQIO,DMCB,=CL8'PUT',(R4),(R5),0,0,DUB,C'UNIM',MSSAGEID          
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCTRF#  NTR1  BASE=*,LABEL=*      Process traffic order number update          
*                                                                               
         L     R2,WKAMQMST         Point to start of MQ message                 
*                                                                               
         CLI   WIPSFLAG,C'Y'       WIP?                                         
         JNE   PRCTR20                                                          
         CLI   SVVERSN#,1          Version 1?                                   
         JE    PRCTR90             No MQ message for WIP1                       
*                                                                               
PRCTR20  LA    R3,L'STATRF#U                                                    
         LA    R4,STATRF#U                                                      
         BRAS  RE,STARTTAG                                                      
*                                                                               
         BRAS  RE,BLDMSG02         Build message (common fields)                
*                                                                               
         BRAS  RE,END__TAG                                                      
*                                                                               
         L     R4,AMQMSG_                                                       
         LR    R5,R2                                                            
         SR    R5,R4               Length of MQ message                         
*                                                                               
         GOTOR VMQIO,DMCB,=CL8'PUT',(R4),(R5),0,0,DUB,C'UNIM',MSSAGEID          
*                                                                               
PRCTR90  NI    TWACONFG,X'FF'-TW_TRFOQ                                          
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCPCF_  NTR1  BASE=*,LABEL=*      Process partial confirmation                 
*                                                                               
         L     R2,WKAMQMST         Point to start of MQ message                 
*                                                                               
         LA    R3,L'STAORDPC                                                    
         LA    R4,STAORDPC                                                      
         BRAS  RE,STARTTAG                                                      
*                                                                               
         BRAS  RE,BLDMSG01         Build message (common fields)                
*                                                                               
         BRAS  RE,END__TAG                                                      
*                                                                               
         L     R4,AMQMSG_                                                       
         LR    R5,R2                                                            
         SR    R5,R4               Length of MQ message                         
*                                                                               
         GOTOR VMQIO,DMCB,=CL8'PUT',(R4),(R5),0,0,DUB,C'UNIM',MSSAGEID          
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCMGOU  NTR1  BASE=*,LABEL=*      Process MGO added                            
*                                                                               
         CLI   SVSTAOFR,YESQ       Station created offer?                       
         JNE   EXITX               Suppress MQ msg for station offers           
*                                                                               
         L     R2,WKAMQMST         Point to start of MQ message                 
*                                                                               
         LA    R3,L'MGOUPDTE                                                    
         LA    R4,MGOUPDTE                                                      
         BRAS  RE,STARTTAG                                                      
*                                                                               
         BRAS  RE,BLDMSG02         Build message (common fields)                
*                                                                               
         BRAS  RE,END__TAG                                                      
*                                                                               
         L     R4,AMQMSG_                                                       
         LR    R5,R2                                                            
         SR    R5,R4               Length of MQ message                         
*                                                                               
         GOTOR VMQIO,DMCB,=CL8'PUT',(R4),(R5),0,0,DUB,C'UNIM',MSSAGEID          
*                                                                               
         NI    TWACONFG,X'FF'-TW_MGOMQ                                          
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSTAOFR NTR1  BASE=*,LABEL=*      Check for station created offers             
*                                                                               
         MVI   SVSTAOFR,NOQ        Init station created offer flag              
         MVI   SVOFRPND,NOQ        Set makegood offer pending to NO             
*                                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R5,KEY                                                           
         USING RMKGKEY,R5                                                       
         MVC   RMGSPTYP,=X'A001'   Offer passive key with status bytes          
         MVC   RMGSPREP,SVCONREP                                                
         MVC   RMGSPSAL,SVCONSAL                                                
         MVC   RMGSPSTA,SVCONSTA                                                
         MVC   RMGSPCON,SVCON9CM   Contract number in 9's compliment            
*                                                                               
         GOTO1 VHIGH                                                            
         J     CKSTAO40                                                         
*                                                                               
CKSTAO20 GOTO1 VSEQ                                                             
*                                                                               
CKSTAO40 CLC   KEY(RMGSPADV-RMGSPTYP),KEYSAVE                                   
         JNE   CKSTAO_X                                                         
         OC    RMGSPGRP,RMGSPGRP   Have offer group code?                       
         JZ    CKSTAO20                                                         
         TM    KEY+27,X'80'        Deleted?                                     
         JNZ   CKSTAO20                                                         
*                                                                               
         TM    RMGSPSTT,RMKGSAPQ   Offer applied?                               
         JNZ   CKSTAO20                                                         
         TM    RMGSPSTT,RMKGSCRQ   Offer created by rep?                        
         JNZ   CKSTAO20                                                         
         MVI   SVSTAOFR,YESQ       Offer is created by station                  
* * * *  TM    RMGSPWIP,RMGF2WPQ   Offer is in WIPS?                            
* * * *  JZ    *+8                                                              
         MVI   SVOFRPND,YESQ       Set makegood offer pending to YES            
*                                                                               
         J     CKSTAO20            Read through all offer passives              
*                                                                               
CKSTAO_X CLI   SVSTAOFR,YESQ       Station created offer?                       
         JE    EXITY                                                            
         J     EXITN                                                            
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCWIPS  NTR1  BASE=*,LABEL=*      Process WIPS                                 
*                                                                               
         CLC   =C'DIS',BUYACT                                                   
         JE    P_WIPS80                                                         
*                                                                               
         L     R2,WKAMQMST         Point to start of MQ message                 
*                                                                               
         CLI   SVVERSN#,1          Version 1?                                   
         JE    P_WIPS80            No MQ message for WIP1                       
*                                                                               
         LA    R3,L'UPDWIPST                                                    
         LA    R4,UPDWIPST                                                      
         BRAS  RE,STARTTAG                                                      
*                                                                               
         BRAS  RE,BLDMSG02         Build message (common fields)                
*                                                                               
         BRAS  RE,END__TAG                                                      
*                                                                               
         L     R4,AMQMSG_                                                       
         LR    R5,R2                                                            
         SR    R5,R4               Length of MQ message                         
*                                                                               
         GOTOR VMQIO,DMCB,=CL8'PUT',(R4),(R5),0,0,DUB,C'UNIM',MSSAGEID          
*                                                                               
P_WIPS80 NI    TWACONFG,X'FF'-TW_WIPSQ                                          
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Check if contract is on Rep/Station side                            *         
* Returns R/S in CONWSIDE                                             *         
* Returns Y/N in CONSNDSW to indicate there's a SEND element or not   *         
* Returns version number in SVVERSN#                                  *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSIDE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   CONWSIDE,C'R'       Deafult to Rep side                          
         CLI   TWAACCS,C'$'        Station?                                     
         JNE   *+8                                                              
         MVI   CONWSIDE,C'S'       Station side                                 
*                                                                               
         MVI   CONSNDSW,NOQ        Init to no SEND element                      
         MVI   SVVERSN#,0          Init version number                          
         LA    R5,RCONREC          Point to contract record                     
         LA    R5,FRSTELEM(R5)                                                  
         MVI   ELCODE,X'20'        Look for Send info elem                      
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         JNE   CKSIDEX             Not found, on Rep side                       
*                                                                               
         USING RCONSEND,R5                                                      
         MVI   CONSNDSW,YESQ       Yes, SEND elem found                         
         MVC   SVVERSN#,RCONSRV    Set to use Rep's version number              
         CLC   RCONSRV,RCONSSV                                                  
         JNL   CKSIDEX                                                          
         MVC   SVVERSN#,RCONSSV    Set to use Sta's version number              
*                                                                               
CKSIDEX  J     EXITX                                                            
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDMSG00 NTR1  BASE=*,LABEL=*      Build message (common fields)                
*                                                                               
         LA    R5,RCONREC          Point to contract record                     
         USING RCONREC,R5                                                       
*                                                                               
         LA    R3,L'MQMSGFRM                                                    
         LA    R4,MQMSGFRM                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVMQMFRM                                                    
         LA    R1,SVMQMFRM                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'MASTREPC                                                    
         LA    R4,MASTREPC                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'TWARMAST                                                    
         LA    R1,TWARMAST                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'REPCODE_                                                    
         LA    R4,REPCODE_                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'AGYALPHA                                                    
         LA    R1,AGYALPHA                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'STATION_                                                    
         LA    R4,STATION_                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'RCONKSTA                                                    
         LA    R1,RCONKSTA                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'REPORDR#                                                    
         LA    R4,REPORDR#                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'CON#CHAR                                                    
         LA    R1,CON#CHAR                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'TRAFORD#                                                    
         LA    R4,TRAFORD#                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVTRORD#,SVTRORD#                                                
         JZ    BM00_29X                                                         
         LA    R0,L'SVTRORD#                                                    
         LA    R1,SVTRORD#                                                      
         BRAS  RE,CKSPCHAR                                                      
BM00_29X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'VERSION_                                                    
         LA    R4,VERSION_                                                      
         BRAS  RE,STARTTAG                                                      
         EDIT  (B1,SVVERSN#),(3,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                  
         AHI   R2,3                                                             
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'MOD_____                                                    
         LA    R4,MOD_____                                                      
         BRAS  RE,STARTTAG                                                      
         CLI   SVMODNUM,X'FF'                                                   
         JE    BM00_31X                                                         
         EDIT  (B1,SVMODNUM),(3,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                  
         AHI   R2,3                                                             
BM00_31X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'WIP_____                                                    
         LA    R4,WIP_____                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'WIPSFLAG                                                    
         LA    R1,WIPSFLAG                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
EXIT_R2  XIT1  REGS=(R2)                                                        
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDMSG01 NTR1  BASE=*,LABEL=*      Build message (common fields)                
*                                                                               
         LA    R5,RCONREC          Point to contract record                     
         USING RCONREC,R5                                                       
*                                                                               
         BRAS  RE,BLDMSG00                                                      
*                                                                               
         LA    R3,L'STATUS__                                                    
         LA    R4,STATUS__                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'MQMSGSTA                                                    
         LA    R1,MQMSGSTA                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'LASSNDON                                                    
         LA    R4,LASSNDON                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVLASSDT,SVLASSDT                                                
         JZ    BM01_34X                                                         
         GOTOR DATCON,DMCB,(2,SVLASSDT),(20,0(R2))                              
         AHI   R2,10                                                            
BM01_34X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'LASSNDTM                                                    
         LA    R4,LASSNDTM                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVSNDTIM,SVSNDTIM                                                
         JZ    *+14                                                             
         MVC   0(L'SVSNDTIM,R2),SVSNDTIM                                        
         AHI   R2,L'SVSNDTIM                                                    
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'CONTYPCD                                                    
         LA    R4,CONTYPCD                                                      
         BRAS  RE,STARTTAG                                                      
         CLI   SVCONTYP,0                                                       
         JNH   BM01_35X                                                         
         LA    R0,L'SVCONTYP                                                    
         LA    R1,SVCONTYP                                                      
         BRAS  RE,CKSPCHAR                                                      
BM01_35X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'CONTYPNM                                                    
         LA    R4,CONTYPNM                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVCTYDES,SVCTYDES                                                
         JZ    BM01_36X                                                         
         LA    R0,L'SVCTYDES                                                    
         LA    R1,SVCTYDES                                                      
         BRAS  RE,CKSPCHAR                                                      
BM01_36X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'AGYCODE_                                                    
         LA    R4,AGYCODE_                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVAGYOFF                                                    
         LA    R1,SVAGYOFF                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'AGYNAME_                                                    
         LA    R4,AGYNAME_                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'TWAAGNM2                                                    
         LA    R1,TWAAGNM2                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'BUYERNAM                                                    
         LA    R4,BUYERNAM                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVCABNAM                                                    
         LA    R1,SVCABNAM                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'ADVTSRCD                                                    
         LA    R4,ADVTSRCD                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'RCONKADV                                                    
         LA    R1,RCONKADV                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'ADVTSRNM                                                    
         LA    R4,ADVTSRNM                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVADVNAM                                                    
         LA    R1,SVADVNAM                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'AGYCONNT                                                    
         LA    R4,AGYCONNT                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVAGYCON                                                    
         LA    R1,SVAGYCON                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'CPE_____                                                    
         LA    R4,CPE_____                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVCPE___                                                    
         LA    R1,SVCPE___                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'PRDNAME_                                                    
         LA    R4,PRDNAME_                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVPRDNAM                                                    
         LA    R1,SVPRDNAM                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'SALPERCD                                                    
         LA    R4,SALPERCD                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVCONSAL                                                    
         LA    R1,SVCONSAL                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'SALPERNM                                                    
         LA    R4,SALPERNM                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVSALNAM                                                    
         LA    R1,SVSALNAM                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'OFFICE__                                                    
         LA    R4,OFFICE__                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVOFFNAM                                                    
         LA    R1,SVOFFNAM                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'FLSTRDAT                                                    
         LA    R4,FLSTRDAT                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVFLSTDT,SVFLSTDT                                                
         JZ    BM01_44X                                                         
         GOTOR DATCON,DMCB,(3,SVFLSTDT),(20,0(R2))                              
         AHI   R2,10                                                            
BM01_44X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'FLENDDAT                                                    
         LA    R4,FLENDDAT                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVFLENDT,SVFLENDT                                                
         JZ    BM01_45X                                                         
         GOTOR DATCON,DMCB,(3,SVFLENDT),(20,0(R2))                              
         AHI   R2,10                                                            
BM01_45X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'FLWEEKS_                                                    
         LA    R4,FLWEEKS_                                                      
         BRAS  RE,STARTTAG                                                      
         EDIT  (B1,SV#WEEKS),(3,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                  
         AHI   R2,3                                                             
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'PRIMDCOD                                                    
         LA    R4,PRIMDCOD                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVPRIDEM,SVPRIDEM   Have primary demo code?                      
         JZ    BM01_47X                                                         
         LA    R0,L'SVPRIDEM                                                    
         LA    R1,SVPRIDEM                                                      
         BRAS  RE,CKSPCHAR                                                      
BM01_47X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'ECDATE__                                                    
         LA    R4,ECDATE__                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVECDATE,SVECDATE                                                
         JZ    BM01_48X                                                         
         GOTOR DATCON,DMCB,(2,SVECDATE),(20,0(R2))                              
         AHI   R2,10                                                            
BM01_48X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'ORDTOTAL                                                    
         LA    R4,ORDTOTAL                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVORDTOT,SVORDTOT                                                
         JZ    BM01_50X                                                         
         EDIT  (B4,SVORDTOT),(10,0(R2)),2,ALIGN=LEFT,ZERO=NOBLANK               
         AHI   R2,10                                                            
BM01_50X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'OFPENREP                                                    
         LA    R4,OFPENREP                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVOFRPND                                                    
         LA    R1,SVOFRPND                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'STASHARE                                                    
         LA    R4,STASHARE                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVSTASHR                                                    
         LA    R1,SVSTASHR                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         J     EXIT_R2                                                          
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDMSG02 NTR1  BASE=*,LABEL=*      Build message (common fields)                
*                                                                               
         BRAS  RE,BLDMSG00                                                      
*                                                                               
         CLI   WKMQMODE,MQMWIPSQ   MQ triggered by WIPS?                        
         JE    BM02_60                                                          
*                                                                               
         LA    R3,L'STATUS__                                                    
         LA    R4,STATUS__                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'MQMSGSTA                                                    
         LA    R1,MQMSGSTA                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'LASSNDON                                                    
         LA    R4,LASSNDON                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVLASSDT,SVLASSDT                                                
         JZ    BM02_34X                                                         
         GOTOR DATCON,DMCB,(2,SVLASSDT),(20,0(R2))                              
         AHI   R2,10                                                            
BM02_34X BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'LASSNDTM                                                    
         LA    R4,LASSNDTM                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVSNDTIM,SVSNDTIM                                                
         JZ    *+14                                                             
         MVC   0(L'SVSNDTIM,R2),SVSNDTIM                                        
         AHI   R2,L'SVSNDTIM                                                    
         BRAS  RE,END__TAG                                                      
*                                                                               
         CLI   WKMQMODE,MQMTRF#Q   MQ triggered by traffic order# upd?          
         JE    BM02_47                                                          
         TM    TWACONFG,TW_MGOMQ   Process MQ mesg for MGO updated?             
         JNZ   BM02_47                                                          
*                                                                               
         J     BM02_48                                                          
*                                                                               
BM02_47  LA    R3,L'OFPENREP                                                    
         LA    R4,OFPENREP                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVOFRPND                                                    
         LA    R1,SVOFRPND                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'STASHARE                                                    
         LA    R4,STASHARE                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVSTASHR                                                    
         LA    R1,SVSTASHR                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         J     BM02_60                                                          
*                                                                               
BM02_48  LA    R3,L'ECDATE__                                                    
         LA    R4,ECDATE__                                                      
         BRAS  RE,STARTTAG                                                      
         OC    SVECDATE,SVECDATE                                                
         JZ    BM02_48X                                                         
         GOTOR DATCON,DMCB,(2,SVECDATE),(20,0(R2))                              
         AHI   R2,10                                                            
BM02_48X BRAS  RE,END__TAG                                                      
*                                                                               
BM02_60  DS    0H                                                               
*                                                                               
         J     EXIT_R2                                                          
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDORDCM NTR1  BASE=*,LABEL=*      Build order comments                         
*                                                                               
         CLI   CONWSIDE,C'R'       Rep?                                         
         JNE   B_ORDC10                                                         
         MVI   ELCODE,X'82'        Set to process Rep order comments            
         LA    R3,L'REPORDCM                                                    
         LA    R4,REPORDCM                                                      
*                                                                               
B_ORDC10 CLI   CONWSIDE,C'S'       Station?                                     
         JNE   B_ORDC20                                                         
         MVI   ELCODE,X'92'        Set to process Sta order comments            
         LA    R3,L'STAORDCM                                                    
         LA    R4,STAORDCM                                                      
*                                                                               
B_ORDC20 BRAS  RE,STARTTAG                                                      
         LA    R5,RCONREC          Point to contract record                     
         LA    R5,FRSTELEM(R5)                                                  
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
B_ORDC30 BRAS  RE,NXTEL                                                         
         JNE   B_ORDC90                                                         
*                                                                               
         XC    WKELEM02,WKELEM02                                                
         LLC   RE,1(R5)            Comment element length                       
         CHI   RE,2                Have comment?                                
         JNH   B_ORDC30                                                         
         AHI   RE,-3               Adjust elem overhead & EX                    
         BASR  RF,0                                                             
         MVC   WKELEM02(0),2(R5)                                                
         EX    RE,0(RF)                                                         
         AHI   RE,1                Length of comment                            
*                                                                               
         LR    R0,RE               Length of comment                            
         LA    R1,WKELEM02         Point to start of comment                    
         BRAS  RE,CKSPCHAR         Check special characters                     
*                                                                               
         MVI   0(R2),C' '          Space to separate next comment line          
         AHI   R2,1                Next set of order comment                    
         J     B_ORDC30                                                         
*                                                                               
B_ORDC90 BRAS  RE,END__TAG                                                      
*                                                                               
         J     EXIT_R2                                                          
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDAUDIT NTR1  BASE=*,LABEL=*      Build audit fields                           
*                                                                               
         LA    R3,L'AUDREPID                                                    
         LA    R4,AUDREPID                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVPIDCL8                                                    
         LA    R1,SVPIDCL8                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         LA    R3,L'AUDREPNM                                                    
         LA    R4,AUDREPNM                                                      
         BRAS  RE,STARTTAG                                                      
         LA    R0,L'SVAUDANM                                                    
         LA    R1,SVAUDANM                                                      
         BRAS  RE,CKSPCHAR                                                      
         BRAS  RE,END__TAG                                                      
*                                                                               
         J     EXIT_R2                                                          
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCEMAL  NTR1  BASE=*,LABEL=*      Process e-mail                               
*                                                                               
         CLI   TWAACCS,C'$'        Station?                                     
         JNE   EXITX                                                            
*                                                                               
         CLI   WKEMAILM,EMLSENDQ   Send?                                        
         JNE   *+12                                                             
         CLI   R_SOMPRF+03,C'Y'    Bit 4: send e-mail on return?                
         JNE   EXITX                                                            
*                                                                               
         CLI   WKEMAILM,EMLCONFQ   Confirmed?                                   
         JNE   *+12                                                             
         CLI   R_SOMPRF+02,C'Y'    Bit 3: send e-mail on confirmed?             
         JNE   EXITX                                                            
*                                                                               
         CLI   WKEMAILM,EMLPCONQ   Partially confirmed?                         
         JNE   *+12                                                             
         CLI   R_SOMPRF+02,C'Y'    Bit 3: send e-mail on Partial CF?            
         JNE   EXITX                                                            
*                                                                               
         CLI   WKEMAILM,EMLMGS_Q   Makegood send?                               
         JNE   *+12                                                             
         CLI   R_SOMPRF+04,C'Y'    Bit 5: send e-mail on Makegood send?         
         JNE   EXITX                                                            
*                                                                               
PRCEM10  XCEFL EMLFLDS,'EMLFLDLN'  Init e-mail fields                           
*                                                                               
PRCEM14  XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R5,KEY                                                           
         USING RSA2KEY,R5                                                       
         MVI   RSA2KTYP,X'46'      Salesperson2 record type                     
         MVC   RSA2KREP,SVCONREP                                                
         MVC   RSA2KSAL,SVCONSAL                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   RSA2KEY,KEYSAVE     Found salesperson2 record?                   
         JNE   PRCEM22H                                                         
         DROP  R5                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,AWKAIO1                                             
         L     R5,AWKAIO1                                                       
         LA    R5,FRSTELEM(R5)                                                  
         XC    WKELEM01,WKELEM01   Prepare to save e-mail addresses             
         MVI   WKBYTE01,0          Init temp flag for S/A e-mail                
*                                                                               
         USING RSALEMEM,R5                                                      
         MVI   ELCODE,X'20'        Salesperson e-mail address elem              
         BRAS  RE,NXTEL                                                         
         JNE   PRCEM20                                                          
         LLC   RE,RSALEMLN         E-mail address elem length                   
         AHI   RE,-3               Adjust elem overhead & EX                    
         BASR  RF,0                                                             
         MVC   WKELEM01+00(0),RSALEMAL                                          
         EX    RE,0(RF)                                                         
*                                                                               
PRCEM20  L     R5,AWKAIO1                                                       
         LA    R5,FRSTELEM(R5)                                                  
         USING RSASEMEM,R5                                                      
         MVI   ELCODE,X'21'        S/A e-mail address elem                      
         BRAS  RE,NXTEL                                                         
         JNE   PRCEM22                                                          
         MVC   WKBYTE01,RSASEMFL   Save S/A flag                                
         LLC   RE,RSASEMLN         E-mail address elem length                   
         AHI   RE,-(RSASEAML-RSASEMEM+1)                                        
         CHI   RE,0                Have S/A email address?                      
         JL    PRCEM22                                                          
         BASR  RF,0                                                             
         MVC   WKELEM01+60(0),RSASEAML                                          
         EX    RE,0(RF)                                                         
         DROP  R5                                                               
*                                                                               
PRCEM22  OC    WKELEM01,WKELEM01   Have e-mail address?                         
         JZ    PRCEM22H                                                         
         OC    WKELEM01(60),WKELEM01     Have S/P e-mail address?               
         JNZ   PRCEM22M                                                         
         OC    SVSALEML,SVSALEML   Have saved S/A e-mail address?               
         JNZ   PRCEM22H                                                         
         MVC   EMLTOADR(60),WKELEM01+60                                         
         MVI   EMLTOADR+60,X'FF'   End of e-mail list (not an array)            
         J     PRCEM22X                                                         
*                                                                               
PRCEM22H OC    SVSALEML,SVSALEML   Have saved S/A e-mail address?               
         JZ    EXITX                                                            
         MVC   EMLTOADR(L'SVSALEML),SVSALEML                                    
         MVI   EMLTOADR+60,X'FF'   End of e-mail list (not an array)            
         J     PRCEM22X                                                         
*                                                                               
PRCEM22M MVC   EMLTOADR(60),WKELEM01                                            
         TM    WKBYTE01,X'80'      E-mail S/A only?                             
         JNZ   PRCEM22R                                                         
         OC    SVSALEML,SVSALEML   Have saved S/A e-mail address?               
         JZ    *+14                                                             
         MVC   EMLTOADR+60(60),SVSALEML                                         
         J     PRCEM22X                                                         
         OC    WKELEM01+60(60),WKELEM01+60   Have S/A e-mail address?           
         JZ    PRCEM22P                                                         
         MVC   EMLTOADR+60(60),WKELEM01+60                                      
         J     PRCEM22X                                                         
*                                                                               
PRCEM22P MVI   EMLTOADR+60,X'FF'   End of e-mail list (not an array)            
         J     PRCEM22X                                                         
*                                                                               
PRCEM22R OC    SVSALEML,SVSALEML   Have saved S/A e-mail address?               
         JZ    *+14                                                             
         MVC   EMLTOADR(60),SVSALEML                                            
         J     PRCEM22P                                                         
         OC    WKELEM01+60(60),WKELEM01+60   Have S/A e-mail address?           
         JZ    EXITX                                                            
         MVC   EMLTOADR(60),WKELEM01+60                                         
         J     PRCEM22P                                                         
*                                                                               
PRCEM22X MVI   EMLTOEND,X'FF'      End of "To" list                             
*                                                                               
         CLC   SVSTAUEM,SPACE150   Have station user e-mail?                    
         JNH   *+14                                                             
         MVC   EMLFMADR(L'SVSTAUEM),SVSTAUEM                                    
         J     *+10                                                             
         MVC   EMLFMADR(60),EMALFROM                                            
         MVI   EMLCCEND,X'FF'      End of "Cc" list                             
         MVC   EMLBCADR(L'EMALBCC_),EMALBCC_                                    
         MVI   EMLBCEND,X'FF'      End of "Bcc" list                            
*                                                                               
         MVC   EMLSUBJ,SPACE150                                                 
         MVC   EMLSUBJ+0(7),=C'Station'                                         
         MVC   EMLSUBJ+8(5),SVCONSTA                                            
         LA    RE,EMLSUBJ+7+1+5+1                                               
*                                                                               
         CLI   WKEMAILM,EMLSENDQ   Send?                                        
         JNE   *+14                                                             
         MVC   0(L'E_RETURN,RE),E_RETURN                                        
         AHI   RE,L'E_RETURN+1                                                  
*                                                                               
         CLI   WKEMAILM,EMLCONFQ   Confirmed?                                   
         JNE   *+14                                                             
         MVC   0(L'E_CONFIR,RE),E_CONFIR                                        
         AHI   RE,L'E_CONFIR+1                                                  
*                                                                               
         CLI   WKEMAILM,EMLPCONQ   Partially confirmed?                         
         JNE   *+14                                                             
         MVC   0(L'E_PARTCF,RE),E_PARTCF                                        
         AHI   RE,L'E_PARTCF+1                                                  
*                                                                               
         CLI   WKEMAILM,EMLMGS_Q   Makegood send?                               
         JNE   *+14                                                             
         MVC   0(L'E_MGSEND,RE),E_MGSEND                                        
         AHI   RE,L'E_MGSEND+1                                                  
*                                                                               
         MVC   0(L'CON#CHAR,RE),CON#CHAR                                        
         AHI   RE,L'CON#CHAR+1                                                  
         MVC   0(14,RE),=C'for advertiser'                                      
         AHI   RE,14+1                                                          
         MVC   0(L'CONADVN,RE),CONADVN                                          
*                                                                               
         LA    RE,EMLDATA          Start of variable string                     
         LA    R0,40                                                            
         MVC   0(100,RE),SPACE150                                               
         LA    RE,100(RE)          Init to spaces                               
         JCT   R0,*-10                                                          
*                                                                               
         LA    R3,EMLDATA                                                       
         MVC   0(2,R3),=AL2(02)    Length of 1st line - blank                   
         MVC   2(2,R3),SPACE150                                                 
         AHI   R3,2+2              Point to 2nd line                            
*                                                                               
         CLI   WKEMAILM,EMLSENDQ   Send?                                        
         JNE   *+10                                                             
         MVC   02(L'RETURNTX,R3),RETURNTX                                       
         CLI   WKEMAILM,EMLCONFQ   Confirmed?                                   
         JNE   *+10                                                             
         MVC   02(L'CONFIRTX,R3),CONFIRTX                                       
         CLI   WKEMAILM,EMLPCONQ   Partially confirmed?                         
         JNE   *+10                                                             
         MVC   02(L'PARTCFTX,R3),PARTCFTX                                       
         CLI   WKEMAILM,EMLMGS_Q   Makegood send?                               
         JNE   *+10                                                             
         MVC   02(L'OFFERDTX,R3),OFFERDTX                                       
*                                                                               
         MVC   27+0(6,R3),=C'ORDER#'                                            
         MVC   27+7(8,R3),CON#CHAR                                              
         MVC   0(2,R3),=AL2(42)    Length of 2nd line                           
         AHI   R3,42+2             Point to 3rd line                            
*                                                                               
         MVC   0(2,R3),=AL2(02)    Length of 3rd line - blank                   
         MVC   2(2,R3),SPACE150                                                 
         AHI   R3,2+2              Point to 4th line                            
*                                                                               
         MVC   02(20,R3),TWAAGNM2                                               
         MVC   25(20,R3),WSALEXP                                                
         MVI   48(R3),C'$'                                                      
         EDIT  (B4,SVORDTOT),(10,49(R3)),2,ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
         CLI   WKEMAILM,EMLSENDQ   Send?                                        
         JNE   PRCEM32F                                                         
         MVC   62(4,R3),=C'Ver#'                                                
         LR    R4,R3                                                            
         AHI   R4,62+4+1                                                        
         EDIT  (B1,SVVERSN#),(3,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                  
         J     PRCEM32X                                                         
*                                                                               
PRCEM32F CLI   WKEMAILM,EMLCONFQ   Confirmed?                                   
         JNE   PRCEM32J                                                         
PRCEM32G MVC   62(4,R3),=C'Mod#'                                                
         LR    R4,R3                                                            
         AHI   R4,62+4+1                                                        
         CLI   SVMODNUM,X'FF'                                                   
         JE    PRCEM32X                                                         
         EDIT  (B1,SVMODNUM),(3,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                  
         J     PRCEM32X                                                         
*                                                                               
PRCEM32J CLI   WKEMAILM,EMLPCONQ   Partially confirmed?                         
         JNE   PRCEM32M                                                         
         J     PRCEM32G                                                         
*                                                                               
PRCEM32M CLI   WKEMAILM,EMLMGS_Q   Makegood send?                               
         JNE   PRCEM32R                                                         
         J     PRCEM32X                                                         
*                                                                               
PRCEM32R DC    H'0'                Unsupported mode                             
*                                                                               
PRCEM32X MVC   0(2,R3),=AL2(90)    Length of 4th line                           
         AHI   R3,90+2             Point to 5th line                            
*                                                                               
         MVC   0(2,R3),=AL2(02)    Length of 5th line - blank                   
         MVC   2(2,R3),SPACE150                                                 
         AHI   R3,2+2              Point to 6th line                            
*                                                                               
         MVC   0(2,R3),=AL2(02)    Length of 6th line - blank                   
         MVC   2(2,R3),SPACE150                                                 
         AHI   R3,2+2              Point to 7th line                            
*                                                                               
         CLI   WKEMAILM,EMLCONFQ   Confirmed?                                   
         JE    PRCEM36                                                          
         CLI   WKEMAILM,EMLMGS_Q   Makegood send?                               
         JE    PRCEM36                                                          
         MVC   0(2,R3),=AL2(14)    Length of 7th line                           
         MVC   2(14,R3),=C'Order Comment:'                                      
         AHI   R3,14+2             Point to 8th line                            
*                                                                               
         CLI   WKEMAILM,EMLPCONQ   Partially confirmed?                         
         JNE   PRCEM34                                                          
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R5,KEY                                                           
         USING RCFCKEY,R5                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,SVCONREP                                                
         MVC   RCFCKCON,SVCONCON                                                
         GOTO1 VHIGH                                                            
         CLC   RCFCKEY,KEYSAVE     Have partial confirmation comment?           
         JNE   PRCEM36                                                          
         TM    RCFCCNTL-2,X'80'    Deleted?                                     
         JNZ   PRCEM36                                                          
         DROP  R5                                                               
         GOTO1 VGETREC,DMCB,AWKAIO1                                             
         L     R5,AWKAIO1                                                       
         LA    R5,FRSTELEM(R5)                                                  
         USING RCFCTEL,R5                                                       
         MVI   ELCODE,X'02'        Looking for comment text lines elem          
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
PRCEM33M BRAS  RE,NXTEL                                                         
         JNE   PRCEM36                                                          
         LLC   RE,RCFCTLEN                                                      
         AHI   RE,-(RCFCTEXT-RCFCTEL)                                           
         BCTR  RE,0                                                             
         BASR  RF,0                                                             
         MVC   2(0,R3),RCFCTEXT                                                 
         EX    RE,0(RF)                                                         
         AHI   RE,1                Length of comment                            
         STCM  RE,3,0(R3)                                                       
         AR    R3,RE                                                            
         AHI   R3,2                Point to next e-mail data line               
         J     PRCEM33M                                                         
         DROP  R5                                                               
*                                                                               
PRCEM34  MVI   ELCODE,X'92'        Set to process Sta order comments            
         LA    R5,RCONREC          Point to contract record                     
         LA    R5,FRSTELEM(R5)                                                  
         CLC   ELCODE,0(R5)                                                     
         JE    *+12                                                             
PRCEM34M BRAS  RE,NXTEL                                                         
         JNE   PRCEM36                                                          
         LLC   RE,1(R5)            Comment element length                       
         AHI   RE,-3               Adjust elem overhead & EX                    
         BASR  RF,0                                                             
         MVC   2(0,R3),2(R5)       Move comment to e-mail data line             
         EX    RE,0(RF)                                                         
         AHI   RE,1                Length of comment                            
         STCM  RE,3,0(R3)                                                       
         AR    R3,RE                                                            
         AHI   R3,2                Point to next e-mail data line               
         J     PRCEM34M                                                         
*                                                                               
PRCEM36  XC    0(2,R3),0(R3)       End of message                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCEM50  LA    R1,SMTPC            Establish email parameter list               
         USING SMTPD,R1                                                         
         XC    SMTPC(36),SMTPC     Init parameter block                         
*                                                                               
         LA    RF,EMLTOADR                                                      
         ST    RF,SMTPTO           Set To address                               
         LA    RF,EMLCCADR                                                      
         CLI   EMLCCADR,C' '       Anything in CC address?                      
         JNH   *+8                 No - leave SMTPCC nulls                      
         ST    RF,SMTPCC           Set Cc address                               
         LA    RF,EMLBCADR                                                      
         ST    RF,SMTPBCC          Set Bcc address                              
         LA    RF,EMLFMADR                                                      
         ST    RF,SMTPFROM         Set From address                             
         LA    RF,EMLSUBJ                                                       
         ST    RF,SMTPSUB          Set Subject address                          
         LA    RF,EMLDATA                                                       
         ST    RF,SMTPDATA         Set body of email address                    
         LA    RF,SMTPMAIL                                                      
         ST    RF,SMTPVRLN         Indicate data is variable                    
*                                                                               
         LA    RF,EMALFROM         Set sender/from who email address            
         ST    RF,SMTPSNDR                                                      
*                                                                               
         MVC   EMLJMOPT,JMOPTION   'Longer from'/'sender'/'replyto'             
         LA    RF,EMLJMOPT                                                      
         ST    RF,SMTPOPTS                                                      
*                                                                               
         CLI   EMLRPLY,C' '        Anything ?                                   
         BH    PRCEM70             Yes - set reply                              
         MVI   EMLJMOPT+L'FROMLONG+L'SENDER,X'FF'    No reply field             
         J     PRCEM80             Done                                         
*                                                                               
PRCEM70  LA    RF,EMLRPLY          set reply address                            
         ST    RF,SMTPRPLY                                                      
*                                                                               
PRCEM80  GOTOR VJESMAIL,(R1)       send e-mail                                  
         DROP  R1                                                               
*                                                                               
         J     EXITX                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE RECNTWR2K                                                      
         PRINT ON                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RECNT81D DSECT                                                                  
*                                                                               
RELO81   DS    F                                                                
MQMSGLEN DS    F                   Length of MQ message                         
*                                                                               
VDEMOCON DS    A                   Address of DEMOCON                           
VMQIO    DS    A                   Address of CMQIO from COMFACS                
VJESMAIL DS    A                   Address of JESMAIL from COMFACS              
WKAMQMST DS    A                   Address of start of MQ message               
*                                                                               
AWKAIO1  DS    A                                                                
AMQMSG_  DS    A                                                                
*                                                                               
WKMQMODE DS    CL1                 MQ message calling mode                      
MQMACTSQ EQU   C'A'                Process message for contract actions         
MQMWIPSQ EQU   C'W'                Process message for WIPS                     
MQMTRF#Q EQU   C'T'                Process message for traffic order#           
*                                                                               
WKEMAILM DS    CL1                 E-mail mode                                  
EMLSENDQ EQU   C'S'                E-mail triggered by SEND                     
EMLCONFQ EQU   C'C'                E-mail triggered by CONFIRM                  
EMLPCONQ EQU   C'P'                E-mail triggered by PARTIAL CONFIRM          
EMLMGS_Q EQU   C'M'                E-mail triggered by MGS                      
*                                                                               
WKBYTE01 DS    XL1                                                              
WKBYTE02 DS    XL1                                                              
WKELEM01 DS    XL256                                                            
WKELEM02 DS    XL256                                                            
*                                                                               
SVPRDNAM DS    CL(L'RCONEXPR)      Expanded product name                        
SVADVNAM DS    CL(L'RADVNAME)      Advertiser name                              
SVOFFNAM DS    CL(L'ROFFNAME)      Office name                                  
SVSALNAM DS    CL(L'RSALNAME)      Salesperson name                             
*                                                                               
SVMQMFRM DS    CL(3+L'CON#CHAR)    MQ message from CON program                  
SVRSTAP6 DS    CL(L'RSTAP6)        Station profile 6                            
SVCONREP DS    CL(L'RCONKREP)      Saved contract key fields                    
SVCONSTA DS    CL(L'RCONKSTA)                                                   
SVCON9CM DS    XL(L'RCONPCON)      Contract # in 9's complement                 
SVCONCON DS    XL(L'RCONKCON)                                                   
SVAGYOFF DS    CL(L'RCONKAGY+1+L'RCONKOFF)                                      
*                                                                               
CON#CHAR DS    CL8                 Contract number in character format          
CONWSIDE DS    CL1                 Contract side - Rep/Station/Both             
CONSNDSW DS    CL1                 Contract has SEND elem switch - Y/N          
SVVERSN# DS    XL1                 Latest version number                        
MSSAGEID DS    XL24                Message ID                                   
WIPSFLAG DS    CL1                                                              
MQMSGSTA DS    CL19                MQ message status                            
*                                                                               
SVTRORD# DS    CL(L'RCONTRF)       Save traffic order number                    
SVMODNUM DS    XL(L'RCONMOD)       Save contract mod number                     
SVMODELM DS    CL1                 Save mod date/time element switch            
SVCURMOD DS    XL(L'RMODEL1M)      Save contract most recent mod #              
SVCURVER DS    XL(L'RMODEL1V)      Save contract most recent version #          
SVLASSDT DS    XL(L'RCONSRDT)      Save last send date                          
SVSNDTIM DS    CL(L'RCONSRTI)      Save last send time                          
SVFLSTDT DS    XL3                 Save flight start date                       
SVFLENDT DS    XL3                 Save flight end date                         
SV#WEEKS DS    XL(L'RCONWKS)       Save number of weeks                         
SVCONTYP DS    CL(L'RCONTYPE)      Save contract type                           
SVCONSAL DS    CL(L'RCONSAL)       Save contract salesperson code               
SVCABNAM DS    CL(L'RCONBUYR)      Save contract agency buyer name              
SVAGYCON DS    CL6                 Save contract agency connect                 
SVCPE___ DS    CL20                Save C/P/E                                   
SVPRIDEM DS    CL6                 Save primary demographic code                
SVECDATE DS    XL(L'RCONECDT)      Save last EC send date (compressed)          
SVORDTOT DS    XL(L'RCONTOT)       Save order total                             
SVCONFSW DS    XL(L'RCONCONF)      Save confirmation switch                     
SVAUDSAG DS    XL(L'RAUDTHAG)      Save SEND security agency                    
SVAUDPID DS    XL(L'RAUDTHPD)      Save SEND PID (binary)                       
SVPIDCL8 DS    CL(L'SAPALPID)      Save SEND PID (character format)             
SVAUDANM DS    CL(L'RAUDTHAS)      Save sales assistant name                    
SVOFRPND DS    CL1                 Save makegood offer pending flag             
SVSTAOFR DS    CL1                 Save station created offer flag              
SVSTASHR DS    CL7                 Save station share                           
SVCTYDES DS    CL(L'RCTYDESC)      Save contract type name                      
SVSTAUEM DS    CL60                Save station user e-mail address             
SVSALEML DS    CL60                Save SA e-mail address                       
*                                                                               
REPPRGPF DS    (RREPPGML*15)X      Set of 15 rep program profiles               
SOMPBITQ EQU   ((RREPQSOM-1)*RREPPGML)+2     Displacement to prof bits          
R_SOMPRF DS    CL64                Bit translated SOM program profile           
*                                                                               
* JESMAIL PARAMETER BLOCK                                                       
*                                                                               
         DS    0F                                                               
SMTPC    DS    XL(SMTPDQ)          PARAMTER BLOCK FOR JES MAIL                  
*                                                                               
* E-MAIL FIELDS                                                                 
*                                                                               
EMLFLDS  DS    0D                                                               
EMLTOADR DS    CL120               To:e-mail address                            
EMLTOEND DS    XL1                 X'FF' end of list                            
EMLCCADR DS    CL180               Cc: e-mail address (up to three)             
EMLCCEND DS    XL1                 X'ff' end of list                            
EMLBCADR DS    CL60                Bcc: e-mail address                          
EMLBCEND DS    XL1                 X'ff' end of list                            
EMLFMADR DS    CL150               From: e-mail address                         
EMLRPLY  DS    CL150               Up to 2 reply to email addresses             
EMLSUBJ  DS    CL100               Subject                                      
EMLJMOPT DS    CL(JMOPTNQ)         Email options                                
EMLDATA  DS    CL4000              Variable string - see fajesmaild             
EMLFLDLN EQU   *-EMLFLDS                                                        
*                                                                               
WKAIO1_  DS    XL4096                                                           
*                                                                               
         DS    0D                                                               
MQMSG    DS    10000C              MQ message build area                        
MQMSGX   DS    0X                                                               
MQMSGLNQ EQU   MQMSGX-MQMSG                                                     
*                                                                               
RECNT81X EQU   *                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FAJESMAILD                                                     
       ++INCLUDE REGLCON           RepPak GLOBBER object equates                
       ++INCLUDE DDGLOBEQUS        GLOBBER requates                             
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
RE_CFC_D DSECT                                                                  
       ++INCLUDE REGENCFC                                                       
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
RE_SAL2D DSECT                                                                  
       ++INCLUDE REGENSAL2                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016RECNT81   04/25/14'                                      
         END                                                                    
