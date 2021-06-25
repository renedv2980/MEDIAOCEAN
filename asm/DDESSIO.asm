*          DATA SET DDESSIO    AT LEVEL 035 AS OF 01/08/09                      
*PHASE ESSIOA                                                                   
*                                                                               
         TITLE 'DDESSION - ESS MAINFRAME APPC IO INTERFACE DRIVER'              
***********************************************************************         
*                                                                     *         
* TITLE:       ESSION - ESS MAINFRAME APPC IO INTERFACE DRIVER        *         
*                                                                     *         
* NOTE:        DDESSION IS NEW VERSION OF DDESSIO WHICH INCLUDES      *         
*              APPC/MVS FACILITIES                                    *         
***********************************************************************         
*                                                                               
         EJECT                                                                  
ESSIO    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**ESSION,=A(R13CHAIN),RA,R9,R8,R5,RR=RE                        
*                                                                               
         LR    R7,RC                                                            
         SH    R7,=H'4'                                                         
         L     R7,0(R7)                                                         
         L     R7,0(R7)            A(R1 PARAMETERS FROM ATTACH)                 
         USING ESSATCD,R7                                                       
         USING ESSAPPCD,R6                                                      
*                                                                               
         LR    R1,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         USING COMMWORK,RC                                                      
         ST    RE,RELO                                                          
*                                                                               
         USING ESSFACSD,RF                                                      
         ICM   RF,15,EATFACS                                                    
         MVC   VDATAMGR,ADATAMGR                                                
         MVC   VDATCON,ADATCON                                                  
         MVC   VDATTIM,ADATTIM                                                  
         MVC   VHEXOUT,AHEXOUT                                                  
         MVC   VHEXIN,AHEXIN                                                    
         MVC   VNUMVAL,ANUMVAL                                                  
         MVC   VEXSERV,AEXSERV                                                  
         MVC   VEIOSQL,AEIOSQL                                                  
         MVC   VEIOPQR,AEIOPQR                                                  
         MVC   VESSLOG,AESSLOG                                                  
         MVC   VLOGIO,ALOGIO                                                    
         MVC   VESSWTO,AESSWTO                                                  
         MVC   VESSAPPC,AESSAPPC                                                
         MVC   VMAJORNM,AMAJORNM                                                
         MVC   VPRINTER,APRINTER                                                
         MVC   VPRNTBL,APRNTBL                                                  
         DROP  RF                                                               
*                                                                               
         LA    R1,EATMTECB         BUILD ECBLIST                                
         STCM  R1,7,EXECBLST+1                                                  
         LA    R1,EATSPECB                                                      
         STCM  R1,7,EXECBLST+5                                                  
*                                                                               
         POST  EATSTECB            SIGNAL TO MAIN TASK AFTER ATTACH             
         WAIT  ECB=EATMTECB                                                     
         XC    EATMTECB,EATMTECB                                                
*                                                                               
         LA    R1,EATSPECB         SET USER ECB IN APPC PARAMETER LIST          
         STCM  R1,15,ALU6ECB                                                    
         XC    EATSPECB,EATSPECB                                                
*                                                                               
         MVI   OPERSTOP,C'N'                                                    
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(22),=C'ESSAPPC#       STARTED'                                 
         SR    RF,RF                                                            
         ICM   RF,3,EATTASKN                                                    
         EDIT  (RF),(5,P+9),ZERO=NOBLANK,FILL=0                                 
         MVC   P+36(6),=C'LUID: '                                               
         MVC   P+42(8),EATELUID                                                 
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         ICM   RE,15,EATESSB                                                    
         USING ESSBD,RE                                                         
         MVC   WTOMSG,SPACES                                                    
         MVC   WTOMSG(8),ESSBJNAM                                               
         MVI   WTOMSG+8,C'.'                                                    
         MVC   WTOMSG+9(8),EATNAME                                              
         MVI   WTOMSG+17,C'.'                                                   
*                                                                               
         MVC   ERRMSG,=CL80'UNDEFINED MESSAGE'                                  
*                                                                               
*                                  BUILD APPCECBL                               
         LA    R1,EATSPECB                                                      
         STCM  R1,7,AAPPCEST+1                                                  
         LA    R1,APPCECB                                                       
         STCM  R1,7,AAPPCECB+1                                                  
*                                                                               
         LA    R1,TESTECB                                                       
         STCM  R1,15,ATESTECB                                                   
         XC    TESTECB,TESTECB                                                  
*                                                                               
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     EQU   *                                                                
*                                  RETURNS HERE IF APPC/MVS                     
MGETREQ  EQU   *                                                                
         MVI   LU6ERROR,C'N'                                                    
         BAS   RE,GETREQ           GET NEXT REQUEST FROM HOSTESS                
         ICM   RF,15,EATAESS                                                    
         ICM   R6,15,ESEAAPPC-ESSESSD(RF)                                       
         CLI   OPERSTOP,C'Y'       SHUTDOWN IF OPERATOR STOP REQUESTED          
         BE    MSHUT                                                            
*                                  PROCESS HOSTESS APPC MESSAGE REQUEST         
         CLI   EATMODE,EATMCONQ                                                 
         BE    MCONNECT                                                         
         CLI   EATMODE,EATMRCVQ                                                 
         BE    MRCV                                                             
         CLI   EATMODE,EATMSNDQ                                                 
         BE    MSEND                                                            
         CLI   EATMODE,EATMTSTQ                                                 
         BE    MTEST                                                            
         DC    H'0'                                                             
*                                                                               
MCONNECT EQU   *                                                                
*                                                                               
         BAS   RE,HANDSHAK         START-UP HANDSHAKE MESSAGE WITH ESS          
         BNE   MERROR                                                           
         CLI   LU6ERROR,C'Y'                                                    
         BE    MDEAL                                                            
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    MCONF                                                            
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    MCONF                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    MCLOS                                                            
         MVC   ECBVALUE,=AL4(EATMCONQ)                                          
         B     MPOST                                                            
*                                                                               
MRCV     EQU   *                                                                
*                                                                               
         BAS   RE,PROCRCV                                                       
         BNE   MERROR                                                           
         CLI   LU6ERROR,C'Y'                                                    
         BE    MCLOS                                                            
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    MCONF                                                            
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    MCONF                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    MCLOS                                                            
         MVC   ECBVALUE,=AL4(EATMRCVQ)                                          
         B     MPOST                                                            
*                                                                               
MSEND    EQU   *                                                                
*                                                                               
         BAS   RE,PROCSND                                                       
         BNE   MERROR                                                           
         CLI   LU6ERROR,C'Y'                                                    
         BE    MCLOS                                                            
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    MCONF                                                            
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    MCONF                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    MDEAL                                                            
         MVC   ECBVALUE,=AL4(EATMSNDQ)                                          
         B     MPOST                                                            
*                                                                               
MTEST    EQU   *                                                                
*                                                                               
         BAS   RE,PROCTST                                                       
         BNE   MERROR                                                           
         CLI   LU6ERROR,C'Y'                                                    
         BE    MCLOS                                                            
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    MCONF                                                            
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    MCONF                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    MDEAL                                                            
         MVC   ECBVALUE,=AL4(EATMTSTQ)                                          
         B     MPOST                                                            
*                                                                               
MERROR   EQU   *                                                                
*                                                                               
         MVC   P(8),EATNAME                                                     
         MVC   P+10(20),=CL20'ESSIO EXCEPTION'                                  
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   ECBVALUE,=AL4(EATMERRQ)                                          
         B     MPOST                                                            
*                                                                               
MCONF    EQU   *                                                                
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BNE   MCLOS               TEST CONFIRM REQUESTED                       
         BAS   RE,CONFAPPC         DO CONFIRM ON DEALLOCATE                     
         BE    MCLOS                                                            
*                                                                               
MDEAL    EQU   *                                                                
*                                                                               
         BAS   RE,DEALAPPC         DEALLOCATE APPC SESSION                      
         B     MCLOS                                                            
*                                                                               
MCLOS    EQU   *                                                                
*                                                                               
         BAS   RE,CLOSAPPC         CLOSE APPC SESSION                           
         CLI   OPERSTOP,C'Y'       CLOSE IF OPERATOR STOP REQUESTED             
         BE    MSHUT                                                            
         MVC   ECBVALUE,=AL4(EATMDISQ)                                          
         B     MPOST                                                            
*                                                                               
MPOST    EQU   *                   POST READY FOR NEXT REQUEST                  
*                                                                               
         ICM   R3,15,ECBVALUE                                                   
         POST  EATSTECB,(R3)       SIGNAL MAIN TASK ECB                         
         MVC   P(40),=CL40'ESSIO POSTED REQUEST COMPLETE ECB'                   
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         CLI   OPERSTOP,C'Y'       CLOSE IF OPERATOR STOP REQUESTED             
         BE    MSHUT                                                            
         B     MGETREQ                                                          
*                                                                               
MSHUT    BAS   RE,SHUTDOWN         SHUT DOWN SUB TASK                           
*                                                                               
         MVI   RETCODE1,X'00'                                                   
         XBASE RC=RETCODE1,RL=1     RETURN                                      
         EJECT                                                                  
***********************************************************************         
*  GET NEXT REQUEST TO SEND TO HOSTESS FROM ESS.                      *         
*  BUILD FIRST MESSAGE IN REQUEST SEQUENCE                            *         
*  SIGNAL MAIN TASK ECB THAT SENDER IS WAITING READY                  *         
*  WAIT ON ECB FOR MAIN TASK TO SIGNAL MESSAGE READY                  *         
***********************************************************************         
         SPACE 1                                                                
GETREQ   NTR1                                                                   
*                                                                               
         MVC   P(40),=CL40'ESSIO WAIT ON NEXT REQUEST'                          
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         WAIT  1,ECBLIST=EXECBLST                                               
*                                                                               
         MVC   WTOMSG+9(8),EATNAME                                              
         MVI   WTOMSG+17,C'.'                                                   
         ICM   RF,15,EATAESS                                                    
         MVC   WTOMSG+18(8),ESEELU-ESSESSD(RF)                                  
*                                                                               
         TM    EATSPECB,X'40'      MAIN TASK POSTED STOP ?                      
         BZ    GREQ010                                                          
         MVI   OPERSTOP,C'Y'                                                    
         XC    EATSPECB,EATSPECB                                                
         MVC   P(40),=CL40'ESSIO PROCESSING OPERATOR STOP'                      
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     GREQX                                                            
*                                                                               
GREQ010  TM    EATMTECB,X'40'      MAIN TASK POSTED READY ?                     
         BO    *+6                                                              
         DC    H'00'                                                            
         MVC   REQCODE,EATMTECB+1                                               
         XC    EATMTECB,EATMTECB                                                
         MVC   P(40),=CL40'HOSTESS REQUEST RECEIVED'                            
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     GREQX                                                            
*                                                                               
GREQX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  WAIT FOR START-UP HANDSHAKE MESSAGE WITH ESS                       *         
***********************************************************************         
         SPACE 1                                                                
HANDSHAK NTR1                                                                   
*                                                                               
         MVC   P(40),=CL40'ESSIO HANDSHAKE'                                     
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         ICM   R4,15,EATDATA                                                    
         USING ESSDATAD,R4                                                      
*                                                                               
HANSREQ  EQU   *                                                                
         BAS   RE,CLEARED                                                       
         BAS   RE,CLRAPPC                                                       
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_ACCESS_TOKEN,ATB_FW0                                     
         MVC   RECEIVE_LENGTH,=A(L'APPC_BUFFER)                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBRCVW '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
HANSRET  EQU   *                                                                
         CLI   OPERSTOP,C'Y'                                                    
         BE    HANSOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    HANSOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    HANSOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   HANSLUER            EXIT IF RETURN CODE ERROR                    
         CLC   DATA_RECEIVED,ATB_FW2                                            
         BNE   HANSLUER            EXIT IF DATA BUFFER NOT COMPLETE             
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   HANSLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
         CLC   STATUS_RECEIVED,ATB_FW1                                          
         BNE   HANSLUER                                                         
*                                  ERROR OF ZERO LENGTH RECEIVED ??             
         OC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         BZ    HANSLUER                                                         
*                                                                               
         CLC   APPC_BUFFER_LENGTH,RECEIVE_LENGTH+2                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APPCDLEN,RECEIVE_LENGTH+2                                        
         BAS   RE,VALMSTR          VALIDATE MESSAGE DATA STRUCTURE              
         BNE   HANSERR                                                          
         BAS   RE,VALHEAD          VALIDATE MESSAGE HEADER FIELDS               
         BNE   HANSERR                                                          
         BAS   RE,VALDATA          VALIDATE HANDSHAKE MESSAGE DATA              
         BNE   HANSERR                                                          
*                                  CONNECT WITH HANDSHAKE MSG DATA              
         BAS   RE,VALHEXC                                                       
         BNE   HANSERR                                                          
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(40),=CL40'ESS HANDSHAKE PROCESSED'                             
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         EJECT                                                                  
*                                  PROCESS HANDSHAKE MESSAGE RESPONSE           
HANSRSP  EQU   *                                                                
         MVC   WTOMSG+9(8),EATNAME                                              
*                                  RETURN HANDSHAKE DATA MESSAGE                
         OI    ESSDMFF,ESSHLSTQ                                                 
         MVI   ESSDHTYP,ESSHDATQ                                                
         BAS   RE,BLDHEAD          BUILD RETURN MESSAGE HEADER                  
         BNE   HANSNO                                                           
         BAS   RE,BLDHEXC                                                       
         BNE   HANSNO                                                           
*                                  SEND RETURN DATA MESSAGE                     
*                                                                               
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         CLI   ESSDMODE,ESSDMRCQ    SEND AND FLUSH IF ESSLUSND                  
         BE    *+10                                                             
         MVC   SEND_TYPE,ATB_SEND_AND_FLUSH                                     
         MVC   SEND_ACCESS_TOKEN,ATB_FW0                                        
         SR    RF,RF                                                            
         ICM   RF,3,APPCDLEN                                                    
         STCM  RF,3,APPC_BUFFER_LENGTH                                          
         STCM  RF,15,SEND_LENGTH                                                
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBSEND '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BE    HANSOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    HANSOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    HANSOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   HANSLUER            EXIT IF RETURN CODE ERROR                    
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   HANSLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
         B     HANSOK                                                           
*                                                                               
HANSECB  EQU   *                   EXIT IF ECB POSTED                           
         MVI   OPERSTOP,C'Y'                                                    
         B     HANSOK                                                           
*                                                                               
HANSERR  EQU   *                   RETURN ERROR MESSAGE                         
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'HANDSHAKE MESSAGE ERROR'                          
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     HANSNO                                                           
*                                                                               
HANSLUER EQU   *                   LU6 RETURN ERROR MESSAGE                     
         MVC   P(10),=CL10'HANDSHAKE'                                           
         GOTO1 =A(PRNTLUER),(RC)                                                
         MVI   LU6ERROR,C'Y'                                                    
         B     HANSOK                                                           
*                                                                               
HANSNO   B     NO                                                               
HANSOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  PROCESS HOSTESS RECEIVER MODE - GET REQUESTS FROM ESS              *         
***********************************************************************         
         SPACE 1                                                                
PROCRCV  NTR1                                                                   
*                                                                               
         MVC   P(40),=CL40'ESSIO RECEIVE'                                       
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         ICM   R4,15,EATDATA                                                    
         USING ESSDATAD,R4                                                      
*                                                                               
         MVI   ESSDMODE,ESSDMRCQ   SET RECEIVE MODE CODE                        
         CLI   EATSTATE,EATSRQPQ                                                
         BE    PRCVREQ                                                          
         CLI   EATSTATE,EATSRSPQ                                                
         BE    PRCVRSP                                                          
         DC    H'0'                                                             
*                                                                               
PRCVREQ  EQU   *                                                                
         BAS   RE,CLEARED                                                       
         BAS   RE,CLRAPPC                                                       
         MVI   ESSDSEQN,ESSDSFSQ   INITIALISE MESSAGE SEQUENCE CODE             
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_ACCESS_TOKEN,ATB_FW0                                     
         MVC   RECEIVE_LENGTH,=A(L'APPC_BUFFER)                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBRCVW '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BE    PRCVOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    PRCVOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    PRCVOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   PRCVLUER            EXIT IF RETURN CODE ERROR                    
         CLC   DATA_RECEIVED,ATB_FW2                                            
         BNE   PRCVLUER            EXIT IF DATA BUFFER NOT COMPLETE             
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   PRCVLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
*                                                                               
*                                  ERROR IF ZERO LENGTH RECEIVED ??             
         OC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         BZ    PRCVLUER                                                         
*                                                                               
         CLC   APPC_BUFFER_LENGTH,RECEIVE_LENGTH+2                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APPCDLEN,RECEIVE_LENGTH+2                                        
         BAS   RE,VALMSTR          VALIDATE MESSAGE DATA STRUCTURE              
         BNE   PRCVERR                                                          
         BAS   RE,VALHEAD          VALIDATE MESSAGE HEADER FIELDS               
         BNE   PRCVERR                                                          
         BAS   RE,VALDATA          VALIDATE HANDSHAKE MESSAGE DATA              
         BNE   PRCVERR                                                          
         TM    ESSDPCF,ESSHLSTQ                                                 
         BO    PRCVLAST                                                         
         DC    H'0'                                                             
*                                                                               
         CLC   STATUS_RECEIVED,ATB_FW1                                          
         BE    PRCVLUER            ERROR IF STAT_RCV SWITCH TO SEND             
         MVI   ESSDSEQN,ESSDSNXQ   SET MESSAGE SEQUENCE CODE                    
*                                                                               
         BAS   RE,CLRAPPC                                                       
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_ACCESS_TOKEN,ATB_FW0                                     
         MVC   RECEIVE_LENGTH,=A(L'APPC_BUFFER)                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBRCVW '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BE    PRCVOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    PRCVOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    PRCVOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   PRCVLUER            EXIT IF RETURN CODE ERROR                    
         CLC   DATA_RECEIVED,ATB_FW2                                            
         BNE   PRCVLUER            EXIT IF DATA BUFFER NOT COMPLETE             
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   PRCVLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
*                                                                               
*                                  ERROR IF ZERO LENGTH RECEIVED ??             
         OC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         BZ    PRCVLUER                                                         
*                                                                               
         MVC   P(40),=CL40'ESSIO REQUEST RECEIVED'                              
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVI   ESSDMODE,ESSDMRCQ   SET RECEIVE NEXT MESSAGE MODE CODE           
         BAS   RE,PROCREQ          PROCESS REQUEST                              
         BNE   PRCVERR                                                          
         B     PRCVREQ                                                          
*                                                                               
PRCVLAST EQU   *                                                                
         CLC   STATUS_RECEIVED,ATB_FW1                                          
         BNE   PRCVLUER            OK IF STAT_RCV SWITCH SEND                   
         MVI   ESSDSEQN,ESSDSFSQ   SET MESSAGE SEQUENCE CODE                    
         MVI   ESSDMODE,ESSDMRCQ   SET RECEIVE LAST MESSAGE MODE CODE           
*                                                                               
         MVC   P(40),=CL40'ESSIO REQUEST RECEIVED'                              
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVI   ESSDMODE,ESSDMRCQ   SET RECEIVE NEXT MESSAGE MODE CODE           
         BAS   RE,PROCREQ          PROCESS REQUEST                              
         BNE   PRCVERR                                                          
         B     PRCVOK                                                           
         EJECT                                                                  
PRCVRSP  EQU   *                                                                
         OI    ESSDMFF,ESSHLSTQ                                                 
*                                                                               
         BAS   RE,BLDHEAD          BUILD RETURN MESSAGE HEADER                  
         BNE   PRCVNO                                                           
         BAS   RE,BLDDATA          BUILD RETURN MESSAGE DATA                    
         BNE   PRCVNO                                                           
         MVC   P(40),=CL40'ESSIO REQUEST ANSWERED'                              
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVC   SEND_ACCESS_TOKEN,ATB_FW0                                        
         SR    RF,RF                                                            
         ICM   RF,3,APPCDLEN                                                    
         STCM  RF,3,APPC_BUFFER_LENGTH                                          
         STCM  RF,15,SEND_LENGTH                                                
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBSEND '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BE    PRCVOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    PRCVOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    PRCVOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   PRCVLUER            EXIT IF RETURN CODE ERROR                    
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   PRCVLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
         B     PRCVOK              EXIT OK                                      
*                                                                               
PRCVERR  EQU   *                   RETURN ERROR MESSAGE                         
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'ESSIO RECEIVER MESSAGE ERROR'                     
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     PRCVNO                                                           
*                                                                               
PRCVLUER EQU   *                   RETURN ERROR MESSAGE                         
         MVC   P(10),=CL40'RECEIVER'                                            
         GOTO1 =A(PRNTLUER),(RC)                                                
         MVI   LU6ERROR,C'Y'                                                    
         B     PRCVOK                                                           
*                                                                               
PRCVECB  EQU   *                   RETURN STOP ECB POSTED                       
         MVI   OPERSTOP,C'Y'                                                    
         B     PRCVOK                                                           
*                                                                               
PRCVNO   B     NO                                                               
PRCVOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  PROCESS HOSTESS SENDER MODE - SEND HOSTESS REQUESTS TO ESS         *         
***********************************************************************         
         SPACE 1                                                                
PROCSND  NTR1                                                                   
*                                                                               
         MVI   FTPFLAG,C'N'                                                     
         ICM   RE,15,EATESSB                                                    
         USING ESSBD,RE                                                         
         TM    ESSBSTA1,ESSBSFTP   FTP=YES FOR THIS HOSTESS?                    
         BZ    PS_NFSX             NO                                           
         TM    EATHFLG1,GESSFNFQ   FTP ENABLED FOR ESS ID?                      
         BZ    PS_NFSX             YES                                          
         DROP  RE                                                               
         MVI   FTPFLAG,C'Y'                                                     
PS_NFSX  EQU   *                                                                
         MVC   P+36(8),=C'FTPFLAG='                                             
         MVC   P+45(1),FTPFLAG                                                  
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         MVC   P(40),=CL40'ESSIO SENDER'                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         ICM   R4,15,EATDATA                                                    
         USING ESSDATAD,R4                                                      
         MVC   ESSDSID,EATENUM     SET ESS ID NUMBER                            
         MVI   ESSDMODE,ESSDMSNQ   SET SEND MODE CODE                           
         CLI   EATSTATE,EATSRQPQ                                                
         BE    PSNDREQ                                                          
*                                                                               
         CLI   EATSTATE,EATSRSPQ                                                
         BE    PSNDRSP                                                          
         DC    H'0'                                                             
*                                  PROCESS SENDER REQUEST                       
PSNDREQ  EQU   *                                                                
         MVI   ESSDSEQN,ESSDSFSQ   SET MESSAGE SEQUENCE CODE                    
         BAS   RE,BLDHEAD          BUILD SENDER MESSAGE HEADER                  
         BNE   PSNDNO                                                           
         BAS   RE,BLDDATA          BUILD SENDER MESSAGE DATA                    
         BNE   PSNDNO                                                           
*                                                                               
         CLI   FTPFLAG,C'Y'        FTP ENABLED?                                 
         BNE   PSREQ20                                                          
*                                  EDIT THE SENDER MESSAGE HEADER               
         L     R1,AAPPCBUF                                                      
         LA    R1,L'APPC_BUFFER_LENGTH(R1)                                      
         USING ESSHDR,R1                                                        
         OI    ESSHPCF,ESSHNFSQ    FILE READY TO SEND VIA NFS                   
         DROP  R1                                                               
*                                                                               
PSREQ20  EQU   *                                                                
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVC   SEND_ACCESS_TOKEN,ATB_FW0                                        
         SR    RF,RF                                                            
         ICM   RF,3,APPCDLEN                                                    
         STCM  RF,3,APPC_BUFFER_LENGTH                                          
         STCM  RF,15,SEND_LENGTH                                                
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBSEND '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BE    PSNDOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    PSNDOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    PSNDOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   PSNDLUER            EXIT IF RETURN CODE ERROR                    
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   PSNDLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
         B     PSNDOK                                                           
         EJECT                                                                  
*                                  PROCESS SENDER RESPONSE                      
PSNDRSP  EQU   *                                                                
         MVI   ESSDSEQN,ESSDSLSQ   SET MESSAGE SEQUENCE CODE                    
         BAS   RE,CLRAPPC                                                       
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_ACCESS_TOKEN,ATB_FW0                                     
         MVC   RECEIVE_LENGTH,=A(L'APPC_BUFFER)                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBRCVW '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BE    PSNDOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    PSNDOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    PSNDOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   PSNDLUER            EXIT IF RETURN CODE ERROR                    
         CLC   DATA_RECEIVED,ATB_FW2                                            
         BNE   PSNDLUER            EXIT IF DATA BUFFER NOT COMPLETE             
* ??     BNE   PSNDOK              EXIT IF DATA BUFFER NOT COMPLETE             
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   PSNDLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
         CLC   STATUS_RECEIVED,ATB_FW1                                          
         BNE   PSNDLUER                                                         
*                                  ERRO IF ZERO LENGTH RECEIVED ??              
         OC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         BZ    PSNDLUER                                                         
*                                                                               
         CLC   APPC_BUFFER_LENGTH,RECEIVE_LENGTH+2                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APPCDLEN,RECEIVE_LENGTH+2                                        
         BAS   RE,VALMSTR          VALIDATE MESSAGE STRUCTURE                   
         BNE   PSNDERR                                                          
         BAS   RE,VALHEAD          VALIDATE MESSAGE HEADER FIELDS               
         BNE   PSNDERR                                                          
         BAS   RE,VALDATA          VALIDATE MESSAGE DATA                        
         BNE   PSNDERR                                                          
         CLI   ESSDBLKD,C'Y'       TEST IF BULK DATA DOWNLOAD                   
         BE    PSNDOK                                                           
*                                                                               
         CLI   ESSDMTYP,ESSHERRQ   ERROR MESSAGE?                               
         BE    PSRSP30             SKIP SENDING FILE VIA NFS                    
*                                                                               
         L     R1,AAPPCBUF                                                      
         LA    R1,L'APPC_BUFFER_LENGTH(R1)                                      
         USING ESSHDR,R1                                                        
         TM    ESSHPCF,ESSHNFSQ    TEST IF NFS TRANSFER                         
         BO    PSNDNFS                                                          
         DROP  R1                                                               
*                                                                               
PSRSP30  EQU   *                                                                
         BAS   RE,PROCREQ          PROCESS DATA AFTER REQUEST SENT              
         BNE   PSNDERR                                                          
         BAS   RE,CLEARED                                                       
         B     PSNDOK                                                           
*                                                                               
PSNDECB  EQU   *                   RETURN STOP ECB POSTED                       
         MVI   OPERSTOP,C'Y'                                                    
         B     PSNDOK                                                           
*                                                                               
PSNDERR  EQU   *                   RETURN ERROR MESSAGE                         
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'ESSIO SENDER MESSAGE ERROR'                       
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     PSNDNO                                                           
*                                                                               
PSNDPSE  EQU   *                   PRODUCT SPECIFIC ERROR                       
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'SEND APPC PRODUCT SPECIFIC ERROR'                 
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   P(10),=CL40'SENDER'                                              
         GOTO1 =A(PRNTLUER),(RC)                                                
         BAS   RE,DEALAPPC         DEALLOCATE APPC SESSION                      
         B     PSNDNO                                                           
*                                                                               
PSNDLUER EQU   *                   RETURN ERROR MESSAGE                         
         MVC   P(10),=CL40'SENDER'                                              
         GOTO1 =A(PRNTLUER),(RC)                                                
         MVI   LU6ERROR,C'Y'                                                    
         B     PSNDOK                                                           
*                                  PROCESS SENDER NFS TRANSFER                  
PSNDNFS  EQU   *                                                                
         MVI   ESSDSEQN,ESSDSNXQ   SET MESSAGE SEQUENCE CODE                    
         MVI   EATSTATE,EATSRQPQ   REQ PENDING FOR ESS RESPOND BACK             
*        BAS   RE,BLDHEAD          BUILD SENDER MESSAGE HEADER                  
*        BNE   PSNDNO                                                           
*        BAS   RE,BLDDATA          BUILD SENDER MESSAGE DATA                    
*        BNE   PSNDNO                                                           
*                                                                               
         L     R1,AAPPCBUF                                                      
         LA    R1,L'APPC_BUFFER_LENGTH(R1)                                      
         USING ESSHDR,R1                                                        
         NI    ESSHPCF,X'FF'-ESSHNFSQ    END OF NFS SEND                        
         DROP  R1                                                               
*                                                                               
         CLI   FTPFLAG,C'Y'        FTP ENABLED?                                 
         BNE   PSNDNFSX                                                         
         BRAS  RE,CPNFS            CP FILE THROUGH NFS                          
         BE    PSNDNFSX                                                         
*&&US*&& BRAS  RE,CPNFS_X          TRY THE OLD WAY                              
         BNE   PSNDERR                                                          
PSNDNFSX EQU   *                                                                
*                                                                               
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVC   SEND_ACCESS_TOKEN,ATB_FW0                                        
         SR    RF,RF                                                            
         ICM   RF,3,APPCDLEN                                                    
         STCM  RF,3,APPC_BUFFER_LENGTH                                          
         STCM  RF,15,SEND_LENGTH                                                
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBSEND '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BE    PSNDOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    PSNDOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    PSNDOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   PSNDLUER            EXIT IF RETURN CODE ERROR                    
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   PSNDLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
         B     PSNDOK                                                           
         EJECT                                                                  
*                                                                               
PSNDNO   B     NO                                                               
PSNDOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  TEST ESS CONNECTION                                                *         
***********************************************************************         
         SPACE 1                                                                
PROCTST  NTR1                                                                   
*                                                                               
         MVC   P(40),=CL40'ESSIO TEST'                                          
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         ICM   R4,15,EATDATA                                                    
         USING ESSDATAD,R4                                                      
         MVC   ESSDSID,EATENUM     SET ESS ID NUMBER                            
         MVI   ESSDMODE,ESSDMSNQ   SET SEND MODE CODE                           
         CLI   EATSTATE,EATSRQPQ                                                
         BE    PTSTREQ                                                          
         CLI   EATSTATE,EATSRSPQ                                                
         BE    PTSTRSP                                                          
         DC    H'0'                                                             
*                                  PROCESS TEST MESSAGE REQUEST                 
PTSTREQ  EQU   *                                                                
         MVC   WTOMSG+9(8),EATNAME                                              
*                                                                               
         MVI   ESSDSEQN,ESSDSFSQ   SET MESSAGE SEQUENCE CODE                    
         MVI   ESSDMFF,ESSHLSTQ                                                 
         MVI   ESSDHTYP,ESSHDATQ                                                
         MVI   ESSDMTYP,ESSHCTLQ                                                
         XC    ESSDDLN,ESSDDLN                                                  
         MVC   ESSDMID,=AL3(ESSHHANQ)                                           
         BAS   RE,BLDHEAD          BUILD MESSAGE HEADER                         
         BNE   PTSTNO                                                           
         BAS   RE,BLDHEXC                                                       
         BNE   PTSTNO                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,3,ESSDDLN                                                     
         ICM   RF,3,APPCDLEN                                                    
         AR    RF,RE                                                            
         STCM  RF,3,APPCDLEN                                                    
*                                  SEND RETURN DATA MESSAGE                     
*                                                                               
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         CLI   ESSDMODE,ESSDMRCQ    SEND AND FLUSH IF ESSLUSND                  
         BE    *+10                                                             
         MVC   SEND_TYPE,ATB_SEND_AND_FLUSH                                     
         MVC   SEND_ACCESS_TOKEN,ATB_FW0                                        
         SR    RF,RF                                                            
         ICM   RF,3,APPCDLEN                                                    
         STCM  RF,3,APPC_BUFFER_LENGTH                                          
         STCM  RF,15,SEND_LENGTH                                                
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   P(40),=CL40'ESSIO TEST - BEFORE SEND'                            
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         MVC   APPCACTN,=CL8'ATBSEND '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BE    PTSTOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    PTSTOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    PTSTOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   PTSTLUER            EXIT IF RETURN CODE ERROR                    
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   PTSTLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
         B     PTSTOK              EXIT IF REQUEST_TO_SEND NOT NULL             
*                                                                               
PTSTRSP  EQU   *                                                                
         MVI   ESSDSEQN,ESSDSLSQ   SET MESSAGE SEQUENCE CODE                    
         BAS   RE,CLEARED                                                       
         BAS   RE,CLRAPPC                                                       
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_ACCESS_TOKEN,ATB_FW0                                     
         MVC   RECEIVE_LENGTH,=A(L'APPC_BUFFER)                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   P(40),=CL40'ESSIO TEST - BEFORE RCV'                             
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         MVC   APPCACTN,=CL8'ATBRCVW '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
PTSTRET  EQU   *                                                                
         CLI   OPERSTOP,C'Y'                                                    
         BE    PTSTOK                                                           
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    PTSTOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    PTSTOK              EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   PTSTLUER            EXIT IF RETURN CODE ERROR                    
         CLC   DATA_RECEIVED,ATB_FW2                                            
         BNE   PTSTLUER            EXIT IF DATA BUFFER NOT COMPLETE             
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   PTSTLUER            EXIT IF REQUEST_TO_SEND NOT NULL             
         CLC   STATUS_RECEIVED,ATB_FW1                                          
         BNE   PTSTLUER                                                         
*                                                                               
*                                  ERROR IF ZERO LENGTH RECEIVED ??             
         OC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         BZ    PTSTLUER                                                         
*                                                                               
         CLC   APPC_BUFFER_LENGTH,RECEIVE_LENGTH+2                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APPCDLEN,RECEIVE_LENGTH+2                                        
         BAS   RE,VALMSTR          VALIDATE MESSAGE DATA STRUCTURE              
         BNE   PTSTERR                                                          
         BAS   RE,VALHEAD          VALIDATE MESSAGE HEADER FIELDS               
         BNE   PTSTERR                                                          
         BAS   RE,VALDATA          VALIDATE HANDSHAKE MESSAGE DATA              
         BNE   PTSTERR                                                          
*                                                                               
         BAS   RE,VALHEXC                                                       
         BNE   PTSTERR                                                          
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(40),=CL40'ESS TEST MESSAGE PROCESSED'                          
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     PTSTOK                                                           
*                                                                               
PTSTECB  EQU   *                   EXIT IF ECB POSTED                           
         MVI   OPERSTOP,C'Y'                                                    
         B     PTSTOK                                                           
*                                                                               
PTSTERR  EQU   *                   RETURN ERROR MESSAGE                         
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'TEST MESSAGE ERROR'                               
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     PTSTNO                                                           
*                                                                               
PTSTLUER EQU   *                   LU6 RETURN ERROR MESSAGE                     
         MVC   P(10),=CL10'TEST     '                                           
         GOTO1 =A(PRNTLUER),(RC)                                                
         MVI   LU6ERROR,C'Y'                                                    
         B     PTSTOK                                                           
*                                                                               
PTSTNO   B     NO                                                               
PTSTOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  PROCESS REQUEST RECEIVED FROM ESS FOR HOSTESS                      *         
***********************************************************************         
         SPACE 1                                                                
PROCREQ  NTR1                                                                   
*                                                                               
         ICM   R4,15,EATDATA                                                    
         USING ESSDATAD,R4                                                      
*                                                                               
         MVC   ESSDSID,EATENUM                                                  
         CLC   ESSDMID,=AL3(ESSHSQLQ)                                           
         BE    PREQSQL             PROCESS SQL EXTRACT SUB SYSTEM               
         CLC   ESSDMID,=AL3(ESSHPQRQ)                                           
         BE    PREQPQR             PROCESS PQ REPORT SUB SYSTEM                 
         DC    H'0'                UNKNOWN ACTION CODE                          
*                                                                               
*                                  REQUEST FOR PQ REPORT SUB SYSTEM             
PREQPQR  GOTO1 VEIOPQR,DMCB,(R7),RR=RELO                                        
         BNE   PREQER1                                                          
         B     PREQ100                                                          
*                                  REQUEST FOR SQL EXTRACT SUB SYSTEM           
PREQSQL  GOTO1 VEIOSQL,DMCB,(R7),RR=RELO                                        
         BNE   PREQER2                                                          
         B     PREQ100                                                          
*                                  REQUEST PROCESSED OK                         
PREQ100  MVC   P(40),=CL40'ESSIO REQUEST PROCESSED'                             
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     PREQOK                                                           
*                                  PQ REPORT REQUEST ERROR                      
PREQER1  MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'ESSIO EIOPQR ERROR'                               
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     PREQNO                                                           
*                                  SQL EXTRACT REQUEST ERROR                    
PREQER2  MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'ESSIO SQL SUBSYSTEM ERROR'                        
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     PREQNO                                                           
*                                                                               
PREQOK   B     YES                                                              
PREQNO   B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  DEALLOCATE APPC CONVERSATION                                      *          
***********************************************************************         
         SPACE 1                                                                
DEALAPPC NTR1                                                                   
*                                                                               
         BAS   RE,CLRAPPC                                                       
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   APPCACTN,=CL8'ATBGTA2 '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   DEALX                                                            
         CLC   CONVERSATION_STATE,ATB_SEND_STATE                                
         BE    DEALSND                                                          
         CLC   CONVERSATION_STATE,ATB_SEND_PENDING_STATE                        
         BE    DEALSND                                                          
         CLC   CONVERSATION_STATE,ATB_CONFIRM_SEND_STATE                        
         BE    DEALSND                                                          
         CLC   CONVERSATION_STATE,ATB_CONFIRM_STATE                             
         BE    DEALSND                                                          
         CLC   CONVERSATION_STATE,ATB_CONFIRM_DEALLOCATE_STATE                  
         BE    DEALSND                                                          
         CLC   CONVERSATION_STATE,ATB_INITIALIZE_STATE                          
         BE    DEALSND                                                          
         CLC   CONVERSATION_STATE,ATB_RECEIVE_STATE                             
         BE    DEALRCV                                                          
         B     DEALX                                                            
*                                                                               
DEALRCV  EQU   *                                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'CLOSE DOWN SESSION IN RECEIVE STATE'              
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   APPCACTN,=CL8'ATBGTA2 '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
         GOTO1 =A(PRNTATTB),(RC)                                                
*                                                                               
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,APPC_BUFFER_LENGTH                                            
         STCM  RE,15,AAPPCBUF                                                   
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_ACCESS_TOKEN,ATB_FW0                                     
         MVC   RECEIVE_LENGTH,=A(L'APPC_BUFFER)                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBRCVI '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLC   RTN_CODE,ATB_UNSUCCESSFUL                                        
         BE    DEALX                                                            
         CLC   RTN_CODE,ATB_OK                                                  
         BE    DEALX                                                            
         B     DEALLUER                                                         
*                                                                               
DEALSND  EQU   *                                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'CLOSE DOWN SESSION IN SEND STATE'                 
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_FLUSH                             
*                                                                               
         MVC   APPCACTN,=CL8'ATBDEAL '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BE    DEALX                                                            
         B     DEALLUER                                                         
*                                                                               
DEALX    MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'CONVERSATION DEALLOCATED'                         
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     DEALOK                                                           
*                                                                               
DEALLUER EQU   *                   RETURN ERROR MESSAGE                         
         MVC   P(10),=CL40'DEALLOCATE '                                         
         GOTO1 =A(PRNTLUER),(RC)                                                
         MVI   LU6ERROR,C'Y'                                                    
         B     DEALOK                                                           
*                                                                               
DEALNO   B     NO                                                               
DEALOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*  CONFIRM RESPONSE ON RECEIVE DEALLOCATE                             *         
***********************************************************************         
         SPACE 1                                                                
CONFAPPC NTR1                                                                   
*                                                                               
         BAS   RE,CLRAPPC                                                       
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBCFND '                                          
         GOTO1 VESSAPPC,DMCB,(R7),(R6)                                          
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   CONFLUER                                                         
*                                                                               
         MVC   WTOMSGD,SPACES                                                   
         MVC   WTOMSGD(8),EATNAME                                               
         MVC   WTOMSGD+9(40),=CL40'CONFIRM'                                     
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         B     CONFOK                                                           
*                                                                               
CONFLUER EQU   *                   RETURN ERROR MESSAGE                         
         MVC   P(10),=CL40'CONFIRM   '                                          
         GOTO1 =A(PRNTLUER),(RC)                                                
         MVI   LU6ERROR,C'Y'                                                    
         B     CONFOK                                                           
*                                                                               
CONFNO   B     NO                                                               
CONFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*  CLOSE DOWN APPC ACB                                               *          
***********************************************************************         
         SPACE 1                                                                
CLOSAPPC NTR1                                                                   
*                                                                               
         MVC   P(8),EATNAME                                                     
         MVC   P+10(10),=CL10'CLOSEDOWN'                                        
         BAS   RE,CLEARED                                                       
         CLI   LU6ERROR,C'Y'                                                    
         BNE   CLOS010                                                          
         MVC   P+22(40),=CL40'APPC LU6 ERROR'                                   
         B     CLOS040                                                          
*                                                                               
CLOS010  CLI   OPERSTOP,C'Y'                                                    
         BNE   CLOS020                                                          
         MVC   P+22(40),=CL40'OPERATOR STOP'                                    
         B     CLOS030                                                          
*                                                                               
CLOS020  CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    CLOS022                                                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    CLOS024                                                          
         B     CLOS030                                                          
CLOS022  MVC   P+22(40),=CL40'DEALLOCATE NORMAL RETURNED'                       
         B     CLOS040                                                          
CLOS024  MVC   P+22(40),=CL40'DEALLOCATE ABEND RETURNED'                        
         B     CLOS040                                                          
*                                                                               
CLOS030  MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         B     CLOS050                                                          
*                                                                               
CLOS040  EQU   *                                                                
         MVC   ERRMSG,P                                                         
         MVC   P(80),=CL80'<ERROR????>'                                         
         MVC   P+12(L'ERRMSG),ERRMSG                                            
         ICM   RF,15,=AL4(ERNEIODC)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     CLOS050                                                          
*                                                                               
CLOS050  EQU   *                                                                
         MVC   WTOMSG+18(8),SPACES                                              
         B     CLOSOK                                                           
*                                                                               
CLOSNO   B     NO                                                               
CLOSOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SHUTDOWN THE TASK PROCESS                                          *          
***********************************************************************         
         SPACE 1                                                                
SHUTDOWN NTR1                                                                   
*                                                                               
         MVC   P(8),=C'SHUTDOWN'                                                
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BNE   SDOWX                                                            
         MVC   P(13),=C'OPERATOR STOP'                                          
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         MVC   WTOMSGD,SPACES                                                   
         MVC   WTOMSGD(8),EATNAME                                               
         MVC   WTOMSGD+9(16),=CL16'SHUTDOWN SUBTASK'                            
         MVC   WTOMSG+18(8),SPACES                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
SDOWX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE MESSAGE STRUCTURE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING ESSDATAD,R4                                                      
VALMSTR  NTR1                                                                   
*                                                                               
         L     R3,AAPPCBUF                                                      
         LA    R3,L'APPC_BUFFER_LENGTH(R3)                                      
         USING ESSHDR,R3                                                        
         SR    R2,R2                                                            
         ICM   R2,3,APPCDLEN                                                    
         STCM  R2,3,ESSDMLN                                                     
*                                                                               
         LR    RF,R3                                                            
         AR    RF,R2                                                            
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         CLC   ESSHDDS,=C'DD'                                                   
         BNE   VMSTER1                                                          
         CLC   ESSHDDS,0(RF)                                                    
         BNE   VMSTER2                                                          
*                                                                               
         CH    R2,=Y(ESSHDRX-ESSHDR+2)                                          
         BL    VMSTER3                                                          
         B     VMSTOK                                                           
*                                  APPC MESSAGE PARAMETER ERROR                 
VMSTER1  EQU   *                                                                
   MVC   P(80),=CL80'<ERROR????> INVALID "DD" START TAG IN ESS MESSAGE'         
         ICM   RF,15,=AL4(ERNEIOST)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     VMSTNO                                                           
*                                                                               
VMSTER2  EQU   *                                                                
   MVC   P(80),=CL80'<ERROR????> INVALID "DD" END TAG IN ESS MESSAGE'           
         ICM   RF,15,=AL4(ERNEIOET)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     VMSTNO                                                           
*                                                                               
VMSTER3  EQU   *                                                                
   MVC   P(80),=CL80'<ERROR????> INVALID ESS MESSAGE LENGTH'                    
         ICM   RF,15,=AL4(ERNEIOML)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     VMSTNO                                                           
*                                                                               
VMSTNO   B     NO                                                               
VMSTOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE MESSAGE HEADER IN APPC BUFFER                              *         
***********************************************************************         
         SPACE 1                                                                
VALHEAD  NTR1                                                                   
*                                                                               
         L     R3,AAPPCBUF                                                      
         LA    R3,L'APPC_BUFFER_LENGTH(R3)                                      
         USING ESSHDR,R3                                                        
         MVI   ERRFLAG,C'N'                                                     
*                                  VALIDATE MESSAGE TYPE                        
         BAS   RE,VALHMTY                                                       
*                                  VALIDATE MESSAGE ID                          
         BAS   RE,VALHMID                                                       
*                                  VALIDATE SENDER ID                           
         BAS   RE,VALHSID                                                       
*                                  VALIDATE MESSAGE DATE/TIME                   
         BAS   RE,VALHDTTM                                                      
*                                  VALIDATE SENDER FLAGS                        
         BAS   RE,VALHPCFL                                                      
*                                  VALIDATE RECEIVER FLAGS                      
         BAS   RE,VALHMFFL                                                      
*                                  VALIDATE HEADER TYPE                         
         BAS   RE,VALHHTY                                                       
*                                                                               
         TM    ESSDHTYP,ESSHXTNQ   TEST IF EXTENDED HEADER                      
         BZ    VHEDEND                                                          
*                                  VALIDATE EXTENDED HEADER 1                   
         SR    RF,RF               CHECK MINIMUM LENGTH                         
         ICM   RF,3,APPCDLEN                                                    
         CH    RF,=Y(ESSHDR1X-ESSHDR+2)                                         
         BL    VHEDER                                                           
*                                  VALIDATE REQUEST REFERENCE                   
         BAS   RE,VALHREF                                                       
*                                  VALIDATE REQUEST PRIORITY                    
         BAS   RE,VALHPRI                                                       
*                                  VALIDATE REQUEST SYSTEM                      
         BAS   RE,VALHSYS                                                       
*                                  VALIDATE REQUEST PROGRAM                     
         BAS   RE,VALHPRG                                                       
*                                  VALIDATE REQUEST USERID                      
         BAS   RE,VALHUID                                                       
*                                  VALIDATE REQUEST PASSWORD                    
         BAS   RE,VALHPWD                                                       
*                                  VALIDATE EXTENDED HEADER FLAGS               
         BAS   RE,VALH1FL                                                       
*                                                                               
VHEDEND  CLI   ERRFLAG,C'Y'                                                     
         BE    VHEDER                                                           
         B     VHEDOK                                                           
*                                  APPC MESSAGE PARAMETER ERROR                 
VHEDER   EQU   *                                                                
   MVC   P(80),=CL80'<ERROR????> ESS MESSAGE HEADER FIELD - '                   
         MVC   P+39(50),ERRMSG                                                  
         ICM   RF,15,=AL4(ERNEIOHF)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     VHEDNO                                                           
*                                                                               
VHEDNO   B     NO                                                               
VHEDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE REVERSE STATE TO SENDER MESSAGE                            *         
***********************************************************************         
         SPACE 1                                                                
VALRVSM  NTR1                                                                   
*                                                                               
         L     R3,AAPPCBUF                                                      
         LA    R3,L'APPC_BUFFER_LENGTH(R3)                                      
         USING ESSHDR,R3                                                        
         MVI   ERRFLAG,C'N'                                                     
*                                                                               
         LA    R3,ESSHDRX-ESSHDR(R3)                                            
         TM    ESSDHTYP,ESSHXTNQ   TEST IF EXTENDED HEADER                      
         BZ    *+8                                                              
         LA    R3,ESSHDR1X-ESSHDR1(R3)                                          
*                                                                               
         CLC   ESSDMID,=CL3'RVS'   ??                                           
         BNE   VRVSER                                                           
         B     VRVSEND                                                          
*                                                                               
VRVSEND  CLI   ERRFLAG,C'Y'                                                     
         BE    VRVSER                                                           
         B     VRVSOK                                                           
*                                                                               
VRVSER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID REVERSE STATE MESSAGE'                       
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     VRVSNO                                                           
*                                                                               
VRVSNO   B     NO                                                               
VRVSOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER MESSAGE TYPE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VALHMTY  NTR1                                                                   
*                                                                               
*                                  VALIDATE MESSAGE TYPE                        
         CLI   ESSHMTY,ESSHCTLQ                                                 
         BE    VMTYCTL                                                          
         CLI   ESSHMTY,ESSHERRQ                                                 
         BE    VMTYERR                                                          
         CLI   ESSHMTY,ESSHINFQ                                                 
         BE    VMTYINF                                                          
         CLI   ESSHMTY,ESSHREQQ                                                 
         BE    VMTYREQ                                                          
         B     VMTYER                                                           
*                                                                               
VMTYCTL  EQU   *                                                                
         MVI   ESSDMTYP,ESSHCTLQ                                                
         B     VMTYOK                                                           
*                                                                               
VMTYERR  EQU   *                                                                
         MVI   ESSDMTYP,ESSHERRQ                                                
         B     VMTYOK                                                           
*                                                                               
VMTYINF  EQU   *                                                                
         MVI   ESSDMTYP,ESSHINFQ                                                
         B     VMTYOK                                                           
*                                                                               
VMTYREQ  EQU   *                                                                
         MVI   ESSDMTYP,ESSHREQQ                                                
         B     VMTYOK                                                           
*                                  APPC MESSAGE PARAMETER ERROR                 
VMTYER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID MESSAGE TYPE CODE'                           
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VMTYNO                                                           
*                                                                               
VMTYNO   B     NO                                                               
VMTYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER MESSAGE ID FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VALHMID  NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSHMID,3                                          
         BNE   VMIDER                                                           
*                                                                               
VMID010  MVC   ESSDMID,ESSHMID     UPDATE MESSAGE ID                            
         B     VMIDOK                                                           
*                                                                               
VMIDER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID MESSAGE ID CODE'                             
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VMIDNO                                                           
*                                                                               
VMIDNO   B     NO                                                               
VMIDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER ESS ID NUMBER FIELD (AKA SENDEER ID)                *         
***********************************************************************         
         SPACE 1                                                                
VALHSID  NTR1                                                                   
*                                                                               
         OC    EATENUM,EATENUM                                                  
         BZ    VSID010                                                          
         CLC   ESSHSID,EATENUM                                                  
         BNE   VSIDER                                                           
*                                                                               
VSID010  MVC   ESSDSID,ESSHSID                                                  
         MVC   ESSDSNAM,EATNAME                                                 
         MVC   ESSDSNAM(3),=CL3'ESS'                                            
         B     VSIDOK                                                           
*                                                                               
VSIDER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID ESS ID NUMBER'                               
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VSIDNO                                                           
*                                                                               
VSIDNO   B     NO                                                               
VSIDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER MESSAGE DATE/TIME FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VALHDTTM NTR1                                                                   
*                                                                               
         MVC   ESSDMDTM,ESSHDTTM                                                
         B     VDTMOK                                                           
*                                                                               
VDTMER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID DATE/TIME FIELD'                             
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VDTMNO                                                           
*                                                                               
VDTMNO   B     NO                                                               
VDTMOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER PC FLAGS                                            *         
***********************************************************************         
         SPACE 1                                                                
VALHPCFL NTR1                                                                   
*                                                                               
         MVC   ESSDPCF,ESSHPCF                                                  
*                                                                               
         TM    ESSDPCF,ESSHBDLQ    TEST IF BULK DOWNLOAD                        
         BZ    VPCF020                                                          
         CLI   ESSDBLKD,C'Y'       TEST BULK DOWNLOAD FLAG IS ON                
         BE    VPCF010                                                          
         MVI   ESSDBLKD,C'Y'       SET BULK DOWNLOAD FLAG ON                    
         MVI   ESSDBLKO,C'N'       SET BULK FILE OPEN FLAG OFF                  
         B     VPCFOK                                                           
*                                                                               
VPCF010  MVI   ESSDBLKD,C'N'       SET BULK DOWNLOAD FLAG OFF                   
         MVI   ESSDBLKO,C'N'       SET BULK FILE OPEN FLAG OFF                  
         B     VPCFOK                                                           
*                                                                               
VPCF020  MVI   ESSDBLKD,C'N'       SET BULK DOWNLOAD FLAG OFF                   
         MVI   ESSDBLKO,C'N'       SET BULK FILE OPEN FLAG OFF                  
         B     VPCFOK                                                           
*                                                                               
VPCFER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID PC CONTROL FLAGS'                            
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VPCFNO                                                           
*                                                                               
VPCFNO   B     NO                                                               
VPCFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER MAIN FRAME FLAGS                                    *         
***********************************************************************         
         SPACE 1                                                                
VALHMFFL NTR1                                                                   
*                                                                               
         MVC   ESSDMFF,ESSHMFF                                                  
         B     VMFFOK                                                           
*                                                                               
VMFFER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID MAINFRAME CONTROL FLAGS'                     
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VMFFNO                                                           
*                                                                               
VMFFNO   B     NO                                                               
VMFFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER TYPE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALHHTY  NTR1                                                                   
*                                                                               
         MVC   ESSDHTYP,ESSHHTY                                                 
         B     VHTYOK                                                           
*                                                                               
VHTYER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HEADER TYPE CONTROL FLAG'                    
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VHTYNO                                                           
*                                                                               
VHTYNO   B     NO                                                               
VHTYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTENDED HEADER 1 REQUEST REFERENCE FIELD                  *         
***********************************************************************         
         SPACE 1                                                                
VALHREF  NTR1                                                                   
*                                                                               
         ICM   RF,15,ESSHREF                                                    
         STCM  RF,15,ESSDREFB                                                   
         CVD   RF,DUB                                                           
         UNPK  ESSDREF,DUB                                                      
* ??     OI    ESSDREF+L'ESSDREF-1,X'F0'                                        
         B     VREFOK                                                           
*                                                                               
VREFER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID REQUEST REFERENCE FIELD'                     
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VREFNO                                                           
*                                                                               
VREFNO   B     NO                                                               
VREFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTENDED HEADER 1 REQUEST PRIORITY FIELD                   *         
***********************************************************************         
         SPACE 1                                                                
VALHPRI  NTR1                                                                   
*                                                                               
         MVC   ESSDPRI,ESSHPRI                                                  
         B     VPRIOK                                                           
*                                                                               
VPRIER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID REQUEST PRIORITY FIELD'                      
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VPRINO                                                           
*                                                                               
VPRINO   B     NO                                                               
VPRIOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTENDED HEADER 1 REQUEST SYSTEM FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
VALHSYS  NTR1                                                                   
*                                                                               
         MVC   ESSDSYS,ESSHSYS                                                  
         B     VSYSOK                                                           
*                                                                               
VSYSER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID REQUEST SYSTEM FIELD'                        
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VSYSNO                                                           
*                                                                               
VSYSNO   B     NO                                                               
VSYSOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTENDED HEADER 1 REQUEST PROGRAM FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
VALHPRG  NTR1                                                                   
*                                                                               
         MVC   ESSDPRG,ESSHPRG                                                  
         B     VREFOK                                                           
*                                                                               
VPRGER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID REQUEST PROGRAM FIELD'                       
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VPRGNO                                                           
*                                                                               
VPRGNO   B     NO                                                               
VPRGOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTENDED HEADER 1 REQUEST USERID FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
VALHUID  NTR1                                                                   
*                                                                               
         MVC   ESSDUID,ESSHUID                                                  
         OC    ESSDUID,SPACES                                                   
         B     VUIDOK                                                           
*                                                                               
VUIDER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID REQUEST USERID FIELD'                        
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VUIDNO                                                           
*                                                                               
VUIDNO   B     NO                                                               
VUIDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTENDED HEADER 1 REQUEST PASSWORD FIELD                   *         
***********************************************************************         
         SPACE 1                                                                
VALHPWD  NTR1                                                                   
*                                                                               
         MVC   ESSDPWD,ESSHPWD                                                  
         B     VPWDOK                                                           
*                                                                               
VPWDER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID REQUEST PASSWORD FIELD'                      
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VPWDNO                                                           
*                                                                               
VPWDNO   B     NO                                                               
VPWDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTENDED HEADER 1 FLAGS FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
VALH1FL  NTR1                                                                   
*                                                                               
         MVC   ESSDH1FL,ESSH1FL                                                 
         B     VREFOK                                                           
*                                                                               
VH1FER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID EXTENDED HEADER CONTROL FLAGS'               
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VH1FNO                                                           
*                                                                               
VH1FNO   B     NO                                                               
VH1FOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE MESSAGE DATA                                               *         
***********************************************************************         
         SPACE 1                                                                
VALDATA  NTR1                                                                   
*                                                                               
         LA    R2,ESSDBUFF                                                      
         CLI   ESSDSEQN,ESSDSNXQ   SET MESSAGE SEQUENCE CODE                    
         BE    VDAT010                                                          
         XC    ESSDDLN,ESSDDLN                                                  
         B     VDAT020                                                          
*                                                                               
VDAT010  SR    RF,RF                                                            
         ICM   RF,3,ESSDDLN                                                     
         AR    R2,RF                                                            
*                                                                               
VDAT020  L     R3,AAPPCBUF                                                      
         LA    R3,L'APPC_BUFFER_LENGTH(R3)                                      
         USING ESSHDR,R3                                                        
         LA    R3,ESSHDRX                                                       
         DROP  R3                                                               
         SR    RF,RF               GET MESSAGE DATA LENGTH                      
         ICM   RF,3,ESSDMLN                                                     
         SH    RF,=Y(ESSHDRX-ESSHDR+2)                                          
         TM    ESSDHTYP,ESSHXTNQ   TEST IF EXTEND HEADER                        
         BZ    VDAT030                                                          
         SH    RF,=Y(ESSHDR1X-ESSHDR1)                                          
         LA    R3,ESSHDR1X-ESSHDR1(R3)                                          
*                                                                               
VDAT030  ST    RF,FULL                                                          
         LTR   RF,RF                                                            
         BZ    VDAT040                                                          
         BM    VDATERR                                                          
*                                                                               
         LR    R0,R2                                                            
         LR    RE,R3                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ICM   RF,3,ESSDDLN                                                     
         A     RF,FULL                                                          
         CLM   RF,15,=A(ESSDATAL)                                               
         BL    VDAT040                                                          
         B     VDATERR                                                          
VDAT040  STCM  RF,3,ESSDDLN                                                     
         B     VDATOK                                                           
*                                                                               
VDATERR  EQU   *                                                                
   MVC   P(80),=CL80'<ERROR????> INVALID ESS MESSAGE DATA LENGTH'               
         ICM   RF,15,=AL4(ERNEIODL)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     VDATNO                                                           
*                                                                               
VDATNO   B     NO                                                               
VDATOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE HANDSHAKE EXCHANGE DATA                                    *         
***********************************************************************         
         SPACE 1                                                                
VALHEXC  NTR1                                                                   
*                                                                               
         L     R3,AAPPCBUF                                                      
         LA    R3,L'APPC_BUFFER_LENGTH(R3)                                      
         USING ESSHDR,R3                                                        
         TM    ESSDHTYP,ESSHXTNQ   TEST IF EXTENDED HEADER                      
         BO    *+12                                                             
         LA    R3,ESSHDRX                                                       
         B     *+8                                                              
         LA    R3,ESSHDR1X                                                      
         DROP  R3                                                               
         USING ESSXDAT,R3                                                       
         MVI   ERRFLAG,C'N'                                                     
*                                  VALIDATE EXCHANGE TEXT C'MYNAMEIS'           
         BAS   RE,VALXTXT                                                       
*                                  VALIDATE UNIQUE INTEGER ID                   
         BAS   RE,VALXID                                                        
*                                  VALIDATE PASSWORD                            
         BAS   RE,VALXPSWD                                                      
*                                  VALIDATE NETWORK ID                          
         BAS   RE,VALXNEID                                                      
*                                  VALIDATE LOGICAL UNIT ID                     
         BAS   RE,VALXLUID                                                      
*                                  VALIDATE TRANSACTION PROGRAM NAME            
         BAS   RE,VALXTPNA                                                      
*                                  VALIDATE VERSION                             
         BAS   RE,VALXVER                                                       
*                                  VALIDATE LEVEL                               
         BAS   RE,VALXLEV                                                       
*                                  VALIDATE DATE YYYYMMDD                       
         BAS   RE,VALXDATE                                                      
*                                  VALIDATE TIME HHMMSS                         
         BAS   RE,VALXTIME                                                      
*                                                                               
         CLI   ERRFLAG,C'Y'                                                     
         BE    VHEXER                                                           
         B     VHEXOK                                                           
*                                  HANDSHAKE EXCHANGE MESSAGE ERROR             
VHEXER   EQU   *                                                                
   MVC   P(80),=CL80'<ERROR????> ESS MESSAGE HANDSHAKE FIELD - '                
         MVC   P+42(50),ERRMSG                                                  
         ICM   RF,15,=AL4(ERNEIOHX)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     VHEXNO                                                           
*                                                                               
VHEXNO   B     NO                                                               
VHEXOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE EXCHANGE TEXT FIELD C'MYNAMEIS'           *         
***********************************************************************         
         SPACE 1                                                                
VALXTXT  NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXTXT,8                                          
         BNE   VXTXER                                                           
         CLC   ESSXTXT,=CL8'MYNAMEIS'                                           
         BNE   VXTXER1                                                          
         MVC   ESSDXTXT,ESSXTXT                                                 
         B     VXTXOK                                                           
*                                                                               
VXTXER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE TEXT'                     
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXTXNO                                                           
*                                                                               
VXTXER1  EQU   *                                                                
         MVC   P(50),=CL50'HANDSHAKE EXCHANGE TEXT DOES NOT MATCH'              
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXTXNO                                                           
*                                                                               
VXTXNO   B     NO                                                               
VXTXOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE UNIQUE INTEGER ID FIELD                   *         
***********************************************************************         
         SPACE 1                                                                
VALXID   NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXID,6                                           
         BNE   VXIDER                                                           
         MVC   ESSDXID,ESSXID                                                   
         B     VXIDOK                                                           
*                                                                               
VXIDER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE ID FIELD'                 
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXIDNO                                                           
*                                                                               
VXIDNO   B     NO                                                               
VXIDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE PASSWORD FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
VALXPSWD NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXPSWD,8                                         
         BNE   VXPSER                                                           
         OC    EATEPWD,EATEPWD                                                  
         BZ    VXPS010                                                          
         CLC   ESSXPSWD,EATEPWD                                                 
         BNE   VXPSER1                                                          
VXPS010  MVC   ESSDXPWD,ESSXPSWD                                                
         B     VXPSOK                                                           
*                                                                               
VXPSER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE PASSWORD'                 
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXPSNO                                                           
*                                                                               
VXPSER1  EQU   *                                                                
         MVC   P(50),=CL50'HANDSHAKE PASSWORD DOES NOT MATCH'                   
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXPSNO                                                           
*                                                                               
VXPSNO   B     NO                                                               
VXPSOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE NETWORK ID FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
VALXNEID NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXNEID,8                                         
         BNE   VXNEER                                                           
         MVC   ESSDXNID,ESSXNEID                                                
         B     VXNEOK                                                           
*                                                                               
VXNEER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE NETWORK ID'               
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXNENO                                                           
*                                                                               
VXNENO   B     NO                                                               
VXNEOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE LOGICAL UNIT ID FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
VALXLUID NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXLUID,8                                         
         BNE   VXLUER                                                           
         MVC   ESSDXLU,ESSXLUID                                                 
         B     VXLUOK                                                           
*                                                                               
VXLUER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE LUID'                     
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXLUNO                                                           
*                                                                               
VXLUNO   B     NO                                                               
VXLUOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE TRANSACTION PROGRAM NAME FIELD            *         
***********************************************************************         
         SPACE 1                                                                
VALXTPNA NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXTPNA,8                                         
         BNE   VXTPER                                                           
         MVC   ESSDXTPN,ESSXTPNA                                                
         CLC   ESSDXTPN,=CL8'ESSLURCV'                                          
         BE    VXTPSND                                                          
         CLC   ESSDXTPN,=CL8'ESSLUSND'                                          
         BE    VXTPRCV                                                          
         B     VXTPER                                                           
VXTPSND  MVI   ESSDMODE,ESSDMSNQ                                                
         B     VXTPOK                                                           
VXTPRCV  MVI   ESSDMODE,ESSDMRCQ                                                
         B     VXTPOK                                                           
*                                                                               
VXTPER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE TPNAME'                   
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXTPNO                                                           
*                                                                               
VXTPNO   B     NO                                                               
VXTPOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE VERSION FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VALXVER  NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXVER,2                                          
         BNE   VXVEER                                                           
         MVC   ESSDXVER,ESSXVER                                                 
         B     VXVEOK                                                           
*                                                                               
VXVEER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE VERSION FIELD'            
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXVENO                                                           
*                                                                               
VXVENO   B     NO                                                               
VXVEOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE LEVEL FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VALXLEV  NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXLEV,2                                          
         BNE   VXLEER                                                           
         MVC   ESSDXLEV,ESSXLEV                                                 
         B     VXLEOK                                                           
*                                                                               
VXLEER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE LEVEL FIELD'              
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXLENO                                                           
*                                                                               
VXLENO   B     NO                                                               
VXLEOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE DATE FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VALXDATE NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXDATE,8                                         
         BNE   VXDAER                                                           
         MVC   ESSDXDAT,ESSXDATE                                                
         B     VXDAOK                                                           
*                                                                               
VXDAER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE DATE FIELD'               
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXDANO                                                           
*                                                                               
VXDANO   B     NO                                                               
VXDAOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE MESSAGE TIME FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VALXTIME NTR1                                                                   
*                                                                               
         GOTO1 VALALNUM,DMCB,ESSXTIME,6                                         
         BNE   VXTIER                                                           
         MVC   ESSDXTIM,ESSXTIME                                                
         B     VXTIOK                                                           
*                                                                               
VXTIER   EQU   *                                                                
         MVC   P(50),=CL50'INVALID HANDSHAKE EXCHANGE TIME FIELD'               
         CLI   ERRFLAG,C'Y'                                                     
         BE    *+14                                                             
         MVI   ERRFLAG,C'Y'                                                     
         MVC   ERRMSG,P                                                         
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     VXTINO                                                           
*                                                                               
VXTINO   B     NO                                                               
VXTIOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE ALPHA NUMERIC FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALALNUM NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
*                                                                               
         LTR   R3,R3                                                            
         BZ    VALNOK                                                           
*                                                                               
VALN010  CLI   0(R2),X'40'                                                      
         BE    VALN030                                                          
         CLI   0(R2),X'81'                                                      
         BL    VALNNO                                                           
         CLI   0(R2),X'A9'                                                      
         BNH   VALN030                                                          
         CLI   0(R2),C'A'                                                       
         BL    VALNNO                                                           
         CLI   0(R2),C'Z'                                                       
         BNH   VALN030                                                          
         CLI   0(R2),C'0'                                                       
         BL    VALNNO                                                           
         CLI   0(R2),C'9'                                                       
         BH    VALNNO                                                           
VALN030  LA    R2,1(R2)                                                         
         BCT   R3,VALN010                                                       
*                                                                               
VALNOK   SR    RC,RC                                                            
VALNNO   LTR   RC,RC                                                            
VALNX    XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD MESSAGE HEADER IN APPC BUFFER                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING ESSDATAD,R4                                                      
BLDHEAD  NTR1                                                                   
*                                                                               
         BAS   RE,CLRAPPC                                                       
*                                                                               
         L     R3,AAPPCBUF                                                      
         LA    R3,L'APPC_BUFFER_LENGTH(R3)                                      
         USING ESSHDR,R3                                                        
         XC    APPCDLEN,APPCDLEN                                                
         CLI   ESSDBLKD,C'Y'       TEST BULK DATA DOWNLOAD                      
         BE    BHEDOK                AND EXIT IF SO                             
*                                  BUILD MESSAGE TYPE                           
         MVC   ESSHDDS(2),=CL2'DD'                                              
*                                  BUILD MESSAGE TYPE                           
         BAS   RE,BLDHMTY                                                       
         BNE   BHEDER                                                           
*                                  BUILD MESSAGE ID                             
         BAS   RE,BLDHMID                                                       
         BNE   BHEDER                                                           
*                                  BUILD SENDER ID                              
         BAS   RE,BLDHSID                                                       
         BNE   BHEDER                                                           
*                                  BUILD MESSAGE DATE/TIME                      
         BAS   RE,BLDHDTTM                                                      
         BNE   BHEDER                                                           
*                                  BUILD PC FLAGS                               
         BAS   RE,BLDHPCF                                                       
         BNE   BHEDER                                                           
*                                  BUILD MAINFRAME FLAGS                        
         BAS   RE,BLDHMFF                                                       
         BNE   BHEDER                                                           
*                                  BUILD HEADER TYPE                            
         BAS   RE,BLDHHTY                                                       
         BNE   BHEDER                                                           
*                                                                               
         TM    ESSDHTYP,ESSHXTNQ   TEST IF EXTENDED HEADER                      
         BO    BHEDEXT                                                          
*                                                                               
         MVC   ESSHDRX(2),=CL2'DD'                                              
*                                                                               
         SR    RF,RF                                                            
* ??     ICM   RF,3,=Y(ESSHDRX-ESSHDR+2)                                        
         ICM   RF,3,=Y(ESSHDRX-ESSHDR+4)                                        
         STCM  RF,3,APPCDLEN                                                    
         B     BHEDOK                                                           
*                                  BUILD REQUEST REFERENCE                      
BHEDEXT  BAS   RE,BLDHREF                                                       
         BNE   BHEDER                                                           
*                                  BUILD REQUEST PRIORITY                       
         BAS   RE,BLDHPRI                                                       
         BNE   BHEDER                                                           
*                                  BUILD REQUEST SYSTEM                         
         BAS   RE,BLDHSYS                                                       
         BNE   BHEDER                                                           
*                                  BUILD REQUEST PROGRAM                        
         BAS   RE,BLDHPRG                                                       
         BNE   BHEDER                                                           
*                                  BUILD REQUEST USERID                         
         BAS   RE,BLDHUID                                                       
         BNE   BHEDER                                                           
*                                  BUILD REQUEST PASSWORD                       
         BAS   RE,BLDHPWD                                                       
         BNE   BHEDER                                                           
*                                  BUILD EXTENDED HEADER FLAGS                  
         BAS   RE,BLDH1FL                                                       
         BNE   BHEDER                                                           
*                                                                               
         MVC   ESSHDR1X(2),=CL2'DD'                                             
*                                                                               
         SR    RF,RF                                                            
* ??     ICM   RF,3,=Y(ESSHDR1X-ESSHDR+2)                                       
         ICM   RF,3,=Y(ESSHDR1X-ESSHDR+4)                                       
         STCM  RF,3,APPCDLEN                                                    
         B     BHEDOK                                                           
*                                  APPC MESSAGE PARAMETR ERROR                  
BHEDER   EQU   *                                                                
         DC    H'00'                                                            
         B     BHEDNO                                                           
*                                                                               
BHEDNO   B     NO                                                               
BHEDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD MESSAGE DATA IN APPC BUFFER                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDDATA  NTR1                                                                   
*                                                                               
         TM    ESSDHTYP,ESSHDATQ   TEST IF MESSAGE DATA IN BUFFER               
         BZ    BDATOK                                                           
*                                                                               
         LA    R2,ESSDBUFF                                                      
         L     R3,AAPPCBUF                                                      
         LA    R3,L'APPC_BUFFER_LENGTH(R3)                                      
         SR    RF,RF                                                            
         ICM   RF,3,ESSDDLN        GET MESSAGE LENGTH                           
         CLI   ESSDBLKD,C'Y'       TEST BULK DATA DOWNLOAD                      
         BE    BDAT010               WITH NO HEADER                             
         LA    R3,ESSHDRX-ESSHDR(R3)                                            
         LA    RF,ESSHDRX-ESSHDR(RF)                                            
         TM    ESSDHTYP,ESSHXTNQ   TEST IF EXTENDED HEADER                      
         BZ    BDAT010                                                          
         LA    R3,ESSHDR1X-ESSHDR1(R3)                                          
         LA    RF,ESSHDR1X-ESSHDR1(RF)                                          
*                                                                               
BDAT010  STCM  RF,3,ESSDMLN        UPDATE MESSAGE LENGTH                        
         ICM   RF,3,ESSDDLN        MOVE IN MESSAGE DATA                         
         LR    R0,R3                                                            
         LR    RE,R2                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         CLI   ESSDBLKD,C'Y'       TEST BULK DATA DOWNLOAD                      
         BE    BDAT020                                                          
*                                  ADD MESSAGE TRAILER                          
         ICM   RF,3,ESSDDLN                                                     
         AR    R3,RF                                                            
         MVC   0(2,R3),=CL2'DD'                                                 
         ICM   RF,3,ESSDMLN        UPDATE MESSAGE LENGTH                        
         LA    RF,2(RF)                                                         
         STCM  RF,3,ESSDMLN                                                     
*                                                                               
BDAT020  SR    RF,RF               UPDATE MESSAGE LENGTH                        
         ICM   RF,3,ESSDMLN                                                     
* ??     CLI   ESSDBLKD,C'Y'       TEST BULK DATA DOWNLOAD                      
* ??     BE    *+8                                                              
         LA    RF,2(RF)                                                         
         STCM  RF,3,APPCDLEN                                                    
         B     BDATOK                                                           
*                                                                               
BDATER   EQU   *                                                                
         DC    H'00'                                                            
         B     BHEDNO                                                           
*                                                                               
BDATNO   B     NO                                                               
BDATOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER MESSAGE TYPE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
BLDHMTY  NTR1                                                                   
*                                  BUILD MESSAGE TYPE                           
         MVC   ESSHMTY,ESSDMTYP                                                 
         B     BMTYOK                                                           
*                                  APPC MESSAGE PARAMETR ERROR                  
BMTYER   EQU   *                                                                
         DC    H'00'                                                            
         B     BMTYNO                                                           
*                                                                               
BMTYNO   B     NO                                                               
BMTYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER MESSAGE ID FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
BLDHMID  NTR1                                                                   
*                                                                               
         MVC   ESSHMID,ESSDMID                                                  
         B     BMIDOK                                                           
*                                                                               
BMIDER   EQU   *                                                                
         DC    H'00'                                                            
         B     BMIDNO                                                           
*                                                                               
BMIDNO   B     NO                                                               
BMIDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER SENDER ID FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
BLDHSID  NTR1                                                                   
*                                                                               
         MVC   ESSHSID,ESSDSID                                                  
         B     BSIDOK                                                           
*                                                                               
BSIDER   EQU   *                                                                
         DC    H'00'                                                            
         B     BSIDNO                                                           
*                                                                               
BSIDNO   B     NO                                                               
BSIDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER MESSAGE DATE/TIME FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
BLDHDTTM NTR1                                                                   
*                                                                               
         GOTO1 VDATTIM,DMCB,(X'01',FULL)                                        
         MVC   ESSHDTTM,FULL                                                    
         B     BDTMOK                                                           
*                                                                               
BDTMER   EQU   *                                                                
         DC    H'0'                                                             
         B     BDTMNO                                                           
*                                                                               
BDTMNO   B     NO                                                               
BDTMOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER PC FLAGS                                               *         
***********************************************************************         
         SPACE 1                                                                
BLDHPCF  NTR1                                                                   
*                                                                               
         MVC   ESSHPCF,ESSDPCF                                                  
         B     BPCFOK                                                           
*                                                                               
BPCFER   EQU   *                                                                
         DC    H'00'                                                            
         B     BPCFNO                                                           
*                                                                               
BPCFNO   B     NO                                                               
BPCFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER MAINFRAME FLAGS                                        *         
***********************************************************************         
         SPACE 1                                                                
BLDHMFF  NTR1                                                                   
*                                                                               
         MVC   ESSHMFF,ESSDMFF                                                  
         B     BMFFOK                                                           
*                                                                               
BMFFER   EQU   *                                                                
         DC    H'00'                                                            
         B     BMFFNO                                                           
*                                                                               
BMFFNO   B     NO                                                               
BMFFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER TYPE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
BLDHHTY  NTR1                                                                   
*                                                                               
         MVC   ESSHHTY,ESSDHTYP                                                 
         B     BHTYOK                                                           
*                                                                               
BHTYER   EQU   *                                                                
         DC    H'00'                                                            
         B     BHTYNO                                                           
*                                                                               
BHTYNO   B     NO                                                               
BHTYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXTENDED HEADER REQUEST REFERENCE FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
BLDHREF  NTR1                                                                   
*                                                                               
         PACK  DUB,ESSDREF                                                      
         CVB   RF,DUB                                                           
         STCM  RF,15,ESSHREF                                                    
         B     BHREOK                                                           
*                                                                               
BHREER   EQU   *                                                                
         DC    H'00'                                                            
         B     BHRENO                                                           
*                                                                               
BHRENO   B     NO                                                               
BHREOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXTENDED HEADER REQUEST PRIORITY FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
BLDHPRI  NTR1                                                                   
*                                                                               
         MVC   ESSHPRI,ESSDPRI                                                  
         B     BHPROK                                                           
*                                                                               
BHPRER   EQU   *                                                                
         DC    H'00'                                                            
         B     BHPRNO                                                           
*                                                                               
BHPRNO   B     NO                                                               
BHPROK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXTENDED HEADER REQUEST SYSTEM FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
BLDHSYS  NTR1                                                                   
*                                                                               
         MVC   ESSHSYS,ESSDSYS                                                  
         B     BHSYOK                                                           
*                                                                               
BHSYER   EQU   *                                                                
         DC    H'00'                                                            
         B     BHSYNO                                                           
*                                                                               
BHSYNO   B     NO                                                               
BHSYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXTENDED HEADER REQUEST PROGRAM FIELD                         *         
***********************************************************************         
         SPACE 1                                                                
BLDHPRG  NTR1                                                                   
*                                                                               
         MVC   ESSHPRG,ESSDPRG                                                  
         B     BHPGOK                                                           
*                                                                               
BHPGER   EQU   *                                                                
         DC    H'00'                                                            
         B     BHPGNO                                                           
*                                                                               
BHPGNO   B     NO                                                               
BHPGOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXTENDED HEADER REQUEST USERID FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
BLDHUID  NTR1                                                                   
*                                                                               
         MVC   ESSHUID,ESSDUID                                                  
         B     BHUIOK                                                           
*                                                                               
BHUIER   EQU   *                                                                
         DC    H'00'                                                            
         B     BHUINO                                                           
*                                                                               
BHUINO   B     NO                                                               
BHUIOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXTENDED HEADER REQUEST PASSWORD FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
BLDHPWD  NTR1                                                                   
*                                                                               
         MVC   ESSHPWD,ESSDPWD                                                  
         B     BHPWOK                                                           
*                                                                               
BHPWER   EQU   *                                                                
         DC    H'00'                                                            
         B     BHPWNO                                                           
*                                                                               
BHPWNO   B     NO                                                               
BHPWOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXTENDED HEADER FLAGS FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDH1FL  NTR1                                                                   
*                                                                               
         MVC   ESSDH1FL,ESSH1FL                                                 
         B     BH1FOK                                                           
*                                                                               
BH1FER   EQU   *                                                                
         DC    H'00'                                                            
         B     BH1FNO                                                           
*                                                                               
BH1FNO   B     NO                                                               
BH1FOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD HANDSHAKE PROCESS EXCHANGE DATA                               *         
***********************************************************************         
         SPACE 1                                                                
BLDHEXC  NTR1                                                                   
*                                                                               
         L     R3,AAPPCBUF                                                      
         LA    R3,L'APPC_BUFFER_LENGTH(R3)                                      
         USING ESSHDR,R3                                                        
         LA    R3,ESSHDRX                                                       
         DROP  R3                                                               
         USING ESSXDAT,R3                                                       
*                                  BUILD EXCHANGE TEXT C'MYNAMEIS'              
         BAS   RE,BLDXTXT                                                       
         BNE   BHEXER                                                           
*                                  BUILD UNIQUE INTEGER ID                      
         BAS   RE,BLDXID                                                        
         BNE   BHEXER                                                           
*                                  BUILD PASSWORD                               
         BAS   RE,BLDXPSWD                                                      
         BNE   BHEXER                                                           
*                                  BUILD NETWORK ID                             
         BAS   RE,BLDXNEID                                                      
         BNE   BHEXER                                                           
*                                  BUILD LOGICAL UNIT ID                        
         BAS   RE,BLDXLUID                                                      
         BNE   BHEXER                                                           
*                                  BUILD TRANSACTION PROGRAM NAME               
         BAS   RE,BLDXTPNA                                                      
         BNE   BHEXER                                                           
*                                  BUILD VERSION                                
         BAS   RE,BLDXVER                                                       
         BNE   BHEXER                                                           
*                                  BUILD LEVEL                                  
         BAS   RE,BLDXLEV                                                       
         BNE   BHEXER                                                           
*                                  BUILD DATE YYYYMMDD                          
         BAS   RE,SDATIME                                                       
*                                  BUILD DATE YYYYMMDD                          
         BAS   RE,BLDXDATE                                                      
         BNE   BHEXER                                                           
*                                  BUILD TIME HHMMSS                            
         BAS   RE,BLDXTIME                                                      
         BNE   BHEXER                                                           
*                                                                               
         MVC   ESSXDATX(2),=CL2'DD'                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,=Y(ESSXDATX-ESSXDAT)                                        
*        STCM  RE,3,ESSDDLN                                                     
         SR    RF,RF                                                            
         ICM   RF,3,APPCDLEN                                                    
         AR    RF,RE                                                            
         STCM  RF,3,APPCDLEN                                                    
         B     BHEXOK                                                           
*                                  EXCHANGE MESSAGE ERROR                       
BHEXER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BHEXNO   B     NO                                                               
BHEXOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE EXCHANGE TEXT FIELD C'MYNAMEIS'              *         
***********************************************************************         
         SPACE 1                                                                
BLDXTXT  NTR1                                                                   
*                                                                               
         MVC   ESSXTXT,ESSDXTXT                                                 
         B     BXTXOK                                                           
*                                                                               
BXTXER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXTXNO   B     NO                                                               
BXTXOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE UNIQUE INTEGER ID FIELD                      *         
***********************************************************************         
         SPACE 1                                                                
BLDXID   NTR1                                                                   
*                                                                               
         MVC   ESSXID,ESSDXID                                                   
         B     BXIDOK                                                           
*                                                                               
*                                                                               
BXIDER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXIDNO   B     NO                                                               
BXIDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE PASSWORD FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
BLDXPSWD NTR1                                                                   
*                                                                               
         MVC   ESSXPSWD,ESSDXPWD                                                
         B     BXPSOK                                                           
*                                                                               
BXPSER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXPSNO   B     NO                                                               
BXPSOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE METWORK ID FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
BLDXNEID NTR1                                                                   
*                                                                               
         MVC   ESSXNEID,ESSDXNID                                                
         B     BXNEOK                                                           
*                                                                               
BXNEER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXNENO   B     NO                                                               
BXNEOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE LOGICAL UNIT ID FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
BLDXLUID NTR1                                                                   
*                                                                               
         MVC   ESSXLUID,ESSDXLU                                                 
         B     BXLUOK                                                           
*                                                                               
BXLUER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXLUNO   B     NO                                                               
BXLUOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE TRANSACTION PROGRAM NAME FIELD               *         
***********************************************************************         
         SPACE 1                                                                
BLDXTPNA NTR1                                                                   
*                                                                               
         MVC   ESSXTPNA,ESSDXTPN                                                
         B     BXTPOK                                                           
*                                                                               
BXTPER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXTPNO   B     NO                                                               
BXTPOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE VERSION FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
BLDXVER  NTR1                                                                   
*                                                                               
         MVC   ESSXVER,ESSDXVER                                                 
         B     BXVEOK                                                           
*                                                                               
*                                                                               
BXVEER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXVENO   B     NO                                                               
BXVEOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE LEVEL FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
BLDXLEV  NTR1                                                                   
*                                                                               
         MVC   ESSXLEV,ESSDXLEV                                                 
         B     BXLEOK                                                           
*                                                                               
*                                                                               
BXLEER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXLENO   B     NO                                                               
BXLEOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE DATE FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDXDATE NTR1                                                                   
*                                                                               
         MVC   ESSXDATE,DATEN                                                   
         B     BXDAOK                                                           
*                                                                               
BXDAER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXDANO   B     NO                                                               
BXDAOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD EXCHANGE MESSAGE TIME FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDXTIME NTR1                                                                   
*                                                                               
         MVC   ESSXTIME,TIMEN                                                   
         B     BXTIOK                                                           
*                                                                               
BXTIER   EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
BXTINO   B     NO                                                               
BXTIOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SET UP DATE AND TIME VALUES                                         *         
***********************************************************************         
         SPACE 1                                                                
SDATIME  NTR1  ,                                                                
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                  GET THE CURRENT TIME IN BINARY               
TIME010  TIME  BIN                                                              
         LTR   R0,R0                                                            
         BZ    TIME010             BAD RETURN FROM MACRO                        
         ST    R0,TIMENB                                                        
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                  GET THE CURRENT MVS DATE/TIME                
TIME020  TIME                                                                   
         LTR   R0,R0                                                            
         BZ    TIME020             BAD RETURN FROM MACRO                        
         ST    R0,MVSTIME                                                       
         ST    R1,MVSDATE                                                       
*                                                                               
*                                  GET DATE NOW IN VARIOUS FORMATS              
         GOTO1 VDATCON,DMCB,(5,0),(20,DATEN)                                    
         GOTO1 VDATCON,DMCB,(5,0),(2,DATENC)                                    
         GOTO1 VDATCON,DMCB,(5,0),(3,DATENB)                                    
*                                                                               
         L     R1,MVSTIME                                                       
         STCM  R1,15,TIMEND                                                     
*                                  CONVERT TIME NOW TO EBCDIC                   
         SRL   R1,28                                                            
         STC   R1,TIMEN                                                         
         OI    TIMEN,X'F0'                                                      
         L     R1,MVSTIME                                                       
         SRL   R1,24                                                            
         STC   R1,TIMEN+1                                                       
         OI    TIMEN+1,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,20                                                            
         STC   R1,TIMEN+2                                                       
         OI    TIMEN+2,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,16                                                            
         STC   R1,TIMEN+3                                                       
         OI    TIMEN+3,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,12                                                            
         STC   R1,TIMEN+4                                                       
         OI    TIMEN+4,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,8                                                             
         STC   R1,TIMEN+5                                                       
         OI    TIMEN+5,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,4                                                             
         STC   R1,TIMEN+6                                                       
         OI    TIMEN+6,X'F0'                                                    
         L     R1,MVSTIME                                                       
         STC   R1,TIMEN+7                                                       
         OI    TIMEN+7,X'F0'                                                    
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAR ESSDATA MESSAGE COMMON DATA SAVE BLOCK                        *         
***********************************************************************         
         SPACE 1                                                                
CLEARED  NTR1                                                                   
*                                                                               
         ICM   RE,15,EATDATA                                                    
* ??     L     RF,=A(ESSDATAL)                                                  
         L     RF,=A(ESSDCLRL)                                                  
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         ICM   RE,15,EATDATA                                                    
         LA    RE,ESSDBUFF-ESSDATAD(RE)                                         
         L     RF,=A(ESSDBUFL)                                                  
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         ICM   RE,15,EATWORK                                                    
         ICM   RF,15,=AL4(ESSWORKL)                                             
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         XIT1                                                                   
         SPACE 1                                                                
***********************************************************************         
* CLEAR APPC BUFFER                                                   *         
***********************************************************************         
         SPACE 1                                                                
CLRAPPC  NTR1                                                                   
*                                                                               
         LA    RE,APPC_BUFFER_LENGTH                                            
         STCM  RE,15,AAPPCBUF                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER+L'APPC_BUFFER_LENGTH)                   
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         XIT1                                                                   
         SPACE 1                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         SPACE 1                                                                
* CREATE SYSOUT DATASETS -- DDNAME DYNAMIC ALLOCATION                           
*                                                                               
         DS    0F                                                               
ARBLKSOC DC    X'80',AL3(RBLKSOCA) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKSOCA DC    X'1401000000000000',A(ASOCALLT),X'0000000000000000'              
*                                                                               
ASOCALLT DC    X'00',AL3(TXTDD)                                                 
         DC    X'80',AL3(TXTSYSOU)                                              
         SPACE 2                                                                
         SPACE 2                                                                
* DYNAMIC UNALLOCATION BY DDNAME                                                
*                                                                               
         DS    0F                                                               
ARBLKUNA DC    X'80',AL3(RBLKUNA)  R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKUNA  DC    X'1402000000000000',A(ACATUNT),X'0000000000000000'               
*                                                                               
ACATUNT  DC    X'80',AL3(TXTUNDD)                                               
         SPACE 2                                                                
* DYNAMIC UNALLOCATION BY DSN - TURN OFF INUSE BIT                              
*                                                                               
         DS    0F                                                               
ARBLKUN2 DC    X'80',AL3(RBLKUNA2) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKUNA2 DC    X'1402000000000000',A(ACATUNT2),X'0000000000000000'              
*                                                                               
ACATUNT2 DC    X'00',AL3(TXTDSN)                                                
         DC    X'80',AL3(TXTREMOV)                                              
         SPACE 3                                                                
TXTDD    DC    AL2(DALDDNAM),X'00010008',CL8' '        DDNAME=........          
TXTDSN   DC    AL2(DALDSNAM),X'0001002C',CL44' '       DSN=............         
TXTUNDD  DC    AL2(DUNDDNAM),X'00010008',CL8' '        DDNAME=........          
TXTREMOV DC    AL2(DUNREMOV),X'0000'                   REMOVE INUSE ATR         
TXTSYSOU DC    AL2(DALSYSOU),X'0000'                   SYSOUT ALLOC.            
         DROP  RB,RA,R9,R8                                                      
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR MESSAGE AFTER LU6 APPC ERROR - MESSAGE TAG IN P(10)     *         
***********************************************************************         
         SPACE 1                                                                
PRNTLUER DS    0H                                                               
*                                                                               
         NMOD1 0,PRNTLUER                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
         MVC   ERRMSG(10),P                                                     
         MVC   P(80),=CL80'<ERROR????>'                                         
         MVC   P+12(10),ERRMSG                                                  
         MVC   P+23(40),=CL40'APPC LU6 ERROR          RC='                      
         ICM   RF,15,=AL4(ERNEIOLU)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         ICM   RF,15,RTN_CODE                                                   
         MVC   P+38(L'APPCACTN),APPCACTN                                        
         EDIT  (RF),(5,P+50),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),PLSPACES                                            
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(L'APPCACTN),APPCACTN                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(40),=CL40'RETURN CODE ='                                    
         ICM   RF,15,RTN_CODE                                                   
         EDIT  (RF),(5,P+34),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVI   P,C'*'                                                           
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(40),=CL40'DATA RECEIVED = '                                 
         ICM   RF,15,DATA_RECEIVED                                              
         EDIT  (RF),(5,P+36),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVI   P,C'*'                                                           
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(40),=CL40'REQUEST TO SEND RECEIVED = '                      
         ICM   RF,15,REQUEST_TO_SEND_RECEIVED                                   
         EDIT  (RF),(5,P+47),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVI   P,C'*'                                                           
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(40),=CL40'STATUS RECEIVED = '                               
         ICM   RF,15,STATUS_RECEIVED                                            
         EDIT  (RF),(5,P+38),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         XIT1                                                                   
         LTORG                                                                  
PLSPACES DC    132C' '                                                          
         EJECT                                                                  
PRNTATTB DS    0H                                                               
*                                                                               
         NMOD1 0,PRNTATTB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         MVC   P(40),=CL40'***************'                                     
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'APPC ATTRIBUTES'                                     
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'***************'                                     
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
         MVC   P(40),=CL40'CONVERSATION_ID'                                     
         GOTO1 VHEXOUT,DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                      
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'PARTNER_LU_NAME'                                     
         MVC   P+30(17),PARTNER_LU_NAME                                         
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'MODE_NAME'                                           
         MVC   P+30(8),MODE_NAME                                                
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'SYNC_LEVEL'                                          
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    PATT010                                                          
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    PATT010                                                          
         MVC   P+30(7),=C'UNKNOWN'                                              
PATT010  MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'TP_NAME_LENGTH'                                      
         EDIT  TPNAMELN,(2,P+30),ALIGN=LEFT,ZERO=NOBLANK                        
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'TP_NAME'                                             
         MVC   P+30(64),TP_NAME                                                 
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'LOCAL_LU_NAME'                                       
         MVC   P+30(8),LOCAL_LU_NAME                                            
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'CONVERSATION_TYPE'                                   
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    PATT020                                                          
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    PATT020                                                          
         MVC   P+30(7),=C'UNKNOWN'                                              
PATT020  MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*        MVC   P(40),=CL40'USER_ID'                                             
*        MVC   P+30(10),USER_ID                                                 
*        MVC   WTOMSGD,P                                                        
*        GOTO1 VESSLOG,DMCB,(R7),P                                              
*        GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*        MVC   P(40),=CL40'PROFILE'                                             
*        MVC   P+30(10),PROFILE                                                 
*        MVC   WTOMSGD,P                                                        
*        GOTO1 VESSLOG,DMCB,(R7),P                                              
*        GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*        MVC   P(40),=CL40'USER_TOKEN'                                          
*        MVC   P+30(80),USER_TOKEN                                              
*        MVC   WTOMSGD,P                                                        
*        GOTO1 VESSLOG,DMCB,(R7),P                                              
*        GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'CONVERSATION_STATE'                                  
         MVC   P+30(4),=C'SEND'                                                 
         CLC   CONVERSATION_STATE,ATB_SEND_STATE                                
         BE    PATT030                                                          
         MVC   P+30(7),=C'RECEIVE'                                              
         CLC   CONVERSATION_STATE,ATB_RECEIVE_STATE                             
         BE    PATT030                                                          
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   CONVERSATION_STATE,ATB_CONFIRM_STATE                             
         BE    PATT030                                                          
         MVC   P+30(10),=C'INITIALIZE'                                          
         CLC   CONVERSATION_STATE,ATB_INITIALIZE_STATE                          
         BE    PATT030                                                          
         MVC   P+30(12),=C'SEND PENDING'                                        
         CLC   CONVERSATION_STATE,ATB_SEND_PENDING_STATE                        
         BE    PATT030                                                          
         MVC   P+30(12),=C'CONFIRM SEND'                                        
         CLC   CONVERSATION_STATE,ATB_CONFIRM_SEND_STATE                        
         BE    PATT030                                                          
         MVC   P+30(18),=C'CONFIRM DEALLOCATE'                                  
         CLC   CONVERSATION_STATE,ATB_CONFIRM_DEALLOCATE_STATE                  
         BE    PATT030                                                          
         MVC   P+30(7),=C'UNKNOWN'                                              
*                                                                               
PATT030 EQU    *                                                                
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
         MVC   P(40),=CL40'*************'                                       
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
PRATTBX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* CP FILE THROUGH NFS                                                 *         
***********************************************************************         
CPNFS_X  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PARM_FIL+3(5),EATNAME+3                                          
         MVC   PARM_ESS+3(5),EATNAME+3                                          
         MVC   PARM_DR,=C'E/esslog'                                             
         MVC   PARM_ERD,DATEN                                                   
         MVC   PARM_ERT,TIMEN                                                   
         L     RF,AAPPCBUF                                                      
         AHI   RF,L'ESSHDR+2       PT TO MESSAGE                                
         USING ESQLDAT,RF                                                       
         MVC   PARM_ERR,ESQLAID                                                 
         DROP  RF                                                               
*                                                                               
         CLC   =C'10000',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C' ''10.253.33.36'                                      
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'11000',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C'''10.253.33.100'                                      
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'12000',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C' ''10.253.33.37'                                      
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'12500',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C'''10.253.34.134'                                      
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'13000',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C'''10.253.33.164'                                      
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'13500',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C' ''10.253.99.10'                                      
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'14000',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C'''10.253.130.25'                                      
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'14100',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C'''10.253.130.67'                                      
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'14500',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C'''10.253.130.27'                                      
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'10600',EATNAME+3                                              
         BNE   *+20                                                             
         MVC   PARM_IP,=C' ''10.253.35.25'                                      
         MVC   PARM_DR,=C'C/ESS   '  USE C: DRIVE INSTEAD OF E:                 
         B     CPNFS010                                                         
*                                                                               
         CLC   =C'10500',EATNAME+3                                              
         BNE   *+14                                                             
         MVC   PARM_IP,=C'''10.253.24.202'                                      
         B     CPNFS010                                                         
*                                                                               
         MVC   P(L'EATNAME),EATNAME                                             
         MVC   P+L'EATNAME(20),=CL20' CANNOT SEND VIA NFS'                      
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         B     CPNFSNO                                                          
*                                                                               
CPNFS010 EQU   *                                                                
         MVC   P(20),=CL20'start mounting'                                      
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         MVC   P(80),PARMMT                                                     
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   P(80),PARMMT+80                                                  
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         LINKX EP=BPXBATCH,PARAM=(PARMMT),VL=1,ERRET=ERRETMT                    
*                                                                               
         MVC   P(20),=CL20'finish mounting'                                     
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         MVI   PARMS,C' '                                                       
         MVC   PARMS+1(L'PARMS-1),PARMS                                         
         MVI   PARMX,C' '                                                       
         MVC   PARMX+1(L'PARMX-1),PARMX                                         
         L     RF,AAPPCBUF                                                      
         AHI   RF,L'ESSHDR+2       PT TO MESSAGE                                
         USING ESQLDAT,RF                                                       
         LA    RE,PARMS                                                         
*                                  GET THE SOURCE DSN                           
         MVC   0(L'ESQLDSN,RE),ESQLDSN                                          
         AHI   RE,L'ESQLDSN                                                     
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         B     *-10                                                             
         AHI   RE,1                                                             
*                                  END OF DSN                                   
         MVC   0(3,RE),=C'\'' '                                                 
         AHI   RE,3                                                             
*                                  MOUNT POINT                                  
         MVC   0(8,RE),=CL8'/u/yyun/'                                           
         AHI   RE,8                                                             
*                                  ESSID                                        
         MVC   0(L'EATNAME,RE),EATNAME                                          
         MVC   0(3,RE),=CL3'ESS'                                                
         AHI   RE,L'EATNAME                                                     
*                                  ESS WORK DIRECTORY                           
         MVC   0(11,RE),=CL11'/work/ftdl/'                                      
         AHI   RE,11                                                            
*                                  ESSID                                        
         MVC   0(L'EATNAME,RE),EATNAME                                          
         MVC   0(3,RE),=CL3'ESS'                                                
         AHI   RE,L'EATNAME                                                     
*                                                                               
         MVI   0(RE),C'/'                                                       
         AHI   RE,1                                                             
*                                  AGENCY                                       
         MVC   0(L'ESQLAID,RE),ESQLAID                                          
         AHI   RE,L'ESQLAID                                                     
*                                                                               
         MVI   0(RE),C'/'                                                       
         AHI   RE,1                                                             
*                                  SYS AND SUB SYS                              
         MVC   0(L'ESQLSYS+L'ESQLSUB,RE),ESQLSYS                                
         AHI   RE,L'ESQLSYS+L'ESQLSUB                                           
*                                                                               
         MVI   0(RE),C'/'                                                       
         AHI   RE,1                                                             
*                                  FILE NUMBER                                  
         MVC   0(L'ESQLFNUM,RE),ESQLFNUM                                        
         MVI   0(RE),C'0'          CLEAR C'3' LOAD CONTROL FLAG                 
         AHI   RE,L'ESQLFNUM                                                    
*                                  .DN FILE EXTENSION                           
         MVC   0(3,RE),=C'.DN'                                                  
         AHI   RE,3                                                             
*                                  ERROR FILE (AGY,SYS,SUBSYS,FIL#)             
         MVC   0(4,RE),=C' 2>>'                                                 
         AHI   RE,4                                                             
         MVC   0(L'ESQLAID+L'ESQLSYS+L'ESQLSUB+L'ESQLFNUM,RE),ESQLAID           
         AHI   RE,L'ESQLAID+L'ESQLSYS+L'ESQLSUB+L'ESQLFNUM                      
*                                                                               
         S     RE,=A(PARM+2)                                                    
         STH   RE,PARM                                                          
         DROP  RF                                                               
*                                                                               
         MVC   P(80),PARM                                                       
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   P(80),PARM+80                                                    
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         LINKX EP=BPXBATCH,PARAM=(PARM),VL=1,ERRET=ERRETCP                      
         MVC   P(40),=CL40'cp completed'                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
*&&DO                              DISABLE THIS, 11/10/05, YYUN                 
         MVI   FSNAME,C' '                                                      
         MVC   FSNAME+1(L'FSNAME-1),FSNAME                                      
         MVC   FSNAME(L'PARM_FIL),PARM_FIL                                      
         BRAS  RE,UNMOUNT                                                       
         MVC   P(10),=CL10'unmounted'                                           
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*&&                                                                             
*                                                                               
         J     YES                                                              
CPNFSNO  J     NO                                                               
*                                                                               
ERRETMT  DC    H'0'                                                             
PARMMT   DC    Y(PARMMTQ)                                                       
         DC    C'SH /usr/sbin/mount'                                            
         DC    C' -t NFS'                                                       
         DC    C' -s nosetuid'                                                  
         DC    C' -o '                                                          
PARM_IP  DC    CL14' ''10.253.33.36'                                            
         DC    C':/'                                                            
PARM_DR  DC    C'E/esslog'                                                      
         DC    C',xlat(Y),Delim(crnl),'                                         
         DC    C'DataCaching(N)'''                                              
         DC    C' -f '                                                          
PARM_FIL DC    CL8'ESS?????'                                                    
         DC    C' /u/yyun/'                                                     
PARM_ESS DC    CL8'ESS?????'                                                    
         DC    C'/'                                                             
         DC    C' 2>>'                                                          
PARM_ERR DC    CL12' '                                                          
         DC    C'_MERR_'                                                        
PARM_ERD DC    CL8' '                                                           
         DC    C'_'                                                             
PARM_ERT DC    CL8' '                                                           
PARMMTQ  EQU   *-PARMMT-2                                                       
*                                                                               
ERRETCP  DC    H'0'                                                             
PARM     DS    H                                                                
         DC    C'SH cp -F crnl //\'''                                           
PARMS    DS    CL250                                                            
PARMX    DS    CL250               EXTENSION AREA FOR PARMS                     
*                                                                               
*                                                                               
*1) MYOUT WILL BE UNIQUE FOR THIS SUBTASK                                       
*2) READ MYOUT, NOTHING -> CP OKAY, SOMETING -> BAD, PRINT ERROR                
*3) DELETE MYOUT AFTER READING                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* CP FILE THROUGH NFS                                                 *         
***********************************************************************         
CPNFS    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*---------------------------------------------------------------------*         
*BUILD NFS MOUNT COMMAND:                                             *         
* XL2(length)                                                         *         
* SH /usr/sbin/mount -t NFS -s nosetuid                               *         
* -o 'NFS_IP:/NFS_TO,xlat(Y),Delim(crnl),DataCaching(N)'              *         
* -f NFS_FILESYSTEM NFS_FR                                            *         
* 2>> ERROR_OUTPUT_FILE                                               *         
*                                                                     *         
* ex:                                                                 *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                               
         LA    RE,PMMT             CLEAR PARM AREA                              
         LHI   RF,PMMTX-PMMT                                                    
         XCEFL                                                                  
*                                                                               
         LA    R1,PMMTS                                                         
         MVC   0(L'PMMT_C1,R1),PMMT_C1                                          
         AHI   R1,L'PMMT_C1                                                     
*                                                                               
         CLC   EATNFSIP,SPACES                                                  
         BNH   CPXERR              MISSING NFS IP ADDR                          
         MVC   0(L'EATNFSIP,R1),EATNFSIP                                        
         AHI   R1,L'EATNFSIP                                                    
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         B     *-10                                                             
         AHI   R1,1                                                             
*                                                                               
         MVC   0(L'PMMT_C2,R1),PMMT_C2                                          
         AHI   R1,L'PMMT_C2                                                     
*                                                                               
         CLC   EATNFSTO,SPACES                                                  
         BNH   CPXERR              MISSING NFS TO DIR                           
         MVC   0(L'EATNFSTO,R1),EATNFSTO                                        
         AHI   R1,L'EATNFSTO                                                    
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         B     *-10                                                             
         AHI   R1,1                                                             
*                                                                               
         MVC   0(L'PMMT_C3,R1),PMMT_C3                                          
         AHI   R1,L'PMMT_C3                                                     
*                                  ESS?????                                     
         MVC   0(3,R1),=CL3'ESS'                                                
         MVC   3(5,R1),EATNAME+3                                                
         MVI   8(R1),C' '                                                       
         AHI   R1,9                                                             
*                                                                               
         CLC   EATNFSFR,SPACES                                                  
         BNH   CPXERR              MISSING NFS FROM DIR                         
         MVC   0(L'EATNFSFR,R1),EATNFSFR                                        
         AHI   R1,L'EATNFSFR                                                    
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         B     *-10                                                             
         AHI   R1,1                                                             
*                                                                               
         MVC   0(L'PMMT_C4,R1),PMMT_C4                                          
         AHI   R1,L'PMMT_C4                                                     
*                                  BUILD ERROR OUTPUT FILE NAME                 
         L     RF,AAPPCBUF                                                      
         AHI   RF,L'ESSHDR+2       PT TO MESSAGE                                
         USING ESQLDAT,RF                                                       
         MVC   PMMT_ERR,ESQLAID                                                 
         DROP  RF                                                               
         MVC   PMMT_ERD,DATEN                                                   
         MVC   PMMT_ERT,TIMEN                                                   
         MVC   0(PMMT_ERQ,R1),PMMT_ERR                                          
         AHI   R1,PMMT_ERQ                                                      
*                                  CALCULATE THE LENGTH                         
         LA    RE,PMMTS                                                         
         SR    R1,RE                                                            
         STH   R1,PMMTL                                                         
*                                                                               
         MVC   P(20),=CL20'start mounting'                                      
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   P(80),PMMT                                                       
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   P(80),PMMT+80                                                    
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   P(80),PMMT+160                                                   
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         LINKX EP=BPXBATCH,PARAM=(PMMT),VL=1,ERRET=ERRXMT                       
         MVC   P(20),=CL20'finish mounting'                                     
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
*---------------------------------------------------------------------*         
*BUILD cp COMMAND:                                                    *         
* XL2(length)                                                         *         
* SH cp -F crnl //\'SOURCE_DATASET_NAME\'                             *         
*  NFS_FR/work/ftdl/ESSID/AGENCY/SYSTEM_SUBSYSTEM/FILE_NUMBER.DN      *         
*  2>>ERROR_OUTPUT_FILE                                               *         
*                                                                     *         
*Note: NFS_FR includes ending '/' already.                            *         
*      FILE_NUMBER's 1st byte is load control flag.                   *         
*                                                                     *         
*EX:                                                                  *         
* .aSH cp -F crnl //\'DDS.XTR.BLRS.G01780.ORD.D081202.T010112\'       *         
* /u/yyun/ESS14500/work/ftdl/ESS14500/BL/RS/00001779.DN               *         
* 2>>BLRS30001779                                                     *         
*---------------------------------------------------------------------*         
*                                                                               
         LA    RE,PMCP             CLEAR PARM AREA                              
         LHI   RF,PMCPX-PMCP                                                    
         XCEFL                                                                  
*                                                                               
         LA    R1,PMCPS                                                         
         MVC   0(L'PMCP_C1,R1),PMCP_C1                                          
         AHI   R1,L'PMCP_C1                                                     
*                                                                               
         L     RF,AAPPCBUF                                                      
         AHI   RF,L'ESSHDR+2       PT TO MESSAGE                                
         USING ESQLDAT,RF                                                       
*                                  GET THE SOURCE DSN                           
         MVC   0(L'ESQLDSN,R1),ESQLDSN                                          
         AHI   R1,L'ESQLDSN                                                     
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         B     *-10                                                             
         AHI   R1,1                                                             
*                                                                               
         MVC   0(L'PMCP_C2,R1),PMCP_C2                                          
         AHI   R1,L'PMCP_C2                                                     
*                                  GET NFS FROM DIR                             
         MVC   0(L'EATNFSFR,R1),EATNFSFR                                        
         AHI   R1,L'EATNFSFR                                                    
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         B     *-10                                                             
         AHI   R1,1                                                             
*                                                                               
         MVC   0(L'PMCP_C3,R1),PMCP_C3                                          
         AHI   R1,L'PMCP_C3                                                     
*                                  ESSID#                                       
         MVC   0(5,R1),EATNAME+3                                                
         AHI   R1,5                                                             
*                                                                               
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
*                                  AGENCY                                       
         MVC   0(L'ESQLAID,R1),ESQLAID                                          
         AHI   R1,L'ESQLAID                                                     
*                                                                               
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
*                                  SYS AND SUB SYS                              
         MVC   0(L'ESQLSYS+L'ESQLSUB,R1),ESQLSYS                                
         AHI   R1,L'ESQLSYS+L'ESQLSUB                                           
*                                                                               
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
*                                  FILE NUMBER                                  
         MVC   0(L'ESQLFNUM,R1),ESQLFNUM                                        
         MVI   0(R1),C'0'          CLEAR C'3' LOAD CONTROL FLAG                 
         AHI   R1,L'ESQLFNUM                                                    
*                                  .DN FILE EXTENSION                           
         MVC   0(L'PMCP_C4,R1),PMCP_C4                                          
         AHI   R1,L'PMCP_C4                                                     
*                                  2>> ERROR OUTPUT                             
         MVC   0(L'PMCP_C5,R1),PMCP_C5                                          
         AHI   R1,L'PMCP_C5                                                     
*                                  ERROR FILE (AGY,SYS,SUBSYS,FIL#)             
         MVC   0(L'ESQLAID+L'ESQLSYS+L'ESQLSUB+L'ESQLFNUM,R1),ESQLAID           
         AHI   R1,L'ESQLAID+L'ESQLSYS+L'ESQLSUB+L'ESQLFNUM                      
         DROP  RF                                                               
*                                  CALCULATE THE LENGTH                         
         LA    RE,PMCPS                                                         
         SR    R1,RE                                                            
         STH   R1,PMCPL                                                         
*                                                                               
         MVC   P(80),PMCP                                                       
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   P(80),PMCP+80                                                    
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         MVC   P(80),PMCP+160                                                   
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
         LINKX EP=BPXBATCH,PARAM=(PMCP),VL=1,ERRET=ERRXCP                       
         MVC   P(40),=CL40'cp completed'                                        
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*                                                                               
*Code pending to be done.                                                       
*1) MYOUT WILL BE UNIQUE FOR THIS SUBTASK                                       
*2) READ MYOUT, NOTHING -> CP OKAY, SOMETHING -> BAD, PRINT ERROR               
*3) DELETE MYOUT AFTER READING                                                  
*                                                                               
*                                                                               
*&&DO                              DISABLE THIS, 11/10/05, YYUN                 
         MVI   FSNAME,C' '                                                      
         MVC   FSNAME+1(L'FSNAME-1),FSNAME                                      
         MVC   FSNAME(3),=CL3'ESS'                                              
         MVC   FSNAME+3(5),EATNAME+3                                            
         BRAS  RE,UNMOUNT                                                       
         MVC   P(10),=CL10'unmounted'                                           
         GOTO1 VESSLOG,DMCB,(R7),P                                              
*&&                                                                             
*                                                                               
         J     YES                                                              
CPXERR   MVC   P(20),=CL20'Missing NFS Info'                                    
         GOTO1 VESSLOG,DMCB,(R7),P                                              
         J     NO                                                               
*                                                                               
ERRXMT   DC    H'0'                                                             
ERRXCP   DC    H'0'                                                             
*                                                                               
PMMT_C1  DC    C'SH /usr/sbin/mount -t NFS -s nosetuid -o '''                   
PMMT_C2  DC    C':/'                                                            
PMMT_C3  DC    C',xlat(Y),Delim(crnl),DataCaching(N)'' -f '                     
PMMT_C4  DC    C' 2>>'                                                          
PMMT_ERR DC    CL12' ',C'_MERR_'                                                
PMMT_ERD DC    CL8' ',C'_'                                                      
PMMT_ERT DC    CL8' '                                                           
PMMT_ERQ EQU   *-PMMT_ERR                                                       
*                                                                               
PMMT     DS    0H                                                               
PMMTL    DS    H                                                                
PMMTS    DS    XL300                                                            
PMMTX    DS    0X                                                               
*                                                                               
PMCP     DS    0H                                                               
PMCPL    DS    H                                                                
PMCPS    DS    CL300                                                            
PMCPX    DS    0X                                                               
*                                                                               
PMCP_C1  DC    C'SH cp -F crnl //\'''                                           
PMCP_C2  DC    C'\'' '                                                          
PMCP_C3  DC    C'work/ftdl/ESS'                                                 
PMCP_C4  DC    C'.DN'                                                           
PMCP_C5  DC    C' 2>>'                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UNMOUNT CALL                                                                  
***********************************************************************         
UNMOUNT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     R1,16(,0)       CVT - COMMON VECTOR TABLE                        
         L     R1,544(R1)      CSRTABLE                                         
         L     R1,24(R1)       CSR SLOT                                         
         MVC   BPX1UMT,BPX1UMTQ(R1)     ADDRESS OF THE SERVICE UMOUNT           
*                                                                               
         XC    MTM(MTM#LENGTH),MTM                                              
         MVI   MTM1,MTMUMOUNT+MTMIMMED                                          
*                                                                               
         L     RF,BPX1UMT                                                       
         CALL  (15),                 READY A FILE SYSTEM FOR USE       +        
               (FSNAME,              INPUT: FILE SYSTEM NAME (44 CHAR) +        
               MTM,                  INPUT: MOUNT MODE         BPXYMTM +        
               RETVAL,               RETURN VALUE: 0 OR -1             +        
               RETCODE,              RETURN CODE                       +        
               RSNCODE),             REASON CODE                       +        
               VL,MF=(E,PLIST)       ----------------------------------         
*                                                                               
         ICM   RF,B'1111',RETVAL   TEST RETVAL                                  
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
*                                                                               
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         SPACE 1                                                                
***********************************************************************         
* APPC/MVS PSEUDONYM DEFINITIONS                                     *          
***********************************************************************         
         SPACE 1                                                                
         ATBSERV                                                                
         SPACE 1                                                                
ATBCTS_WAIT    EQU   ATB_FW2                                                    
TESTECB  DC    F'0'                                                             
TESTRC   DC    F'0'                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
VDATAMGR DS    V                                                                
VDATCON  DS    V                                                                
VDATTIM  DS    V                                                                
VHEXOUT  DS    V                                                                
VHEXIN   DS    V                                                                
VNUMVAL  DS    V                                                                
VEXSERV  DS    V                                                                
VEIOSQL  DS    V                                                                
VEIOPQR  DS    V                                                                
VESSLOG  DS    V                                                                
VLOGIO   DS    V                                                                
VESSWTO  DS    V                                                                
VESSAPPC DS    V                                                                
VMAJORNM DS    V                                                                
VPRINTER DS    V                                                                
VPRNTBL  DS    V                                                                
         EJECT                                                                  
         SPACE 2                                                                
DMCB     DS    6F                                                               
DMWORK   DS    12D                                                              
FULL     DS    F                                                                
RELO     DS    A                                                                
DUB      DS    D                                                                
ALU6ECB  DS    A                                                                
ATESTECB DS    A                                                                
RETCODE1 DS    XL1                                                              
P        DS    CL132                                                            
WORK     DS    CL256                                                            
ELCODE   DS    X                                                                
DATADISP DS    H                                                                
STIMERID DS    XL4                 FOR WAIT ROUTINE                             
ERRFLAG  DS    CL1                 'Y' = VALIDATION ERROR                       
FIRSTQ   EQU   0                                                                
NEXTQ    EQU   1                                                                
MFFSAVE  DS    XL1                 SAVE VALUE OF ESSDMFF                        
*                                                                               
MVSTIME  DS    F                   IBM TIME BINARY 100S SECS                    
MVSDATE  DS    F                   IBM DATE JULIAN                              
*                                                                               
DATEN    DS    CL8                 DATE NOW EBCDIC                              
DATENC   DS    XL2                 DATE NOW COMPRESSED                          
DATENB   DS    XL3                 DATE NOW BINARY                              
TIMEN    DS    CL8                 TIME NOW EBCDIC (HHMMSSTH)                   
TIMEND   DS    XL4                 TIME NOW DECIMAL UNSIGNED PACKED             
TIMENB   DS    F                   TIME NOW BINARY FULL WORD                    
*                                                                               
REQCODE  DS    XL3                                                              
*                                  APPC/MVS BUFFER PARAMETERS                   
APPCDLEN DS    XL2                 APPC DATA RECEIVE LENGTH                     
*                                                                               
TODAY    DS    CL8                 DATE C'DD/MM/YY' OR C'MM/DD/YY'              
*                                                                               
*                                                                               
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
LU6ERROR DC    C'N'                'Y' = APPC LU6 ERROR DETECTED                
APPCECB  DC    F'0'                                                             
ECBVALUE DC    A(0)                                                             
*                                                                               
EXECBLST DS    0F                  APPC/MVS ECB LIST                            
         DC    X'00',AL3(0)        A(EATMTECB)                                  
         DC    X'80',AL3(0)        A(EATSPECB)                                  
*                                                                               
APPCECBL DS    0F                  APPC/MVS ECB LIST                            
AAPPCEST DC    X'00',AL3(0)                                                     
AAPPCECB DC    X'80',AL3(0)                                                     
         SPACE 2                                                                
*                                                                               
ERRMSG   DS    CL80                                                             
*                                                                               
SPACES   DC    132C' '                                                          
OPMSG    DC    CL50' '                                                          
*                                                                               
WTOALARM DC    CL2'*A'             WTO ALARM MESSAGE                            
WTOMSG   DS    0CL128              WTO MESSAGE AREA                             
WTOMSGK  DS    CL28                KEY PART                                     
WTOMSGD  DS    CL100               DETAIL PART                                  
*                                                                               
RETCODE  DS    F                   RETURN CODE (ERRNO)                          
RSNCODE  DS    F                   REASON CODE                                  
RETVAL   DS    F                   RETURN VALUE (0, -1 OR OTHER)                
*                                                                               
BPX1UMTQ EQU   208                 UMOUNT                                       
BPX1UMT  DS    A                   A(UMOUNT)                                    
*                                                                               
FTPFLAG  DC    C'N'                                                             
FSNAME   DS    CL44                                                             
PLIST    DS    13A              CALL PARMLIST WORK AREA                         
*                                                                               
         BPXYMTM  DSECT=NO                                                      
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE DDESSAPPCD                                                     
         EJECT                                                                  
       ++INCLUDE DDESSD                                                         
         EJECT                                                                  
       ++INCLUDE GEGENESS                                                       
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
         IEFZB4D2                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035DDESSIO   01/08/09'                                      
         END                                                                    
