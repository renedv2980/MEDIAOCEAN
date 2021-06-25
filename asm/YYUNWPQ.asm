*          DATA SET YYUNWPQ    AT LEVEL 045 AS OF 05/01/02                      
*PHASE YYUNWPQA                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'DEIS TEST APPC'                                                 
*                                                                               
* READ FROM A DATA SET AND THEN ASCH JOB TO PUT A REPORT ON THE PQ              
*                                                                               
         MACRO                                                                  
&NAME    PRNT  &A,&PRINT=                                                       
&NAME    DS    0H                                                               
         AIF   (T'&A EQ 'O').NOMOVE                                             
         MVC   P(17),=CL17'&A'                                                  
.NOMOVE  ANOP                                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+18(2),PRNTTIME                                                 
         MVI   P+20,C':'                                                        
         MVC   P+21(2),PRNTTIME+2                                               
         MVI   P+23,C':'                                                        
         MVC   P+24(2),PRNTTIME+4                                               
         MVI   P+26,C'.'                                                        
         MVC   P+27(2),PRNTTIME+6                                               
         AIF   ('&PRINT' EQ 'ALWAYS').SKIP                                      
         CLI   TRACEFLG,C'Y'                                                    
         BNE   *+10                                                             
.SKIP    L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MEND                                                                   
         EJECT                                                                  
DEISTEST CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**DEIS**,=A(R13CHAIN),R9,R7                                    
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BAS   RE,INITIAL                                                       
*                                                                               
         MVC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         MVC   PARTNER_LU_NAME(8),=C'KTZDDS1T'                                  
*********MVC   PARTNER_LU_NAME(8),=C'KTZDDS1P'                                  
         MVC   TP_NAME_LENGTH,=F'32'                                            
         MVC   TP_NAME(32),=C'KATZ_TO_DDS_REP_REPORT_XFER_TEST'                 
*********MVC   TP_NAME_LENGTH,=F'31'                                            
*********MVC   TP_NAME(31),=C'KATZ_TO_DDS_REP_REPORT_TRANSFER'                  
         MVC   MODE_NAME,=C'DDSLU62 '                                           
         MVC   RETURN_CONTROL,ATB_WHEN_SESSION_ALLOCATED                        
         MVC   SYNC_LEVEL,ATB_CONFIRM                                           
         MVC   SECURITY_TYPE,ATB_SECURITY_SAME                                  
         MVC   USER_ID,=C'DEIS      '                                           
*                                                                               
         LA    R2,ATBALC2_STRUCTURE                                             
         BAS   RE,CALLAPPC         ALLOCATE THE CONVERSATION                    
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,ATBCFM_STRUCTURE                                              
         BAS   RE,CALLAPPC         REQUEST CONFIRMATION                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,PRNTATTB         PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(61),=C'VERSION=1,USERID=SJRDEIS   ,SUBID=SDE,DESC+        
               RIPTION=TEST RPT.  '                                             
         LA    R5,61                                                            
         MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         BAS   RE,LINXMIT                                                       
         BE    SENDDATA                                                         
*                                                                               
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_ABEND                             
         CLC   RETURN_CODE,ATB_PROGRAM_ERROR_NO_TRUNC                           
         BE    DEALLOC                                                          
         CLC   RETURN_CODE,ATB_PROGRAM_ERROR_PURGING                            
         BE    DEALLOC                                                          
         CLC   RETURN_CODE,ATB_PROGRAM_ERROR_TRUNC                              
         BE    DEALLOC                                                          
         DC    H'0'                                                             
*                                                                               
SENDDATA XC    BUFFER,BUFFER                                                    
         MVC   BUFFER,=CL132'    *HDR*                                 +        
                                          M'                                    
         LA    R5,132                                                           
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BAS   RE,LINXMIT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(60),=CL60'++DDS      BCC YYUNN'                           
         LA    R5,60                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BAS   RE,LINXMIT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(60),=CL60'++DDS      SUB EZ SOMETHING'                    
         LA    R5,60                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BAS   RE,LINXMIT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (FILE,INPUT)                                                     
         GET   FILE,REC                                                         
         MVC   REC2,REC                                                         
*                                                                               
GETMORE  GET   FILE,REC                                                         
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(160),REC2    SEND PREVIOUS REC                            
         MVC   REC2,REC            SAVE CURRENT REC                             
         LA    R5,160                                                           
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BAS   RE,LINXMIT                                                       
         B     GETMORE                                                          
*                                                                               
EOF      CLOSE FILE                                                             
*                                                                               
*                                                                               
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(160),REC     SEND THE CURRENT LAST REC                    
         LA    R5,160                                                           
         MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         BAS   RE,LINXMIT                                                       
         BNE   *+14                                                             
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_SYNC_LEVEL                        
         B     DEALLOC                                                          
*                                                                               
*        XC    BUFFER,BUFFER                                                    
*        MVC   BUFFER(14),=C'END OF REPORT '                                    
*        LA    R5,14                                                            
*        MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
*        BAS   RE,LINXMIT                                                       
*        BNE   *+14                                                             
*        MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_SYNC_LEVEL                        
*        B     DEALLOC                                                          
*                                                                               
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_ABEND                             
         CLC   RETURN_CODE,ATB_PROGRAM_ERROR_NO_TRUNC                           
         BE    DEALLOC                                                          
         CLC   RETURN_CODE,ATB_PROGRAM_ERROR_PURGING                            
         BE    DEALLOC                                                          
         CLC   RETURN_CODE,ATB_PROGRAM_ERROR_TRUNC                              
         BE    DEALLOC                                                          
         DC    H'0'                                                             
*                                                                               
DEALLOC  LA    R2,ATBDEAL_STRUCTURE                                             
         BAS   RE,CALLAPPC         DEALLOCATE THE CONVERSATION                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         PRNT  DEALLOCATEDOK,PRINT=ALWAYS                                       
*                                                                               
         PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         MVC   TITLE(21),=C'APPC/MVS TEST PROGRAM'                              
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
         LOAD  DE=ATBALC2                                                       
         ST    R0,ATBALC2_STRUCTURE                                             
         LOAD  DE=ATBDEAL                                                       
         ST    R0,ATBDEAL_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE                                             
         LOAD  DE=ATBCFM                                                        
         ST    R0,ATBCFM_STRUCTURE                                              
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
LINXMIT  NTR1                                                                   
*                                                                               
* ON ENTRY, R5 = LENGTH OF DATA                                                 
*                                                                               
         ST    R5,SEND_LENGTH                                                   
*                                                                               
         LA    RE,=C'BUFFER DATA'                                               
         ST    RE,DMCB                                                          
         MVI   DMCB,11                                                          
         CLC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BE    LINX10                                                           
         LA    RE,=C'SEND AND CONFIRM'                                          
         ST    RE,DMCB                                                          
         MVI   DMCB,16                                                          
         CLC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         BE    LINX10                                                           
         DC    H'0'                UNKNOWN SEND TYPE                            
LINX10   GOTO1 =V(PRNTBL),DMCB,,BUFFER,C'DUMP',(R5),=C'1D'                      
*                                                                               
LINX20   LA    R2,ATBSEND_STRUCTURE                                             
         BAS   RE,CALLAPPC                                                      
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    LINXOK              RETURN CODE WAS OK                           
*                                                                               
         LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     LINXX                                                            
*                                                                               
LINXOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
LINXX    XIT1                                                                   
         EJECT                                                                  
PRNTATTB NTR1                                                                   
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  GETATTRIBUTES,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
         BAS   RE,CALLAPPC                                                      
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+30,4,=C'TOG'                   
         PRNT  CONVERSATION_ID,PRINT=ALWAYS                                     
         MVC   P+30(17),PARTNER_LU_NAME                                         
         PRNT  PARTNER_LU_NAME,PRINT=ALWAYS                                     
         MVC   P+30(8),MODE_NAME                                                
         PRNT  MODE_NAME,PRINT=ALWAYS                                           
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    *+22                                                             
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    *+6                                                              
         DC    H'0'                UNKNOWN SYNC LEVEL                           
         PRNT  SYNC_LEVEL,PRINT=ALWAYS                                          
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TP_NAME,PRINT=ALWAYS                                             
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LOCAL_LU_NAME,PRINT=ALWAYS                                       
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    *+22                                                             
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    *+6                                                              
         DC    H'0'                UNKNOWN CONVERSATION TYPE                    
         PRNT  CONVERSATION_TYPE,PRINT=ALWAYS                                   
         MVC   P+30(4),=C'SEND'                                                 
         CLC   CONVERSATION_STATE,ATB_SEND_STATE                                
         BE    PRATTB10                                                         
         MVC   P+30(7),=C'RECEIVE'                                              
         CLC   CONVERSATION_STATE,ATB_RECEIVE_STATE                             
         BE    PRATTB10                                                         
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   CONVERSATION_STATE,ATB_CONFIRM_STATE                             
         BE    PRATTB10                                                         
         MVC   P+30(10),=C'INITIALIZE'                                          
         CLC   CONVERSATION_STATE,ATB_INITIALIZE_STATE                          
         BE    PRATTB10                                                         
         MVC   P+30(12),=C'SEND PENDING'                                        
         CLC   CONVERSATION_STATE,ATB_SEND_PENDING_STATE                        
         BE    PRATTB10                                                         
         MVC   P+30(12),=C'CONFIRM SEND'                                        
         CLC   CONVERSATION_STATE,ATB_CONFIRM_SEND_STATE                        
         BE    PRATTB10                                                         
         MVC   P+30(18),=C'CONFIRM DEALLOCATE'                                  
         CLC   CONVERSATION_STATE,ATB_CONFIRM_DEALLOCATE_STATE                  
         BE    PRATTB10                                                         
         DC    H'0'                UNKNOWN CONVERSATION STATE                   
PRATTB10 PRNT  CONVERSATION_STATE,PRINT=ALWAYS                                  
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
CALLAPPC NTR1                                                                   
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, RETURN_CODE CONTAINS THE APPC/MVS RETURN CODE                    
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         TM    20(R2),X'80'                                                     
         BZ    *+10                                                             
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,0(R2)            RF = A(APPC/MVS ROUTINE)                     
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         TM    20(R2),X'80'        SYNCHRONOUS CALL?                            
         BO    CALL20              YES, SO IT'S DONE                            
*                                                                               
         CLC   RETURN_CODE,ATB_OK                                               
         BE    CALL10              APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         B     CALL20              APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALL10   WAIT  1,ECB=APPCECB       WAIT FOR COMPLETION                          
*                                                                               
         TM    APPCECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPCECB                                              
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPCECB,APPCECB                                                  
*                                                                               
CALL20   MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CALLX                                                            
         TM    20(R2),X'40'        CAN CALL EXTRACT_ERROR NOW?                  
         BZ    CALLX                                                            
         BAS   RE,APPCERR          YES                                          
*                                                                               
CALLX    XIT1                                                                   
         EJECT                                                                  
APPCERR  NTR1                                                                   
*                                                                               
* PERFORM APPC EXTRACT_ERROR FUNCTION.                                          
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         LA    R2,ATBEES3_STRUCTURE                                             
         L     RF,0(R2)            RF = A(APPC/MVS ROUTINE)                     
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_ERROR_EXTRACT,(5,P+44),ALIGN=LEFT,          +        
               ZERO=NOBLANK                                                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE_ERROR_EXTRACT,RETURN_CODE_ERROR_EXTRACT              
         BNZ   APPCEX              BAD RETURN CODE, SO NO ERROR INFO            
*                                                                               
         MVC   P+30(37),=C'APPC SERVICE XXXXXXXX, REASON_CODE = '               
         MVC   P+43(8),SERVICE_NAME                                             
         EDIT  SERVICE_REASON_CODE,(5,P+67),ALIGN=LEFT,ZERO=NOBLANK             
         PRNT  ERROR_EXTRACT                                                    
*                                                                               
         ICM   R3,15,MESSAGE_TEXT_LENGTH                                        
         BZ    APPCE20             NOTHING IN FIELD                             
         C     R3,=F'132'          MESSAGE > 132 CHARACTERS?                    
         BH    APPCE10             YES, BREAK MESSAGE UP INTO TWO LINES         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MESSAGE_TEXT                                                
         GOTO1 =V(PRINTER)                                                      
         B     APPCE20                                                          
*                                                                               
APPCE10  S     R3,=F'132'                                                       
         MVC   P,MESSAGE_TEXT                                                   
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MESSAGE_TEXT+132                                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
APPCE20  ICM   R3,15,ERROR_LOG_INFORMATION_LENGTH                               
         BZ    APPCEX              NOTHING IN FIELD                             
         C     R3,=F'132'          MESSAGE > 132 CHARACTERS?                    
         BH    APPCE30             YES, BREAK MESSAGE UP                        
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION                                       
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX                                                           
*                                                                               
APPCE30  C     R3,=F'264'          WILL MESSAGE FIT ON TWO LINES?               
         BH    APPCE40                                                          
         S     R3,=F'132'          YES                                          
         MVC   P,ERROR_LOG_INFORMATION                                          
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION+132                                   
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX                                                           
*                                                                               
APPCE40  C     R3,=F'396'          WILL MESSAGE FIT ON THREE LINES?             
         BH    APPCE50                                                          
         S     R3,=F'264'          YES                                          
         MVC   P,ERROR_LOG_INFORMATION                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION+132                                      
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION+264                                   
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX                                                           
*                                                                               
APPCE50  S     R3,=F'396'          SPLIT INTO FOUR LINES                        
         MVC   P,ERROR_LOG_INFORMATION                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION+132                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION+264                                      
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION+396                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
APPCEX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         ATBSERV                                                                
         SPACE 2                                                                
* APPC/MVS CALL PARAMETERS                                                      
*                                                                               
CONVERSATION_TYPE              DS    F                                          
SYM_DEST_NAME                  DC    CL8' '                                     
PARTNER_LU_NAME                DC    CL17' '                                    
LOCAL_LU_NAME                  DC    CL8' '                                     
MODE_NAME                      DS    CL8                                        
TP_NAME_LENGTH                 DS    F                                          
TP_NAME                        DC    CL64' '                                    
RECEIVER_TP_NAME               DC    CL64' '                                    
CONVERSATION_CORRELATOR        DS    CL8                                        
LUW_ID                         DS    XL26                                       
RETURN_CONTROL                 DS    F                                          
SYNC_LEVEL                     DS    F                                          
SECURITY_TYPE                  DS    F                                          
USER_ID                        DC    CL10' '                                    
PASSWORD                       DC    CL10' '                                    
PROFILE                        DC    CL10' '                                    
USER_TOKEN                     DC    XL80'00'                                   
CONVERSATION_ID                DS    CL8                                        
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY_TYPE         
TPID                           DC    XL8'00'                                    
RETURN_CODE                    DS    F                                          
CONVERSATION_STATE             DS    F                                          
DEALLOCATE_TYPE                DS    F                                          
REQUEST_TO_SEND_VALUE          DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
SEND_TYPE                      DS    F                                          
SEND_LENGTH                    DS    F                                          
SEND_ACCESS_TOKEN              DC    F'0'                                       
BUFFER                         DS    CL256                                      
SERVICE_NAME                   DS    CL8                                        
SERVICE_REASON_CODE            DS    F                                          
MESSAGE_TEXT_LENGTH            DS    F                                          
MESSAGE_TEXT                   DS    CL256                                      
ERROR_LOG_PRODUCT_SET_ID_LENGTH DS   F                                          
ERROR_LOG_PRODUCT_SET_ID       DS    CL256                                      
ERROR_LOG_INFORMATION_LENGTH   DS    F                                          
ERROR_LOG_INFORMATION          DS    CL512                                      
REASON_CODE_ERROR_EXTRACT      DS    F                                          
RETURN_CODE_ERROR_EXTRACT      DS    F                                          
                               EJECT                                            
MYSPACES DC    CL256' '                                                         
         SPACE 2                                                                
REC2     DS    CL255                                                            
FILE     DCB   DDNAME=FILE,MACRF=GM,DSORG=PS,EODAD=EOF                          
REC      DS    CL255                                                            
         DS    CL200                                                            
         SPACE 2                                                                
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
PRNTTIME DS    CL9                                                              
WORK     DS    CL256                                                            
TRACEFLG DC    C'Y'                'Y' TO PRINT DETAILED TRACE                  
ERRCODE  DS    X                   ERROR REASON CODE                            
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
         SPACE 2                                                                
         EJECT                                                                  
* PARAMETER LISTS FOR APPC/MVS CALLS                                            
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*       X'80': ROUTINE IS SYNCHRONOUS ONLY                                      
*       X'40': ROUTINE CAN BE FOLLOWED BY EXTRACT_ERROR CALL                    
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
ATBALC2_STRUCTURE         DS    A                                               
                          DC    CL16'ALLOCATE'                                  
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(SYM_DEST_NAME)                        
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(TP_NAME_LENGTH)                       
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(RETURN_CONTROL)                       
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(SECURITY_TYPE)                        
                          DC    X'00',AL3(USER_ID)                              
                          DC    X'00',AL3(PASSWORD)                             
                          DC    X'00',AL3(PROFILE)                              
                          DC    X'00',AL3(USER_TOKEN)                           
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(TPID)                                 
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBCFM_STRUCTURE          DS    A                                               
                          DC    CL16'REQUEST_CONFIRM'                           
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED)             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBDEAL_STRUCTURE         DS    A                                               
                          DC    CL16'DEALLOCATE'                                
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(DEALLOCATE_TYPE)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBSEND_STRUCTURE         DS    A                                               
                          DC    CL16'SEND_DATA'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(SEND_TYPE)                            
                          DC    X'00',AL3(SEND_LENGTH)                          
                          DC    X'00',AL3(SEND_ACCESS_TOKEN)                    
                          DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(REQUEST_TO_SEND_VALUE)                
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBGTA2_STRUCTURE         DS    A                                               
                          DC    CL16'GET_ATTRIBUTES'                            
                          DC    X'C0'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(CONVERSATION_CORRELATOR)              
                          DC    X'00',AL3(LUW_ID)                               
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(CONVERSATION_STATE)                   
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBEES3_STRUCTURE         DS    A                                               
                          DC    CL16'ERROR_EXTRACT'                             
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(SERVICE_NAME)                         
                          DC    X'00',AL3(SERVICE_REASON_CODE)                  
                          DC    X'00',AL3(MESSAGE_TEXT_LENGTH)                  
                          DC    X'00',AL3(MESSAGE_TEXT)                         
                          DC X'00',AL3(ERROR_LOG_PRODUCT_SET_ID_LENGTH)         
                          DC    X'00',AL3(ERROR_LOG_PRODUCT_SET_ID)             
                          DC    X'00',AL3(ERROR_LOG_INFORMATION_LENGTH)         
                          DC    X'00',AL3(ERROR_LOG_INFORMATION)                
                          DC    X'00',AL3(REASON_CODE_ERROR_EXTRACT)            
                          DC    X'80',AL3(RETURN_CODE_ERROR_EXTRACT)            
         EJECT                                                                  
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBALC2  DC    CL8'ATBALC2'                                                     
         DC    XL52'00'                                                         
ATBCFM   DC    CL8'ATBCFM'                                                      
         DC    XL52'00'                                                         
ATBDEAL  DC    CL8'ATBDEAL'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045YYUNWPQ   05/01/02'                                      
         END                                                                    
