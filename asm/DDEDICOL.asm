*          DATA SET DDEDICOL   AT LEVEL 022 AS OF 11/06/08                      
*PHASE EDICOLA                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDICOL -- TRANSMIT PQ REPORTS TO COLUMBINE           *         
*                           VIA COLXFER PROGRAM (USING APPC/MVS)      *         
*                                                                     *         
*  COMMENTS:     CALLED BY EDICT                                      *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- PARAMETERS FROM EDICT                          *         
*                R6 -- APPC/MVS CONTROL BLOCKS                        *         
*                R7 -- WORK                                           *         
*                R8 -- S P A R E                                      *         
*                R9 -- SECOND BASE                                    *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDICOL -- PERFORM COLUMBINE TRANSMISSIONS'                    
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
EDICOL   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDICOL*,=A(R13CHAIN),R9                                       
*                                                                               
         LR    R5,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         USING COMMWORK,RC                                                      
*                                                                               
         SH    R5,=H'4'                                                         
         L     R5,0(R5)                                                         
         L     R5,0(R5)            A(R1 PARAMETERS FROM ATTACH)                 
         USING SUBPARMD,R5                                                      
*                                                                               
         USING APPCD,R6                                                         
*                                                                               
         MVC   TRACEFLG,STRACEON   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'COLTRACE'  DDNAME=COLTRACE              
*                                                                               
         MVC   TITLE(26),=C'EDICT: SENDER TO COLUMBINE'                         
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         BRAS  RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         BRAS  RE,INITIAL          INITIALIZE                                   
         EJECT                                                                  
WAIT     PRNT  ABOUTTOWAIT,PRINT=ALWAYS                                         
*                                                                               
         L     R3,AECBLIST                                                      
         WAIT  1,ECBLIST=(3)       WAIT FOR SOMETHING TO HAPPEN                 
*                                                                               
         L     RF,4(R3)            A(ECB) SAYING STOP                           
         TM    0(RF),X'40'         STOP?                                        
         BZ    DONTSTOP            NO                                           
*                                                                               
         XC    0(4,RF),0(RF)       CLEAR ECB                                    
         PRNT  STOPREQUESTED,PRINT=ALWAYS                                       
         MVI   OPERSTOP,C'Y'       OPERATOR WANTS TO STOP                       
*                                                                               
WINDDOWN L     R6,AAPPCTAB         APPC/MVS CONTROL TABLE                       
ALLFREE  OC    LUTABLE_ENTRY_ADDRESS,LUTABLE_ENTRY_ADDRESS                      
         BNZ   WAIT                CONVERSATION NOT YET COMPLETED               
         AH    R6,=Y(APPCDLEN)     BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R6)       END OF TABLE?                                
         BNE   ALLFREE                                                          
*                                                                               
         PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DONTSTOP CLI   OPERSTOP,C'Y'       OPERATOR WANTS TO STOP?                      
         BE    CHKAPPC             YES - DON'T BOTHER LOOKING FOR MORE          
*                                                                               
         L     RF,0(R3)            A(ECB) SAYING LOOK                           
         TM    0(RF),X'40'         MAIN TASK POSTED READY?                      
         BO    *+12                YES                                          
         LA    R3,8(R3)            POINT TO FIRST ECB ADDRESS                   
         B     CHKAPPC                                                          
*                                                                               
         XC    0(4,RF),0(RF)       CLEAR ECB                                    
*                                                                               
         BRAS  RE,FINDREP          LOOK FOR A COLUMBINE REPORT                  
         B     WAIT                                                             
*                                                                               
CHKAPPC  L     RF,0(R3)            RF NOW CONTAINS ECB                          
         TM    0(RF),X'40'         WAS THIS CONVERSATION POSTED?                
         BO    DOAPPC              YES                                          
         TM    0(R3),X'80'         NO - END OF ECBLIST?                         
         BO    WAIT                YES                                          
         LA    R3,4(R3)            BUMP TO NEXT ECB ADDRESS                     
         B     CHKAPPC                                                          
*                                                                               
DOAPPC   LR    R6,RF               APPC CONTROL TABLE ENTRY ADDRESS             
         MVC   RETURN_CODE,APPC_ECB  RETURN CODE IS IN ECB                      
         MVI   RETURN_CODE,0       CLEAR HOB                                    
         XC    APPC_ECB,APPC_ECB   CLEAR ECB                                    
         L     RF,APPC_COMPLETION_ENTRYPT                                       
         BR    RF                  PERFORM NEXT APPC/MVS FUNCTION               
         EJECT                                                                  
         LTORG                                                                  
INITIAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF APPC/MVS ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
         LOAD  DE=ATBALC2                                                       
         ST    R0,AATBALC2                                                      
         LOAD  DE=ATBCFM                                                        
         ST    R0,AATBCFM                                                       
         LOAD  DE=ATBDEAL                                                       
         ST    R0,AATBDEAL                                                      
         LOAD  DE=ATBEES3                                                       
         ST    R0,AATBEES3                                                      
         LOAD  DE=ATBGTA2                                                       
         ST    R0,AATBGTA2                                                      
         LOAD  DE=ATBRCVW                                                       
         ST    R0,AATBRCVW                                                      
         LOAD  DE=ATBSEND                                                       
         ST    R0,AATBSEND                                                      
*                                                                               
         L     R2,MAXCONV          MAXIMUM NUMBER OF CONVERSATIONS              
         MH    R2,=Y(APPCDLEN)     R2 = AMOUNT OF SPACE NEEDED IN TABLE         
         LA    R2,12(R2)           ADD ROOM FOR EYE-CATCHER, EOT MARKER         
         STORAGE OBTAIN,LENGTH=(2),BNDRY=PAGE                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*APPCTAB'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,AAPPCTAB         A(START OF APPC CONTROL TABLE)               
*                                                                               
         L     R2,MAXCONV          MAXIMUM NUMBER OF CONVERSATIONS              
         SLL   R2,2                R2 = SPACE NEEDED FOR ECBS                   
         LA    R2,16(R2)           EYE-CATCHER, PLUS TWO MORE ECBS              
         STORAGE OBTAIN,LENGTH=(2)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*ECBLIST'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,AECBLIST         A(START OF ECBLIST)                          
*                                                                               
         L     R3,AECBLIST         BUILD ECBLIST                                
         MVI   0(R3),0                                                          
         L     R1,SLOOKECB                                                      
         STCM  R1,7,1(R3)          1ST ECB: EDICT SAYS LOOK                     
         LA    R3,4(R3)                                                         
         MVI   0(R3),0                                                          
         L     R1,SSTOPECB                                                      
         STCM  R1,7,1(R3)          2ND ECB: EDICT SAYS STOP                     
*                                                                               
         L     R6,AAPPCTAB                                                      
         L     R0,MAXCONV                                                       
*                                                                               
INIT10   LA    R3,4(R3)            BUMP TO NEXT POSITION IN ECBLIST             
         ST    R6,0(R3)            FIRST FIELD IN TABLE IS ECB                  
         MVC   TRANSMIT_TO_STATION_FLAG,XMIT2STA                                
         MVC   MAXIMUM_TRANSMIT_TIME,MAXTIME                                    
         XC    EDICT_TABLE_ENTRY,EDICT_TABLE_ENTRY                              
         XC    LUTABLE_ENTRY_ADDRESS,LUTABLE_ENTRY_ADDRESS                      
         XC    APPC_ECB,APPC_ECB                                                
         LA    R1,APPC_ECB                                                      
         ST    R1,NOTIFY_TYPE+4                                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         MVC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         MVC   SYM_DEST_NAME,=CL8' '                                            
         MVC   PARTNER_LU_NAME,PARTLUID                                         
         MVC   LOCAL_LU_NAME,=CL8' '                                            
         MVC   MODE_NAME,=CL8'DDSLU62'                                          
         MVC   TP_NAME_LENGTH,TPNAMELN                                          
         MVC   TP_NAME,TPNAME                                                   
         MVC   RETURN_CONTROL,ATB_WHEN_SESSION_ALLOCATED                        
         MVC   SYNC_LEVEL,ATB_CONFIRM                                           
         MVC   SECURITY_TYPE,ATB_SECURITY_PROGRAM                               
         MVC   USER_ID,=CL10'APPC'                                              
         MVC   PASSWORD,=CL10'APPC'                                             
         MVC   PROFILE,=CL10' '                                                 
         XC    USER_TOKEN,USER_TOKEN                                            
         XC    TPID,TPID                                                        
         XC    RECEIVE_ACCESS_TOKEN,RECEIVE_ACCESS_TOKEN                        
         XC    SEND_ACCESS_TOKEN,SEND_ACCESS_TOKEN                              
*                                                                               
         MVC   ATBALC2_STRUCTURE,AATBALC2                                       
         MVC   ATBALC2_STRUCTURE+4(16),=CL16'ALLOCATE'                          
         MVI   ATBALC2_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_TYPE                                             
         ST    R1,ATBALC2_STRUCTURE+24                                          
         LA    R1,SYM_DEST_NAME                                                 
         ST    R1,ATBALC2_STRUCTURE+28                                          
         LA    R1,PARTNER_LU_NAME                                               
         ST    R1,ATBALC2_STRUCTURE+32                                          
         LA    R1,MODE_NAME                                                     
         ST    R1,ATBALC2_STRUCTURE+36                                          
         LA    R1,TP_NAME_LENGTH                                                
         ST    R1,ATBALC2_STRUCTURE+40                                          
         LA    R1,TP_NAME                                                       
         ST    R1,ATBALC2_STRUCTURE+44                                          
         LA    R1,RETURN_CONTROL                                                
         ST    R1,ATBALC2_STRUCTURE+48                                          
         LA    R1,SYNC_LEVEL                                                    
         ST    R1,ATBALC2_STRUCTURE+52                                          
         LA    R1,SECURITY_TYPE                                                 
         ST    R1,ATBALC2_STRUCTURE+56                                          
         LA    R1,USER_ID                                                       
         ST    R1,ATBALC2_STRUCTURE+60                                          
         LA    R1,PASSWORD                                                      
         ST    R1,ATBALC2_STRUCTURE+64                                          
         LA    R1,PROFILE                                                       
         ST    R1,ATBALC2_STRUCTURE+68                                          
         LA    R1,USER_TOKEN                                                    
         ST    R1,ATBALC2_STRUCTURE+72                                          
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBALC2_STRUCTURE+76                                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBALC2_STRUCTURE+80                                          
         LA    R1,TPID                                                          
         ST    R1,ATBALC2_STRUCTURE+84                                          
         LA    R1,LOCAL_LU_NAME                                                 
         ST    R1,ATBALC2_STRUCTURE+88                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBALC2_STRUCTURE+92                                          
*                                                                               
         MVC   ATBRCVW_STRUCTURE,AATBRCVW                                       
         MVC   ATBRCVW_STRUCTURE+4(16),=CL16'RECEIVE_AND_WAIT'                  
         MVI   ATBRCVW_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBRCVW_STRUCTURE+24                                          
         LA    R1,FILL                                                          
         ST    R1,ATBRCVW_STRUCTURE+28                                          
         LA    R1,RECEIVE_LENGTH                                                
         ST    R1,ATBRCVW_STRUCTURE+32                                          
         LA    R1,RECEIVE_ACCESS_TOKEN                                          
         ST    R1,ATBRCVW_STRUCTURE+36                                          
         LA    R1,BUFFER                                                        
         ST    R1,ATBRCVW_STRUCTURE+40                                          
         LA    R1,STATUS_RECEIVED                                               
         ST    R1,ATBRCVW_STRUCTURE+44                                          
         LA    R1,DATA_RECEIVED                                                 
         ST    R1,ATBRCVW_STRUCTURE+48                                          
         LA    R1,REQUEST_TO_SEND_RECEIVED                                      
         ST    R1,ATBRCVW_STRUCTURE+52                                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBRCVW_STRUCTURE+56                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBRCVW_STRUCTURE+60                                          
*                                                                               
         MVC   ATBDEAL_STRUCTURE,AATBDEAL                                       
         MVC   ATBDEAL_STRUCTURE+4(16),=CL16'DEALLOCATE'                        
         MVI   ATBDEAL_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBDEAL_STRUCTURE+24                                          
         LA    R1,DEALLOCATE_TYPE                                               
         ST    R1,ATBDEAL_STRUCTURE+28                                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBDEAL_STRUCTURE+32                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBDEAL_STRUCTURE+36                                          
*                                                                               
         MVC   ATBCFM_STRUCTURE,AATBCFM                                         
         MVC   ATBCFM_STRUCTURE+4(16),=CL16'CONFIRM'                            
         MVI   ATBCFM_STRUCTURE+20,X'40'                                        
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBCFM_STRUCTURE+24                                           
         LA    R1,REQUEST_TO_SEND_RECEIVED                                      
         ST    R1,ATBCFM_STRUCTURE+28                                           
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBCFM_STRUCTURE+32                                           
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBCFM_STRUCTURE+36                                           
*                                                                               
         MVC   ATBSEND_STRUCTURE,AATBSEND                                       
         MVC   ATBSEND_STRUCTURE+4(16),=CL16'SEND_DATA'                         
         MVI   ATBSEND_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBSEND_STRUCTURE+24                                          
         LA    R1,SEND_TYPE                                                     
         ST    R1,ATBSEND_STRUCTURE+28                                          
         LA    R1,SEND_LENGTH                                                   
         ST    R1,ATBSEND_STRUCTURE+32                                          
         LA    R1,SEND_ACCESS_TOKEN                                             
         ST    R1,ATBSEND_STRUCTURE+36                                          
         LA    R1,BUFFER                                                        
         ST    R1,ATBSEND_STRUCTURE+40                                          
         LA    R1,REQUEST_TO_SEND_VALUE                                         
         ST    R1,ATBSEND_STRUCTURE+44                                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBSEND_STRUCTURE+48                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBSEND_STRUCTURE+52                                          
*                                                                               
         MVC   ATBGTA2_STRUCTURE,AATBGTA2                                       
         MVC   ATBGTA2_STRUCTURE+4(16),=CL16'GET_ATTRIBUTES'                    
         MVI   ATBGTA2_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBGTA2_STRUCTURE+24                                          
         LA    R1,PARTNER_LU_NAME                                               
         ST    R1,ATBGTA2_STRUCTURE+28                                          
         LA    R1,MODE_NAME                                                     
         ST    R1,ATBGTA2_STRUCTURE+32                                          
         LA    R1,SYNC_LEVEL                                                    
         ST    R1,ATBGTA2_STRUCTURE+36                                          
         LA    R1,CONVERSATION_CORRELATOR                                       
         ST    R1,ATBGTA2_STRUCTURE+40                                          
         LA    R1,LUW_ID                                                        
         ST    R1,ATBGTA2_STRUCTURE+44                                          
         LA    R1,WORK                                                          
         ST    R1,ATBGTA2_STRUCTURE+48                                          
         LA    R1,TP_NAME                                                       
         ST    R1,ATBGTA2_STRUCTURE+52                                          
         LA    R1,LOCAL_LU_NAME                                                 
         ST    R1,ATBGTA2_STRUCTURE+56                                          
         LA    R1,CONVERSATION_TYPE                                             
         ST    R1,ATBGTA2_STRUCTURE+60                                          
         LA    R1,WORK                                                          
         ST    R1,ATBGTA2_STRUCTURE+64                                          
         LA    R1,WORK                                                          
         ST    R1,ATBGTA2_STRUCTURE+68                                          
         LA    R1,WORK                                                          
         ST    R1,ATBGTA2_STRUCTURE+72                                          
         LA    R1,CONVERSATION_STATE                                            
         ST    R1,ATBGTA2_STRUCTURE+76                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBGTA2_STRUCTURE+80                                          
*                                                                               
         MVC   ATBEES3_STRUCTURE,AATBEES3                                       
         MVC   ATBEES3_STRUCTURE+4(16),=CL16'ERROR_EXTRACT'                     
         MVI   ATBEES3_STRUCTURE+20,0                                           
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBEES3_STRUCTURE+24                                          
         LA    R1,SERVICE_NAME                                                  
         ST    R1,ATBEES3_STRUCTURE+28                                          
         LA    R1,SERVICE_REASON_CODE                                           
         ST    R1,ATBEES3_STRUCTURE+32                                          
         LA    R1,MESSAGE_TEXT_LENGTH                                           
         ST    R1,ATBEES3_STRUCTURE+36                                          
         LA    R1,MESSAGE_TEXT                                                  
         ST    R1,ATBEES3_STRUCTURE+40                                          
         LA    R1,ERROR_LOG_PRODUCT_SET_ID_LENGTH                               
         ST    R1,ATBEES3_STRUCTURE+44                                          
         LA    R1,ERROR_LOG_PRODUCT_SET_ID                                      
         ST    R1,ATBEES3_STRUCTURE+48                                          
         LA    R1,ERROR_LOG_INFORMATION_LENGTH                                  
         ST    R1,ATBEES3_STRUCTURE+52                                          
         LA    R1,ERROR_LOG_INFORMATION                                         
         ST    R1,ATBEES3_STRUCTURE+56                                          
         LA    R1,REASON_CODE_ERROR_EXTRACT                                     
         ST    R1,ATBEES3_STRUCTURE+60                                          
         LA    R1,RETURN_CODE_ERROR_EXTRACT                                     
         O     R1,=X'80000000'                                                  
         ST    R1,ATBEES3_STRUCTURE+64                                          
*                                                                               
         AH    R6,=Y(APPCDLEN)     BUMP TO NEXT CONVERSATION                    
*                                                                               
         BCT   R0,INIT10                                                        
*                                                                               
         MVC   0(4,R6),EOTMARK     MARK END OF APPC CONTROL TABLE               
         MVI   0(R3),X'80'         MARK END OF ECBLIST                          
*                                                                               
XIT      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
READCRDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ PARAMETER CARDS BETWEEN "++COLUMBINE" AND "++COLUMBINEEND" CARDS         
*                                                                               
*   *......           "*" IN COLUMN ONE IS A COMMENT CARD                       
*   TRACE=YES         OVERRIDE TRACE FLAG                                       
*   PARTNERLUID=      LUID OF COLXFER PROGRAM                                   
*   TPNAME=           TPNAME OF OUR ASCH TRANSFER SCHEDULER PROGRAM             
*   COLTPNAME=        TPNAME OF COLUMBINE'S RECEIVING PROGRAM                   
*   MAXCONVERSATIONS= MAXIMUM NUMBER OF SIMULTANEOUS CONVERSATIONS              
*   XMITTOSTATIONS=   Y/N: N= DON'T SEND MESSAGES TO COLUMBINE STATIONS         
*   MAXTRANSMITTIME=  NN: MAXIMUM NO. OF MINUTES FOR MESSAGE TRANSMIT           
*   USERID=UUUUUUUU  TRANSFER REPORTS FROM THIS USERID ONLY                     
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++COLUMBINE',CARD LOOK FOR START OF PARAMETERS                
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++COLUMBINEEND',CARD LOOK FOR END OF PARAMETERS               
         BE    RCX                                                              
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC20                YES                                          
*                                                                               
         LA    RE,CARD             BLANK DELIMITS A COMMENT                     
         LA    R0,C' '                                                          
         SRST  R0,RE               R0 = A(FIRST BLANK)                          
         LR    R1,R0                                                            
         LA    R1,1(R1)            BUMP TO NEXT CHARACTER IN CARD               
         C     R1,=A(CARD+79)      STOP AT END OF CARD                          
         BH    *+12                                                             
         MVI   0(R1),C' '          REPLACE COMMENT WITH BLANKS                  
         B     *-16                                                             
*                                                                               
         CLC   =C'TRACE=',CARD     TRACE=                                       
         BNE   RC30                                                             
         CLC   =C'NO',CARD+6                                                    
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'YES',CARD+6                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'PARTNERLUID=',CARD                                            
         BNE   *+14                                                             
         MVC   PARTLUID,CARD+12                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MAXCONVERSATIONS=',CARD  MAXCONVERSATIONS=N                   
         BNE   RC40                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+17,(2,0)                                    
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         MVC   MAXCONV,DMCB+4                                                   
         B     RC20                                                             
*                                                                               
RC40     CLC   =C'XMITTOSTATIONS=',CARD  XMITTOSTATIONS=N                       
         BNE   RC50                                                             
         CLC   =C'YES',CARD+15                                                  
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'NO',CARD+15                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMIT2STA,C'N'       XMITTOSTATIONS=                              
         B     RC20                                                             
*                                                                               
RC50     CLC   =C'MAXTRANSMITTIME=',CARD  MAXTRANSMITTIME=N                     
         BNE   RC60                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+16,(2,0)                                    
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         MVC   MAXTIME,DMCB+4                                                   
         B     RC20                                                             
*                                                                               
RC60     CLC   =C'TPNAME=',CARD                                                 
         BNE   RC70                                                             
         MVC   TPNAME,CARD+7       TRANSACTION PROGRAM NAME                     
         SR    RE,RE               COUNT CHARACTERS                             
         LA    RF,CARD+7                                                        
         CLI   0(RF),C' '          SCAN FORWARD TO FIRST BLANK                  
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     *-16                                                             
         ST    RE,TPNAMELN         TRANSACTION PROGRAM NAME LENGTH              
         B     RC20                                                             
*                                                                               
RC70     CLC   =C'COLTPNAME=',CARD                                              
         BNE   RC80                                                             
         MVC   COLTPNAM,CARD+10    COLUMBINE'S TP NAME                          
         B     RC20                                                             
*                                                                               
RC80     CLC   =C'USERID=',CARD    USERID=XXX                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         LA    RF,KEY                                                           
         USING CTIREC,RF                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,CARD+7                                                    
         DROP  RF                                                               
*                                                                               
         L     R7,SMAJORNM                                                      
         LA    R3,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R7)                                                    
         MVC   P+40(6),0(R3)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   ((7),(3),E,6)       ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 SADTAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ID RECORD                                 
*                                                                               
         L     R7,SMAJORNM                                                      
         LA    R3,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R7)                                                    
         MVC   P+40(6),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((7),(3),6)         DEQUEUE THE CONTROL FILE                     
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PICK UP HEX USERID                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   USERFILT,CTDSC-CTDSCD(R4)                                        
         B     RC20                                                             
*                                                                               
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
FINDREP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R2,=C'LUTABLE '     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R3)                                                    
         MVC   P+40(8),0(R2)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   ((3),(2),E,8)       ENQUEUE THE LUTABLE                          
*                                                                               
FIND10   L     R3,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R2,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R3)                                                    
         MVC   P+40(8),0(R2)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   ((3),(2),E,8)       ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R2,SXMTTBLE         R2 = A(REPORT TABLE)                         
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
         XC    WORK,WORK           WORK = EARLIEST ENTRY --> FIFO               
*                                                                               
FIND20   MVC31 XMTENTRY,0(R2)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BE    FIND40              YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   FIND30                                                           
         CLI   XMTMETH,C'C'        YES -- COLUMBINE METHOD?                     
         BNE   FIND30                                                           
         TM    XMTSTAT,EDFSTWTG    YES -- WAITING TO BE SENT?                   
         BZ    FIND30                                                           
         OC    USERFILT,USERFILT   YES -- IS THERE A USERID FILTER?             
         BZ    *+14                                                             
         CLC   XMTUSRID,USERFILT   YES - DOES THIS REPORT MATCH FILTER?         
         BNE   FIND30                                                           
         TM    XMTFLAGS,XMTTEMPQ   YES -- ALREADY LOOKED AT THIS ONE?           
         BO    FIND30                                                           
*                                                                               
*                                                                               
         L     R6,AAPPCTAB         YES -- EXAMINE APPC CONTROL TABLE            
FIND25   LA    RF,EDICT_TABLE_ENTRY                                             
         CLC   XMTDSKAD-XMTTABLD(,RF),XMTDSKAD   IS REPORT IN PROCESS?          
         BE    FIND30              YES -- SKIP IT                               
         AH    R6,=Y(APPCDLEN)     BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R6)       END OF TABLE?                                
         BNE   FIND25              NO                                           
*                                                                               
         OC    WORK,WORK           IS THIS THE FIRST ONE WE'VE CHECKED?         
         BZ    *+14                YES, SO SAVE THE DAY/TIME                    
         CLC   XMTCRDTM-XMTTABLD+WORK,XMTCRDTM                                  
         BNH   FIND30                                                           
         MVC   WORK(XMTTBLQ),XMTENTRY   EARLIEST ENTRY FOUND SO FAR             
         LR    R4,R2               SAVE A(EARLIEST ENTRY) SO FAR                
*                                                                               
FIND30   AHI   R2,XMTTBLQ          BUMP TO NEXT REPORT                          
         B     FIND20                                                           
*                                                                               
FIND40   OC    WORK,WORK           ANYTHING FOUND TO SEND?                      
         BZ    FIND42              NO                                           
         MVC   TABLNTRY,WORK       SAVE XMIT TABLE KEY                          
*                                                                               
         MVC   XMTENTRY,WORK                                                    
         OI    XMTFLAGS,XMTTEMPQ   WE'RE CONSIDERING IT                         
         MVC31 0(L'XMTENTRY,R4),XMTENTRY   CHANGE FLAG ON THE XMTTABLE          
         DROP  R3                                                               
*                                                                               
FIND42   L     R3,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R2,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R3)                                                    
         MVC   P+40(8),0(R2)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((3),(2),8)         DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         OC    WORK,WORK           ANYTHING FOUND TO SEND?                      
         BZ    FIND90              NO - EXIT                                    
*                                                                               
         LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,SADTAMGR                                                 
         GOTO1 =V(INITPQRD)                                                     
         BNE   FIND50                                                           
         DROP  R1                                                               
*                                                                               
         L     R4,SLUTABLE         LUNAME TABLE                                 
         USING LUTABLED,R4                                                      
FIND45   CLC   LUEDCKEY,EDICTHDR+16 FIND MATCH ON EDICT= KEY                    
         BE    FIND60                                                           
         LA    R4,LUTABLEQ(,R4)    BUMP TO NEXT ENTRY                           
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   FIND45              NO                                           
*                                                                               
FIND50   LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK UNSENDABLE                              
         MVI   EERRORCD,EDFERNPQ   REPORT NOT FOUND ON PRINT QUEUE              
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   FIND10              COULD NOT UPDATE EDICT FILE                  
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     FIND10              LOOK FOR ANOTHER ONE                         
         DROP  R1                                                               
*                                                                               
FIND60   TM    LUSTATUS,LUNOGOOD                                                
         BO    FIND10              COMMS PROBLEM WITH THIS LU                   
         TM    LUSTATUS,LUINUSE                                                 
         BO    FIND10              PARTNER IS BUSY -- GET NEXT REPORT           
*                                                                               
         L     R6,AAPPCTAB         APPC/MVS CONTROL TABLE                       
FIND70   OC    LUTABLE_ENTRY_ADDRESS,LUTABLE_ENTRY_ADDRESS                      
         BZ    FIND80              HERE'S A FREE TABLE ENTRY                    
         AH    R6,=Y(APPCDLEN)     BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R6)       END OF TABLE?                                
         BNE   FIND70                                                           
         B     FIND90              YES -- WE'LL HAVE TO WAIT                    
*                                                                               
FIND80   MVC   EDICT_TABLE_ENTRY,TABLNTRY                                       
         ST    R4,LUTABLE_ENTRY_ADDRESS                                         
         OI    LUSTATUS,LUINUSE    FLAG LUNAME AS IN PROCESS                    
         MVC   DSTNAME,LUEDCKEY    SAVE DESTINATION                             
         DROP  R4                                                               
*                                                                               
         LA    R3,EDICT_TABLE_ENTRY                                             
         USING XMTTABLD,R3                                                      
         MVC   P+30(8),DSTNAME     PRINT DESTINATION                            
         MVC   P+40(3),XMTSUBID    PRINT PQ SUBID                               
         EDIT  XMTREFNO,(5,P+45),ALIGN=LEFT                                     
         GOTO1 =V(HEXOUT),DMCB,XMTDSKAD,P+53,4,=C'TOG'                          
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
         PRNT  XFERONEREPORT,PRINT=ALWAYS                                       
*                                                                               
         LA    R2,ATBALC2_STRUCTURE                                             
         BRAS  RE,CALLAPPC         ALLOCATE A CONVERSATION                      
         MVC   APPC_COMPLETION_ENTRYPT,=A(CONFALLC)                             
         B     FIND10                                                           
*                                                                               
FIND90   L     R3,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R2,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R3)                                                    
         MVC   P+40(8),0(R2)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   ((3),(2),E,8)       ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R2,SXMTTBLE         R2 = A(REPORT TABLE)                         
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
*                                                                               
FIND100  MVC31 XMTENTRY,0(R2)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BE    FIND110             YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   *+16                                                             
         CLI   XMTMETH,C'C'        YES -- COLUMBINE METHOD?                     
         BNE   FIND105                                                          
         NI    XMTFLAGS,X'FF'-XMTTEMPQ  WE CAN LOOK AT THIS ONE LATER           
         MVC31 0(L'XMTENTRY,R2),XMTENTRY                                        
FIND105  AHI   R2,XMTTBLQ          BUMP TO NEXT REPORT                          
         B     FIND100                                                          
         DROP  R3                                                               
*                                                                               
FIND110  L     R3,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R2,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R3)                                                    
         MVC   P+40(8),0(R2)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((3),(2),8)         DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R3,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R2,=C'LUTABLE '     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R3)                                                    
         MVC   P+40(8),0(R2)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((3),(2),8)         DEQUEUE THE LUTABLE                          
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
CONFALLC MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR          CALL ERROR_EXTRACT ROUTINE                   
         DC    H'0'                COULDN'T ALLOCATE COLXFER SCHEDULER          
*                                                                               
         LA    R2,ATBCFM_STRUCTURE                                              
         BRAS  RE,CALLAPPC         ASK FOR CONFIRMATION                         
         MVC   APPC_COMPLETION_ENTRYPT,=A(SENDPARM)                             
*                                                                               
         J     WAIT                                                             
         EJECT                                                                  
SENDPARM MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR          CALL ERROR_EXTRACT ROUTINE                   
         DC    H'0'                NO ALLOCATE/CONFIRM FROM COLXFER             
*                                                                               
         BRAS  RE,PRNTATTB         PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         PRNT  SENDINGCTLDATA                                                   
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(CTLMSG1L),CTLMSG1   CONTROL MESSAGE                       
         LA    R4,CTLMSG1L         R4 = L'DATA                                  
         ST    R4,SEND_LENGTH                                                   
         L     RF,LUTABLE_ENTRY_ADDRESS                                         
         USING LUTABLED,RF                                                      
         MVC   BUFFER+30(8),LUNAME                                              
         MVC   BUFFER+83(10),LUUSERID                                           
         MVC   BUFFER+103(10),LUPASSWD                                          
         DROP  RF                                                               
         LA    R3,EDICT_TABLE_ENTRY                                             
         USING XMTTABLD,R3                                                      
         SR    R0,R0                                                            
         ICM   R0,3,XMTUSRID                                                    
         CVD   R0,DUB                                                           
         UNPK  BUFFER+45(5),DUB                                                 
         OI    BUFFER+49,X'F0'                                                  
         MVC   BUFFER+51(3),XMTSUBID                                            
         SR    R0,R0                                                            
         ICM   R0,3,XMTREFNO                                                    
         CVD   R0,DUB                                                           
         UNPK  BUFFER+55(5),DUB                                                 
         OI    BUFFER+59,X'F0'                                                  
         MVC   BUFFER+66(1),TRANSMIT_TO_STATION_FLAG                            
         ICM   R0,15,MAXIMUM_TRANSMIT_TIME                                      
         CVD   R0,DUB                                                           
         UNPK  BUFFER+73(2),DUB                                                 
         OI    BUFFER+74,X'F0'                                                  
         MVC   BUFFER+121(64),COLTPNAM                                          
         DROP  R3                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'SEND AND PREPARE TO RECEIVE',BUFFER, +        
               C'DUMP',(R4),=C'1D'                                              
*                                                                               
         MVC   SEND_TYPE,ATB_SEND_AND_PREP_TO_RECEIVE                           
*                                                                               
         LA    R2,ATBSEND_STRUCTURE                                             
         BRAS  RE,CALLAPPC         SEND DATA                                    
         MVC   APPC_COMPLETION_ENTRYPT,=A(RPTSNT1)                              
*                                                                               
         J     WAIT                                                             
         EJECT                                                                  
RPTSNT1  MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR          CALL ERROR_EXTRACT ROUTINE                   
         DC    H'0'                COULDN'T SEND PRNTQ KEY TO COLXFER           
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         BRAS  RE,CALLAPPC         RECEIVE AND WAIT                             
         MVC   APPC_COMPLETION_ENTRYPT,=A(RPTSNT2)                              
*                                                                               
         J     WAIT                                                             
         EJECT                                                                  
RPTSNT2  MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR          CALL ERROR_EXTRACT ROUTINE                   
         DC    H'0'                BAD RECEIVE FROM COLXFER                     
*                                                                               
         CLC   DATA_RECEIVED,ATB_NO_DATA_RECEIVED                               
         BNE   CHKANSR5                                                         
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         BRAS  RE,CALLAPPC         RECEIVE AND WAIT                             
         MVC   APPC_COMPLETION_ENTRYPT,=A(CHKANSR)                              
*                                                                               
         J     WAIT                                                             
         EJECT                                                                  
CHKANSR  MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR          CALL ERROR_EXTRACT ROUTINE                   
         DC    H'0'                BAD RECEIVE FROM COLXFER                     
*                                                                               
         CLC   DATA_RECEIVED,ATB_NO_DATA_RECEIVED                               
         BNE   *+6                                                              
         DC    H'0'                WHERE IS RETURN DATA FROM COLXFER?           
*                                                                               
CHKANSR5 L     R2,RECEIVE_LENGTH                                                
         GOTO1 =V(PRNTBL),DMCB,=C'CONTROL RECORD RECEIVED',            +        
               BUFFER,C'DUMP',(R2),=C'1D'                                       
*                                                                               
         CLC   BUFFER(CTLMSG2L),CTLMSG2                                         
         BNE   *+12                                                             
         MVI   REPORT_STATUS,REPORT_TRANSMIT_OK                                 
         B     RPTSENT                                                          
*                                                                               
         CLC   BUFFER+7(5),CTLMSG3+7  MESSAGE STARTS WITH 'ERROR'?              
         BE    *+6                                                              
         DC    H'0'                WHAT CAME BACK FROM COLXFER?                 
*                                                                               
         CLC   =C'REPORT',BUFFER   PROBLEM WITH REPORT ITSELF?                  
         BNE   *+12                                                             
         MVI   REPORT_STATUS,REPORT_CONTENTS_BAD                                
         B     RPTSENT                                                          
*                                                                               
         MVI   REPORT_STATUS,REPORT_TRANSMIT_BAD                                
         MVC   WORK(32),=C'EDICT01 *COLUMBINE COMMS ERROR: '                    
         MVC   WORK+32(45),BUFFER+14                                            
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(77,WORK)                             
         MVC   WORK(39),=C'EDICT01 *COLUMBINE COMMS ERROR: LUID = '             
         L     RF,LUTABLE_ENTRY_ADDRESS                                         
         USING LUTABLED,RF                                                      
         OI    LUSTATUS,LUNOGOOD                                                
         NI    LUSTATUS,X'FF'-LUINUSE                                           
         MVC   WORK+39(8),LUNAME                                                
         DROP  RF                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(47,WORK)                             
*                                                                               
RPTSENT  CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BE    DEALLOC5                                                         
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         BRAS  RE,CALLAPPC         RECEIVE AND WAIT                             
         MVC   APPC_COMPLETION_ENTRYPT,=A(DEALLOC)                              
*                                                                               
         J     WAIT                                                             
         EJECT                                                                  
DEALLOC  MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR          CALL ERROR_EXTRACT ROUTINE                   
         DC    H'0'                BAD RECEIVE FROM COLXFER                     
*                                                                               
         CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BE    *+6                                                              
         DC    H'0'                WHY WON'T COLXFER LET US SEND?               
*                                                                               
DEALLOC5 MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_SYNC_LEVEL                        
*                                                                               
         LA    R2,ATBDEAL_STRUCTURE                                             
         BRAS  RE,CALLAPPC         DEALLOCATE THE CONVERSATION                  
         MVC   APPC_COMPLETION_ENTRYPT,=A(MSGSENT)                              
*                                                                               
         J     WAIT                                                             
         EJECT                                                                  
MSGSENT  PRNT  SENTVIACOL,PRINT=ALWAYS                                          
*                                                                               
         CLI   REPORT_STATUS,REPORT_TRANSMIT_OK                                 
         BNE   NOGOOD                                                           
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         LA    RE,EDICT_TABLE_ENTRY                                             
         ST    RE,ETBLNTRY                                                      
         MVI   EACTION,EACTSNTQ+EACTDLVQ   MARK SENT AND DELIVERED              
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   NEXTREP             COULD NOT UPDATE EDICT FILE                  
         DROP  R1                                                               
         PRNT  RPTMARKEDSENTDLVD,PRINT=ALWAYS                                   
         B     NEXTREP                                                          
*                                                                               
NOGOOD   CLI   REPORT_STATUS,REPORT_CONTENTS_BAD                                
         BE    RPTJUNK                                                          
         CLI   REPORT_STATUS,REPORT_TRANSMIT_BAD                                
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  COULDNOTTRANSMIT,PRINT=ALWAYS                                    
         B     NEXTREP                                                          
*                                                                               
RPTJUNK  LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         LA    RE,EDICT_TABLE_ENTRY                                             
         ST    RE,ETBLNTRY                                                      
         MVI   EACTION,EACTJNKQ    MARK UNSENDABLE                              
         MVI   EERRORCD,EDFERBCR   BAD COLUMBINE REPORT CONTENTS                
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   NEXTREP             COULD NOT UPDATE EDICT FILE                  
         DROP  R1                                                               
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     NEXTREP                                                          
*                                                                               
NEXTREP  L     RF,LUTABLE_ENTRY_ADDRESS                                         
         NI    (LUSTATUS-LUTABLED)(RF),X'FF'-LUINUSE                            
         XC    LUTABLE_ENTRY_ADDRESS,LUTABLE_ENTRY_ADDRESS                      
         XC    APPC_COMPLETION_ENTRYPT,APPC_COMPLETION_ENTRYPT                  
         XC    EDICT_TABLE_ENTRY,EDICT_TABLE_ENTRY                              
*                                                                               
         CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         JE    WINDDOWN            YES - DON'T LOOK FOR MORE REPORTS            
*                                                                               
         BRAS  RE,FINDREP          LOOK FOR A COLUMBINE REPORT                  
*                                                                               
         J     WAIT                                                             
         LTORG                                                                  
         EJECT                                                                  
PRNTATTB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  GETATTRIBUTES,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
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
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR          CALL ERROR_EXTRACT ROUTINE                   
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                   
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
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
CALLAPPC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
*                                                                               
         MVC   P(16),=C'ABOUT TO PERFORM'                                       
         MVC   P+30(16),4(R2)      PRINT ROUTINE NAME                           
         PRNT                                                                   
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
         CLC   RETURN_CODE,ATB_OK                                               
         BE    CALLX               APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALLX    J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
APPCERR  NTR1  BASE=*,LABEL=*                                                   
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
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE_ERROR_EXTRACT,RETURN_CODE_ERROR_EXTRACT              
         BNZ   APPCEX              BAD RETURN CODE, SO NO ERROR INFO            
*                                                                               
         MVC   P+30(37),=C'APPC SERVICE XXXXXXXX, REASON_CODE = '               
         MVC   P+43(8),SERVICE_NAME                                             
         EDIT  SERVICE_REASON_CODE,(5,P+67),ALIGN=LEFT,ZERO=NOBLANK             
         PRNT  ERROR_EXTRACT,PRINT=ALWAYS                                       
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
APPCEX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         ATBSERV                                                                
         SPACE 3                                                                
         GETEL R4,28,ELCODE                                                     
         SPACE 3                                                                
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                                                                
EOTMARK  DC    X'FFFFFFFF'                                                      
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
ELCODE   DS    X                                                                
WORK     DS    CL256                                                            
         SPACE 2                                                                
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE ENTRY                       
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
TRACEFLG DS    C                   'Y' = PRINT DETAILED TRACE                   
XMIT2STA DC    C'Y'                'N' = DON'T SEND MSGS TO STATIONS            
OPERSTOP DC    C'N'                'Y' = OPERATOR WANTS TO STOP                 
PARTLUID DS    CL17                LUID OF COLUMBINE SENDER                     
TPNAME   DS    CL64                TPNAME OF COLUMBINE XFER SCHEDULER           
TPNAMELN DS    F                   TPNAME LENGTH                                
COLTPNAM DS    CL64                TPNAME OF COLUMBINE'S RECEIVER               
DSTNAME  DS    CL8                 DESTINATION                                  
MAXCONV  DS    F                   MAXIMUM NUMBER OF CONVERSATIONS              
MAXTIME  DS    F                   MAX MINUTES TO TRANSMIT MESSAGE              
AAPPCTAB DS    A                   A(APPC CONTROL TABLE)                        
AECBLIST DS    A                   A(ECBLIST)                                   
USERFILT DC    H'0'                ONLY XMIT REPORTS FOR THIS USERID            
CARD     DS    CL80                FOR CONTROL CARDS                            
KEY      DS    XL25                CTFILE KEY                                   
CTLMSG1  DC    C'EDICT CONTROL MESSAGE 1: '                                     
CTLMSG1A DC    C'LUID=XXXXXXXX,PQKEY=NNNNN,CCC,NNNNN,XMIT=Y,TIME=NN,USE+        
               RID=XXXXXXXXXX,PASSWORD=XXXXXXXXXX,TPNAME=XXXXXXXXXXXXXX+        
               XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'              
CTLMSG1L EQU   *-CTLMSG1                                                        
CTLMSG2  DC    C'REPORT SENT OK'                                                
CTLMSG2L EQU   *-CTLMSG2                                                        
CTLMSG3  DC    C'XXXXXX ERROR: '   'COMMS  ERROR' OR 'REPORT ERROR'             
CTLMSG3A DS    CL45                ERROR MESSAGE IS HERE                        
CTLMSG3L EQU   *-CTLMSG3                                                        
AATBALC2 DS    A                   A(APPC/MVS ALLOCATE ROUTINE)                 
AATBCFM  DS    A                   A(APPC/MVS CONFIRM ROUTINE)                  
AATBDEAL DS    A                   A(APPC/MVS DEALLOCATE ROUTINE)               
AATBGTA2 DS    A                   A(APPC/MVS GET_ATTRIBUTES ROUTINE)           
AATBRCVW DS    A                   A(APPC/MVS RECEIVE_AND_WAIT ROUTINE)         
AATBSEND DS    A                   A(APPC/MVS SEND_DATA ROUTINE)                
AATBEES3 DS    A                   A(APPC/MVS ERROR_EXTRACT ROUTINE)            
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         DS    XL4                                                              
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
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
ATBRCVW  DC    CL8'ATBRCVW'                                                     
         DC    XL52'00'                                                         
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'**I/O***'                                                      
IO       DC    2000X'00'           CTFILE/GENFILE I/O BUFFER                    
*                                                                               
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          PRINT QUEUE INDEX BUFFER                     
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
APPCD    DSECT                                                                  
*                                                                               
APPC_ECB                       DS    F     MUST BE FIRST FIELD                  
APPC_COMPLETION_ENTRYPT        DS    A     A(START OF NEXT APPC/MVS)            
LUTABLE_ENTRY_ADDRESS          DS    A     A(LUNAME TABLE ENTRY)                
EDICT_TABLE_ENTRY              DS    XL(XMTTBLQ) SAVED XMIT TABLE ENTRY         
MAXIMUM_TRANSMIT_TIME          DS    F     IN MINUTES (REAL TIME)               
TRANSMIT_TO_STATION_FLAG       DS    C     N = DON'T SEND TO STATION            
REPORT_STATUS                  DS    C                                          
REPORT_TRANSMIT_OK             EQU   1                                          
REPORT_TRANSMIT_BAD            EQU   2                                          
REPORT_CONTENTS_BAD            EQU   3                                          
CONVERSATION_TYPE              DS    F                                          
SYM_DEST_NAME                  DS    CL8                                        
LOCAL_LU_NAME                  DS    CL8                                        
MODE_NAME                      DS    CL8                                        
PARTNER_LU_NAME                DS    CL17                                       
TP_NAME_LENGTH                 DS    F                                          
TP_NAME                        DS    CL64                                       
RETURN_CONTROL                 DS    F                                          
SYNC_LEVEL                     DS    F                                          
SECURITY_TYPE                  DS    F                                          
NOTIFY_TYPE                    DS    F                                          
                               DS    A     MUST FOLLOW NOTIFY_TYPE              
USER_ID                        DS    CL10                                       
PASSWORD                       DS    CL10                                       
PROFILE                        DS    CL10                                       
USER_TOKEN                     DS    XL80                                       
CONVERSATION_ID                DS    CL8                                        
CONVERSATION_CORRELATOR        DS    CL8                                        
TPID                           DS    XL8                                        
LUW_ID                         DS    XL26                                       
CONVERSATION_STATE             DS    F                                          
RETURN_CODE                    DS    F                                          
DEALLOCATE_TYPE                DS    F                                          
FILL                           DS    F                                          
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DS    F                                          
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
REQUEST_TO_SEND_VALUE          DS    F                                          
SEND_TYPE                      DS    F                                          
SEND_LENGTH                    DS    F                                          
SEND_ACCESS_TOKEN              DS    F                                          
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
*                                                                               
* EACH STRUCTURE BELOW CONSISTS OF                                              
*   A(ROUTINE)                                                                  
*   CL16'ROUTINE NAME'                                                          
*   XL1  FLAGS                                                                  
*        X'40': ROUTINE CAN BE FOLLOWED BY ERROR_EXTRACT CALL                   
*   XL3  SPARE                                                                  
*   ROUTINE PARAMETER LIST                                                      
*                                                                               
ATBALC2_STRUCTURE              DS    (6+18)F                                    
ATBRCVW_STRUCTURE              DS    (6+10)F                                    
ATBDEAL_STRUCTURE              DS    (6+4)F                                     
ATBCFM_STRUCTURE               DS    (6+4)F                                     
ATBSEND_STRUCTURE              DS    (6+8)F                                     
ATBGTA2_STRUCTURE              DS    (6+15)F                                    
ATBEES3_STRUCTURE              DS    (6+11)F                                    
*                                                                               
APPCDLEN EQU   *-APPCD                                                          
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022DDEDICOL  11/06/08'                                      
         END                                                                    
