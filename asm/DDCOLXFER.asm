*          DATA SET DDCOLXFER  AT LEVEL 022 AS OF 05/01/02                      
*PHASE COLXFERA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        DDCOLXFER -- TRANSMIT REPORT TO COLUMBINE (APPC/MVS) *         
*                                                                     *         
*  COMMENTS:     ATTACHED BY EDICTCOL (EDICT COLUMBINE SUBTASK)       *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- S P A R E                                      *         
*                R6 -- PROGRAM FIFTH BASE                             *         
*                R7 -- PROGRAM FOURTH BASE                            *         
*                R8 -- PROGRAM THIRD BASE                             *         
*                R9 -- PROGRAM SECOND BASE                            *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- S P A R E                                      *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDCOLXFER -- TRANSMIT COLUMBINE REPORTS VIA APPC/MVS'           
         MACRO                                                                  
&NAME    PRNT  &A                                                               
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
         GOTO1 =V(PRINTER)                                                      
         MEND                                                                   
         EJECT                                                                  
COLXFER  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,COLXFER,=A(R13CHAIN),R9,R8,R7,R6                               
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(18),=C'COLUMBINE TRANSFER'                                 
*                                                                               
         BAS   RE,INITIAL          INITIALIZE                                   
*                                                                               
GETTRANS LA    R2,ATBGTRN_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       GET MULTI-TRANS TRANSACTION                  
*                                                                               
         CLC   RETURN_CODE_E,=F'8' ANYTHING MORE TO DO?                         
         BE    GOODBYE                                                          
         CLC   RETURN_CODE_E,=F'28'                                             
         BE    GOODBYE             NO                                           
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,ATBGETC_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       GET A CONVERSATION WITH EDICT                
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,PRNTATTB_E       PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         MVC   RECEIVE_LENGTH_E,=A(L'BUFFER_E)                                  
         LA    R2,ATBRCVW_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       RECEIVE AND WAIT                             
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         CLC   STATUS_RECEIVED_E,ATB_CONFIRM_RECEIVED                           
         BE    *+6                                                              
         DC    H'0'                EDICT SHOULD HAVE DONE CONFIRM               
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         MVC   RECEIVE_LENGTH_E,=A(L'BUFFER_E)                                  
         LA    R2,ATBRCVW_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       RECEIVE AND WAIT                             
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         L     R2,RECEIVE_LENGTH_E                                              
         GOTO1 =V(PRNTBL),DMCB,=C'CONTROL RECORD RECEIVED',            +        
               BUFFER_E,C'DUMP',(R2),=C'1D'                                     
*                                                                               
         CLC   BUFFER_E(CTLMSG1L),CTLMSG1                                       
         BE    *+6                                                              
         DC    H'0'                WHERE'S THE HANDSHAKE RECORD?                
*                                                                               
         MVC   PARTNER_LU_NAME_C,BLANKS                                         
         MVC   PARTNER_LU_NAME_C(8),BUFFER_E+30   LUNAME                        
         MVC   USER_ID_C,BUFFER_E+83              USERID                        
         MVC   PASSWORD_C,BUFFER_E+103            PASSWORD                      
         MVC   TP_NAME_C,BUFFER_E+121             TPNAME                        
         SR    RE,RE               COUNT CHARACTERS IN TPNAME                   
         LA    RF,TP_NAME_C                                                     
         CLI   0(RF),C' '          SCAN FORWARD TO FIRST BLANK                  
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     *-16                                                             
         ST    RE,TP_NAME_LENGTH_C TRANSACTION PROGRAM NAME LENGTH              
         PACK  DUB,BUFFER_E+45(5)  USERID                                       
         CVB   R0,DUB                                                           
         STCM  R0,3,PQINDEX                                                     
         MVC   PQINDEX+2(3),BUFFER_E+51  SUB-ID                                 
         PACK  DUB,BUFFER_E+55(5)  REFERENCE NUMBER                             
         CVB   R0,DUB                                                           
         STCM  R0,3,PQINDEX+5                                                   
         MVC   XFERFLAG,BUFFER_E+66 TRANSMIT TO COLUMBINE: Y/N                  
         PACK  DUB,BUFFER_E+73(2)  MAXIMUM TRANSMIT TIME (MINUTES)              
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         M     R0,=F'6000'         SCALE TIME FOR STIMER                        
         ST    R1,WAITTIME                                                      
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
         EJECT                                                                  
         PRNT  SETTINGTIMER                                                     
         STIMERM SET,ID=STIMERID,BINTVL=WAITTIME,EXIT=TIMERXIT                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CTLMSG3A,BLANKS     NO ERROR YET                                 
         MVI   TIMEDOUT,C'N'       NO TIMEOUT YET                               
         MVI   CONVALLC,C'N'       NO CONVERSATION YET                          
*                                                                               
         MVC   CONVERSATION_TYPE_C,ATB_MAPPED_CONVERSATION                      
         MVC   RETURN_CONTROL_C,ATB_WHEN_SESSION_ALLOCATED                      
         MVC   SYNC_LEVEL_C,ATB_NONE                                            
         MVC   SECURITY_TYPE_C,ATB_SECURITY_PROGRAM                             
*                                                                               
         CLI   XFERFLAG,C'N'       WE'RE NOT REALLY SENDING                     
         BE    SENDHEAD                                                         
*                                                                               
         LA    R2,ATBALC2_STRUCTURE_C                                           
         BAS   RE,CALLAPPC_C       ALLOCATE THE CONVERSATION                    
         BE    CHKALLOC                                                         
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'TIMED OUT WAITING FOR RESPONSE'                   
         B     WEREDONE                                                         
*                                                                               
CHKALLOC OC    RETURN_CODE_C,RETURN_CODE_C                                      
         BZ    ALLOCOK                                                          
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'ALLOCATE ERROR. APPC RETURN CODE = '              
         EDIT  RETURN_CODE_C,(5,CTLMSG3A+35),ALIGN=LEFT                         
         B     WEREDONE                                                         
*                                                                               
ALLOCOK  BAS   RE,PRNTATTB_C       PRINT CONVERSATION ATTRIBUTES                
         MVI   CONVALLC,C'Y'       WE HAVE A CONVERSATION                       
*                                                                               
SENDHEAD BAS   RE,CHKFMT           DETERMINE IF NEW FORMAT                      
         BNE   WEREDONE                                                         
*                                                                               
         CLI   NEWFMT,C'Y'                                                      
         BE    NOHDR                                                            
*                                                                               
         PRNT  SENDINGHEADER                                                    
         LA    RE,L'BUFFER_C                                                    
         LA    RF,BUFFER_C-1(RE)                                                
         MVI   0(RF),C' '                                                       
         BCT   RE,*-8                                                           
*                                                                               
         MVC   BUFFER_C(4),=C'HEAD'                                             
         LA    R3,80               R3 = L'DATA                                  
         BAS   RE,XMITLINE                                                      
         BE    HDRSNTOK                                                         
*                                                                               
         CLI   TIMEDOUT,C'Y'                                                    
         BE    WEREDONE            ERROR MESSAGE ALREADY SET                    
*                                                                               
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'CAN''T SEND HEADER. APPC RETURN CODE = '          
         EDIT  RETURN_CODE_C,(5,CTLMSG3A+38),ALIGN=LEFT                         
         B     WEREDONE                                                         
*                                                                               
HDRSNTOK PRNT  HEADERSENT                                                       
*                                                                               
NOHDR    BAS   RE,SENDRPT          TRANSMIT THE PQ REPORT                       
         BNE   WEREDONE            PROBLEM SENDING REPORT                       
*                                                                               
         CLI   NEWFMT,C'Y'                                                      
         BE    NOTRLR                                                           
*                                                                               
         PRNT  SENDINGTRAILER                                                   
         LA    RE,L'BUFFER_C                                                    
         LA    RF,BUFFER_C-1(RE)                                                
         MVI   0(RF),C' '                                                       
         BCT   RE,*-8                                                           
*                                                                               
         MVC   BUFFER_C(4),=C'TRLR'                                             
         MVI   BUFFER_C+4,C'7'     USE 7 CHARACTERS OF CONTRACT NUMBER          
         MVC   BUFFER_C+5(7),CONTRACT                                           
         L     R0,RECCOUNT         NUMBER OF RECORDS IN PQ REPORT               
         CVD   R0,DUB                                                           
         UNPK  BUFFER_C+12(3),DUB                                               
         OI    BUFFER_C+14,X'F0'                                                
         LA    R3,80               R3 = L'DATA                                  
         BAS   RE,XMITLINE                                                      
         BNE   WEREDONE            COULD NOT SEND TRLR RECORD                   
*                                                                               
         PRNT  TRAILERSENT                                                      
*                                                                               
NOTRLR   CLI   XFERFLAG,C'N'       WE'RE NOT REALLY SENDING                     
         BE    WEREDONE                                                         
*                                                                               
         MVC   DEALLOCATE_TYPE_C,ATB_DEALLOCATE_FLUSH                           
*                                                                               
         LA    R2,ATBDEAL_STRUCTURE_C                                           
         BAS   RE,CALLAPPC_C       DEALLOCATE THE CONVERSATION                  
         BE    CHKDEAL                                                          
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'TIMED OUT WAITING FOR RESPONSE'                   
         B     WEREDONE                                                         
*                                                                               
CHKDEAL  OC    RETURN_CODE_C,RETURN_CODE_C                                      
         BZ    DEALLOK             RETURN CODE WAS OK                           
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'DEALLOCATE ERROR. APPC RETURN CODE = '            
         EDIT  RETURN_CODE_C,(5,CTLMSG3A+37),ALIGN=LEFT                         
         B     WEREDONE                                                         
*                                                                               
DEALLOK  PRNT  DEALLOCATED                                                      
         EJECT                                                                  
WEREDONE CLC   CTLMSG3(6),=C'COMMS ' DID A COMMS ERROR OCCUR?                   
         BE    DEALABND            YES -- DEALLOCATE WITH ABEND                 
         PRNT  CANCELLINGTIMER                                                  
         STIMERM CANCEL,ID=STIMERID                                             
         LTR   RF,RF                                                            
         BZ    CHKMSG                                                           
         DC    H'0'                                                             
*                                                                               
DEALABND CLI   XFERFLAG,C'N'       WE'RE NOT REALLY SENDING                     
         BE    CHKMSG                                                           
*                                                                               
         CLI   CONVALLC,C'Y'       DID WE ALLOCATE A CONVERSATION?              
         BNE   CHKMSG              NO                                           
*                                                                               
         LA    R2,ATBDEAL_STRUCTURE_C_ABEND                                     
         MVC   DEALLOCATE_TYPE_C,ATB_DEALLOCATE_ABEND                           
         MVC   NOTIFY_TYPE_C_DEAL_ABEND,ATB_NOTIFY_TYPE_NONE                    
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
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_C_DEAL_ABEND,(5,P+44),                      +        
               ALIGN=LEFT,ZERO=NOBLANK                                          
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE_C_DEAL_ABEND,RETURN_CODE_C_DEAL_ABEND                
         BZ    CHKMSG              RETURN CODE WAS OK                           
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'DEALLOCATE ERROR. APPC RETURN CODE = '            
         EDIT  RETURN_CODE_C_DEAL_ABEND,(5,CTLMSG3A+37),ALIGN=LEFT              
*                                                                               
CHKMSG   XC    BUFFER_E,BUFFER_E                                                
         CLC   CTLMSG3A,BLANKS     ANY ERRORS FOUND?                            
         BE    MSGOK               NO                                           
         MVC   BUFFER_E(CTLMSG3L),CTLMSG3       ERROR CONTROL MESSAGE           
         LA    R4,CTLMSG3L         R4 = L'DATA                                  
         B     SENDCTL                                                          
*                                                                               
MSGOK    MVC   BUFFER_E(CTLMSG2L),CTLMSG2       OK CONTROL MESSAGE              
         LA    R4,CTLMSG2L         R4 = L'DATA                                  
*                                                                               
SENDCTL  ST    R4,SEND_LENGTH_E                                                 
         GOTO1 =V(PRNTBL),DMCB,=C'BUFFER DATA',BUFFER_E,C'DUMP',       +        
               (R4),=C'1D'                                                      
*                                                                               
         MVC   SEND_TYPE_E,ATB_BUFFER_DATA                                      
*                                                                               
         LA    R2,ATBSEND_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       SEND DATA                                    
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                                                              
         DC    H'0'                COULD NOT SEND CONTROL MSG TO EDICT          
*                                                                               
         MVC   PREP_TO_RECEIVE_TYPE_E,ATB_PREP_TO_RECEIVE_FLUSH                 
*                                                                               
         LA    R2,ATBPTR_STRUCTURE_E                                            
         BAS   RE,CALLAPPC_E       PREPARE TO RECEIVE                           
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         MVC   RECEIVE_LENGTH_E,=A(L'BUFFER_E)                                  
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       RECEIVE AND WAIT                             
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         CLC   STATUS_RECEIVED_E,ATB_CONFIRM_DEALLOC_RECEIVED                   
         BE    CONFIRMD                                                         
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       RECEIVE AND WAIT                             
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         CLC   STATUS_RECEIVED_E,ATB_CONFIRM_DEALLOC_RECEIVED                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CONFIRMD LA    R2,ATBCFMD_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         LA    R2,ATBRTRN_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       RETURN MULTI-TRANS TRANSACTION               
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKERR   CLC   CTLMSG3A,BLANKS     ANY ERRORS FOUND?                            
         BE    GETTRANS            NO -- GET ANOTHER TRANSACTION                
*                                                                               
GOODBYE  PRNT  *****************                                                
         PRNT  *****************                                                
         PRNT  *****EXITING*****                                                
         PRNT  *****************                                                
         PRNT  *****************                                                
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         PRNT  *****************                                                
         PRNT  *****************                                                
         PRNT  ***INITIALIZE****                                                
         PRNT  *****************                                                
         PRNT  *****************                                                
*                                                                               
         L     R2,PSATOLD-PSA      TCB ADDRESS                                  
         GOTO1 =V(HEXOUT),DMCB,(R2),P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  TCBADDRESS                                                       
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
         LOAD  DE=ATBALC2                                                       
         ST    R0,ATBALC2_STRUCTURE_C                                           
         LOAD  DE=ATBCFMD                                                       
         ST    R0,ATBCFMD_STRUCTURE_E                                           
         LOAD  DE=ATBDEAL                                                       
         ST    R0,ATBDEAL_STRUCTURE_C                                           
         ST    R0,ATBDEAL_STRUCTURE_C_ABEND                                     
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE_C                                           
         LOAD  DE=ATBGETC                                                       
         ST    R0,ATBGETC_STRUCTURE_E                                           
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE_C                                           
         ST    R0,ATBGTA2_STRUCTURE_E                                           
         LOAD  DE=ATBPTR                                                        
         ST    R0,ATBPTR_STRUCTURE_E                                            
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE_C                                           
         ST    R0,ATBRCVW_STRUCTURE_E                                           
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE_C                                           
         ST    R0,ATBSEND_STRUCTURE_E                                           
         LOAD  DE=ATBGTRN                                                       
         ST    R0,ATBGTRN_STRUCTURE_E                                           
         LOAD  DE=ATBRTRN                                                       
         ST    R0,ATBRTRN_STRUCTURE_E                                           
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
SENDRPT  NTR1                                                                   
*                                                                               
         LA    R3,PQINDEX                                                       
         USING UKRECD,R3                                                        
         SR    R0,R0                                                            
         ICM   R0,3,UKSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),UKSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,UKREPNO                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         PRNT  SENDINGREPORT                                                    
*                                                                               
         LA    RE,CIREC                                                         
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
         SR    R2,R2               CLEAR RECORD COUNTER                         
         MVI   LASTSEEN,C'N'       LAST RECORD NOT YET SEEN                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'GFILE'),=C'PRTQUE',PQINDEX,      +        
               0,A(CIREC)                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRTQNAME,UKUSRINF                                                
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'INDEX'),PRTQNAME,PQINDEX,0,      +        
               A(CIREC)                                                         
         CLI   DMCB+8,0                                                         
         BE    FINDFRST                                                         
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'INDEX ENTRY NOT FOUND'                            
         B     RPTBAD                                                           
*                                                                               
FINDFRST CLI   NEWFMT,C'Y'                                                      
         BE    FINDFST2                                                         
*                                                                               
FINDFST1 GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),PRTQNAME,0,R,A(CIREC)              
         CLI   8(R1),0                                                          
         BE    CHECKHDR                                                         
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'COULD NOT READ REPORT'                            
         B     RPTBAD                                                           
*                                                                               
CHECKHDR CLI   R+1,C'H'            FIND FIRST DATA RECORD IN REPORT             
         BNE   FINDFST1                                                         
         LA    R3,128              LENGTH OF A HEADER RECORD                    
         MVC   CONTRACT,R+5        SAVE CONTRACT NUMBER                         
*                                                                               
SENDLINE BCTR  R3,0                                                             
         EX    R3,*+8              MOVE IN LINE                                 
         B     *+10                                                             
         MVC   BUFFER_C(0),R+1                                                  
         LA    R3,1(R3)            RESTORE LENGTH OF DATA                       
         BAS   RE,XMITLINE                                                      
         BNE   RPTBAD                                                           
*                                                                               
         LA    R2,1(R2)            INCREMENT RECORD COUNTER                     
         CLI   LASTSEEN,C'Y'       ANY MORE RECORDS TO SEND?                    
         BE    RPTOK               NO                                           
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),PRTQNAME,0,R,A(CIREC)              
         CLI   8(R1),0                                                          
         BE    CHECKEND                                                         
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'COULD NOT READ REPORT'                            
         B     RPTBAD                                                           
*                                                                               
CHECKEND CLI   R+1,C'E'            FIND LAST DATA RECORD IN REPORT              
         BNE   *+16                                                             
         LA    R3,19               LENGTH OF AN ENDING RECORD                   
         MVI   LASTSEEN,C'Y'       THIS IS THE LAST RECORD                      
         B     SENDLINE                                                         
         CLI   R+1,C'M'                                                         
         BNE   *+12                                                             
         LA    R3,86               LENGTH OF A MAINLINE RECORD                  
         B     SENDLINE                                                         
         CLI   R+1,C'A'                                                         
         BNE   *+12                                                             
         LA    R3,126              LENGTH OF AN ADDITIONAL RECORD               
         B     SENDLINE                                                         
         CLI   R+1,C'C'                                                         
         BE    *+20                                                             
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'UNKNOWN RECORD TYPE IN REPORT'                    
         B     RPTBAD                                                           
         LA    R3,101              LENGTH OF A COMMENT RECORD                   
         B     SENDLINE                                                         
*                                                                               
FINDFST2 GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),PRTQNAME,0,R,A(CIREC)              
         CLI   8(R1),0                                                          
         BE    CHKHDR2                                                          
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'COULD NOT READ REPORT'                            
         B     RPTBAD                                                           
*                                                                               
CHKHDR2  CLC   R+1(4),=C'0301'     FIND FIRST DATA RECORD IN REPORT             
         BNE   FINDFST2                                                         
*                                                                               
SNDLINE2 LA    R4,BUFFER_C                                                      
         LA    R3,2                GET 2 MORE PQ LINE                           
GETNLNE  MVC   0(110,R4),R+1                                                    
         GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),PRTQNAME,0,R,A(CIREC)              
         CLI   8(R1),0                                                          
         BE    NEXTLNE                                                          
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'COULD NOT READ REPORT'                            
         B     RPTBAD                                                           
NEXTLNE  AHI   R4,110                                                           
         BCT   R3,GETNLNE                                                       
         MVC   0(110,R4),R+1                                                    
*                                                                               
         LA    R3,264                                                           
         BAS   RE,XMITLINE                                                      
         BNE   RPTBAD                                                           
*                                                                               
         LA    R2,1(R2)            INCREMENT RECORD COUNTER                     
         CLI   LASTSEEN,C'Y'       ANY MORE RECORDS TO SEND?                    
         BE    RPTOK               NO                                           
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),PRTQNAME,0,R,A(CIREC)              
         CLI   8(R1),0                                                          
         BE    CHKEND2                                                          
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'COULD NOT READ REPORT'                            
         B     RPTBAD                                                           
*                                                                               
CHKEND2  CLC   R+1(4),=C'0310'     FIND LAST DATA RECORD IN REPORT              
         BNE   *+12                                                             
         MVI   LASTSEEN,C'Y'       THIS IS THE LAST RECORD                      
         B     SNDLINE2                                                         
         CLC   R+1(2),=C'03'                                                    
         BE    SNDLINE2                                                         
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'UNKNOWN RECORD TYPE IN REPORT'                    
         B     RPTBAD                                                           
*                                                                               
RPTOK    ST    R2,RECCOUNT         SAVE NUMBER OF RECORDS TRANSMITTED           
         CR    RB,RB                                                            
         B     RPTX                                                             
*                                                                               
RPTBAD   LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
*                                                                               
RPTX     B     XIT                                                              
         EJECT                                                                  
CHKFMT   NTR1                                                                   
*                                                                               
         MVI   NEWFMT,C'N'         ASSUME THE OLD FORMAT                        
         LA    R3,PQINDEX                                                       
         USING UKRECD,R3                                                        
*                                                                               
         LA    RE,CIREC                                                         
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'GFILE'),=C'PRTQUE',PQINDEX,      +        
               0,A(CIREC)                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRTQNAME,UKUSRINF                                                
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'INDEX'),PRTQNAME,PQINDEX,0,      +        
               A(CIREC)                                                         
         CLI   DMCB+8,0                                                         
         BE    CHKF10                                                           
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'INDEX ENTRY NOT FOUND'                            
         B     RPTBAD                                                           
*                                                                               
CHKF10   GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),PRTQNAME,0,R,A(CIREC)              
         CLI   8(R1),0                                                          
         BE    CHKF20                                                           
         MVC   CTLMSG3(6),=C'REPORT'                                            
         MVC   CTLMSG3A,=CL45'COULD NOT READ REPORT'                            
         B     RPTBAD                                                           
*                                                                               
CHKF20   CLC   R+1(4),=C'0310'     NEW FORMAT?                                  
         BNE   *+12                                                             
         MVI   NEWFMT,C'Y'         THIS IS NEW FORMAT                           
         B     CHKFOK                                                           
*                                                                               
         CLI   R+1,C'H'            OLD FORMAT?                                  
         BNE   CHKF10              READ NEXT LINE                               
*                                                                               
CHKFOK   CR    RB,RB                                                            
         B     CHKFX                                                            
*                                                                               
CHKFBAD  LTR   RB,RB               BAD REPORT - SET CC NOT EQUAL                
*                                                                               
CHKFX    B     XIT                                                              
         EJECT                                                                  
XMITLINE NTR1                                                                   
*                                                                               
* ON ENTRY, R3 = LENGTH OF DATA                                                 
*                                                                               
         LR    R2,R3                                                            
         ST    R2,SEND_LENGTH_C                                                 
*                                                                               
         MVC   SEND_TYPE_C,ATB_SEND_AND_PREP_TO_RECEIVE                         
         CLI   NEWFMT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   SEND_TYPE_C,ATB_BUFFER_DATA                                      
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'SEND AND PREPARE TO RECEIVE',        +        
               BUFFER_C,C'DUMP',(R2),=C'1D'                                     
*                                                                               
         CLI   XFERFLAG,C'N'       WE'RE NOT REALLY SENDING                     
         BE    XMITOK                                                           
*                                                                               
         LA    R2,ATBSEND_STRUCTURE_C                                           
         BAS   RE,CALLAPPC_C       SEND DATA                                    
         BE    CHKSEND                                                          
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'TIMED OUT WAITING FOR RESPONSE'                   
         B     XMITBAD                                                          
*                                                                               
CHKSEND  OC    RETURN_CODE_C,RETURN_CODE_C                                      
         BZ    SENDOK                                                           
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'SEND ERROR. APPC RETURN CODE = '                  
         EDIT  RETURN_CODE_C,(5,CTLMSG3A+31),ALIGN=LEFT                         
         B     XMITBAD                                                          
*                                                                               
SENDOK   CLI   NEWFMT,C'Y'                                                      
         BE    XMITOK                                                           
         MVC   RECEIVE_LENGTH_C,=A(L'BUFFER_C)                                  
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE_C                                           
         BAS   RE,CALLAPPC_C       RECEIVE AND WAIT                             
         BE    CHKRCV                                                           
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'TIMED OUT WAITING FOR RESPONSE'                   
         B     XMITBAD                                                          
*                                                                               
CHKRCV   OC    RETURN_CODE_C,RETURN_CODE_C                                      
         BZ    GOTDATA                                                          
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'RECEIVE ERROR. APPC RETURN CODE = '               
         EDIT  RETURN_CODE_C,(5,CTLMSG3A+34),ALIGN=LEFT                         
         B     XMITBAD                                                          
*                                                                               
GOTDATA  L     R2,RECEIVE_LENGTH_C                                              
         GOTO1 =V(PRNTBL),DMCB,=C'OK RECEIVED',BUFFER_C,C'DUMP',       +        
               (R2),=C'1D'                                                      
*                                                                               
         CLC   =C'OK',BUFFER_C                                                  
         BE    OKRCVD                                                           
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'DID NOT GET "OK" FROM COLUMBINE'                  
         B     XMITBAD                                                          
*                                                                               
OKRCVD   CLC   STATUS_RECEIVED_C,ATB_SEND_RECEIVED                              
         BNE   TRYAGAIN                                                         
         PRNT  SWITCHINGTOSEND                                                  
         B     XMITOK                                                           
*                                                                               
TRYAGAIN PRNT  RECEIVE_AGAIN                                                    
         MVC   RECEIVE_LENGTH_C,=A(L'BUFFER_C)                                  
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE_C                                           
         BAS   RE,CALLAPPC_C       RECEIVE AND WAIT                             
         BE    CHKRCV2                                                          
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'TIMED OUT WAITING FOR RESPONSE'                   
         B     XMITBAD                                                          
*                                                                               
CHKRCV2  OC    RETURN_CODE_C,RETURN_CODE_C                                      
         BZ    RCVOK                                                            
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'RECEIVE ERROR. APPC RETURN CODE = '               
         EDIT  RETURN_CODE_C,(5,CTLMSG3A+34),ALIGN=LEFT                         
         B     XMITBAD                                                          
*                                                                               
RCVOK    CLC   STATUS_RECEIVED_C,ATB_SEND_RECEIVED                              
         BE    SENDNOW                                                          
         MVC   CTLMSG3(6),=C'COMMS '                                            
         MVC   CTLMSG3A,=CL45'COLUMBINE WON''T LET US SEND'                     
         B     XMITBAD                                                          
*                                                                               
SENDNOW  PRNT  SWITCHINGTOSEND                                                  
         B     XMITOK                                                           
*                                                                               
XMITBAD  LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     XMITLINX                                                         
*                                                                               
XMITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
XMITLINX B     XIT                                                              
         EJECT                                                                  
PRNTATTB_C NTR1                                                                 
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************                                                    
         PRNT  GETATTRIBUTES                                                    
         PRNT  *************                                                    
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE_C                                           
         BAS   RE,CALLAPPC_C       GET CONVERSATION ATTRIBUTES                  
*                                                                               
         OC    RETURN_CODE_C,RETURN_CODE_C                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID_C,P+30,8,=C'TOG'                 
         PRNT  CONVERSATION_ID                                                  
         MVC   P+30(17),PARTNER_LU_NAME_C                                       
         PRNT  PARTNER_LU_NAME                                                  
         MVC   P+30(8),MODE_NAME_C                                              
         PRNT  MODE_NAME                                                        
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL_C,ATB_NONE                                            
         BE    *+22                                                             
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL_C,ATB_CONFIRM                                         
         BE    *+6                                                              
         DC    H'0'                UNKNOWN SYNC LEVEL                           
         PRNT  SYNC_LEVEL                                                       
         MVC   P+30(64),TP_NAME_C                                               
         PRNT  TP_NAME                                                          
         MVC   P+30(8),LOCAL_LU_NAME_C                                          
         PRNT  LOCAL_LU_NAME                                                    
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE_C,ATB_BASIC_CONVERSATION                       
         BE    *+22                                                             
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE_C,ATB_MAPPED_CONVERSATION                      
         BE    *+6                                                              
         DC    H'0'                UNKNOWN CONVERSATION TYPE                    
         PRNT  CONVERSATION_TYPE                                                
         MVC   P+30(4),=C'SEND'                                                 
         CLC   CONVERSATION_STATE_C,ATB_SEND_STATE                              
         BE    PRATTBC5                                                         
         MVC   P+30(7),=C'RECEIVE'                                              
         CLC   CONVERSATION_STATE_C,ATB_RECEIVE_STATE                           
         BE    PRATTBC5                                                         
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   CONVERSATION_STATE_C,ATB_CONFIRM_STATE                           
         BE    PRATTBC5                                                         
         MVC   P+30(10),=C'INITIALIZE'                                          
         CLC   CONVERSATION_STATE_C,ATB_INITIALIZE_STATE                        
         BE    PRATTBC5                                                         
         MVC   P+30(12),=C'SEND PENDING'                                        
         CLC   CONVERSATION_STATE_C,ATB_SEND_PENDING_STATE                      
         BE    PRATTBC5                                                         
         MVC   P+30(12),=C'CONFIRM SEND'                                        
         CLC   CONVERSATION_STATE_C,ATB_CONFIRM_SEND_STATE                      
         BE    PRATTBC5                                                         
         MVC   P+30(18),=C'CONFIRM DEALLOCATE'                                  
         CLC   CONVERSATION_STATE_C,ATB_CONFIRM_DEALLOCATE_STATE                
         BE    PRATTBC5                                                         
         DC    H'0'                UNKNOWN CONVERSATION STATE                   
PRATTBC5 PRNT  CONVERSATION_STATE                                               
*                                                                               
         PRNT  *************                                                    
         PRNT  *************                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
APPCERR_C NTR1                                                                  
*                                                                               
* PERFORM APPC EXTRACT_ERROR FUNCTION.                                          
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         LA    R2,ATBEES3_STRUCTURE_C                                           
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
         EDIT  RETURN_CODE_ERROR_EXTRACT_C,(5,P+44),ALIGN=LEFT,        +        
               ZERO=NOBLANK                                                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE_ERROR_EXTRACT_C,RETURN_CODE_ERROR_EXTRACT_C          
         BNZ   APPCEX_C            BAD RETURN CODE, SO NO ERROR INFO            
*                                                                               
         MVC   P+30(37),=C'APPC SERVICE XXXXXXXX, REASON_CODE = '               
         MVC   P+43(8),SERVICE_NAME_C                                           
         EDIT  SERVICE_REASON_CODE_C,(5,P+67),ALIGN=LEFT,ZERO=NOBLANK           
         PRNT  ERROR_EXTRACT                                                    
*                                                                               
         ICM   R3,15,MESSAGE_TEXT_LENGTH_C                                      
         BZ    APPCE20_C           NOTHING IN FIELD                             
         C     R3,=F'132'          MESSAGE > 132 CHARACTERS?                    
         BH    APPCE10_C           YES, BREAK MESSAGE UP INTO TWO LINES         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MESSAGE_TEXT_C                                              
         GOTO1 =V(PRINTER)                                                      
         B     APPCE20_C                                                        
*                                                                               
APPCE10_C DS   0H                                                               
         S     R3,=F'132'                                                       
         MVC   P,MESSAGE_TEXT_C                                                 
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MESSAGE_TEXT_C+132                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
APPCE20_C DS   0H                                                               
         ICM   R3,15,ERROR_LOG_INFORMATION_LENGTH_C                             
         BZ    APPCEX_C            NOTHING IN FIELD                             
         C     R3,=F'132'          MESSAGE > 132 CHARACTERS?                    
         BH    APPCE30_C           YES, BREAK MESSAGE UP                        
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION_C                                     
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX_C                                                         
*                                                                               
APPCE30_C DS   0H                                                               
         C     R3,=F'264'          WILL MESSAGE FIT ON TWO LINES?               
         BH    APPCE40_C                                                        
         S     R3,=F'132'          YES                                          
         MVC   P,ERROR_LOG_INFORMATION_C                                        
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION_C+132                                 
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX_C                                                         
*                                                                               
APPCE40_C DS   0H                                                               
         C     R3,=F'396'          WILL MESSAGE FIT ON THREE LINES?             
         BH    APPCE50_C                                                        
         S     R3,=F'264'          YES                                          
         MVC   P,ERROR_LOG_INFORMATION_C                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION_C+132                                    
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION_C+264                                 
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX_C                                                         
*                                                                               
APPCE50_C DS   0H                                                               
         S     R3,=F'396'          SPLIT INTO FOUR LINES                        
         MVC   P,ERROR_LOG_INFORMATION_C                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION_C+132                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION_C+264                                    
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION_C+396                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
APPCEX_C B     XIT                                                              
         EJECT                                                                  
PRNTATTB_E NTR1                                                                 
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************                                                    
         PRNT  GETATTRIBUTES                                                    
         PRNT  *************                                                    
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE_E                                           
         BAS   RE,CALLAPPC_E       GET CONVERSATION ATTRIBUTES                  
*                                                                               
         OC    RETURN_CODE_E,RETURN_CODE_E                                      
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID_E,P+30,8,=C'TOG'                 
         PRNT  CONVERSATION_ID                                                  
         MVC   P+30(17),PARTNER_LU_NAME_E                                       
         PRNT  PARTNER_LU_NAME                                                  
         MVC   P+30(8),MODE_NAME_E                                              
         PRNT  MODE_NAME                                                        
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL_E,ATB_NONE                                            
         BE    *+22                                                             
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL_E,ATB_CONFIRM                                         
         BE    *+6                                                              
         DC    H'0'                UNKNOWN SYNC LEVEL                           
         PRNT  SYNC_LEVEL                                                       
         EDIT  TP_NAME_LENGTH_E,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                
         PRNT  TP_NAME_LENGTH                                                   
         MVC   P+30(64),TP_NAME_E                                               
         PRNT  TP_NAME                                                          
         MVC   P+30(8),LOCAL_LU_NAME_E                                          
         PRNT  LOCAL_LU_NAME                                                    
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE_E,ATB_BASIC_CONVERSATION                       
         BE    *+22                                                             
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE_E,ATB_MAPPED_CONVERSATION                      
         BE    *+6                                                              
         DC    H'0'                UNKNOWN CONVERSATION TYPE                    
         PRNT  CONVERSATION_TYPE                                                
         MVC   P+30(10),USER_ID_E                                               
         PRNT  USER_ID                                                          
         MVC   P+30(10),PROFILE_E                                               
         PRNT  PROFILE                                                          
         MVC   P+30(80),USER_TOKEN_E                                            
         PRNT  USER_TOKEN                                                       
         MVC   P+30(4),=C'SEND'                                                 
         CLC   CONVERSATION_STATE_E,ATB_SEND_STATE                              
         BE    PRATTBE5                                                         
         MVC   P+30(7),=C'RECEIVE'                                              
         CLC   CONVERSATION_STATE_E,ATB_RECEIVE_STATE                           
         BE    PRATTBE5                                                         
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   CONVERSATION_STATE_E,ATB_CONFIRM_STATE                           
         BE    PRATTBE5                                                         
         MVC   P+30(10),=C'INITIALIZE'                                          
         CLC   CONVERSATION_STATE_E,ATB_INITIALIZE_STATE                        
         BE    PRATTBE5                                                         
         MVC   P+30(12),=C'SEND PENDING'                                        
         CLC   CONVERSATION_STATE_E,ATB_SEND_PENDING_STATE                      
         BE    PRATTBE5                                                         
         MVC   P+30(12),=C'CONFIRM SEND'                                        
         CLC   CONVERSATION_STATE_E,ATB_CONFIRM_SEND_STATE                      
         BE    PRATTBE5                                                         
         MVC   P+30(18),=C'CONFIRM DEALLOCATE'                                  
         CLC   CONVERSATION_STATE_E,ATB_CONFIRM_DEALLOCATE_STATE                
         BE    PRATTBE5                                                         
         DC    H'0'                UNKNOWN CONVERSATION STATE                   
PRATTBE5 PRNT  CONVERSATION_STATE                                               
*                                                                               
         PRNT  *************                                                    
         PRNT  *************                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
CALLAPPC_C NTR1                                                                 
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, RTN_CODE CONTAINS THE APPC/MVS RETURN CODE                       
*                                                                               
         MVC   NOTIFY_TYPE_C,ATB_NOTIFY_TYPE_ECB                                
         TM    20(R2),X'80'                                                     
         BZ    *+10                                                             
         MVC   NOTIFY_TYPE_C,ATB_NOTIFY_TYPE_NONE                               
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
         TM    20(R2),X'80'        SYNCHRONOUS CALL?                            
         BO    CALLC30             YES, SO WE'RE DONE                           
*                                                                               
         CLC   RETURN_CODE_C,ATB_OK                                             
         BE    CALLC10             APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_C,(5,P+64),ALIGN=LEFT                                
         PRNT  **BAD_APPC_CALL**                                                
         B     CALLC40             APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALLC10  WAIT  1,ECBLIST=ECBLST_C  WAIT FOR COMPLETION OR TIMER POP             
*                                                                               
         TM    TIMERECB,X'40'      DID TIMER POP?                               
         BZ    CALLC20                                                          
         XC    TIMERECB,TIMERECB   YES                                          
         MVI   TIMEDOUT,C'Y'                                                    
         B     CALLCBAD                                                         
*                                                                               
CALLC20  TM    APPCECBC,X'40'                                                   
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE_C,APPCECBC                                           
         MVI   RETURN_CODE_C,0     CLEAR HIGH-ORDER BYTE                        
         XC    APPCECBC,APPCECBC                                                
*                                                                               
CALLC30  MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_C,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                   
         PRNT                                                                   
*                                                                               
CALLC40  OC    RETURN_CODE_C,RETURN_CODE_C                                      
         BZ    CALLCOK                                                          
         TM    20(R2),X'40'        CAN CALL ERROR_EXTRACT NOW?                  
         BZ    CALLCOK                                                          
         BAS   RE,APPCERR_C        YES                                          
*                                                                               
CALLCOK  CR    RB,RB               APPC CALL COMPLETED -- SET CC EQUAL          
         B     CALLCX                                                           
*                                                                               
CALLCBAD LTR   RB,RB               TIMER POPPED -- SET CC NOT EQUAL             
*                                                                               
CALLCX   B     XIT                                                              
         EJECT                                                                  
CALLAPPC_E NTR1                                                                 
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, RETURN_CODE_E CONTAINS THE APPC/MVS RETURN CODE                  
*                                                                               
         MVC   NOTIFY_TYPE_E,ATB_NOTIFY_TYPE_ECB                                
         TM    20(R2),X'80'                                                     
         BZ    *+10                                                             
         MVC   NOTIFY_TYPE_E,ATB_NOTIFY_TYPE_NONE                               
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
         TM    20(R2),X'80'        SYNCHRONOUS CALL?                            
         BO    CALLEX              YES, SO WE'RE DONE                           
*                                                                               
         CLC   RETURN_CODE_E,ATB_OK                                             
         BE    CALLE10             APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_E,(5,P+64),ALIGN=LEFT                                
         PRNT  **BAD_APPC_CALL**                                                
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALLE10  WAIT  ECB=APPCECBE        WAIT FOR COMPLETION                          
*                                                                               
         TM    APPCECBE,X'40'                                                   
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE_E,APPCECBE                                           
         MVI   RETURN_CODE_E,0     CLEAR HIGH-ORDER BYTE                        
         XC    APPCECBE,APPCECBE                                                
*                                                                               
CALLEX   MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_E,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                   
         PRNT                                                                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         DROP  RB,R9,R8,R7,R6                                                   
         SPACE 3                                                                
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         SPACE 2                                                                
TIMERECB DS    F                                                                
         SPACE 3                                                                
         USING COLXFER,RB,R9,R8,R7,R6                                           
         EJECT                                                                  
         ATBSERV                                                                
         SPACE 2                                                                
ECBLST_C DS    0F                                                               
         DC    X'00',AL3(TIMERECB)                                              
         DC    X'80',AL3(APPCECBC)                                              
         SPACE 2                                                                
***********************************************************************         
         DS    0D                                                               
*FASSBOFF                                                                       
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
UTL      DC    F'0',X'0A'                                                       
***********************************************************************         
* APPC/MVS CALL PARAMETERS                                                      
*                                                                               
CONVERSATION_TYPE_C            DS    F                                          
SYM_DEST_NAME_C                DC    CL8' '                                     
PARTNER_LU_NAME_C              DC    CL17' '                                    
LOCAL_LU_NAME_C                DC    CL8' '                                     
MODE_NAME_C                    DC    CL8'DDSLU62 '                              
TP_NAME_LENGTH_C               DS    F                                          
TP_NAME_C                      DS    CL64                                       
RETURN_CONTROL_C               DS    F                                          
SYNC_LEVEL_C                   DS    F                                          
SECURITY_TYPE_C                DS    F                                          
CONVERSATION_CORRELATOR_C      DS    CL8                                        
USER_ID_C                      DS    CL10                                       
PASSWORD_C                     DS    CL10                                       
PROFILE_C                      DC    CL10' '                                    
USER_TOKEN_C                   DC    XL80'00'                                   
CONVERSATION_ID_C              DS    CL8                                        
LUW_ID_C                       DS    XL26                                       
CONVERSATION_STATE_C           DS    F                                          
NOTIFY_TYPE_C                  DS    F                                          
                               DC    A(APPCECBC) MUST FOLLOW NOTIFY TYP         
NOTIFY_TYPE_C_DEAL_ABEND       DS    F                                          
TPID_C                         DC    XL8'00'                                    
RETURN_CODE_C                  DS    F                                          
RETURN_CODE_C_DEAL_ABEND       DS    F                                          
FILL_C                         DS    F                                          
RECEIVE_LENGTH_C               DS    F                                          
RECEIVE_ACCESS_TOKEN_C         DC    F'0'                                       
STATUS_RECEIVED_C              DS    F                                          
DATA_RECEIVED_C                DS    F                                          
REQUEST_TO_SEND_RECEIVED_C     DS    F                                          
REQUEST_TO_SEND_VALUE_C        DS    F                                          
DEALLOCATE_TYPE_C              DS    F                                          
SEND_TYPE_C                    DS    F                                          
SEND_LENGTH_C                  DS    F                                          
SEND_ACCESS_TOKEN_C            DC    F'0'                                       
BUFFER_C                       DS    CL330                                      
SERVICE_NAME_C                 DS    CL8                                        
SERVICE_REASON_CODE_C          DS    F                                          
MESSAGE_TEXT_LENGTH_C          DS    F                                          
MESSAGE_TEXT_C                 DS    CL256                                      
ERROR_LOG_PRODUCT_SET_ID_LENGTH_C DS F                                          
ERROR_LOG_PRODUCT_SET_ID_C     DS    CL256                                      
ERROR_LOG_INFORMATION_LENGTH_C DS    F                                          
ERROR_LOG_INFORMATION_C        DS    CL512                                      
RETURN_CODE_ERROR_EXTRACT_C    DS    F                                          
REASON_CODE_ERROR_EXTRACT_C    DS    F                                          
         SPACE 3                                                                
CONVERSATION_TYPE_E            DS    F                                          
SYM_DEST_NAME_E                DC    CL8' '                                     
PARTNER_LU_NAME_E              DC    CL17' '                                    
LOCAL_LU_NAME_E                DC    CL8' '                                     
MODE_NAME_E                    DC    CL8' '                                     
TP_NAME_LENGTH_E               DS    F                                          
TP_NAME_E                      DC    CL64' '                                    
RETURN_CONTROL_E               DS    F                                          
SYNC_LEVEL_E                   DS    F                                          
SECURITY_TYPE_E                DS    F                                          
CONVERSATION_CORRELATOR_E      DS    CL8                                        
USER_ID_E                      DC    CL10' '                                    
PASSWORD_E                     DC    CL10' '                                    
PROFILE_E                      DC    CL10' '                                    
USER_TOKEN_E                   DC    XL80'00'                                   
CONVERSATION_ID_E              DS    CL8                                        
LUW_ID_E                       DS    XL26                                       
CONVERSATION_STATE_E           DS    F                                          
NOTIFY_TYPE_E                  DS    F                                          
                               DC    A(APPCECBE) MUST FOLLOW NOTIFY TYP         
TPID_E                         DC    XL8'00'                                    
RETURN_CODE_E                  DS    F                                          
FILL_E                         DS    F                                          
RECEIVE_LENGTH_E               DS    F                                          
RECEIVE_ACCESS_TOKEN_E         DC    F'0'                                       
STATUS_RECEIVED_E              DS    F                                          
DATA_RECEIVED_E                DS    F                                          
REQUEST_TO_SEND_RECEIVED_E     DS    F                                          
REQUEST_TO_SEND_VALUE_E        DS    F                                          
DEALLOCATE_TYPE_E              DS    F                                          
SEND_TYPE_E                    DS    F                                          
SEND_LENGTH_E                  DS    F                                          
SEND_ACCESS_TOKEN_E            DC    F'0'                                       
FUNCTION                       DC    F'2'   ASYNC_MANAGER CLEANUP               
ASYNCHRONOUS_NUMBER            DS    F                                          
LOCKS_E                        DS    F                                          
PREP_TO_RECEIVE_TYPE_E         DS    F                                          
BUFFER_E                       DS    CL256                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                                                                
FULL     DS    F                                                                
PRNTTIME DS    CL9                                                              
WORK     DS    CL256                                                            
BLANKS   DC    CL256' '                                                         
APPCECBC DC    F'0'                ECB FOR APPC/MVS CALLS TO COLUMBINE          
APPCECBE DC    F'0'                ECB FOR APPC/MVS CALLS WITH EDICT            
STIMERID DS    XL4                                                              
WAITTIME DS    F                   MAX ALLOWABLE TIME FOR MESSAGE XMIT          
RECCOUNT DS    F                   RECORD COUNT IN MESSAGE                      
CONTRACT DS    CL7                 CONTRACT NUMBER                              
PQINDEX  DS    XL40                CURRENT REPORT KEY                           
PRTQNAME DS    CL8                 NAME OF SPECIFIC PRTQUE                      
LASTSEEN DS    C                   'Y' IF WE'VE SEEN LAST REPORT RECORD         
XFERFLAG DS    C                   'N' IF WE'RE NOT REALLY SENDING              
TIMEDOUT DS    C                   'Y' IF WE TIMED OUT TRYING TO SEND           
CONVALLC DS    C                   'Y' IF WE'VE ALLOCATED WITH COLUMB.          
NEWFMT   DS    C                   'Y' IF NEW REPORT FORMAT                     
CTLMSG1  DC    C'EDICT CONTROL MESSAGE 1'                                       
CTLMSG1L EQU   *-CTLMSG1                                                        
CTLMSG2  DC    C'REPORT SENT OK'                                                
CTLMSG2L EQU   *-CTLMSG2                                                        
CTLMSG3  DC    C'XXXXXX ERROR: '   'COMMS  ERROR' OR 'REPORT ERROR'             
CTLMSG3A DS    CL45                ERROR MESSAGE GOES HERE                      
CTLMSG3L EQU   *-CTLMSG3                                                        
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL210               PQ RECORD DATA                               
         EJECT                                                                  
* PARAMETER LISTS FOR APPC/MVS CALLS                                            
*                                                                               
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*       X'80': ROUTINE IS SYNCHRONOUS ONLY                                      
*       X'40': ROUTINE CAN BE FOLLOWED BY EXTRACT_ERROR CALL                    
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
ATBALC2_STRUCTURE_C       DS    A                                               
                          DC    CL16'ALLOCATE'                                  
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_TYPE_C)                  
                          DC    X'00',AL3(SYM_DEST_NAME_C)                      
                          DC    X'00',AL3(PARTNER_LU_NAME_C)                    
                          DC    X'00',AL3(MODE_NAME_C)                          
                          DC    X'00',AL3(TP_NAME_LENGTH_C)                     
                          DC    X'00',AL3(TP_NAME_C)                            
                          DC    X'00',AL3(RETURN_CONTROL_C)                     
                          DC    X'00',AL3(SYNC_LEVEL_C)                         
                          DC    X'00',AL3(SECURITY_TYPE_C)                      
                          DC    X'00',AL3(USER_ID_C)                            
                          DC    X'00',AL3(PASSWORD_C)                           
                          DC    X'00',AL3(PROFILE_C)                            
                          DC    X'00',AL3(USER_TOKEN_C)                         
                          DC    X'00',AL3(CONVERSATION_ID_C)                    
                          DC    X'00',AL3(NOTIFY_TYPE_C)                        
                          DC    X'00',AL3(TPID_C)                               
                          DC    X'00',AL3(LOCAL_LU_NAME_C)                      
                          DC    X'80',AL3(RETURN_CODE_C)                        
*                                                                               
ATBGETC_STRUCTURE_E       DS    A                                               
                          DC    CL16'GET_CONVERSATION'                          
                          DC    X'C0'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_E)                    
                          DC    X'00',AL3(CONVERSATION_TYPE_E)                  
                          DC    X'00',AL3(PARTNER_LU_NAME_E)                    
                          DC    X'00',AL3(MODE_NAME_E)                          
                          DC    X'00',AL3(SYNC_LEVEL_E)                         
                          DC    X'00',AL3(CONVERSATION_CORRELATOR_E)            
                          DC    X'80',AL3(RETURN_CODE_E)                        
*                                                                               
ATBDEAL_STRUCTURE_C       DS    A                                               
                          DC    CL16'DEALLOCATE'                                
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_C)                    
                          DC    X'00',AL3(DEALLOCATE_TYPE_C)                    
                          DC    X'00',AL3(NOTIFY_TYPE_C)                        
                          DC    X'80',AL3(RETURN_CODE_C)                        
*                                                                               
ATBDEAL_STRUCTURE_C_ABEND DS    A                                               
                          DC    CL16'DEALLOCATE/ABEND'                          
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_C)                    
                          DC    X'00',AL3(DEALLOCATE_TYPE_C)                    
                          DC    X'00',AL3(NOTIFY_TYPE_C_DEAL_ABEND)             
                          DC    X'80',AL3(RETURN_CODE_C_DEAL_ABEND)             
*                                                                               
ATBCFMD_STRUCTURE_E       DS    A                                               
                          DC    CL16'CONFIRMED'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_E)                    
                          DC    X'00',AL3(NOTIFY_TYPE_E)                        
                          DC    X'80',AL3(RETURN_CODE_E)                        
*                                                                               
ATBPTR_STRUCTURE_E        DS    A                                               
                          DC    CL16'PREP_TO_RECEIVE'                           
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_E)                    
                          DC    X'00',AL3(PREP_TO_RECEIVE_TYPE_E)               
                          DC    X'00',AL3(LOCKS_E)                              
                          DC    X'00',AL3(NOTIFY_TYPE_E)                        
                          DC    X'80',AL3(RETURN_CODE_E)                        
*                                                                               
ATBRCVW_STRUCTURE_C       DS    A                                               
                          DC    CL16'RECEIVE_AND_WAIT'                          
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_C)                    
                          DC    X'00',AL3(FILL_C)                               
                          DC    X'00',AL3(RECEIVE_LENGTH_C)                     
                          DC    X'00',AL3(RECEIVE_ACCESS_TOKEN_C)               
                          DC    X'00',AL3(BUFFER_C)                             
                          DC    X'00',AL3(STATUS_RECEIVED_C)                    
                          DC    X'00',AL3(DATA_RECEIVED_C)                      
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED_C)           
                          DC    X'00',AL3(NOTIFY_TYPE_C)                        
                          DC    X'80',AL3(RETURN_CODE_C)                        
*                                                                               
ATBRCVW_STRUCTURE_E       DS    A                                               
                          DC    CL16'RECEIVE_AND_WAIT'                          
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_E)                    
                          DC    X'00',AL3(FILL_E)                               
                          DC    X'00',AL3(RECEIVE_LENGTH_E)                     
                          DC    X'00',AL3(RECEIVE_ACCESS_TOKEN_E)               
                          DC    X'00',AL3(BUFFER_E)                             
                          DC    X'00',AL3(STATUS_RECEIVED_E)                    
                          DC    X'00',AL3(DATA_RECEIVED_E)                      
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED_E)           
                          DC    X'00',AL3(NOTIFY_TYPE_E)                        
                          DC    X'80',AL3(RETURN_CODE_E)                        
*                                                                               
ATBSEND_STRUCTURE_C       DS    A                                               
                          DC    CL16'SEND_DATA'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_C)                    
                          DC    X'00',AL3(SEND_TYPE_C)                          
                          DC    X'00',AL3(SEND_LENGTH_C)                        
                          DC    X'00',AL3(SEND_ACCESS_TOKEN_C)                  
                          DC    X'00',AL3(BUFFER_C)                             
                          DC    X'00',AL3(REQUEST_TO_SEND_VALUE_C)              
                          DC    X'00',AL3(NOTIFY_TYPE_C)                        
                          DC    X'80',AL3(RETURN_CODE_C)                        
*                                                                               
ATBSEND_STRUCTURE_E       DS    A                                               
                          DC    CL16'SEND_DATA'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_E)                    
                          DC    X'00',AL3(SEND_TYPE_E)                          
                          DC    X'00',AL3(SEND_LENGTH_E)                        
                          DC    X'00',AL3(SEND_ACCESS_TOKEN_E)                  
                          DC    X'00',AL3(BUFFER_E)                             
                          DC    X'00',AL3(REQUEST_TO_SEND_VALUE_E)              
                          DC    X'00',AL3(NOTIFY_TYPE_E)                        
                          DC    X'80',AL3(RETURN_CODE_E)                        
*                                                                               
ATBEES3_STRUCTURE_C       DS    A                                               
                          DC    CL16'ERROR_EXTRACT'                             
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_C)                    
                          DC    X'00',AL3(SERVICE_NAME_C)                       
                          DC    X'00',AL3(SERVICE_REASON_CODE_C)                
                          DC    X'00',AL3(MESSAGE_TEXT_LENGTH_C)                
                          DC    X'00',AL3(MESSAGE_TEXT_C)                       
                        DC X'00',AL3(ERROR_LOG_PRODUCT_SET_ID_LENGTH_C)         
                          DC    X'00',AL3(ERROR_LOG_PRODUCT_SET_ID_C)           
                          DC  X'00',AL3(ERROR_LOG_INFORMATION_LENGTH_C)         
                          DC    X'00',AL3(ERROR_LOG_INFORMATION_C)              
                          DC    X'00',AL3(REASON_CODE_ERROR_EXTRACT_C)          
                          DC    X'80',AL3(RETURN_CODE_ERROR_EXTRACT_C)          
*                                                                               
ATBGTA2_STRUCTURE_C       DS    A                                               
                          DC    CL16'GET_ATTRIBUTES'                            
                          DC    X'C0'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_C)                    
                          DC    X'00',AL3(PARTNER_LU_NAME_C)                    
                          DC    X'00',AL3(MODE_NAME_C)                          
                          DC    X'00',AL3(SYNC_LEVEL_C)                         
                          DC    X'00',AL3(CONVERSATION_CORRELATOR_C)            
                          DC    X'00',AL3(LUW_ID_C)                             
                          DC    X'00',AL3(FULL)                                 
                          DC    X'00',AL3(TP_NAME_C)                            
                          DC    X'00',AL3(LOCAL_LU_NAME_C)                      
                          DC    X'00',AL3(CONVERSATION_TYPE_C)                  
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(CONVERSATION_STATE_C)                 
                          DC    X'80',AL3(RETURN_CODE_C)                        
*                                                                               
ATBGTA2_STRUCTURE_E       DS    A                                               
                          DC    CL16'GET_ATTRIBUTES'                            
                          DC    X'C0'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID_E)                    
                          DC    X'00',AL3(PARTNER_LU_NAME_E)                    
                          DC    X'00',AL3(MODE_NAME_E)                          
                          DC    X'00',AL3(SYNC_LEVEL_E)                         
                          DC    X'00',AL3(CONVERSATION_CORRELATOR_E)            
                          DC    X'00',AL3(LUW_ID_E)                             
                          DC    X'00',AL3(TP_NAME_LENGTH_E)                     
                          DC    X'00',AL3(TP_NAME_E)                            
                          DC    X'00',AL3(LOCAL_LU_NAME_E)                      
                          DC    X'00',AL3(CONVERSATION_TYPE_E)                  
                          DC    X'00',AL3(USER_ID_E)                            
                          DC    X'00',AL3(PROFILE_E)                            
                          DC    X'00',AL3(USER_TOKEN_E)                         
                          DC    X'00',AL3(CONVERSATION_STATE_E)                 
                          DC    X'80',AL3(RETURN_CODE_E)                        
*                                                                               
ATBGTRN_STRUCTURE_E       DS    A                                               
                          DC    CL16'GET_TRANSACTION'                           
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'80',AL3(RETURN_CODE_E)                        
*                                                                               
ATBRTRN_STRUCTURE_E       DS    A                                               
                          DC    CL16'RTN_TRANSACTION'                           
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'80',AL3(RETURN_CODE_E)                        
         EJECT                                                                  
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBALC2  DC    CL8'ATBALC2'                                                     
         DC    XL52'00'                                                         
ATBCFMD  DC    CL8'ATBCFMD'                                                     
         DC    XL52'00'                                                         
ATBDEAL  DC    CL8'ATBDEAL'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGETC  DC    CL8'ATBGETC'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBGTRN  DC    CL8'ATBGTRN'                                                     
         DC    XL52'00'                                                         
ATBPTR   DC    CL8'ATBPTR'                                                      
         DC    XL52'00'                                                         
ATBRCVW  DC    CL8'ATBRCVW'                                                     
         DC    XL52'00'                                                         
ATBRTRN  DC    CL8'ATBRTRN'                                                     
         DC    XL52'00'                                                         
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE REPORT BUFFER                    
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
* DDDPRINT                                                                      
* DMPRTQK                                                                       
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         IEFZB4D2                                                               
         EJECT                                                                  
         IHAPSA                                                                 
         EJECT                                                                  
         IKJTCB                                                                 
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022DDCOLXFER 05/01/02'                                      
         END                                                                    
