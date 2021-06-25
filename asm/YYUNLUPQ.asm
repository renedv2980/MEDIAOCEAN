*          DATA SET YYUNLUPQ   AT LEVEL 036 AS OF 05/01/02                      
*PHASE YYUNLPQA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        DDLU62PQ -- RECIEVE MESSAGES VIA LU6.2, WRITE TO PQ  *         
*                                                                     *         
*  COMMENTS:     APPC SCHEDULER PROGRAM, LAUNCHED BY TPNAME ALLOCATE  *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- S P A R E                                      *         
*                R6 -- S P A R E                                      *         
*                R7 -- S P A R E                                      *         
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
         TITLE 'DDLU62PQ -- WRITE INCOMING LU6.2 MESSAGES TO PQ'                
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
LU62PQ   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,LU62PQ,=A(R13CHAIN),R9,R8                                      
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(18),=C'LU6.2 PRINT QUEUE PROGRAM'                          
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         BAS   RE,INITIAL          INITIALIZE                                   
*                                                                               
GETTRANS LA    R2,ATBGTRN_STRUCTURE                                             
         BAS   RE,CALLAPPC         GET MULTI-TRANS TRANSACTION                  
*                                                                               
         CLC   RETURN_CODE,=F'8'   ANYTHING MORE TO DO?                         
         BE    GOODBYE                                                          
         CLC   RETURN_CODE,=F'28'                                               
         BE    GOODBYE             NO                                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,ATBGETC_STRUCTURE                                             
         BAS   RE,CALLAPPC         GET A CONVERSATION WITH KATZ                 
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,PRNTATTB         PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         TIME  BIN                                                              
         ST    R0,STRTTIME         REMEMBER TIME WE START RECEIVING             
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         MVI   TRCCALL,C'Y'        TRACE THIS CALL                              
         LA    R2,ATBRCVW_STRUCTURE                                             
         BAS   RE,CALLAPPC         RECEIVE AND WAIT                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   COMMSERR                                                         
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BE    *+6                                                              
         DC    H'0'                KATZ SHOULD HAVE DONE CONFIRM                
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         BAS   RE,CALLAPPC         ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   COMMSERR                                                         
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         MVI   TRCCALL,C'Y'        TRACE THIS CALL                              
         LA    R2,ATBRCVW_STRUCTURE                                             
         BAS   RE,CALLAPPC         RECEIVE AND WAIT                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   COMMSERR                                                         
*                                                                               
         L     R2,RECEIVE_LENGTH                                                
         GOTO1 =V(PRNTBL),DMCB,=C'HEADER RECORD RECEIVED',             +        
               BUFFER,C'DUMP',(R2),=C'1D'                                       
*                                                                               
         CLC   =C'VERSION=1,USERID=',BUFFER                                     
         BNE   BADHEAD             BAD START TO HEADER RECORD                   
         CLC   =C',SUBID=',BUFFER+27                                            
         BNE   BADHEAD                                                          
         CLC   =C',DESCRIPTION=',BUFFER+37                                      
         BNE   BADHEAD                                                          
*                                                                               
         MVC   USERID,BUFFER+17    USERID                                       
         MVC   SUBID,BUFFER+34     SUBID                                        
         MVC   DESCRIP,BUFFER+50   DESCRIPTION                                  
*                                                                               
         PRNT  ABOUT_TO_READ_USERID_CTFILE                                      
*                                                                               
         XC    KEY,KEY             BUILD USERID RECORD KEY                      
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),USERID                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,IO                
         CLI   8(R1),0                                                          
         BE    *+14                USERID RECORD WAS FOUND                      
         TM    8(R1),X'FF'-X'10'                                                
         BZ    BADHEAD             USERID RECORD NOT FOUND                      
         DC    H'0'                SOME OTHER DATAMGR ERROR                     
*                                                                               
         LA    R4,IO               USERID RECORD                                
         MVI   ELCODE,X'02'        PASSIVE POINTER ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                ELEMENT NOT PRESENT                          
         MVC   USERIDX,2(R4)       SAVE HEX USERID                              
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    RF,R                                                             
         USING PQPLD,RF                                                         
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSRCID,USERIDX                                                  
         MVC   QLSUBID,SUBID                                                    
         MVI   QLCLASS,C'G'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,160                                                      
         MVC   QLRETNL,=H'8'                                                    
         MVC   QLRETND,=H'1'                                                    
         MVC   QLDESC,DESCRIP                                                   
         DROP  RF                                                               
*                                                                               
         L     RE,=A(CIRECX)       CLEAR PQSAVE AREA (LAST 2048 BYTES)          
         LA    RF,2048             SET LENGTH TO CLEAR                          
         SR    RE,RF               SET 'TO' ADDRESS                             
         SR    R0,R0               SET FROM ADDRESS                             
         SR    R1,R1               SET FROM LEN                                 
         MVCL  RE,R0                                                            
*                                                                               
         PRNT  ABOUT_TO_OPEN_PQ_REPORT                                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,CIREC            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT OPEN PQ REPORT                     
*                                                                               
         PRNT  ABOUT_TO_OPEN_PQ_REPORT_DONE                                     
*                                                                               
         MVC   P+30(L'USERID),USERID                                            
         MVI   P+40,C','                                                        
         MVC   P+41(L'SUBID),SUBID                                              
         MVI   P+44,C','                                                        
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         EDIT  QLREPRNO,(5,P+45),ALIGN=LEFT                                     
         DROP  R2                                                               
         PRNT  CREATE_PQ_REPORT                                                 
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         BAS   RE,CALLAPPC         ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RCVRPT              RETURN CODE WAS OK                           
         B     COMMSERR                                                         
*                                                                               
BADHEAD  MVC   ERROR_DIRECTION,ATB_RECEIVE_ERROR                                
         LA    R2,ATBSERR_STRUCTURE                                             
         BAS   RE,CALLAPPC         ISSUE SEND_ERROR                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RCVDEAL             RETURN CODE WAS OK                           
         B     COMMSERR                                                         
*                                                                               
RCVRPT   PRNT  RECEIVE_REPORT                                                   
         SR    R3,R3               RESET REPORT LINE COUNTER                    
*                                                                               
RCVREC   MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         MVI   TRCCALL,C'N'        DON'T TRACE THIS CALL                        
         LA    R2,ATBRCVW_STRUCTURE                                             
         BAS   RE,CALLAPPC         RECEIVE AND WAIT                             
*                                                                               
         CLC   RETURN_CODE,ATB_DEALLOCATED_NORMAL                               
         BE    RETURN              RETURN TRANSACTION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   COMMSERR            RETURN CODE WAS OK                           
*                                                                               
         CLC   DATA_RECEIVED,ATB_NO_DATA_RECEIVED                               
         BE    BADRPT              SHOULD NOT GET EMPTY RECORDS                 
*                                                                               
         LA    R3,1(R3)            INCREMENT LINE COUNTER                       
*                                                                               
         CLI   TRACEFLG,C'Y'       PRODUCE FULL TRACE?                          
         BNE   CHKEJECT                                                         
         L     R2,RECEIVE_LENGTH                                                
         GOTO1 =V(PRNTBL),DMCB,=C'DATA RECORD RECEIVED',               +        
               BUFFER,C'DUMP',(R2),=C'1D'                                       
*                                                                               
CHKEJECT MVI   R,C' '                                                           
         MVC   R+1(L'R-1),R                                                     
         CLC   =C'+++EJECT',BUFFER KATZ WANTS A PAGE EJECT?                     
         BNE   *+12                                                             
         MVI   R,X'89'             EJECT PAGE                                   
         B     PUTLINE                                                          
*                                                                               
         L     R1,RECEIVE_LENGTH                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   R+1(0),BUFFER                                                    
         MVI   R,X'09'             SINGLE SPACE                                 
*                                                                               
PUTLINE  GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,CIREC            
         CLI   DMCB+8,0                                                         
         BNE   BADRPT              COULD NOT WRITE PQ LINE                      
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BNE   RCVREC              GET NEXT RECORD                              
*                                                                               
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,CIREC            
         CLI   DMCB+8,0                                                         
         BNE   BADRPT              COULD NOT CLOSE PQ REPORT                    
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         BAS   RE,CALLAPPC         ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RCVDEAL             RETURN CODE WAS OK                           
         B     COMMSERR                                                         
*                                                                               
BADRPT   MVC   ERROR_DIRECTION,ATB_RECEIVE_ERROR                                
         LA    R2,ATBSERR_STRUCTURE                                             
         BAS   RE,CALLAPPC         ISSUE SEND_ERROR                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   COMMSERR                                                         
*                                                                               
RCVDEAL  LA    R2,ATBRCVW_STRUCTURE                                             
         MVI   TRCCALL,C'Y'        TRACE THIS CALL                              
         BAS   RE,CALLAPPC         RECEIVE AND WAIT                             
*                                                                               
         CLC   RETURN_CODE,ATB_DEALLOCATED_ABEND                                
         BE    RETURN              RETURN TRANSACTION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   COMMSERR                                                         
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_DEALLOC_RECEIVED                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         BAS   RE,CALLAPPC         ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   COMMSERR                                                         
*                                                                               
RETURN   EDIT  (R3),(6,RATEMSG)    NUMBER OF RECIEVED LINES                     
         TIME  BIN                                                              
         S     R0,STRTTIME         COMPUTE ELAPSED TIME                         
         BNM   *+8                                                              
         A     R0,=A(24*3600*100)  ADD 24HRS, JUST WENT PAST MIDNIGHT           
*                                                                               
         SRDL  R0,32               CONVERT SEC*100 TO SEC                       
         D     R0,=F'100'          LEAVES SECONDS IN R1                         
         LTR   R1,R1               IF R1 = 0, THEN IT TOOK LT 1 SECOND          
         BNZ   *+8                                                              
         LA    R1,1                ROUND UP TO 1 SECOND                         
         LR    RF,R1               SAVE ELAPSED TIME IN SECONDS                 
         SR    R0,R0               PREPARE FOR DIVIDE                           
         D     R0,=F'3600'         GIVES HOURS IN R1                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RATEMSG+16(2),DUB                                                
         MVI   RATEMSG+18,C':'                                                  
         SRDL  R0,32                                                            
         D     R0,=F'60'           GIVES MINUTES IN R1/SECS IN R0               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RATEMSG+19(2),DUB                                                
         MVI   RATEMSG+21,C':'                                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RATEMSG+22(2),DUB                                                
*                                                                               
         SR    R2,R2               PREPARE FOR DIVIDE                           
         DR    R2,RF               R3 = LINES PER SECOND                        
         EDIT  (R3),(6,RATEMSG+33)                                              
         MVC   P+30(49),RATEMSG                                                 
         PRNT  RATE_STATS                                                       
*                                                                               
         LA    R2,ATBRTRN_STRUCTURE                                             
         BAS   RE,CALLAPPC         RETURN MULTI-TRANS TRANSACTION               
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     GETTRANS                                                         
*                                                                               
GOODBYE  PRNT  *****************                                                
         PRNT  *****************                                                
         PRNT  *****EXITING*****                                                
         PRNT  *****************                                                
         PRNT  *****************                                                
*                                                                               
         XBASE                                                                  
         SPACE 3                                                                
COMMSERR GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'KATZ PRTQ TRANSFER COMMUNI+        
               CATIONS ERROR'                                                   
         MVC   WORK(55),=C'LOCAL LUNAME XXXXXXXX, PARTNER LUNAME XXXXXX+        
               XXXXXXXXXXX'                                                     
         MVC   WORK+13(8),LOCAL_LU_NAME                                         
         MVC   WORK+38(17),PARTNER_LU_NAME                                      
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(55,WORK)                             
*                                                                               
         ABEND 710,DUMP                                                         
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
         BLDL  0,ENTRYPTS          BUILD LIST OF APPC/MVS ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
         LOAD  DE=ATBCFMD                                                       
         ST    R0,ATBCFMD_STRUCTURE                                             
         LOAD  DE=ATBGETC                                                       
         ST    R0,ATBGETC_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE                                             
         LOAD  DE=ATBGTRN                                                       
         ST    R0,ATBGTRN_STRUCTURE                                             
         LOAD  DE=ATBRTRN                                                       
         ST    R0,ATBRTRN_STRUCTURE                                             
         LOAD  DE=ATBSERR                                                       
         ST    R0,ATBSERR_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IO,0                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
READCRDS NTR1                                                                   
*                                                                               
* READ PARAMETER CARDS                                                          
*                                                                               
         OPEN  (MYSYSIN)           OPEN PARAMETER CARD FILE                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RC10     GET   MYSYSIN,CARD        READ A PARAMETER CARD                        
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC10                YES                                          
*                                                                               
         CLC   =C'TRACE=',CARD     TRACE CARD?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'NO',CARD+6                                                    
         BE    RC10                TRACE=NO IS THE DEFAULT                      
         CLC   =C'YES',CARD+6                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC10                                                             
*                                                                               
RCX      CLOSE (MYSYSIN)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
PRNTATTB NTR1                                                                   
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************                                                    
         PRNT  GETATTRIBUTES                                                    
         PRNT  *************                                                    
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
         BAS   RE,CALLAPPC         GET CONVERSATION ATTRIBUTES                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                   
         PRNT  CONVERSATION_ID                                                  
         MVC   P+30(17),PARTNER_LU_NAME                                         
         PRNT  PARTNER_LU_NAME                                                  
         MVC   P+30(8),MODE_NAME                                                
         PRNT  MODE_NAME                                                        
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    *+22                                                             
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    *+6                                                              
         DC    H'0'                UNKNOWN SYNC LEVEL                           
         PRNT  SYNC_LEVEL                                                       
         EDIT  TP_NAME_LENGTH,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         PRNT  TP_NAME_LENGTH                                                   
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TP_NAME                                                          
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LOCAL_LU_NAME                                                    
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    *+22                                                             
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    *+6                                                              
         DC    H'0'                UNKNOWN CONVERSATION TYPE                    
         PRNT  CONVERSATION_TYPE                                                
         MVC   P+30(10),USER_ID                                                 
         PRNT  USER_ID                                                          
         MVC   P+30(10),PROFILE                                                 
         PRNT  PROFILE                                                          
         MVC   P+30(80),USER_TOKEN                                              
         PRNT  USER_TOKEN                                                       
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
PRATTB10 PRNT  CONVERSATION_STATE                                               
*                                                                               
         PRNT  *************                                                    
         PRNT  *************                                                    
*                                                                               
         B     XIT                                                              
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
         TM    20(R2),X'20'        TRACE SUPPRESSED BY DEFAULT?                 
         BZ    *+12                NO -- TRACE IT                               
         CLI   TRCCALL,C'Y'        IS SUPPRESSION OVERRIDDEN?                   
         BNE   CALL5               NO                                           
         MVC   P(16),=C'ABOUT TO PERFORM'                                       
         MVC   P+30(16),4(R2)      PRINT ROUTINE NAME                           
         PRNT                                                                   
*                                                                               
CALL5    LA    RE,*+10                                                          
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
         BO    CALL20              YES, SO WE'RE DONE                           
*                                                                               
         CLC   RETURN_CODE,ATB_OK                                               
         BE    CALL10              APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  **BAD_APPC_CALL**                                                
         B     CALL30              APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALL10   WAIT  ECB=APPCECB         WAIT FOR COMPLETION                          
*                                                                               
         TM    APPCECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPCECB                                              
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPCECB,APPCECB                                                  
*                                                                               
CALL20   OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   CALL25              ON ERROR, TRACE UNCONDITIONALLY              
         TM    20(R2),X'20'        TRACE SUPPRESSED BY DEFAULT?                 
         BZ    CALL25              NO -- TRACE IT                               
         CLI   TRCCALL,C'Y'        IS SUPPRESSION OVERRIDDEN?                   
         BNE   CALL30              NO                                           
*                                                                               
CALL25   MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
CALL30   OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CALLX                                                            
         TM    20(R2),X'40'        CAN CALL ERROR_EXTRACT NOW?                  
         BZ    CALLX                                                            
         BAS   RE,APPCERR          YES                                          
*                                                                               
CALLX    B     XIT                                                              
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
APPCEX   B     XIT                                                              
         EJECT                                                                  
         GETEL R4,28,ELCODE                                                     
         SPACE 2                                                                
         ATBSERV                                                                
         SPACE 2                                                                
MYSYSIN  DCB   DDNAME=MYSYSIN,MACRF=GM,DSORG=PS,RECFM=FB,LRECL=80,     +        
               EODAD=RCX                                                        
         SPACE 2                                                                
RATEMSG  DC    C'NNNNNN LINES IN HH:MM:SS, RATE = NNNNNN LINES/SEC'             
         SPACE 2                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
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
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         SPACE 2                                                                
* APPC/MVS CALL PARAMETERS                                                      
*                                                                               
CONVERSATION_TYPE              DS    F                                          
PARTNER_LU_NAME                DC    CL17' '                                    
LOCAL_LU_NAME                  DC    CL8' '                                     
MODE_NAME                      DC    CL8' '                                     
TP_NAME_LENGTH                 DS    F                                          
TP_NAME                        DC    CL64' '                                    
RETURN_CONTROL                 DS    F                                          
SYNC_LEVEL                     DS    F                                          
CONVERSATION_CORRELATOR        DS    CL8                                        
USER_ID                        DC    CL10' '                                    
PASSWORD                       DC    CL10' '                                    
PROFILE                        DC    CL10' '                                    
USER_TOKEN                     DC    XL80'00'                                   
CONVERSATION_ID                DS    CL8                                        
LUW_ID                         DS    XL26                                       
CONVERSATION_STATE             DS    F                                          
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY TYPE         
RETURN_CODE                    DS    F                                          
FILL                           DS    F                                          
ERROR_DIRECTION                DS    F                                          
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DC    F'0'                                       
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
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
         LTORG                                                                  
         EJECT                                                                  
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                                                                
PRNTTIME DS    CL9                                                              
STRTTIME DS    F                   TIME WE STARTED RECEIVING                    
TRACEFLG DC    C'N'                FULL TRACE?                                  
TRCCALL  DS    C                   APPC CALL TRACE OVERRIDE                     
ELCODE   DS    X                                                                
WORK     DS    CL256                                                            
CARD     DS    CL80                FOR PARAMETER CARDS                          
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS WITH KATZ             
USERID   DS    CL10                PQ USERID (EBCDIC)                           
USERIDX  DS    XL2                 PQ USERID (BINARY)                           
SUBID    DS    CL3                 PQ SUBID                                     
DESCRIP  DS    CL11                PQ DESCRIPTION                               
KEY      DS    XL25                CTFILE KEY                                   
PQINDEX  DS    XL40                CURRENT REPORT KEY                           
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
*       X'20': DON'T TRACE THIS CALL BY DEFAULT                                 
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
ATBGETC_STRUCTURE         DS    A                                               
                          DC    CL16'GET_CONVERSATION'                          
                          DC    X'C0'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(CONVERSATION_CORRELATOR)              
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBCFMD_STRUCTURE         DS    A                                               
                          DC    CL16'CONFIRMED'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRCVW_STRUCTURE         DS    A                                               
                          DC    CL16'RECEIVE_AND_WAIT'                          
                          DC    X'60'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(FILL)                                 
                          DC    X'00',AL3(RECEIVE_LENGTH)                       
                          DC    X'00',AL3(RECEIVE_ACCESS_TOKEN)                 
                          DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(STATUS_RECEIVED)                      
                          DC    X'00',AL3(DATA_RECEIVED)                        
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED)             
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
                          DC    X'00',AL3(TP_NAME_LENGTH)                       
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(USER_ID)                              
                          DC    X'00',AL3(PROFILE)                              
                          DC    X'00',AL3(USER_TOKEN)                           
                          DC    X'00',AL3(CONVERSATION_STATE)                   
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBSERR_STRUCTURE         DS    A                                               
                          DC    CL16'SEND_ERROR'                                
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED)             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(ERROR_DIRECTION)                      
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBGTRN_STRUCTURE         DS    A                                               
                          DC    CL16'GET_TRANSACTION'                           
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRTRN_STRUCTURE         DS    A                                               
                          DC    CL16'RTN_TRANSACTION'                           
                          DC    X'80'                                           
                          DS    XL3                                             
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
ATBCFMD  DC    CL8'ATBCFMD'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGETC  DC    CL8'ATBGETC'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBGTRN  DC    CL8'ATBGTRN'                                                     
         DC    XL52'00'                                                         
ATBRCVW  DC    CL8'ATBRCVW'                                                     
         DC    XL52'00'                                                         
ATBRTRN  DC    CL8'ATBRTRN'                                                     
         DC    XL52'00'                                                         
ATBSERR  DC    CL8'ATBSERR'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'**I/O***'                                                      
IO       DC    1024X'00'           CTFILE RECORD BUFFER                         
*                                                                               
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE REPORT BUFFER                    
CIRECX   EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
* DDDPRINT                                                                      
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
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
**PAN#1  DC    CL21'036YYUNLUPQ  05/01/02'                                      
         END                                                                    
