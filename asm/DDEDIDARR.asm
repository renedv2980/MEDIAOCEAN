*          DATA SET DDEDIDARR  AT LEVEL 039 AS OF 08/08/13                      
*PHASE EDIDARRA                                                                 
*&&      SET   DB=Y                                                             
*INCLUDE BINSR31                                                                
*INCLUDE CALLOFF                                                                
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE REPPNTAB                                                               
*INCLUDE GETPROF                                                                
*INCLUDE MQRPT                                                                  
*                                                                               
         TITLE 'DDEDIDARR -- RECEIVE DARE TRANSMISSIONS VIA APPC/MVS'           
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIDARR -- RECEIVE DARE TRANSMISSIONS VIA APPC/MVS   *         
*                                                                     *         
*  CALLED BY:    DDEDIDARS (EDICT SENDER TO DARE)                     *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- PARAMETERS FROM EDIDARS (VIA R1)               *         
*                R7 -- S P A R E                                      *         
*                R8 -- COMMON STORAGE AREA (2ND BASE)                 *         
*                R9 -- S P A R E                                      *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
                                                                                
***********************************************************************         
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
***********************************************************************         
                                                                                
EDIDARR  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDIDARR*,=A(R13CHAIN)                                          
*                                                                               
         LR    R6,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R8,RC                                                            
         AHI   R8,4096                                                          
         USING COMMWORK,RC,R8                                                   
*                                                                               
         AHI   R6,-4                                                            
         L     R6,0(R6)                                                         
         L     R6,0(R6)            A(R1 PARAMETERS FROM ATTACH)                 
         USING DRRPARMD,R6                                                      
*                                                                               
         MVC   DATAMGR,DRRADMGR    A(DATAMGR)                                   
         MVC   TRACEFLG,DRRTRACE   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'DRRTRACE'  DDNAME=DRRTRACE              
*                                                                               
         MVC   TITLE(34),=C'EDICT: RECEIVING SUBTASK FROM DARE'                 
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         GOTO1 =A(INITIAL),(RC)    INITIALIZE                                   
*                                                                               
         L     RF,DRRQMGNM         MQSERIES QUEUE MANAGER NAME                  
         MVC   QMGRNAME,0(RF)                                                   
*                                                                               
         MVC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         L     RF,DRRDLUID         DARE LUID                                    
         MVC   PARTNER_LU_NAME(8),0(RF)                                         
         L     RF,DRRVMODE         VTAM MODE                                    
         MVC   MODE_NAME,0(RF)                                                  
         MVC   TP_NAME_LENGTH,=F'8'                                             
         L     RF,DRRTPNAM         TRANSACTION PROGRAM NAME                     
         MVC   TP_NAME(7),0(RF)                                                 
         MVI   TP_NAME+7,C'S'      TARGET THE SENDER WITH SUFFIX 'S'            
         MVC   RETURN_CONTROL,ATB_WHEN_SESSION_ALLOCATED                        
         MVC   SYNC_LEVEL,ATB_CONFIRM                                           
         MVC   SECURITY_TYPE,ATB_SECURITY_NONE                                  
         L     RF,DRRUSRID         APPC USER_ID (FOR SECURITY)                  
         MVC   USER_ID,0(RF)                                                    
         L     RF,DRRPASWD         APPC USER_ID (FOR SECURITY)                  
         MVC   PASSWORD,0(RF)                                                   
         CLC   USER_ID,=CL10' '    WAS A USERID PROVIDED?                       
         BE    *+10                NO, SO NO SECURITY                           
         MVC   SECURITY_TYPE,ATB_SECURITY_PROGRAM                               
*                                                                               
         LA    R2,ATBALC2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ALLOCATE THE CONVERSATION                    
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    DRALLCOK            ALLOCATE WAS OK                              
*                                                                               
         CLC   RETURN_CODE,ATB_PARAMETER_ERROR                                  
         BNE   CANTALLC            CAN'T ALLOCATE                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *DARE OPEN PARAMET+        
               ER ERROR. PROBABLE CAUSE: UNDEFINED LOCAL LUNAME'                
         ABEND 705,DUMP                                                         
*                                                                               
CANTALLC PRNT  ALLOCATEFAILED,PRINT=ALWAYS                                      
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *DARE OPEN/SEND FA+        
               ILED'                                                            
         ABEND 701,DUMP                                                         
*                                                                               
DRALLCOK PRNT  OPENDARERCVR,PRINT=ALWAYS                                        
*                                                                               
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         XC    BUFFER,BUFFER                                                    
         LHI   R3,APPCM1LQ                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM1    INITIAL SIGNON                               
         L     RF,DRRMAJNM         A(MAJOR RESOURCE NAME)                       
         MVC   BUFFER+17(1),5(RF)  'A' FOR EDICTA, ETC.                         
         GOTO1 =A(DATETIME),(RC)   PUT DATE/TIME INTO MESSAGE                   
         MVC   BUFFER+47(8),YYYYMMDD                                            
         MVC   BUFFER+61(6),HHMMSS                                              
         LA    R3,1(R3)            R3 = L'DATA                                  
         LR    RF,R3                                                            
         LA    RF,2(RF)            L'DATA + 2 BYTES FOR CRLF                    
         ST    RF,SEND_LENGTH                                                   
         LR    R2,RF               FOR PRNTBL                                   
         LA    RF,BUFFER(R3)                                                    
         MVC   0(2,RF),CRLF        TERMINATE RECORD WITH CRLF                   
         GOTO1 =V(PRNTBL),DMCB,=C'BUFFER DATA',BUFFER,C'DUMP',         +        
               (R2),=C'1D'                                                      
*                                                                               
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         LA    R2,ATBSEND_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   SEND DATA                                    
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         MVC   PREP_TO_RECEIVE_TYPE,ATB_PREP_TO_RECEIVE_FLUSH                   
         LA    R2,ATBPTR_STRUCTURE                                              
         GOTO1 =A(CALLAPPC),(RC)   PREPARE TO RECEIVE                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         L     R2,RECEIVE_LENGTH                                                
         GOTO1 =V(PRNTBL),DMCB,=C'HANDSHAKE RECEIVED',BUFFER,          +        
               C'DUMP',(R2),=C'1D'                                              
*                                                                               
         CLC   APPCM2(APPCM2LQ),BUFFER                                          
         BE    *+6                                                              
         DC    H'0'                DARE DID NOT RESPOND READY                   
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BE    *+6                                                              
         DC    H'0'                I EXPECT A CONFIRMATION REQUEST              
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRMATION                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         PRNT  FIRSTRECRECEIVED,PRINT=ALWAYS                                    
*                                                                               
NEXTMSG  GOTO1 =A(DRLINRCV),(RC)   FILL UP AN INPUT BUFFER                      
         BE    CHKSTOP                                                          
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
CHKSTOP  CLI   OPERSTOP,C'Y'       DO WE STOP NOW?                              
         BE    GOODBYE             YES                                          
*                                                                               
         LA    R3,DAREBUF                                                       
         CLC   =C'+++DARE SEND ',0(R3)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CRLF,24(R3)                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,26(R3)           BUMP PAST 'DARE SEND' RECORD                 
         MVC   RECCOUNT,=F'1'      ONE RECORD RECEIVED SO FAR                   
*                                                                               
         XC    EDICTDA,EDICTDA                                                  
         CLC   =C'+++DARE LOGGING=',0(R3)                                       
         BNE   CHKAGY              NO 'DARE LOGGING' RECORD                     
         CLI   16(R3),C'3'         VERSION THREE?                               
         BNE   BUMPLOG             OBSOLETE -- IGNORE THESE                     
*                                                                               
         CLC   25(1,R3),DRRLOGID   'A' FOR EDICTA, ETC.                         
         BNE   BUMPLOG             NOT FOR THIS INSTANCE OF EDICT               
*                                                                               
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,DATAMGR    A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKO   CONVERT REFERENCE NUMBER TO DSKADDR          
         LA    RE,17(R3)                                                        
         ST    RE,CONVOFF          A(CUSTOMER ID)                               
         GOTO1 =V(CONVDSKA)                                                     
         MVC   FULL,CONVDSK                                                     
         DROP  R1                                                               
         GOTO1 =V(HEXOUT),DMCB,FULL,EDICTDA,4,=C'TOG'                           
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BUMPLOG  LA    R3,1(R3)            BUMP PAST LOGGING INFO                       
         CLC   CRLF,0(R3)                                                       
         BNE   *-10                                                             
         LA    R3,2(R3)            BUMP PAST CRLF                               
         L     RE,RECCOUNT                                                      
         LA    RE,1(RE)                                                         
         ST    RE,RECCOUNT         ANOTHER RECORD RECEIVED                      
*                                                                               
CHKAGY   XC    USERID,USERID                                                    
*                                                                               
* SAVE TRANSMISSION TYPE FOR DAREMAIL PROCESSING                                
*                                                                               
         MVC   RMTYPE,0(R3)                                                     
*                                                                               
         CLC   =C'AGYHDR',0(R3)    IS THIS AN ORDER?                            
*        BE    *+14                                                             
*        CLC   =C'VARHDR',0(R3)            OR A VAR ORDER?                      
         BNE   CHKDLN                                                           
*                                                                               
* SAVE STATION/AGENCY/TIME CODES FOR REPMAIL PROCESSING                         
*                                                                               
* CHECK IF REVISION                                                             
         LA    RE,6(R3)                                                         
         AHI   RE,1                                                             
         CLC   CRLF,0(RE)          FIND END OF LINE                             
         BNE   *-10                                                             
         SR    RE,R3                                                            
         CHI   RE,PAHDREVN-PAGYHDRD+3                                           
         BL    CHKAGY10            LINE TOO SHORT, NO REV# PRESENT              
*                                                                               
         CLC   PAHDREVN-PAGYHDRD(3,R3),=C'001'                                  
         BL    CHKAGY10                                                         
         CLC   PAHDREVN-PAGYHDRD(3,R3),=C'999'                                  
         BH    CHKAGY10                                                         
         MVC   RMTYPE,=C'REVHDR'                                                
*                                                                               
CHKAGY10 DS    0H                                                               
         MVC   RMSTA,PAHDQSTA-PAGYHDRD(R3)                                      
         MVC   RMAGY,PAHDROUT-PAGYHDRD(R3)                                      
         MVC   RMTIME,PAHDTIME-PAGYHDRD(R3)                                     
*                                                                               
         MVC   DAREREC,0(R3)       SAVE AGYHDR HDR FOR EAGYHDR                  
         GOTO1 =A(AGYHDR),(RC)     YES -- WRITE IT TO PRINT QUEUE               
         BRAS  RE,EAGYHDR          ALSO WRITE IT TO EDICT FILE W/PQKEY          
         GOTO1 =A(REPMAIL),(RC)    SETUP DARE MAIL INFO FOR REPS                
         B     CHKCONF             CHECK FOR CONFIRMATION                       
*                                                                               
CHKDLN   CLC   =C'DLNNOT',0(R3)    IS THIS A DELIVERY NOTIFICATION?             
         BNE   CHKERR              NO                                           
         USING RDLNNOTD,R3                                                      
         MVC   USERID,RDNTTOID                                                  
         MVC   RECEIVER(10),RDNTFRID  THE MESSAGE WENT HERE                     
         GOTO1 =A(DLNNOT),(RC)     WRITE IT TO EDICT FILE                       
         BNE   CHKCONF             PROBLEM FOUND: IGNORE MESSAGE                
         MVI   BYTE,C'D'                                                        
         GOTO1 =A(POSTSTAT),(RC)   MARK ORIGINAL EDICT ENTRY AS DLVD            
*                                                                               
         CLI   DRREDTYP,C'R'       NO DLNNOT DAREMAIL FOR REPPAK                
         BE    CHKCONF                                                          
*                                                                               
         CLC   =C'DLNNOT',DAREREC                                               
         BNE   CHKCONF             MESSAGE IS BAD: BAIL OUT NOW                 
         LA    R3,DAREREC                                                       
         MVC   MAILTIME,RDNTTTIM   TIME                                         
         LA    RF,RDNTRTRN         FOR BUYER CODE                               
         MVC   MAILBYER,RTNBUYER-RTN2SNDR(RF)                                   
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     CHKCONF             CHECK FOR CONFIRMATION                       
         DROP  R3                                                               
*                                                                               
CHKERR   CLC   =C'ERRNOT',0(R3)    IS THIS AN ERROR NOTIFICATION?               
         BNE   CHKOAP              NO                                           
         USING RDLNNOTD,R3                                                      
         MVC   USERID,RDNTTOID                                                  
         MVC   RECEIVER(10),RDNTFRID  THE MESSAGE WENT HERE                     
         GOTO1 =A(ERRNOT),(RC)     WRITE IT TO EDICT FILE                       
         MVI   BYTE,C'C'                                                        
         GOTO1 =A(POSTSTAT),(RC)   MARK ORIGINAL EDICT ENTRY AS BAD             
*                                                                               
         CLI   DRREDTYP,C'R'       NO ERRNOT DAREMAIL FOR REPPAK                
         BE    CHKCONF                                                          
*                                                                               
         CLC   =C'ERRNOT',DAREREC                                               
         BNE   CHKCONF             MESSAGE IS BAD: BAIL OUT NOW                 
         LA    R3,DAREREC                                                       
         MVC   MAILTIME,RDNTTTIM   TIME                                         
         LA    RF,RDNTRTRN         FOR BUYER CODE                               
         MVC   MAILBYER,RTNBUYER-RTN2SNDR(RF)                                   
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     CHKCONF             CHECK FOR CONFIRMATION                       
         DROP  R3                                                               
*                                                                               
CHKOAP   CLC   =C'ORDAPP',0(R3)    IS THIS AN ORDER APPROVAL?                   
         BNE   CHKORJ                                                           
         USING RORDAPPD,R3                                                      
         MVC   USERID,ROAPTOID                                                  
         GOTO1 =A(ORDAPP),(RC)     YES -- WRITE IT TO EDICT FILE                
         CLC   =C'ORDAPP',DAREREC                                               
         BNE   CHKCONF             MESSAGE IS BAD: BAIL OUT NOW                 
         LA    R3,DAREREC                                                       
         MVC   MAILTIME,ROAPTIME   TIME                                         
         LA    RF,ROAPRTRN         FOR BUYER CODE                               
         MVC   MAILBYER,RTNBUYER-RTN2SNDR(RF)                                   
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     CHKCONF             CHECK FOR CONFIRMATION                       
         DROP  R3                                                               
*                                                                               
CHKORJ   CLC   =C'ORDREJ',0(R3)    IS THIS AN ORDER REJECT?                     
         BNE   CHKOCM                                                           
         USING RORDREJD,R3                                                      
         MVC   USERID,RORJTOID                                                  
         GOTO1 =A(ORDREJ),(RC)     YES -- WRITE IT TO EDICT FILE                
         BNE   CHKCONF             PROBLEM FOUND: IGNORE MESSAGE                
         CLC   =C'ORDREJ',DAREREC                                               
         BNE   CHKCONF             MESSAGE IS BAD: BAIL OUT NOW                 
         LA    R3,DAREREC                                                       
         MVC   MAILTIME,RORJTIME   TIME                                         
         LA    RF,RORJRTRN         FOR BUYER CODE                               
         MVC   MAILBYER,RTNBUYER-RTN2SNDR(RF)                                   
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     CHKCONF             CHECK FOR CONFIRMATION                       
         DROP  R3                                                               
*                                                                               
CHKOCM   CLC   =C'ORDCFM',0(R3)    IS THIS AN ORDER CONFIRM?                    
         BNE   CHKORC                                                           
         USING RORDCFMD,R3                                                      
         MVC   USERID,ROCFTOID                                                  
         GOTO1 =A(ORDCFM),(RC)     YES -- WRITE IT TO EDICT FILE                
         BNE   CHKCONF             PROBLEM FOUND: IGNORE MESSAGE                
         CLC   =C'ORDCFM',DAREREC                                               
         BNE   CHKCONF             MESSAGE IS BAD: BAIL OUT NOW                 
         LA    R3,DAREREC                                                       
         MVC   MAILTIME,ROCFTIME   TIME                                         
         LA    RF,ROCFRTRN         FOR BUYER CODE                               
         MVC   MAILBYER,RTNBUYER-RTN2SNDR(RF)                                   
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     CHKCONF             CHECK FOR CONFIRMATION                       
         DROP  R3                                                               
*                                                                               
CHKORC   CLC   =C'ORDRCL',0(R3)    IS THIS AN ORDER RECALL?                     
         BNE   CHKARC                                                           
         USING RORDRCLD,R3                                                      
         MVC   USERID,RORCTOID                                                  
         GOTO1 =A(ORDRCL),(RC)     YES -- WRITE IT TO EDICT FILE                
         BNE   CHKCONF             PROBLEM FOUND: IGNORE MESSAGE                
         CLC   =C'ORDRCL',DAREREC                                               
         BNE   CHKCONF             MESSAGE IS BAD: BAIL OUT NOW                 
         LA    R3,DAREREC                                                       
         MVC   MAILTIME,RORCTIME   TIME                                         
         LA    RF,RORCRTRN         FOR BUYER CODE                               
         MVC   MAILBYER,RTNBUYER-RTN2SNDR(RF)                                   
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     CHKCONF             CHECK FOR CONFIRMATION                       
         DROP  R3                                                               
*                                                                               
CHKARC   CLC   =C'AGYRCL',0(R3)    IS THIS AN AGENCY RECALL?                    
         BNE   CHKACN                                                           
*                                                                               
* SAVE TYPE/STATION/AGENCY/TIME CODES FOR REPMAIL PROCESSING                    
*                                                                               
         MVC   RMTYPE,0(R3)                                                     
         MVC   RMSTA,PARCQSTA-PAGYRCLD(R3)                                      
         MVC   RMAGY,PARCROUT-PAGYRCLD(R3)                                      
         MVC   RMTIME,PARCTIME-PAGYRCLD(R3)                                     
*                                                                               
         GOTO1 =A(AGYRCL),(RC)     YES -- WRITE IT TO EDICT FILE                
         GOTO1 =A(REPMAIL),(RC)    SETUP DARE MAIL INFO FOR REPS                
         B     CHKCONF             CHECK FOR CONFIRMATION                       
*                                                                               
CHKACN   CLC   =C'AGYCAN',0(R3)    IS THIS AN AGENCY CANCELLATION?              
         BNE   CHKMKG                                                           
*                                                                               
* SAVE TYPE/STATION/AGENCY/TIME CODES FOR REPMAIL PROCESSING                    
*                                                                               
         MVC   RMTYPE,0(R3)                                                     
         MVC   RMSTA,PACNQSTA-PAGYCAND(R3)                                      
         MVC   RMAGY,PACNROUT-PAGYCAND(R3)                                      
         MVC   RMTIME,PACNTIME-PAGYCAND(R3)                                     
*                                                                               
         MVC   DAREREC,0(R3)       SAVE AGYCAN HDR FOR EAGYCAN                  
         GOTO1 =A(AGYCAN),(RC)     YES -- WRITE IT TO PRINT QUEUE               
         BRAS  RE,EAGYCAN          ALSO WRITE IT TO EDICT FILE W/PQKEY          
         GOTO1 =A(REPMAIL),(RC)    SETUP DARE MAIL INFO FOR REPS                
         B     CHKCONF             CHECK FOR CONFIRMATION                       
*                                                                               
CHKMKG   CLC   =C'MKGHDR',0(R3)    IS THIS A MAKEGOOD OFFER?                    
         BNE   CHKMAP                                                           
         USING MOFRHDRD,R3                                                      
         MVC   MAILTIME,MOHDTIME   GET MAIL INFO BEFORE R3 CHANGES              
         LA    RF,MOHDRTNS         FOR BUYER CODE                               
         MVC   MAILBYER,RTNBUYER-RTN2SNDR(RF)                                   
*                                                                               
         MVC   DAREREC,0(R3)       SAVE MAKEGOOD HDR FOR EMKGHDR                
         GOTO1 =A(MKGHDR),(RC)     YES -- WRITE IT TO PRINT QUEUE               
         BRAS  RE,EMKGHDR          ALSO WRITE IT TO EDICT FILE W/PQKEY          
*                                                                               
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     CHKCONF             CHECK FOR CONFIRMATION                       
         DROP  R3                                                               
*                                                                               
CHKMAP   CLC   =C'MKGAPP',0(R3)    IS THIS A MAKEGOOD APPROVAL?                 
         BNE   CHKMRJ                                                           
*                                                                               
* SAVE TYPE/STATION/AGENCY/TIME CODES FOR REPMAIL PROCESSING                    
*                                                                               
         MVC   RMTYPE,0(R3)                                                     
         MVC   RMSTA,MOAPQSTA-MOFRAPPD(R3)                                      
         MVC   RMAGY,MOAPROUT-MOFRAPPD(R3)                                      
         MVC   RMTIME,MOAPTIME-MOFRAPPD(R3)                                     
*                                                                               
         MVC   DAREREC,0(R3)       SAVE MKGAPP HDR FOR EMKGAPP                  
         GOTO1 =A(MKGAPP),(RC)     YES -- WRITE IT TO PRINT QUEUE               
         BRAS  RE,EMKGAPP          ALSO WRITE IT TO EDICT FILE W/PQKEY          
         GOTO1 =A(REPMAIL),(RC)    SETUP DARE MAIL INFO FOR REPS                
*                                                                               
         B     CHKCONF             CHECK FOR CONFIRMATION                       
*                                                                               
CHKMRJ   CLC   =C'MKGREJ',0(R3)    IS THIS A MAKEGOOD REJECTION?                
         BNE   CHKMOK                                                           
*                                                                               
* SAVE TYPE/STATION/AGENCY/TIME CODES FOR REPMAIL PROCESSING                    
*                                                                               
         MVC   RMTYPE,0(R3)                                                     
         MVC   RMSTA,MORJQSTA-MOFRREJD(R3)                                      
         MVC   RMAGY,MORJROUT-MOFRREJD(R3)                                      
         MVC   RMTIME,MORJTIME-MOFRREJD(R3)                                     
*                                                                               
         MVC   DAREREC,0(R3)       SAVE MKGREJ HDR FOR EMKGREJ                  
         GOTO1 =A(MKGREJ),(RC)     YES -- WRITE IT TO PRINT QUEUE               
         BRAS  RE,EMKGREJ          ALSO WRITE IT TO EDICT FILE W/PQKEY          
         GOTO1 =A(REPMAIL),(RC)    SETUP DARE MAIL INFO FOR REPS                
*                                                                               
         B     CHKCONF             CHECK FOR CONFIRMATION                       
*                                                                               
CHKMOK   CLC   =C'MKGROK',0(R3)    IS THIS A MAKEGOOD OK?                       
         BNE   CHKMCN                                                           
         USING MOFRCFMD,R3                                                      
         MVC   USERID,MOCFTOID                                                  
         GOTO1 =A(MKGROK),(RC)     YES -- WRITE IT TO EDICT FILE                
         BNE   CHKCONF             PROBLEM FOUND: IGNORE MESSAGE                
         CLC   =C'MKGROK',DAREREC                                               
         BNE   CHKCONF             MESSAGE IS BAD: BAIL OUT NOW                 
         LA    R3,DAREREC                                                       
         MVC   MAILTIME,MOCFTIME   TIME                                         
         LA    RF,MOCFRTNS         FOR BUYER CODE                               
         MVC   MAILBYER,RTNBUYER-RTN2SNDR(RF)                                   
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     CHKCONF             CHECK FOR CONFIRMATION                       
         DROP  R3                                                               
*                                                                               
CHKMCN   CLC   =C'MKGCAN',0(R3)    IS THIS A MAKEGOOD CANCELLATION?             
         BNE   UNKMESS                                                          
         USING MOFRCAND,R3                                                      
         MVC   USERID,MOCNTOID                                                  
*                                                                               
         CLI   MOCNNEWO,C'Y'       MAKEGOOD RECALL?                             
         BNE   *+10                                                             
         MVC   RMTYPE,=C'MKGREC'                                                
*                                                                               
         GOTO1 =A(MKGCAN),(RC)     YES -- WRITE IT TO EDICT FILE                
         BNE   CHKCONF             PROBLEM FOUND: IGNORE MESSAGE                
         CLC   =C'MKGCAN',DAREREC                                               
         BNE   CHKCONF             MESSAGE IS BAD: BAIL OUT NOW                 
         LA    R3,DAREREC                                                       
         MVC   MAILTIME,MOCNTIME   TIME                                         
         LA    RF,MOCNRTNS         FOR BUYER CODE                               
         MVC   MAILBYER,RTNBUYER-RTN2SNDR(RF)                                   
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     CHKCONF             CHECK FOR CONFIRMATION                       
         DROP  R3                                                               
*                                                                               
UNKMESS  DC    H'0'                UNKNOWN MESSAGE TYPE                         
*                                                                               
CHKCONF  CLI   RQSTCONF,C'Y'       CONFIRMATION REQUESTED?                      
         BE    *+6                                                              
         DC    H'0'                NO -- HOW DID WE GET HERE?                   
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRMATION                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL8'ISGENQ',C'#02K'                                
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    NEXTMSG             RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
GOODBYE  PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INITIAL  NMOD1 0,INITIAL                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         LA    R1,DRRSTOP                                                       
         STCM  R1,7,ASTOPECB+1     SET A(STOPPING ECB)                          
         L     RF,DRRMAJNM         A(MAJOR RESOURCE NAME)                       
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
         LOAD  DE=ATBALC2                                                       
         ST    R0,ATBALC2_STRUCTURE                                             
         LOAD  DE=ATBCFMD                                                       
         ST    R0,ATBCFMD_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBPTR                                                        
         ST    R0,ATBPTR_STRUCTURE                                              
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE                                             
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE                                             
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
         LOAD  DE=CSQBPUT1                                                      
         ST    R0,CSQBPUT1_STRUCTURE                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL8'ISGENQ',C'#02K'  SET SUBTASK #02               
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES AN INCOMING DARE ORDER TO THE PRINT QUEUES.               
*   ON ENTRY, R3 = A(AGYHDR RECORD)                                             
***********************************************************************         
AGYHDR   NMOD1 0,AGYHDR                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  AGYHDRPQWRITE,PRINT=ALWAYS                                       
*                                                                               
         MVC   USERID,PAHDTOID-PAGYHDRD(R3)                                     
         GOTO1 =A(GETUID),(RC)     GET HEX USERID                               
         OC    HALF,HALF           USERID RECORD FOUND?                         
         BNZ   AGPQW10                                                          
AGPQW5   CLI   RQSTCONF,C'Y'       NO -- DID WE GET THE WHOLE MESSAGE?          
         BE    AGPQWX                                                           
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    AGPQW5                                                           
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
AGPQW10  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,PRTQNAME,E,5)  ENQUEUE THE PRINT QUEUE                 
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE_OK                                                       
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         MVC   QLSRCID,HALF        USERID                                       
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,=C'DAR'                                                  
         MVI   QLCLASS,C'N'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,164                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'DARE ORDER '                                           
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         PRNT  ABOUT_OPEN_PQ                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    AGPQW18                                                          
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         B     AGCLOPUR                                                         
*                                                                               
AGPQW18  DS    0H                                                               
         MVC   SVPQUID,QLSRCID     SAVE USERID                                  
         MVC   SVPQSID,QLSUBID     SAVE SUB ID                                  
         MVC   SVPQRNO,QLREPRNO    SAVE REPORT SEQ#                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),QLSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,QLREPRNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         PRNT  OPEN_PQ_OK                                                       
         DROP  R2                                                               
*                                                                               
*        MVI   MQACTION,C'O'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
AGPQW20  MVI   R,X'09'                                                          
         MVI   R+1,C' '                                                         
         MVC   R+2(L'R-2),R+1                                                   
         ST    R3,FULL             START OF TEXT TO BE PRINTED                  
         LA    R4,R+1              START OF PRINT LINE                          
*                                                                               
AGPQW30  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R5,R1                                                            
         CLI   0(R5),ETX                                                        
         BNE   *+14                                                             
         MVC   P+30(19),=C'+++DARE END MISSING'                                 
         B     AGCLOPUR                                                         
         CLI   0(R5),ETB           ANY MORE RECORDS IN BUFFER?                  
         BNE   AGPQW40                                                          
*                                                                               
         GOTO1 =A(DRLINRCV),(RC)   NO -- GET NEXT BUFFER                        
         BNE   AGCLOPUR            ERROR                                        
         LA    R3,DAREBUF          CONTINUATION OF MESSAGE                      
         ST    R3,FULL                                                          
         B     AGPQW30                                                          
*                                                                               
AGPQW40  CLC   CRLF,0(R5)                                                       
         BE    *+16                                                             
         MVI   0(R5),C' '          REPLACE INVALID CHARACTER WITH BLANK         
         LA    R3,1(R5)                                                         
         B     AGPQW30                                                          
*                                                                               
         L     R3,FULL                                                          
         SR    R5,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R5,R5               ANY DATA TO PRINT?                           
         BNZ   *+12                YES                                          
         LA    R3,2(R3)            NO - BUMP PAST CRLF                          
         B     AGPQW20                                                          
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R3,3(R5,R3)         LENGTH OF STRING + BCTR + CRLF               
*                                                                               
         LA    R5,1(R5)            RESTORE LENGTH VALUE                         
*                                                                               
         L     R1,RECCOUNT                                                      
         LA    R1,1(R1)            INCREMENT RECORD COUNT                       
         ST    R1,RECCOUNT                                                      
         CLC   =C'+++DARE END ',0(R4)  WAS THIS THE LAST LINE?                  
         BE    AGPQW50             YES                                          
*                                                                               
*        MVI   MQACTION,C'P'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
AGPQW45  GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    AGPQW20                                                          
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     AGCLOPUR                                                         
*                                                                               
AGPQW50  PACK  DUB,30(6,R4)        RECORD COUNT IN '+++DARE END'                
         CVB   R0,DUB                                                           
         C     R0,RECCOUNT                                                      
         BE    *+6                                                              
         DC    H'0'                MISMATCH ON ENVELOPE RECORD COUNT            
*                                                                               
*        MVI   MQACTION,C'C'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
AGPQW60  PRNT  ENDAGYHDR,PRINT=ALWAYS                                           
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    AGDEQPQ                                                          
*                                                                               
AGCLOPUR MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'CLO/PUR'),=C'PRTQUE',0,R,A(CIREC)             
*                                                                               
AGRCVALL CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    AGDEQPQ                                                          
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    AGRCVALL                                                         
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
AGDEQPQ  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,PRTQNAME,5)    DEQUEUE THE PRINT QUEUE                 
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE_OK                                                       
*                                                                               
AGPQWX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES A DARE AGENCY CANCELLATION TO THE PRINT QUEUE             
*   ON ENTRY, R3 = A(AGYCAN RECORD)                                             
***********************************************************************         
AGYCAN   NMOD1 0,AGYCAN                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  AGYCANPQWRITE,PRINT=ALWAYS                                       
*                                                                               
         MVC   USERID,PACNTOID-PAGYCAND(R3)                                     
         GOTO1 =A(GETUID),(RC)     GET HEX USERID                               
         OC    HALF,HALF           USERID RECORD FOUND?                         
         BNZ   ACPQW10                                                          
ACPQW5   CLI   RQSTCONF,C'Y'       NO -- DID WE GET THE WHOLE MESSAGE?          
         BE    ACPQWX                                                           
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    ACPQW5                                                           
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
ACPQW10  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,PRTQNAME,E,5)  ENQUEUE THE PRINT QUEUE                 
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE_OK                                                       
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         MVC   QLSRCID,HALF        USERID                                       
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,=C'DAR'                                                  
         MVI   QLCLASS,C'N'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'NOTDARE    '                                           
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         PRNT  ABOUT_OPEN_PQ                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    ACPQW18                                                          
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         B     ACCLOPUR                                                         
*                                                                               
ACPQW18  DS    0H                                                               
         MVC   SVPQUID,QLSRCID     SAVE USERID                                  
         MVC   SVPQSID,QLSUBID     SAVE SUB ID                                  
         MVC   SVPQRNO,QLREPRNO    SAVE REPORT SEQ#                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),QLSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,QLREPRNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         PRNT  OPEN_PQ_OK                                                       
         DROP  R2                                                               
*                                                                               
*        MVI   MQACTION,C'O'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
ACPQW20  MVI   R,X'09'                                                          
         MVI   R+1,C' '                                                         
         MVC   R+2(L'R-2),R+1                                                   
         ST    R3,FULL             START OF TEXT TO BE PRINTED                  
         LA    R4,R+1              START OF PRINT LINE                          
*                                                                               
ACPQW30  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R5,R1                                                            
         CLI   0(R5),ETX                                                        
         BNE   *+14                                                             
         MVC   P+30(19),=C'+++DARE END MISSING'                                 
         B     ACCLOPUR                                                         
         CLI   0(R5),ETB           ANY MORE RECORDS IN BUFFER?                  
         BNE   ACPQW40                                                          
*                                                                               
         GOTO1 =A(DRLINRCV),(RC)   NO -- GET NEXT BUFFER                        
         BNE   ACCLOPUR            ERROR                                        
         LA    R3,DAREBUF          CONTINUATION OF MESSAGE                      
         ST    R3,FULL                                                          
         B     ACPQW30                                                          
*                                                                               
ACPQW40  CLC   CRLF,0(R5)                                                       
         BE    *+16                                                             
         MVI   0(R5),C' '          REPLACE INVALID CHARACTER WITH BLANK         
         LA    R3,1(R5)                                                         
         B     ACPQW30                                                          
*                                                                               
         L     R3,FULL                                                          
         SR    R5,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R5,R5               ANY DATA TO PRINT?                           
         BNZ   *+12                YES                                          
         LA    R3,2(R3)            NO - BUMP PAST CRLF                          
         B     ACPQW20                                                          
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R3,3(R5,R3)         LENGTH OF STRING + BCTR + CRLF               
*                                                                               
         L     R1,RECCOUNT                                                      
         LA    R1,1(R1)            INCREMENT RECORD COUNT                       
         ST    R1,RECCOUNT                                                      
         CLC   =C'+++DARE END ',0(R4)  WAS THIS THE LAST LINE?                  
         BE    ACPQW50             YES                                          
*                                                                               
*        MVI   MQACTION,C'P'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    ACPQW20                                                          
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     ACCLOPUR                                                         
*                                                                               
ACPQW50  PACK  DUB,30(6,R4)        RECORD COUNT IN '+++DARE END'                
         CVB   R0,DUB                                                           
         C     R0,RECCOUNT                                                      
         BE    *+6                                                              
         DC    H'0'                MISMATCH ON ENVELOPE RECORD COUNT            
*                                                                               
*        MVI   MQACTION,C'C'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
         PRNT  ENDAGYCAN,PRINT=ALWAYS                                           
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    ACDEQPQ                                                          
*                                                                               
ACCLOPUR MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'CLO/PUR'),=C'PRTQUE',0,R,A(CIREC)             
*                                                                               
ACRCVALL CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    ACDEQPQ                                                          
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    ACRCVALL                                                         
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
ACDEQPQ  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,PRTQNAME,5)    DEQUEUE THE PRINT QUEUE                 
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE_OK                                                       
*                                                                               
ACPQWX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES AN INCOMING MAKEGOOD OFFER TO THE PRINT QUEUES.           
*   ON ENTRY, R3 = A(MKGHDR RECORD)                                             
***********************************************************************         
MKGHDR   NMOD1 0,MKGHDR                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  MKGHDRPQWRITE,PRINT=ALWAYS                                       
*                                                                               
         MVC   USERID,MOHDTOID-MOFRHDRD(R3)                                     
         GOTO1 =A(GETUID),(RC)     GET HEX USERID                               
         OC    HALF,HALF           USERID RECORD FOUND?                         
         BNZ   MGPQW10                                                          
MGPQW5   CLI   RQSTCONF,C'Y'       NO -- DID WE GET THE WHOLE MESSAGE?          
         BE    MGPQWX                                                           
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    MGPQW5                                                           
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
MGPQW10  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,PRTQNAME,E,5)  ENQUEUE THE PRINT QUEUE                 
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE_OK                                                       
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         MVC   QLSRCID,HALF        USERID                                       
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,=C'DMG'                                                  
         MVI   QLCLASS,C'N'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,164                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'MKGOOD OFFR'                                           
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         PRNT  ABOUT_OPEN_PQ                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    MGPQW15                                                          
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         B     MGCLOPUR                                                         
*                                                                               
MGPQW15  MVC   SVPQUID,QLSRCID     SAVE USERID                                  
         MVC   SVPQSID,QLSUBID     SAVE SUB ID                                  
         MVC   SVPQRNO,QLREPRNO    SAVE REPORT SEQ#                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),QLSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,QLREPRNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         PRNT  OPEN_PQ_OK                                                       
         DROP  R2                                                               
*                                                                               
MGPQW20  MVI   R,X'09'                                                          
         MVI   R+1,C' '                                                         
         MVC   R+2(L'R-2),R+1                                                   
         ST    R3,FULL             START OF TEXT TO BE PRINTED                  
         LA    R4,R+1              START OF PRINT LINE                          
*                                                                               
MGPQW30  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R5,R1                                                            
         CLI   0(R5),ETX                                                        
         BNE   *+14                                                             
         MVC   P+30(19),=C'+++DARE END MISSING'                                 
         B     MGCLOPUR                                                         
         CLI   0(R5),ETB           ANY MORE RECORDS IN BUFFER?                  
         BNE   MGPQW40                                                          
*                                                                               
         GOTO1 =A(DRLINRCV),(RC)   NO -- GET NEXT BUFFER                        
         BNE   MGCLOPUR            ERROR                                        
         LA    R3,DAREBUF          CONTINUATION OF MESSAGE                      
         ST    R3,FULL                                                          
         B     MGPQW30                                                          
*                                                                               
MGPQW40  CLC   CRLF,0(R5)                                                       
         BE    *+16                                                             
         MVI   0(R5),C' '          REPLACE INVALID CHARACTER WITH BLANK         
         LA    R3,1(R5)                                                         
         B     MGPQW30                                                          
*                                                                               
         L     R3,FULL                                                          
         SR    R5,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R5,R5               ANY DATA TO PRINT?                           
         BNZ   *+12                YES                                          
         LA    R3,2(R3)            NO - BUMP PAST CRLF                          
         B     MGPQW20                                                          
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R3,3(R5,R3)         LENGTH OF STRING + BCTR + CRLF               
*                                                                               
         L     R1,RECCOUNT                                                      
         LA    R1,1(R1)            INCREMENT RECORD COUNT                       
         ST    R1,RECCOUNT                                                      
         CLC   =C'+++DARE END ',0(R4)  WAS THIS THE LAST LINE?                  
         BE    MGPQW50             YES                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    MGPQW20                                                          
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     MGCLOPUR                                                         
*                                                                               
MGPQW50  PACK  DUB,30(6,R4)        RECORD COUNT IN '+++DARE END'                
         CVB   R0,DUB                                                           
         C     R0,RECCOUNT                                                      
         BE    *+6                                                              
         DC    H'0'                MISMATCH ON ENVELOPE RECORD COUNT            
*                                                                               
         PRNT  ENDMKGHDR,PRINT=ALWAYS                                           
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    MGDEQPQ                                                          
*                                                                               
MGCLOPUR MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'CLO/PUR'),=C'PRTQUE',0,R,A(CIREC)             
*                                                                               
MGRCVALL CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    MGDEQPQ                                                          
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    MGRCVALL                                                         
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
MGDEQPQ  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,PRTQNAME,5) DEQUEUE THE PRINT QUEUE                    
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE_OK                                                       
*                                                                               
MGPQWX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES AN INCOMING MAKEGOOD APPROVAL TO THE PRINT QUEUE.         
*   ON ENTRY, R3 = A(MKGAPP RECORD)                                             
***********************************************************************         
MKGAPP   NMOD1 0,MKGAPP                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  MKGAPPPQWRITE,PRINT=ALWAYS                                       
*                                                                               
         MVC   USERID,MOAPTOID-MOFRAPPD(R3)                                     
         GOTO1 =A(GETUID),(RC)     GET HEX USERID                               
         OC    HALF,HALF           USERID RECORD FOUND?                         
         BNZ   MAPQW10                                                          
MAPQW5   CLI   RQSTCONF,C'Y'       NO -- DID WE GET THE WHOLE MESSAGE?          
         BE    MAPQWX                                                           
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    MAPQW5                                                           
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
MAPQW10  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,PRTQNAME,E,5)  ENQUEUE THE PRINT QUEUE                 
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE_OK                                                       
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         MVC   QLSRCID,HALF        USERID                                       
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,=C'DAR'                                                  
         MVI   QLCLASS,C'N'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'MKGOOD APPR'                                           
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         PRNT  ABOUT_OPEN_PQ                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    MAPQW18                                                          
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         B     MACLOPUR                                                         
*                                                                               
MAPQW18  DS    0H                                                               
         MVC   SVPQUID,QLSRCID     SAVE USERID                                  
         MVC   SVPQSID,QLSUBID     SAVE SUB ID                                  
         MVC   SVPQRNO,QLREPRNO    SAVE REPORT SEQ#                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),QLSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,QLREPRNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         PRNT  OPEN_PQ_OK                                                       
         DROP  R2                                                               
*                                                                               
*        MVI   MQACTION,C'O'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
MAPQW20  MVI   R,X'09'                                                          
         MVI   R+1,C' '                                                         
         MVC   R+2(L'R-2),R+1                                                   
         ST    R3,FULL             START OF TEXT TO BE PRINTED                  
         LA    R4,R+1              START OF PRINT LINE                          
*                                                                               
MAPQW30  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R5,R1                                                            
         CLI   0(R5),ETX                                                        
         BNE   *+14                                                             
         MVC   P+30(19),=C'+++DARE END MISSING'                                 
         B     MACLOPUR                                                         
         CLI   0(R5),ETB           ANY MORE RECORDS IN BUFFER?                  
         BNE   MAPQW40                                                          
*                                                                               
         GOTO1 =A(DRLINRCV),(RC)   NO -- GET NEXT BUFFER                        
         BNE   MACLOPUR            ERROR                                        
         LA    R3,DAREBUF          CONTINUATION OF MESSAGE                      
         ST    R3,FULL                                                          
         B     MAPQW30                                                          
*                                                                               
MAPQW40  CLC   CRLF,0(R5)                                                       
         BE    *+16                                                             
         MVI   0(R5),C' '          REPLACE INVALID CHARACTER WITH BLANK         
         LA    R3,1(R5)                                                         
         B     MAPQW30                                                          
*                                                                               
         L     R3,FULL                                                          
         SR    R5,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R5,R5               ANY DATA TO PRINT?                           
         BNZ   *+12                YES                                          
         LA    R3,2(R3)            NO - BUMP PAST CRLF                          
         B     MAPQW20                                                          
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R3,3(R5,R3)         LENGTH OF STRING + BCTR + CRLF               
*                                                                               
         L     R1,RECCOUNT                                                      
         LA    R1,1(R1)            INCREMENT RECORD COUNT                       
         ST    R1,RECCOUNT                                                      
         CLC   =C'+++DARE END ',0(R4)  WAS THIS THE LAST LINE?                  
         BE    MAPQW50             YES                                          
*                                                                               
*        MVI   MQACTION,C'P'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    MAPQW20                                                          
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     MACLOPUR                                                         
*                                                                               
MAPQW50  PACK  DUB,30(6,R4)        RECORD COUNT IN '+++DARE END'                
         CVB   R0,DUB                                                           
         C     R0,RECCOUNT                                                      
         BE    *+6                                                              
         DC    H'0'                MISMATCH ON ENVELOPE RECORD COUNT            
*                                                                               
*        MVI   MQACTION,C'C'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
         PRNT  ENDMKGAPP,PRINT=ALWAYS                                           
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    MADEQPQ                                                          
*                                                                               
MACLOPUR MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'CLO/PUR'),=C'PRTQUE',0,R,A(CIREC)             
*                                                                               
MARCVALL CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    MADEQPQ                                                          
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    MARCVALL                                                         
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
MADEQPQ  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,PRTQNAME,5) DEQUEUE THE PRINT QUEUE                    
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE_OK                                                       
*                                                                               
MAPQWX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES AN INCOMING MAKEGOOD REJECTION TO THE PRINT QUEUE         
*   ON ENTRY, R3 = A(MKGREJ RECORD)                                             
***********************************************************************         
MKGREJ   NMOD1 0,MKGREJ                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  MKGREJPQWRITE,PRINT=ALWAYS                                       
*                                                                               
         MVC   USERID,MORJTOID-MOFRREJD(R3)                                     
         GOTO1 =A(GETUID),(RC)     GET HEX USERID                               
         OC    HALF,HALF           USERID RECORD FOUND?                         
         BNZ   MRPQW10                                                          
MRPQW5   CLI   RQSTCONF,C'Y'       NO -- DID WE GET THE WHOLE MESSAGE?          
         BE    MRPQWX                                                           
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    MRPQW5                                                           
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
MRPQW10  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,PRTQNAME,E,5)  ENQUEUE THE PRINT QUEUE                 
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE_OK                                                       
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         MVC   QLSRCID,HALF        USERID                                       
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,=C'DAR'                                                  
         MVI   QLCLASS,C'N'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'MKGOOD REJ '                                           
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         PRNT  ABOUT_OPEN_PQ                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    MRPQW18                                                          
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         B     MRCLOPUR                                                         
*                                                                               
MRPQW18  DS    0H                                                               
         MVC   SVPQUID,QLSRCID     SAVE USERID                                  
         MVC   SVPQSID,QLSUBID     SAVE SUB ID                                  
         MVC   SVPQRNO,QLREPRNO    SAVE REPORT SEQ#                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),QLSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,QLREPRNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         PRNT  OPEN_PQ_OK                                                       
         DROP  R2                                                               
*                                                                               
*        MVI   MQACTION,C'O'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
MRPQW20  MVI   R,X'09'                                                          
         MVI   R+1,C' '                                                         
         MVC   R+2(L'R-2),R+1                                                   
         ST    R3,FULL             START OF TEXT TO BE PRINTED                  
         LA    R4,R+1              START OF PRINT LINE                          
*                                                                               
MRPQW30  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R5,R1                                                            
         CLI   0(R5),ETX                                                        
         BNE   *+14                                                             
         MVC   P+30(19),=C'+++DARE END MISSING'                                 
         B     MRCLOPUR                                                         
         CLI   0(R5),ETB           ANY MORE RECORDS IN BUFFER?                  
         BNE   MRPQW40                                                          
*                                                                               
         GOTO1 =A(DRLINRCV),(RC)   NO -- GET NEXT BUFFER                        
         BNE   MRCLOPUR            ERROR                                        
         LA    R3,DAREBUF          CONTINUATION OF MESSAGE                      
         ST    R3,FULL                                                          
         B     MRPQW30                                                          
*                                                                               
MRPQW40  CLC   CRLF,0(R5)                                                       
         BE    *+16                                                             
         MVI   0(R5),C' '          REPLACE INVALID CHARACTER WITH BLANK         
         LA    R3,1(R5)                                                         
         B     MRPQW30                                                          
*                                                                               
         L     R3,FULL                                                          
         SR    R5,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R5,R5               ANY DATA TO PRINT?                           
         BNZ   *+12                YES                                          
         LA    R3,2(R3)            NO - BUMP PAST CRLF                          
         B     MRPQW20                                                          
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R3,3(R5,R3)         LENGTH OF STRING + BCTR + CRLF               
*                                                                               
         L     R1,RECCOUNT                                                      
         LA    R1,1(R1)            INCREMENT RECORD COUNT                       
         ST    R1,RECCOUNT                                                      
         CLC   =C'+++DARE END ',0(R4)  WAS THIS THE LAST LINE?                  
         BE    MRPQW50             YES                                          
*                                                                               
*        MVI   MQACTION,C'P'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    MRPQW20                                                          
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     MRCLOPUR                                                         
*                                                                               
MRPQW50  PACK  DUB,30(6,R4)        RECORD COUNT IN '+++DARE END'                
         CVB   R0,DUB                                                           
         C     R0,RECCOUNT                                                      
         BE    *+6                                                              
         DC    H'0'                MISMATCH ON ENVELOPE RECORD COUNT            
*                                                                               
*        MVI   MQACTION,C'C'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
         PRNT  ENDMKGREJ,PRINT=ALWAYS                                           
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    MRDEQPQ                                                          
*                                                                               
MRCLOPUR MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'CLO/PUR'),=C'PRTQUE',0,R,A(CIREC)             
*                                                                               
MRRCVALL CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    MRDEQPQ                                                          
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    MRRCVALL                                                         
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
MRDEQPQ  MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,PRTQNAME,5) DEQUEUE THE PRINT QUEUE                    
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE_OK                                                       
*                                                                               
MRPQWX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES AN INCOMING DARE MESSAGE TO THE EDICT FILE.               
*   ON ENTRY, R3 = A(DARE RECORD)   (E.G., ORDAPP, ETC)                         
***********************************************************************         
DLNNOT   DS    0H                                                               
ERRNOT   DS    0H                                                               
ORDAPP   DS    0H                                                               
ORDREJ   DS    0H                                                               
ORDCFM   DS    0H                                                               
ORDRCL   DS    0H                                                               
AGYRCL   DS    0H                                                               
MKGROK   DS    0H                                                               
MKGCAN   DS    0H                                                               
*                                                                               
         NMOD1 0,WRTEDICT                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  DAREEDICTWRITE,PRINT=ALWAYS                                      
*                                                                               
         MVC   DAREREC,SPACES      FIRST RECORD IN BUFFER GOES HERE             
         OC    USERID,USERID                                                    
         BZ    DREDW10                                                          
         GOTO1 =A(GETUID),(RC)     GET HEX USERID IN 'HALF'                     
         OC    HALF,HALF           USERID RECORD FOUND?                         
         BNZ   DREDW10                                                          
DREDW5   CLI   RQSTCONF,C'Y'       NO -- DID WE GET THE WHOLE MESSAGE?          
         BE    DREDWXBD                                                         
         GOTO1 =A(DRLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    DREDW5                                                           
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
         USING EDFILD,R2                                                        
DREDW10  LA    R2,EDICTREC         BUILD AN EDICT FILE RECORD                   
         XC    EDICTREC,EDICTREC   CLEAR RECORD BUFFER                          
         MVI   EDFSYS,EDFDAREQ     MARK AS A DARE RECORD                        
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB)                                    
         MVC   EDFMON,DUB+1        MONTH NUMBER IN BINARY                       
         ST    R3,FULL             START OF MESSAGE TO BE SAVED                 
         LA    R4,EDFDARE          START OF SAVE AREA                           
*                                                                               
DREDW20  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R5,R1                                                            
         CLI   0(R5),ETX                                                        
         BNE   *+6                                                              
         DC    H'0'                WHERE WAS THE '+++DARE END '?                
         CLI   0(R5),ETB                                                        
         BNE   DREDW30             GET THE NEXT BUFFER                          
*                                                                               
         GOTO1 =A(DRLINRCV),(RC)   GET NEXT BUFFER                              
         BE    *+6                                                              
         DC    H'0'                ERROR                                        
         LA    R3,DAREBUF          CONTINUATION OF MESSAGE                      
         ST    R3,FULL                                                          
         B     DREDW20                                                          
*                                                                               
DREDW30  CLC   CRLF,0(R5)                                                       
         BE    *+16                                                             
         MVI   0(R5),C' '          REPLACE INVALID CHARACTER WITH BLANK         
         LA    R3,1(R5)                                                         
         B     DREDW20                                                          
*                                                                               
         L     R3,FULL                                                          
         SR    R5,R3               LENGTH OF MESSAGE TO BE SAVED                
         LTR   R5,R5               ANY DATA TO PRINT?                           
         BNZ   *+12                YES                                          
         LA    R3,2(R3)            NO - BUMP PAST CRLF                          
         B     DREDW10                                                          
*                                                                               
         LA    R1,0(R5,R4)                                                      
         LA    R0,EDFDARE+L'EDFDARE                                             
         CR    R1,R0               MESSAGE RECORD IS TOO LONG?                  
         BNH   *+6                                                              
         DC    H'0'                YES                                          
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO BUFFER AREA                 
*                                                                               
         CLC   DAREREC,SPACES      DID WE SAVE A RECORD YET?                    
         BNE   DREDW40                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   DAREREC(0),0(R3)    NO, SAVE IT NOW                              
*                                                                               
DREDW40  LA    R3,3(R5,R3)         LENGTH OF STRING + BCTR + CRLF               
         L     R1,RECCOUNT                                                      
         LA    R1,1(R1)            INCREMENT RECORD COUNT                       
         ST    R1,RECCOUNT                                                      
         CLC   =C'+++DARE END ',0(R4)  WAS THIS THE LAST LINE?                  
         BE    DREDW50             YES                                          
*                                                                               
         PRNT  ADDEDICTRECORD                                                   
         CLI   TRACEFLG,C'Y'                                                    
         BNE   DREDW45                                                          
         GOTO1 =V(PRNTBL),DMCB,0,EDICTREC,C'DUMP',256,=C'1D'                    
DREDW45  GOTO1 DRREDADD,DMCB,DRRMAJNM,EDICTREC,0                                
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
         B     DREDW10                                                          
*                                                                               
DREDW50  PACK  DUB,30(6,R4)        RECORD COUNT IN '+++DARE END'                
         CVB   R0,DUB                                                           
         C     R0,RECCOUNT                                                      
         BE    DREDWXOK                                                         
         DC    H'0'                MISMATCH ON ENVELOPE RECORD COUNT            
*                                                                               
DREDWXBD LTR   RB,RB               SET CC NOT EQUAL                             
         B     DREDWXX                                                          
*                                                                               
DREDWXOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
DREDWXX  XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES MKGHDR MESSAGE W/ PQKEY(CL8) TO THE EDICT FILE.           
*   ON ENTRY, R3 = A(MKGHDR RECORD)                                             
***********************************************************************         
EMKGHDR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  DAREEDIWRT_MKGHDR,PRINT=ALWAYS                                   
*                                                                               
         USING EDFILD,R2                                                        
         LA    R2,EDICTREC         BUILD AN EDICT FILE RECORD                   
         XC    EDICTREC,EDICTREC   CLEAR RECORD BUFFER                          
         MVI   EDFSYS,EDFDAREQ     MARK AS A DARE RECORD                        
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB)                                    
         MVC   EDFMON,DUB+1        MONTH NUMBER IN BINARY                       
*        ST    R3,FULL             START OF MESSAGE TO BE SAVED                 
*                                                                               
         LA    R4,EDFDARE          START OF SAVE AREA                           
         USING PMKGHDRD,R4                                                      
         MVC   0(PMKGUSER-PMKGHDRD,R4),DAREREC    COPY CRITICAL DATA            
         MVC   PMKGRTNS,DAREREC+MOHDRTNS-MOFRHDRD                               
         MVC   PMKGOFRI,DAREREC+MOHDOFRI-MOFRHDRD                               
         MVC   PMKGSEQN,DAREREC+MOHDSEQN-MOFRHDRD                               
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,SVPQKEY,PMKGUSER,2,=C'TOG'    USER ID            
         MVC   PMKGSUBI,SVPQKEY+2                                               
         GOTO1 (RF),(R1),SVPQKEY+5,PMKGREPN,2,=C'TOG'  SEQ#                     
         DROP  R4                                                               
*                                                                               
         PRNT  ADDEDICTRECORD                                                   
         CLI   TRACEFLG,C'Y'                                                    
         BNE   EMKG10                                                           
         GOTO1 =V(PRNTBL),DMCB,0,EDICTREC,C'DUMP',256,=C'1D'                    
*                                                                               
EMKG10   GOTO1 DRREDADD,DMCB,DRRMAJNM,EDICTREC,0                                
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES AGYHDR MESSAGE W/ PQKEY(CL8) TO THE EDICT FILE.           
*   ON ENTRY, R3 = A(AGYHDR RECORD)                                             
***********************************************************************         
EAGYHDR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  DAREEDIWRT_AGYHDR,PRINT=ALWAYS                                   
*                                                                               
         USING EDFILD,R2                                                        
         LA    R2,EDICTREC         BUILD AN EDICT FILE RECORD                   
         XC    EDICTREC,EDICTREC   CLEAR RECORD BUFFER                          
         MVI   EDFSYS,EDFDAREQ     MARK AS A DARE RECORD                        
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB)                                    
         MVC   EDFMON,DUB+1        MONTH NUMBER IN BINARY                       
*                                                                               
         LA    R4,EDFDARE          START OF SAVE AREA                           
         USING PAGYHDRD,R4                                                      
         MVC   0(PAHDPQUR-PAGYHDRD,R4),DAREREC    COPY CRITICAL DATA            
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,SVPQKEY,PAHDPQUR,2,=C'TOG'    USER ID            
         MVC   PAHDPQSB,SVPQKEY+2                                               
         GOTO1 (RF),(R1),SVPQKEY+5,PAHDPQRE,2,=C'TOG'  SEQ#                     
         DROP  R4                                                               
*                                                                               
         PRNT  ADDEDICTRECORD                                                   
         CLI   TRACEFLG,C'Y'                                                    
         BNE   EAHD10                                                           
         GOTO1 =V(PRNTBL),DMCB,0,EDICTREC,C'DUMP',256,=C'1D'                    
*                                                                               
EAHD10   GOTO1 DRREDADD,DMCB,DRRMAJNM,EDICTREC,0                                
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES AGYCAN MESSAGE W/ PQKEY(CL8) TO THE EDICT FILE.           
*   ON ENTRY, R3 = A(AGYCAN RECORD)                                             
***********************************************************************         
EAGYCAN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  DAREEDIWRT_AGYCAN,PRINT=ALWAYS                                   
*                                                                               
         USING EDFILD,R2                                                        
         LA    R2,EDICTREC         BUILD AN EDICT FILE RECORD                   
         XC    EDICTREC,EDICTREC   CLEAR RECORD BUFFER                          
         MVI   EDFSYS,EDFDAREQ     MARK AS A DARE RECORD                        
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB)                                    
         MVC   EDFMON,DUB+1        MONTH NUMBER IN BINARY                       
*                                                                               
         LA    R4,EDFDARE          START OF SAVE AREA                           
         USING PAGYCAND,R4                                                      
         MVC   0(PACNPQUR-PAGYCAND,R4),DAREREC    COPY CRITICAL DATA            
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,SVPQKEY,PACNPQUR,2,=C'TOG'    USER ID            
         MVC   PACNPQSB,SVPQKEY+2                                               
         GOTO1 (RF),(R1),SVPQKEY+5,PACNPQRE,2,=C'TOG'  SEQ#                     
         DROP  R4                                                               
*                                                                               
         PRNT  ADDEDICTRECORD                                                   
         CLI   TRACEFLG,C'Y'                                                    
         BNE   EACN10                                                           
         GOTO1 =V(PRNTBL),DMCB,0,EDICTREC,C'DUMP',256,=C'1D'                    
*                                                                               
EACN10   GOTO1 DRREDADD,DMCB,DRRMAJNM,EDICTREC,0                                
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES MKGAPP MESSAGE W/ PQKEY(CL8) TO THE EDICT FILE.           
*   ON ENTRY, R3 = A(MKGAPP RECORD)                                             
***********************************************************************         
EMKGAPP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  DAREEDIWRT_MKGAPP,PRINT=ALWAYS                                   
*                                                                               
         USING EDFILD,R2                                                        
         LA    R2,EDICTREC         BUILD AN EDICT FILE RECORD                   
         XC    EDICTREC,EDICTREC   CLEAR RECORD BUFFER                          
         MVI   EDFSYS,EDFDAREQ     MARK AS A DARE RECORD                        
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB)                                    
         MVC   EDFMON,DUB+1        MONTH NUMBER IN BINARY                       
*                                                                               
         LA    R4,EDFDARE          START OF SAVE AREA                           
         USING MOFRAPPD,R4                                                      
         MVC   0(MOAPPQUR-MOFRAPPD,R4),DAREREC    COPY CRITICAL DATA            
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,SVPQKEY,MOAPPQUR,2,=C'TOG'    USER ID            
         MVC   MOAPPQSB,SVPQKEY+2                                               
         GOTO1 (RF),(R1),SVPQKEY+5,MOAPPQRE,2,=C'TOG'  SEQ#                     
         DROP  R4                                                               
*                                                                               
         PRNT  ADDEDICTRECORD                                                   
         CLI   TRACEFLG,C'Y'                                                    
         BNE   EMAP10                                                           
         GOTO1 =V(PRNTBL),DMCB,0,EDICTREC,C'DUMP',256,=C'1D'                    
*                                                                               
EMAP10   GOTO1 DRREDADD,DMCB,DRRMAJNM,EDICTREC,0                                
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE WRITES MKGREJ MESSAGE W/ PQKEY(CL8) TO THE EDICT FILE.           
*   ON ENTRY, R3 = A(MKGREJ RECORD)                                             
***********************************************************************         
EMKGREJ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  DAREEDIWRT_MKGREJ,PRINT=ALWAYS                                   
*                                                                               
         USING EDFILD,R2                                                        
         LA    R2,EDICTREC         BUILD AN EDICT FILE RECORD                   
         XC    EDICTREC,EDICTREC   CLEAR RECORD BUFFER                          
         MVI   EDFSYS,EDFDAREQ     MARK AS A DARE RECORD                        
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB)                                    
         MVC   EDFMON,DUB+1        MONTH NUMBER IN BINARY                       
*                                                                               
         LA    R4,EDFDARE          START OF SAVE AREA                           
         USING MOFRREJD,R4                                                      
         MVC   0(MORJPQUR-MOFRREJD,R4),DAREREC    COPY CRITICAL DATA            
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,SVPQKEY,MORJPQUR,2,=C'TOG'    USER ID            
         MVC   MORJPQSB,SVPQKEY+2                                               
         GOTO1 (RF),(R1),SVPQKEY+5,MORJPQRE,2,=C'TOG'  SEQ#                     
         DROP  R4                                                               
*                                                                               
         PRNT  ADDEDICTRECORD                                                   
         CLI   TRACEFLG,C'Y'                                                    
         BNE   EMRJ10                                                           
         GOTO1 =V(PRNTBL),DMCB,0,EDICTREC,C'DUMP',256,=C'1D'                    
*                                                                               
EMRJ10   GOTO1 DRREDADD,DMCB,DRRMAJNM,EDICTREC,0                                
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ THE ALPHA USERID RECORD IN 'USERID'.                                     
* RETURN THE HEX USERID IN 'HALF'.                                              
* PUT THE NUMERIC USERID IN 'MAILAGY' (FOR DAREMAIL).                           
* PUT REP SE NUMBER IN 'RSENUM' (FOR REP DAREMAIL).                             
* PUT AGENCY EBCDIC POWER CODE IN PWRCODE (FOR DAREMAIL).                       
***********************************************************************         
GETUID   NMOD1 0,GETUID                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         XC    HALF,HALF           NO USERID FOUND YET                          
         XC    RSENUM,RSENUM       NO SE NUMBER FOUND YET                       
*                                                                               
         LA    R5,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R5)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(5),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),USERID   ALPHA USER-ID                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
         CLI   DMCB+8,0                                                         
         BE    GETUID10            ID RECORD FOUND                              
         MVC   P+11(30),=C'*** ERROR *** USERID NOT FOUND'                      
         GOTO1 =V(PRINTER)                                                      
         B     GETUID90                                                         
*                                                                               
GETUID10 L     RF,=A(IO)                                                        
         LA    RF,28(RF)           FIND ID ELEMENT (X'02')                      
GETUID20 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                IT'S NOT THERE                               
         CLI   0(RF),X'02'                                                      
         BE    *+16                                                             
         SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GETUID20                                                         
         MVC   HALF,2(RF)          SENDING USERID                               
*                                                                               
* RETREIVE POWER CODE FOR REP SYSTEM                                            
* RETREIVE SE NUMBER FOR REP SYSTEM                                             
*                                                                               
GETUID40 DS    0H                                                               
         CLI   0(RF),0                                                          
         BE    GETUID90                                                         
         CLI   0(RF),X'06'                                                      
         BE    GETUID60                                                         
         CLI   0(RF),X'21'                                                      
         BE    GETUID80                                                         
GETUID50 SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GETUID40                                                         
*                                                                               
GETUID60 DS    0H                                                               
         MVC   PWRCODE,2(RF)                                                    
         B     GETUID50                                                         
*                                                                               
GETUID80 DS    0H                                                               
         CLI   2(RF),X'08'         REP?                                         
         BNE   GETUID50                                                         
         MVC   RSENUM,3(RF)                                                     
*                                                                               
GETUID90 LA    R5,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R5)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(5),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MAILAGY,DUB         PUT NUMERIC USERID IN DAREMAIL MSG           
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
POSTSTAT NMOD1 0,POSTSTAT                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* WHEN WE GET A DELIVERY NOTIFICATION, MARK THE ORIGINAL EDICT FILE             
* ENTRY WITH STATUS DLVD, AND THE DELIVERY DATE/TIME.  IF WE GET AN             
* ERROR NOTIFICATION, MARK THE REPORT AS CANCELLED.                             
*   UPON ENTRY, BYTE = C'D' = DLNNOT                                            
*               BYTE = C'C' = ERRNOT                                            
*               R3   = A(DLNNOT OR ERRNOT RECORD)                               
*                                                                               
         USING RDLNNOTD,R3                                                      
*                                                                               
         OC    EDICTDA,EDICTDA     DISK ADDRESS IS PRESENT?                     
         BZ    PSX                 NO                                           
*                                                                               
         GOTO1 =V(HEXIN),DMCB,EDICTDA,FULL,8  EDICT FILE DSK/ADDR               
         CLC   =F'4',DMCB+12                                                    
         BE    *+14                                                             
         MVC   P+30(27),=C'RETURNED D/A IS INVALID HEX'                         
         B     CANTPOST                                                         
*                                                                               
         OC    FULL(2),FULL        IF DISK ADDRESS IS INVALID                   
         BNZ   *+14                                                             
         MVC   P+30(34),=C'RETURNED D/A HAS TRACK NUMBER ZERO'                  
         B     CANTPOST                                                         
*                                                                               
         LA    R5,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R5)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(5),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R2,TABLNTRY         ASSUME WE'LL FIND REPORT IN TABLE            
         MVI   BYTE2,C'T'          'T' FOR TABLE                                
*                                                                               
         L     R5,DRRXMTTB         A(TRANSMIT TABLE)                            
         USING XMTTABLD,R4                                                      
         LA    R4,XMTENTRY                                                      
*                                                                               
PS10     MVC31 XMTENTRY,0(R5)                                                   
         CLI   0(R4),0             END OF TABLE?                                
         BNE   *+16                                                             
         L     R2,FULL             YES -- USE EDICT FILE DISK ADDRESS           
         MVI   BYTE2,C'F'          'F' FOR FILE                                 
         B     PS20                                                             
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   *+14                                                             
         CLC   FULL,XMTDSKAD       FIND MATCH ON DISK ADDRESS                   
         BE    *+12                                                             
         AHI   R5,XMTTBLQ          BUMP TO NEXT ENTRY                           
         B     PS10                                                             
         MVC   TABLNTRY,0(R4)      SAVE XMIT TABLE ENTRY                        
         DROP  R4                                                               
*                                                                               
PS20     LA    R5,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R5)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(5),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         GOTO1 =V(HEXIN),DMCB,RDNTDATE+4,FULL,2   DAY NUMBER                    
         CLC   =F'1',DMCB+12                                                    
         BE    *+14                                                             
         MVC   P+30(20),=C'RETURNED DAY INVALID'                                
         B     CANTPOST                                                         
*                                                                               
         GOTO1 =V(HEXIN),DMCB,RDNTTIME,FULL+1,4   TIME DELIVERED                
         CLC   =F'2',DMCB+12                                                    
         BE    *+14                                                             
         MVC   P+30(21),=C'RETURNED TIME INVALID'                               
         B     CANTPOST                                                         
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,DRRXMTTB                                                
         ST    R2,ETBLNTRY                                                      
         MVI   EACTION,EACTDLVQ    ASSUME DELIVERED                             
         CLI   BYTE,C'C'           ERRNOT RECEIVED?                             
         BNE   *+8                                                              
         MVI   EACTION,EACTCANQ    POST CANCELLED                               
         CLI   BYTE2,C'F'          DISK ADDRESS BEING PASSED?                   
         BNE   *+8                                                              
         OI    EACTION,EACTFILQ    YES                                          
         OI    EACTION,EACTDSTQ    UPDATE WITH NEW DESTINATION                  
         MVC   EADEST,=A(RECEIVER)                                              
         MVC   EMAJORNM,DRRMAJNM                                                
         MVC   EDAYNUM,FULL        DELIVERY DAY                                 
         MVC   ETIME,FULL+1        DELIVERY TIME                                
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   CANTPOST            CAN'T UPDATE EDICT FILE                      
         DROP  R1                                                               
*                                                                               
         MVC   P+30(8),EDICTDA     EDICT FILE DISK ADDRESS                      
         PRNT  EDICTFILEPOSTED,PRINT=ALWAYS                                     
         B     PSX                                                              
*                                                                               
CANTPOST MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PSX      XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DRLINRCV NMOD1 0,DRLINRCV                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* RECEIVE RECORDS FROM DARE, AND FILL UP A BUFFER                               
*                                                                               
         MVI   RQSTCONF,C'N'       NO REQUEST FOR CONFIRMATION YET              
         LA    R3,DAREBUF                                                       
         LR    RE,R3                                                            
         LA    RF,DAREBUFL         CLEAR BUFFER                                 
         XCEFL                                                                  
         LA    R5,10               10 RECORDS PER BUFFER MAXIMUM                
*                                                                               
DRRCV10  MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    DRLINXOK                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   DRLINBAD            BAD RETURN CODE                              
*                                                                               
         XC    DRRSTOP,DRRSTOP                                                  
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_DEALLOC_RECEIVED                     
         BNE   DRRCV40                                                          
*                                                                               
         PRNT  GOTSTOPANDCONFIRM,PRINT=ALWAYS                                   
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRMATION                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    DRLINXOK                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                WHY WOULD WE NOT BE ABLE TO CONFIRM?         
*                                                                               
         MVI   OPERSTOP,C'Y'       WE CAN STOP NOW                              
         B     DRLINXOK                                                         
*                                                                               
DRRCV40  CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BNE   DRRCV50                                                          
         MVI   RQSTCONF,C'Y'                                                    
         PRNT  CONFIRMREQUESTED,PRINT=ALWAYS                                    
*                                                                               
DRRCV50  L     R4,RECEIVE_LENGTH                                                
         LTR   R1,R4                                                            
         BZ    DRRCV60             NO DATA RETURNED                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BUFFER      PLACE IN MY BUFFER                           
         AR    R3,R4               POINT TO NEXT POSITION IN BUFFER             
         CLI   TRACEFLG,C'Y'                                                    
         BNE   DRRCV60                                                          
         GOTO1 =V(PRNTBL),DMCB,0,BUFFER,C'DUMP',(R4),=C'1D'                     
*                                                                               
DRRCV60  CLI   RQSTCONF,C'Y'       CONFIRMATION REQUESTED?                      
         BNE   *+12                NO                                           
         MVI   0(R3),ETX           END OF MESSAGE MARKER                        
         B     DRLINXOK                                                         
*                                                                               
         BCT   R5,DRRCV10          GET NEXT LINE                                
*                                                                               
         MVI   0(R3),ETB           END OF BUFFER MARKER                         
         B     DRLINXOK                                                         
*                                                                               
DRLINBAD LTR   RB,RB               SET CC NOT EQUAL                             
         B     DRLINXX                                                          
*                                                                               
DRLINXOK CLI   TRACEFLG,C'Y'                                                    
         BNE   DRLINX                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'DARE INPUT BUFFER',A(DAREBUF),       +        
               C'DUMP',2000,=C'2D'                                              
*                                                                               
DRLINX   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
DRLINXX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* UPON ENTRY, R3 = A(AGYHDR RECORD)                                             
*                                                                               
* EACH TSAR RECORD IS COMPRISED OF:                                             
* CL2 REP CODE                                                                  
* CL5 AGENCY CODE                                                               
* CL5 STATION CALL LETTERS                                                      
* CL3 SALESPERSON CODE                                                          
*                                                                               
* IF STATION CALL LETTERS ARE NULLS, THEN WE WANT TO NOTIFY ALL OF THE          
* SALESPEOPLE LISTED UNDER THAT AGENCY. THESE SALESPEOPLE ALL BELONG            
* TO THE SAME TEAM AS CREATED BY REP/SFM DNA RECORD                             
*                                                                               
REPMAIL  NMOD1 0,REPMAIL                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         CLI   DRREDTYP,C'R'       PROCESS ONLY FOR REPPAK                      
         BNE   RMAILX                                                           
         CLC   MAILAGY,=C'00000'   WAS A USERID FOUND?                          
         BE    RMAILX              NO -- DON'T GENERATE DARE MAIL MSG           
         CLI   RSENUM,0            SE NUMBER FOUND?                             
         BE    RMAILX              NO -- DON'T GENERATE DARE MAIL MSG           
*                                                                               
*&&DB                                                                           
         MVC   P(26),=C'*** PROCESSING REPMAIL ***'                             
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
*                                                                               
* LOOK FOR DARE NOTIFICATION ASSIGNMENT RECORD IN TSAR BUFFER                   
*                                                                               
RMAIL10  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),PWRCODE      FIRST, CHECK IF WE NEED TO NOTIFY            
         MVC   KEY+2(5),RMAGY      THE ENTIRE TEAM OF SALESPEOPLE               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
*&&DB                                                                           
         MVC   P(9),=C'*** HIGH:'                                               
         MVC   P+10(15),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         XC    TSARBLK,TSARBLK                                                  
         XC    PARM,PARM                                                        
         MVC   PARM(4),DATAMGR                                                  
         MVC   PARM+4(4),=V(CALLOFF)                                            
         MVC   PARM+8(4),DRRATSAR                                               
         MVC   PARM+12(4),DRRDNATB                                              
         LA    RE,TSARBLK                                                       
         ST    RE,PARM+16                                                       
         GOTO1 =V(REPPNTAB),DMCB,(3,PARM),KEY                                   
*                                                                               
*&&DB                                                                           
         MVC   P(9),=C'*** RET :'                                               
         MVC   P+10(15),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    RMAIL13                                                          
*                                                                               
* REP DID NOT SETUP ANY DNA RECORDS, NOTIFY ENTIRE COMPANY                      
*                                                                               
         MVC   MAILTIME,RMTIME     MAIL TIME                                    
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
         B     RMAIL100                                                         
*                                                                               
RMAIL13  DS    0H                                                               
         OC    KEY+7(5),KEY+7      AGENCY SET UP FOR TEAM NOTIFY                
         BZ    RMAIL20             SINCE STATION CALLS ARE NULLS                
*                                                                               
RMAIL15  DS    0H                                                               
         MVC   KEY+7(5),RMSTA      AGENCY/STA SET UP FOR SALES NOTIFY           
         XC    KEY+12(3),KEY+12    CLEAR REST OF KEY FOR HIGH READ              
         MVC   KEYSAVE,KEY                                                      
*                                                                               
*&&DB                                                                           
         MVC   P(9),=C'*** HIGH:'                                               
         MVC   P+10(15),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         XC    TSARBLK,TSARBLK                                                  
         XC    PARM,PARM                                                        
         MVC   PARM(4),DATAMGR                                                  
         MVC   PARM+4(4),=V(CALLOFF)                                            
         MVC   PARM+8(4),DRRATSAR                                               
         MVC   PARM+12(4),DRRDNATB                                              
         LA    RE,TSARBLK                                                       
         ST    RE,PARM+16                                                       
         GOTO1 =V(REPPNTAB),DMCB,(3,PARM),KEY                                   
*                                                                               
*&&DB                                                                           
         MVC   P(9),=C'*** RET :'                                               
         MVC   P+10(15),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   RMAIL100                                                         
*                                                                               
RMAIL20  DS    0H                                                               
         MVC   MAILBYER,KEY+12                                                  
*                                                                               
RMAIL40  DS    0H                                                               
         MVC   MAILTIME,RMTIME     MAIL TIME                                    
*                                                                               
* SEND MAIL MESSAGE                                                             
*                                                                               
         GOTO1 =A(SENDMAIL),(RC)   PUT DAREMAIL INFO TO MQ                      
*                                                                               
* GET NEXT RECORD FROM TSAR BUFFER                                              
*                                                                               
*&&DB                                                                           
         MVC   P(8),=C'*** SEQ:'                                                
         MVC   P+10(15),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         XC    PARM,PARM                                                        
         MVC   PARM(4),DATAMGR                                                  
         MVC   PARM+4(4),=V(CALLOFF)                                            
         MVC   PARM+8(4),DRRATSAR                                               
         MVC   PARM+12(4),DRRDNATB                                              
         LA    RE,TSARBLK                                                       
         ST    RE,PARM+16                                                       
         GOTO1 =V(REPPNTAB),DMCB,(4,PARM),KEY                                   
*                                                                               
*&&DB                                                                           
         MVC   P(8),=C'*** RET:'                                                
         MVC   P+10(15),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         CLI   KEY,0                                                            
         BE    RMAIL100                                                         
         CLC   KEY(12),KEYSAVE                                                  
         BE    RMAIL20                                                          
         OC    KEYSAVE+7(5),KEYSAVE+7                                           
         BNZ   RMAIL100            FINISHED TEAM NOTIFY                         
         MVC   KEY,KEYSAVE                                                      
         B     RMAIL15             NOW NOTIFY OTHER SALESPEOPLE                 
*                                                                               
RMAIL100 DS    0H                                                               
*&&DB                                                                           
         MVC   P(25),=C'*** REPMAIL PROCESSED ***'                              
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
*                                                                               
RMAILX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SENDMAIL NMOD1 0,SENDMAIL                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         CLC   MAILAGY,=C'00000'   WAS A USERID FOUND?                          
         BE    SNDMAILX            NO -- DON'T GENERATE DARE MAIL MSG           
*        CLC   MAILBYER,SPACES     BUYER SPECIFIED?                             
*        BE    SNDMAILX            NO -- DON'T GENERATE DARE MAIL MSG           
*        OC    MAILBYER,MAILBYER   BUYER SPECIFIED?                             
*        BZ    SNDMAILX            NO -- DON'T GENERATE DARE MAIL MSG           
*                                                                               
         CLI   DRREDTYP,C'A'       PROCESS ONLY FOR ADV'S                       
         BNE   SMAIL10                                                          
*                                                                               
* CHECK IF PERSONAL NOTIFICATION PROFILE IS TURNED ON FOR THIS USERID           
* DEFAULT IS ON                                                                 
*                                                                               
*&&DB                                                                           
         MVC   P(24),=C'*** RETRIEVING PROFILE :'                               
         MVC   P+25(5),MAILAGY                                                  
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
*                                                                               
* GETPROF USES CTFILE, SO WE'LL NEED TO ENQUEUE THE FOLLOWING PROCESS.          
*                                                                               
         LA    R5,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R5)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(5),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         XC    WORK,WORK                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'A2'           NEED LOWERCASE S                             
         MVC   KEY+1(3),=C'DAR'                                                 
         PACK  DUB,MAILAGY(5)                                                   
         CVB   R0,DUB                                                           
         STCM  R0,3,KEY+4                                                       
*&&DB                                                                           
         MVC   P(24),=C'*** PROFILE KEY ****** :'                               
         MVC   P+25(10),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         GOTO1 =V(GETPROF),DMCB,(X'C0',KEY),WORK,DATAMGR                        
*                                                                               
*&&DB                                                                           
         MVC   P(24),=C'*** WORK        ****** :'                               
         MVC   P+25(16),WORK                                                    
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         LA    R5,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R5)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(5),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         OC    WORK(16),WORK                                                    
         BZ    SMAIL10             NO PROFILE, DEFAULT TO YES                   
*                                                                               
         CLI   WORK+5,C'Y'                                                      
         BE    SMAIL10                                                          
*                                                                               
SMAIL05  XC    MAILBYER,MAILBYER   RECEIVE ALL MAIL FOR AGENCY                  
*&&DB                                                                           
         MVC   P(28),=C'*** DARE MAIL PROFILE: N ***'                           
         B     SMAIL15                                                          
*&&                                                                             
*                                                                               
SMAIL10  DS    0H                                                               
*&&DB                                                                           
         MVC   P(28),=C'*** DARE MAIL PROFILE: Y ***'                           
SMAIL15  GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         L     RF,DRRMQQNM                    A(FACPAK CONTROL QUEUE)           
         MVC   OBJDESC_OBJECTNAME,0(RF)       QUEUE NAME                        
*                                                                               
         LA    RF,MQPMO_NO_SYNCPOINT                                            
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
*                                                                               
* INSERT MAIL TYPE                                                              
*                                                                               
         MVI   MAILTYPE,C'?'                                                    
         LA    R4,DRMLTYPE                                                      
*                                                                               
SMAIL20  DS    0H                                                               
         CLC   RMTYPE,0(R4)                                                     
         BE    SMAIL30                                                          
         LA    R4,L'DRMLTYPE(R4)                                                
         CLI   0(R4),X'FF'                                                      
         BNE   SMAIL20                                                          
         B     SMAIL40                                                          
*                                                                               
SMAIL30  DS    0H                                                               
         MVC   MAILTYPE,6(R4)                                                   
*                                                                               
SMAIL40  DS    0H                                                               
         CLI   DRREDTYP,C'A'       PROCESS ONLY FOR ADV'S                       
         BNE   SMAIL50                                                          
         MVC   MAILAGY(3),=C'***'  WANT TO BE NOTIFIED BY POWER CODE            
         MVC   MAILAGY+3(2),PWRCODE  INSTEAD OF USER ID NUMBER                  
*                                                                               
SMAIL50  DS    0H                                                               
*                                                                               
* NEED TO USE CURRENT TIME INSTEAD OF THE ORIGINATING TRANSMISSION              
* TIME. IT IS POSSIBLE THAT ITEMS WILL NOT BE PROCESSED UNTIL THE NEXT          
* MORNING. USING THE ORIGINAL TRANSMISSION TIME WOULD BE THEREFORE BE           
* INCORRECT.                                                                    
*                                                                               
         GOTO1 =A(GETHHMM),(RC)    OVERRIDE MAILTIME AND INSERT                 
*                                  CURRENT TIME                                 
*                                                                               
         MVC   MQBUFFER(MAILMSGL),MAILMSG                                       
         MVC   BUFFERLENGTH,=A(MAILMSGL)                                        
         MVC   P+30(MAILMSGL),MQBUFFER                                          
         PRNT  DAREMAIL_MESSAGE                                                 
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     PUT THE MESSAGE TO THE MQ QUEUE              
*                                                                               
         LA    R2,CSQBDISC_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     DISCONNECT FROM MQ QUEUE MANAGER             
*                                                                               
SNDMAILX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DATETIME NMOD1 0,DATETIME                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   HHMMSS,PRNTTIME                                                  
         GOTO1 =V(DATCON),DMCB,(5,0),(20,YYYYMMDD)                              
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETHHMM: INSERT HHMM IN MILITARY TIME IN MAILTIME                             
***********************************************************************         
GETHHMM  NMOD1 0,GETHHMM                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,WORK,MAILTIME,2,=C'TOG'                          
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PRNTATTB NMOD1 0,PRNTATTB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  GETATTRIBUTES,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   GET CONVERSATION ATTRIBUTES                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
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
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CALLAPPC NMOD1 0,CALLAPPC                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
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
         TM    20(R2),X'80'        ASYNCHRONOUS CALL?                           
         BZ    CALL10              YES, SO WE MUST WAIT                         
*                                                                               
         L     RF,ASTOPECB         SYNCHRONOUS CALL, SO IT'S DONE               
         TM    0(RF),X'40'         STOP?                                        
         BZ    CALL30              NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     CALL30                                                           
*                                                                               
CALL10   CLC   RETURN_CODE,ATB_OK                                               
         BE    CALL20              APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         TM    20(R2),X'40'        CAN CALL ERROR_EXTRACT NOW?                  
         BZ    CALL15                                                           
         GOTO1 =A(APPCERR),(RC)    YES - DO IT                                  
CALL15   DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALL20   WAIT  1,ECBLIST=ECBLST    WAIT FOR COMPLETION OR OPERATOR              
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    *+18                NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     CALL30                                                           
*                                                                               
         TM    APPCECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPCECB                                              
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPCECB,APPCECB                                                  
*                                                                               
CALL30   MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CALLX                                                            
         TM    20(R2),X'40'        CAN CALL ERROR_EXTRACT NOW?                  
         BZ    CALLX                                                            
         GOTO1 =A(APPCERR),(RC)    YES - DO IT                                  
*                                                                               
CALLX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CALLMQ   NMOD1 0,CALLMQ                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
*                                                                               
         CLI   DRRMAIL,C'Y'        DAREMAIL DISABLED VIA PARAMETERS?            
         BNE   CALLMQX             YES -- DON'T DO ANY MQ CALLS                 
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    *+14                NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
*                                                                               
         MVI   P,C'+'              '+' MEANS IT'S AN MQ CALL                    
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+30(12),=C'COMPLETED OK'                                        
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    CALLMQ20                                                         
         MVC   P+30(12),=C'WARNING     '                                        
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BE    CALLMQ10                                                         
         MVC   P+30(12),=C'** FAILED **'                                        
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         BE    CALLMQ10                                                         
         DC    H'0'                UNKNOWN COMPLETION CODE                      
*                                                                               
CALLMQ10 MVC   P+50(9),=C'REASON = '                                            
         EDIT  REASON,(5,P+59),ALIGN=LEFT,ZERO=NOBLANK                          
         MVC   P+66(13),=C'*** ERROR ***'                                       
*                                                                               
CALLMQ20 PRNT                                                                   
*                                                                               
CALLMQX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* DISABLED (2/6/09, SKUI)                                                       
*&&DO                                                                           
CCMOMQ   NMOD1 0,CCMOMQ                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* SPECIAL ROUTINE TO CARBON COPY EDI'S TO MEDIAOCEAN VIA MQ                     
* UPON ENTRY,  PARM1, BYTE 0 HAS FLAG FOR MQ ACTION                             
*                                                                               
         CLI   DRRMQMO,C'Y'                                                     
         BNE   CCMOMQX                                                          
         CLI   DRREDTYP,C'R'       DO THIS ONLY FOR REPPAK                      
         BNE   CCMOMQX                                                          
         CLI   MQFLAG,C'Y'                                                      
         BE    CCMOMQ05                                                         
         MVI   MQPUTBYT,X'A0'                                                   
         LA    R2,MOMQNAME                                                      
*        LA    R2,TESTMQ                                                        
         CLC   =C'ROBAT',USERID                                                 
         BNE   CCMOMQ02                                                         
         OI    MQPUTBYT,X'01'      USE TEST BROKER                              
         B     CCMOMQ03                                                         
*                                                                               
CCMOMQ02 CLC   =C'ETVNY',USERID                                                 
         BNE   CCMOMQX                                                          
CCMOMQ03 MVI   MQFLAG,C'Y'                                                      
*                                                                               
CCMOMQ05 DS    0H                                                               
         CLI   MQACTION,C'O'       OPEN?                                        
         BE    CCMOMQ10                                                         
         CLI   MQACTION,C'P'       PUT ?                                        
         BE    CCMOMQ40                                                         
         CLI   MQACTION,C'C'       CLOSE?                                       
         BE    CCMOMQ80                                                         
         B     CCMOMQX                                                          
*                                                                               
CCMOMQ10 DS    0H                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'OPEN'),(0,(R2)),(MQPUTBYT,0),0              
         PRNT  OPENMQTOMO,PRINT=ALWAYS                                          
         B     CCMOMQX                                                          
*                                                                               
CCMOMQ40 DS    0H                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'PUT'),(R4),((R5)),0                         
*        GOTO1 =V(PRNTBL),DMCB,=C'SENDING TO MQ BUFFER',(R4),          +        
               C'DUMP',(R5),=C'1D'                                              
         B     CCMOMQX                                                          
*                                                                               
CCMOMQ80 DS    0H                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'CLOSE'),0,0,0                               
         PRNT  CLOSEMQTOMO,PRINT=ALWAYS                                         
         MVI   MQFLAG,C'N'                                                      
*                                                                               
CCMOMQX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
APPCERR  NMOD1 0,APPCERR                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PERFORM APPC ERROR_EXTRACT FUNCTION.                                          
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
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         SPACE 2                                                                
         ATBSERV                                                                
         SPACE 2                                                                
         CMQA LIST=YES,EQUONLY=NO                                               
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
RETURN_CONTROL                 DS    F                                          
SYNC_LEVEL                     DS    F                                          
SECURITY_TYPE                  DS    F                                          
USER_ID                        DS    CL10                                       
PASSWORD                       DS    CL10                                       
PROFILE                        DC    CL10' '                                    
USER_TOKEN                     DC    XL80'00'                                   
LUW_ID                         DS    XL26                                       
CONVERSATION_ID                DS    CL8                                        
CONVERSATION_CORRELATOR        DS    CL8                                        
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY_TYPE         
TPID                           DC    XL8'00'                                    
RETURN_CODE                    DS    F                                          
FILL                           DS    F                                          
CONVERSATION_STATE             DS    F                                          
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DC    F'0'                                       
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
REQUEST_TO_SEND_VALUE          DS    F                                          
SEND_TYPE                      DS    F                                          
SEND_LENGTH                    DS    F                                          
SEND_ACCESS_TOKEN              DC    F'0'                                       
PREP_TO_RECEIVE_TYPE           DS    F                                          
LOCKS                          DS    F                                          
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
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME     DS      CL48                  NAME OF QUEUE MANAGER                
HCONN        DS      F                     CONNECTION HANDLE                    
HOBJ         DS      F                     OBJECT HANDLE                        
COMPCODE     DS      F                     COMPLETION CODE                      
REASON       DS      F                     QUALIFIES COMPLETION CODE            
BUFFERLENGTH DS      F                     LENGTH OF DATA IN MQBUFFER           
MQBUFFER     DS      CL256                                                      
OBJDESC      CMQODA  DSECT=NO,LIST=YES     OBJECT DESCRIPTOR                    
MSGDESC      CMQMDA  DSECT=NO,LIST=YES     MESSAGE DESCRIPTOR                   
PUTMSGOPTS   CMQPMOA DSECT=NO,LIST=YES     PUT MESSAGE OPTIONS                  
         EJECT                                                                  
CRLF     DS    0XL2                                                             
CR       DC    X'0D'                                                            
LF       DC    X'25'                                                            
ETB      EQU   X'26'                                                            
ETX      EQU   X'03'                                                            
         SPACE 2                                                                
APPCM1   DC    C'+++DARE SENDER=EDX CONV=RECEIVE VERSION=1 DATE=YYYYMMD+        
               D TIME=HHMMSS'                                                   
APPCM1LQ EQU   *-APPCM1                                                         
         SPACE 2                                                                
APPCM2   DC    C'+++DARE SENDER=DDS VERSION=1 STATUS=READY '                    
APPCM2LQ EQU   *-APPCM2                                                         
APPCM2A  DC    C'DATE=YYYYMMDD TIME=HHMMSS'                                     
APPCM2L2 EQU   *-APPCM2                                                         
         SPACE 2                                                                
TRINBND  DS    0XL256              CHECK OUT ANY NON-PRINTING CHAR              
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00-0F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10-1F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20-2F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30-3F                        
         DC    X'00FFFFFFFFFFFFFFFFFF000000000000' 40-4F                        
         DC    X'00FFFFFFFFFFFFFFFFFF000000000000' 50-5F                        
         DC    X'0000FFFFFFFFFFFFFFFF000000000000' 60-6F                        
         DC    X'FFFFFFFFFFFFFFFFFFFF000000000000' 70-7F                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' 80-8F                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' 90-9F                        
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' A0-AF                        
         DC    X'00000000000000000000FFFFFFFFFFFF' B0-BF                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' C0-CF                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' D0-DF                        
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E0-EF                        
         DC    X'00000000000000000000FFFFFFFFFFFF' F0-FF                        
         EJECT                                                                  
DMCB     DS    10F                                                              
PARM     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
EDITDUB  DS    D                   FOR EDIT MACRO                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
DATAMGR  DS    A                   A(DATAMGR)                                   
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
EDITWRK  DS    CL17                FOR EDIT MACRO                               
YYYYMMDD DS    CL8                 DATE                                         
HHMMSS   DS    CL6                 TIME                                         
EDICTREC DS    XL256               FOR BUILDING EDICT FILE RECORD               
WORK     DS    CL256                                                            
KEY      DS    CL32                FOR READS                                    
KEYSAVE  DS    CL32                                                             
         SPACE 2                                                                
ECBLST   DS    0F                                                               
ASTOPECB DC    X'00',AL3(0)        A(STOPECB)                                   
AAPPCECB DC    X'80',AL3(APPCECB)  A(APPCECB)                                   
         SPACE 2                                                                
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE ENTRY                       
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR XMIT TABLE ENTRY            
TODAY    DS    CL6                 EBCDIC DATE TODAY (YYMMDD)                   
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
RECCOUNT DS    F                   MESSAGE RECORD COUNTER                       
RECEIVER DC    CL16' '             RECEIVING REP-ID                             
PRTQNAME DC    C'PRTQU'                                                         
USERID   DS    CL10                ALPHA USERID                                 
RSENUM   DS    X                   REP FILE SE NUMBER                           
PWRCODE  DS    CL2                 REP POWER CODE                               
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
TRACEFLG DS    C                   'Y' = PRINT DETAILED TRACE                   
RQSTCONF DS    C                   'Y' = WE GOT REQUEST FOR CONFIRM             
MQACTION DS    C                                                                
MQFLAG   DC    C'N'                'Y' TO SEND DARE EDI COPY VIA MQ             
MQPUTBYT DS    X                   P3 B0 OF MQRPT PUT COMMAND                   
*                                                                               
MOMQNAME DC    C'MEDIAOCEANEDI***'                                              
TESTMQ   DC    C'DDSTESTINGQUEUE*'                                              
*                                                                               
SVPQKEY  DS    0CL7                SAVED PQKEY                                  
SVPQUID  DS    XL2                 SAVED PQ USERID                              
SVPQSID  DS    CL3                 SAVED PQ SUBID                               
SVPQRNO  DS    XL2                 SAVED PQ REPORT SEQ#                         
*                                                                               
MAILMSG  DS    0C                  MQ DARE MAIL NOTIFICATION MESSAGE            
         DC    C'DAREMSG1'         DDMQIO LOOKS FOR THIS HEADER                 
MAILAGY  DS    CL5                 NUMERIC USERID                               
MAILBYER DS    CL3                 BUYER CODE                                   
MAILTIME DS    CL4                 MILITARY DELIVERY TIME (HHMM)                
MAILTYPE DS    C                   TYPE CODE                                    
MAILMSGL EQU   *-MAILMSG                                                        
*                                                                               
DAREREC  DS    CL(L'EDFDARE)       SAVE FIRST DARE RECORD IN MESSAGE            
EDICTDA  DS    CL8                 EDICT FILE DISK ADDRESS                      
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
*                                                                               
TSARBLK  DS    XL(TSARDL)                                                       
RMSTA    DS    CL5                 REPMAIL STATION                              
RMAGY    DS    CL5                 REPMAIL AGENCY CODE                          
RMTIME   DS    CL4                 REPMAIL TIME                                 
RMTYPE   DS    CL6                 REPMAIL TYPE                                 
         EJECT                                                                  
* PARAMETER LISTS FOR APPC/MVS AND MQSERIES CALLS                               
*                                                                               
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*       X'80': APPC/MVS ROUTINE IS SYNCHRONOUS ONLY                             
*       X'40': APPC/MVS ROUTINE CAN BE FOLLOWED BY EXTRACT_ERROR CALL           
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
ATBPTR_STRUCTURE          DS    A                                               
                          DC    CL16'PREP_TO_RECEIVE'                           
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(PREP_TO_RECEIVE_TYPE)                 
                          DC    X'00',AL3(LOCKS)                                
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRCVW_STRUCTURE         DS    A                                               
                          DC    CL16'RECEIVE_AND_WAIT'                          
                          DC    X'40'                                           
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
ATBCFMD_STRUCTURE         DS    A                                               
                          DC    CL16'ISSUE_CONFIRM'                             
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
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
*                                                                               
CSQBCONN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CONNECT'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(QMGRNAME)                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_DISCONNECT'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT1_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_PUT1'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC)                              
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(PUTMSGOPTS)                           
                          DC    X'00',AL3(BUFFERLENGTH)                         
                          DC    X'00',AL3(MQBUFFER)                             
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
         SPACE 3                                                                
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBALC2  DC    CL8'ATBALC2'                                                     
         DC    XL52'00'                                                         
ATBCFMD  DC    CL8'ATBCFMD'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBPTR   DC    CL8'ATBPTR'                                                      
         DC    XL52'00'                                                         
ATBRCVW  DC    CL8'ATBRCVW'                                                     
         DC    XL52'00'                                                         
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBPUT1 DC    CL8'CSQBPUT1'                                                    
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
* DARE MAIL TYPES                                                               
       ++INCLUDE DRMAILTYP                                                      
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*DAREBUF'                                                      
DAREBUF  DS    (DAREBUFL)X'00'     DARE LINE BUFFER                             
DAREBUFL EQU   3000                                                             
*                                                                               
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE C/I BUFFER                       
*                                                                               
         DS    0D                                                               
         DC    C'**I/O***'                                                      
IO       DC    1024X'00'           CTFILE BUFFER                                
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         SPACE 3                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE SPDARMKGDD                                                     
         EJECT                                                                  
       ++INCLUDE REGENDNA                                                       
         EJECT                                                                  
* DMGREQUS                                                                      
* DMPRTQL                                                                       
* DMPRTQK                                                                       
* DDDPRINT                                                                      
* DDTSARD                                                                       
       ++INCLUDE DMGREQUS                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039DDEDIDARR 08/08/13'                                      
         END                                                                    
