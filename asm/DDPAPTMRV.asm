*          DATA SET DDPAPTMRV  AT LEVEL 034 AS OF 07/15/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTMRVA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DATCON                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE SMTP                                                                   
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'CONFIRM PANAPT MR ELIGIBILITY FOR SELECTION'                    
PAPTMRV  CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,PAPTMRV,=V(REGSAVE),R9                                         
*                                                                               
         LR    R1,RC                                                            
         AHI   R1,-4                                                            
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)          L'PARM STRING                                
         JZ    *+2                 PARM ABSENT ?!?                              
*                                                                               
* PARAMETER LIST:                                                               
*   1. SUBMITTER'S TSO USERID                                                   
*   2. "I" (CALLED FROM "IN" JOB), OR "P" (CALLED BY PAPTSTC SUBTASK)           
*   3. 3-BYTE ENVIRONMENT (SBX / DDS)                                           
*   4. ITMF TICKET SUBMITTER'S TSO USERID                                       
*                                                                               
         LA    R1,2(R1)            BUMP PAST L'PARAMETER LIST                   
         LA    RE,SUBMITTR         SAVE SUBMITTER'S TSO USERID                  
BLDSUBID DS    0H                                                               
         CLI   0(R1),C' '          END OF USERID?                               
         BNE   *+14                NO                                           
         MVC   WHENFLAG,1(R1)      YES: SAVE 2ND PARAMETER                      
         B     BLDITMF                                                          
*                                                                               
         MVC   0(1,RE),0(R1)       BUILD USERID                                 
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,BLDSUBID         STOP IF SECOND PARM IS ABSENT                
         J     *+2                 FATAL: 2ND PARAMETER IS MISSING              
*                                                                               
BLDITMF  DS    0H                                                               
         CHI   RF,2                3RD PARAMETER IS PRESENT?                    
         JNH   *+2                 NO: FATAL                                    
*                                                                               
         MVC   ENV,3(R1)           SAVE 3RD PARAMETER                           
*                                                                               
         LA    R1,7(R1)            POINT TO 4TH PARAMETER                       
         SHI   RF,7                                                             
         JNP   *+2                 FATAL: 4TH PARAMETER IS MISSING              
         LA    RE,ITMFWHO          BUILD ITMF TICKET SUBMITTER'S USERID         
BLDITMF5 DS    0H                                                               
         CLI   0(R1),C' '          END OF USERID?                               
         BE    MAIN05              YES                                          
         MVC   0(1,RE),0(R1)       BUILD USERID                                 
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,BLDITMF5         DON'T LOOK BEYOND END OF PARM                
*                                                                               
MAIN05   DS    0H                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         MVC   TITLE,=CL60'PANAPT - CONFIRM MR ELIGIBILITY FOR SELECTIO+        
               N'                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OPEN  MRFILE                                                           
         LTR   RF,RF                                                            
         JNZ   *+2                 CAN'T OPEN MR EXTRACT FILE ?!?               
*                                                                               
         LA    R5,MRREC+4          INPUT BUFFER                                 
         USING MMMDES,R5                                                        
         GET   MRFILE,MRREC                                                     
         CLC   DESTYPE,=C'01'      FIRST RECORD MUST BE MR DESCRIPTION          
         JNE   *+2                 IT ISN'T ?!?                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MRREC          RECORD LENGTH (FROM RDW)                     
         GOTO1 =V(PRNTBL),DMCB,=C'MOVE REQUEST "01" DESCRIPTION RECORD'+        
               ,(R5),C'DUMP',(R0),=C'1D',(C'P',V(PRINT))                        
*                                                                               
         IF (CLI,WHENFLAG,EQ,C'I') IF WE WERE CALLED BY APIK200:                
           BRAS  RE,GETITMF          GET EXTRACTED JIRA TICKET FIELDS           
           IF (CLC,=C'ITMF-EMERGENCY',NE,JIRA_ITMFTKT),AND,                     
              (CLC,JIRA_ITMFTKT,NE,SPACES),AND,                                 
              (CLC,JIRARSP#,NE,=F'200')                                         
*                                    HANDLE EXCEPTION CASES                     
             IF (CLC,JIRARSP#,EQ,=F'404')                                       
               LA   R3,JIRA_ISSUE_NOT_FOUND                                     
               MVC  JIRA_ISSUE_NOT_FOUND_ISSUE#,JIRA_ITMFTKT                    
               MVC  RETCODE,=F'8'       FATAL ERROR                             
               BAS  RE,SNDEMAIL         SEND NOTIFICATION E-MAIL                
               B    CLOSE               *** UNSTRUCTURED BRANCH OUT ***         
             ELSEIF (CLC,JIRARSP#,EQ,=F'550')                                   
               LA   R3,JIRA_SERVER_UNAVAILABLE_MSG                              
               MVC  RETCODE,=F'4'       WARNING                                 
             ELSEIF (CLC,JIRARSP#,EQ,=F'-2')                                    
               LA   R3,JIRA_UNEXPECTED_RESPONSE_MSG                             
               MVC  RETCODE,=F'4'       WARNING                                 
             ELSEIF (CLC,JIRARSP#,EQ,=F'-1')                                    
               LA   R3,JIRA_NO_RESPONSE_MSG                                     
               MVC  RETCODE,=F'4'       WARNING                                 
             ENDIF ,                                                            
           ENDIF ,                                                              
         ENDIF ,                                                                
*                                                                               
         L     R1,FLCCVT-PSA(,0)      A(CVT)                                    
         LA    R1,CVTSNAME-CVT(,R1)   SYSTEM ID (E.G., 'SY7     ')              
         MVC   LPAR,0(R1)             SAVE THE LPAR                             
*&&DO                                                                           
* ENABLE THIS CODE IF WE WANT TO PROHIBIT MOVE REQUESTS FROM BEING              
* SUBMITTED FROM A PARTICULAR LPAR.                                             
*                                                                               
         CLI   WHENFLAG,C'P'       WERE WE CALLED BY PAPTSTC?                   
         BE    CHKMTYP             YES: DON'T BOTHER CHECKING THE LPAR          
*                                                                               
*&&US*&& CLC   =C'SYT ',LPAR       MR SUBMITTED FROM A TEST LPAR?               
*&&UK*&& CLC   =C'SY2 ',LPAR                                                    
         BNE   CHKMTYP             NO: OKAY                                     
*                                                                               
         MVC   RETCODE,=F'8'       FATAL ERROR                                  
         LA    R3,BADLPAR          SUBMITTED FROM INVALID LPAR                  
         BAS   RE,SNDEMAIL         SEND NOTIFICATION EMAIL VIA SMTP             
         B     CLOSE                                                            
*                                                                               
CHKMTYP  DS    0H                                                               
*&&                                                                             
         IF (CLC,=C'ITMF-EMERGENCY',NE,JIRA_ITMFTKT),AND,                       
            (CLC,JIRA_ITMFTKT,NE,SPACES),AND,                                   
            (CLC,JIRARSP#,EQ,=F'200')                                           
*                                                                               
           IF (CLC,JIRA_MR#,NE,DESNUMB)  ENSURE MR# MATCHES                     
             LA   R3,JIRA_MR#_MISMATCH                                          
             MVC  JIRA_MR#_MISMATCH_PANAPT,DESNUMB                              
             MVC  JIRA_MR#_MISMATCH_ITMF,JIRA_MR#                               
             MVC  RETCODE,=F'8'       FATAL ERROR                               
             BAS  RE,SNDEMAIL         SEND NOTIFICATION E-MAIL                  
             B    CLOSE               *** UNSTRUCTURED BRANCH OUT ***           
           ENDIF ,                                                              
*                                                                               
           MVC  MR_DIRECTION_LEVEL_MISMATCH_ITMF,JIRA_LEVEL  JUST...            
           MVC  MR_DIRECTION_LEVEL_MISMATCH_STATUS,DESSTAT   ...IN CASE         
*                                                                               
           SELECT CLC,JIRA_DIRECTION,EQ                                         
             WHEN (=CL10'PROMOTE')                                              
               IF (CLC,JIRA_LEVEL,NE,DESCLSN),OR,       CHECK LVL MATCH         
                  (CLC,DESCLSTA,NE,=AL2(DESSTAPP)),AND, APP. FOR MOVE           
                  (CLC,DESCLSTA,NE,=AL2(DESSTSLM))      SEL. FOR MOVE           
                 LA   R3,MR_DIRECTION_LEVEL_MISMATCH                            
                 MVC  MR_DIRECTION,=C'Promotion to'                             
                 MVC  RETCODE,=F'8'     FATAL ERROR                             
                 BAS  RE,SNDEMAIL       SEND NOTIFICATION E-MAIL                
                 B    CLOSE             *** UNSTRUCTURED BRANCH OUT ***         
               ENDIF ,                                                          
             WHEN (=CL10'BACKOUT')                                              
               IF (CLC,JIRA_LEVEL,NE,DESCLSN),OR,       CHECK LVL MATCH         
                  (CLC,DESCLSTA,NE,=AL2(DESSTAPB)),AND, APP. FOR BKOT           
                  (CLC,DESCLSTA,NE,=AL2(DESSTSLB))      SEL. FOR BKOT           
                 LA   R3,MR_DIRECTION_LEVEL_MISMATCH                            
                 MVC  MR_DIRECTION,=C'Backout from'                             
                 MVC  RETCODE,=F'8'     FATAL ERROR                             
                 BAS  RE,SNDEMAIL       SEND NOTIFICATION E-MAIL                
                B    CLOSE              *** UNSTRUCTURED BRANCH OUT ***         
               ENDIF ,                                                          
             OTHRWISE                                                           
               J *+2                    BAD "DIRECTION" FROM JIRA ?!?           
           ENDSEL ,                                                             
*                                                                               
         ENDIF ,                                                                
*                                                                               
         CLI   DESMTYPE,C'M'       THE CURRENTLY-PREFERRED MOVE TYPE            
         BE    CHKAUTH                                                          
         CLI   DESMTYPE,C'T'       TEMPORARY MR (E.G., FOR FILE FIXES)          
         BE    CHKAUTH                                                          
*                                                                               
         MVC   BADMRTYP,DESMTYPE   INVALID MOVE TYPE                            
         MVC   RETCODE,=F'8'       FATAL ERROR                                  
         LA    R3,BADMSGMT                                                      
         BAS   RE,SNDEMAIL         SEND NOTIFICATION EMAIL VIA SMTP             
         B     CLOSE                                                            
*                                                                               
CHKAUTH  DS    0H                                                               
         CLC   ENV,=C'SBX'         IN THE SANDBOX, ANYONE CAN "SUB"             
         BE    CHKSTAT                                                          
*&&US*&& CLC   =C'SYT ',LPAR       MR SUBMITTED FROM A TEST LPAR?               
*&&UK*&& CLC   =C'SY2 ',LPAR                                                    
         BE    CHKSTAT             YES: ANYONE CAN "SUB"                        
*                                                                               
         LOAD  EP=OPERLST          LIST OF COMPUTER ROOM OPERATOR IDS           
         LTR   RF,RF                                                            
         JNZ   *+2                 CAN'T LOAD THE LOAD MODULE ?!?               
         LR    R6,R0               R6: A(LIST OF AUTHORIZED SUBMITTERS)         
*                                                                               
         USING OPERLSTD,R6                                                      
         LH    RE,0(,R6)           L'ENTRY                                      
         L     RF,2(,R6)           A(LAST BYTE OF TABLE)                        
         LA    R6,6(,R6)           A(FIRST ENTRY)                               
CHKAUTH5 DS    0H                                                               
         CLC   SUBMITTR,OPRUSRID   USER IS ALLOWED TO ISSUE A "SUB"?            
         BE    CHKSTAT             YES: NOW CHECK THE MR STATUS                 
         BXLE  R6,RE,CHKAUTH5      TRY NEXT TABLE ENTRY                         
         DROP  R6                                                               
*                                                                               
         MVC   RETCODE,=F'8'       FATAL ERROR                                  
         LA    R3,NOTAUTH          SUBMITTER IS NOT AUTHORIZED TO SUB           
         MVI   CC_OPS,C'N'         NO NEED TO CC THE COMPUTER ROOM              
         BAS   RE,SNDEMAIL         SEND NOTIFICATION EMAIL VIA SMTP             
         B     CLOSE                                                            
*                                                                               
CHKSTAT  DS    0H                                                               
         CLC   DESCLSTA,=AL2(DESSTAPP)  MR IS APPROVED FOR PROMOTION?           
         BE    CHKDATE                  YES: NOW CHECK THE MOVE DATE            
         CLC   DESCLSTA,=AL2(DESSTSLM)  MR IS SELECTED FOR PROMOTION?           
         BE    CHKDATE                  YES: NOW CHECK THE MOVE DATE            
         CLC   DESCLSTA,=AL2(DESSTAPB)  MR IS APPROVED FOR BACKOUT?             
         BE    CLOSE                    YES: NOTHING ELSE TO CHECK              
         CLC   DESCLSTA,=AL2(DESSTSLB)  MR IS SELECTED FOR BACKOUT?             
         BE    CLOSE                    YES: NOTHING ELSE TO CHECK              
*                                                                               
         MVC   BADSTAT,DESSTAT     INELIGIBLE MR STATUS                         
         MVC   RETCODE,=F'8'       FATAL ERROR                                  
         LA    R3,BADMSGST                                                      
         BAS   RE,SNDEMAIL         SEND NOTIFICATION EMAIL VIA SMTP             
         B     CLOSE                                                            
*                                                                               
CHKDATE  DS    0H                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY) TODAY'S DATE: YYYYMMDD          
         CLC   DESCLSN,=C'PROD'    MOVE TO PROD?                                
         BNE   NEXTDATE            NO: MOVE IS TO A LOWER LEVEL                 
*                                                                               
         CLC   TODAY,DESFMVDA      ELIGIBLE TO PROMOTE TO PROD?                 
         BNL   CHKVERAP            YES                                          
*                                                                               
         MVC   BADFMDTC,DESMOVCC   'FINAL MOVE DATE' IS AFTER TODAY             
         MVC   BADFMDTY,DESMOVYY                                                
         MVC   BADFMDTM,DESMOVMM                                                
         MVC   BADFMDTD,DESMOVDD                                                
         MVC   RETCODE,=F'8'       FATAL ERROR                                  
         LA    R3,BADMSGFD                                                      
         BAS   RE,SNDEMAIL         SEND NOTIFICATION EMAIL VIA SMTP             
         B     CLOSE                                                            
*                                                                               
NEXTDATE DS    0H                                                               
         CLC   TODAY,DESNXMDA      ELIGIBLE TO PROMOTE TO NEXT LEVEL?           
         BNL   CHKVERAP            YES                                          
*                                                                               
         MVC   BADNMDTC,DESNXMCC   'NEXT MOVE DATE' IS AFTER TODAY              
         MVC   BADNMDTY,DESNXMYY                                                
         MVC   BADNMDTM,DESNXMMM                                                
         MVC   BADNMDTD,DESNXMDD                                                
         MVC   RETCODE,=F'8'       FATAL ERROR                                  
         LA    R3,BADMSGND                                                      
         BAS   RE,SNDEMAIL         SEND NOTIFICATION EMAIL VIA SMTP             
         B     CLOSE                                                            
*                                                                               
CHKVERAP DS    0H                                                               
*                                                                               
* ENSURE THAT ALL REQUIRED VERIFICATIONS AND APPROVALS FOR THE CURRENT          
* LEVEL HAVE BEEN GRANTED. THIS IS TO PROTECT AGAINST A PANAPT BASE             
* PRODUCT EXPOSURE, IN WHICH IT IS POSSIBLE TO USE THE STA(TUS) COMMAND         
* TO STATUS A MOVE REQUEST DOWNWARD TO "APPROVED FOR" A LOWER LEVEL,            
* AND THEREBY BYPASS REQUIRED VERIFICATIONS AND/OR APPROVALS.                   
*                                                                               
         CLC   DESCLVNF(20),DESCLVSF  ALL VERIFICATIONS OBTAINED?               
         BNE   *+14                   NO                                        
         CLC   DESCLANF(20),DESCLASF  YES: APPROVALS OBTAINED AS WELL?          
         BE    CLOSE                  YES: MR IS OKAY                           
*                                                                               
         MVC   RETCODE,=F'8'       FATAL ERROR                                  
         LA    R3,MISVERAP                                                      
         BAS   RE,SNDEMAIL         SEND NOTIFICATION EMAIL VIA SMTP             
*                                                                               
CLOSE    DS    0H                                                               
         CLOSE MRFILE                                                           
*                                                                               
         CLC   RETCODE,=F'4'       MUST GENERATE A WARNING E-MAIL?              
         BNE   *+8                                                              
         BAS   RE,SNDEMAIL         YES: SEND EMAIL VIA SMTP                     
*                                                                               
         MVI   P,0                 ASSUME MR IS ELIGIBLE FOR SELECTION          
         GOTO1 =V(PRINTER)                                                      
         MVC   P(46),=C'MOVE REQUEST NNNNNN IS ELIGIBLE FOR SELECTION.'         
         MVC   P+13(6),DESNUMB     PRINT MR#                                    
         CLC   RETCODE,=F'8'       IS THE MR ELIGIBLE FOR PROMOTION?            
         BNE   *+16                YES                                          
         MVC   P(58),=C'**ERROR** MOVE REQUEST NNNNNN IS INELIGIBLE FOR+        
                SELECTION.'                                                     
         MVC   P+23(6),DESNUMB     PRINT MR#                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     RF,RETCODE          RETURN CODE                                  
         XBASE RC=(RF)                                                          
         EJECT                                                                  
SNDEMAIL NTR1                                                                   
*                                                                               
* UPON ENTRY, R3 MUST POINT TO AN ERROR MESSAGE BLOCK.                          
*                                                                               
         CLI   WHENFLAG,C'P'       WERE WE CALLED BY PAPTSTC?                   
         BE    XIT                 YES: DON'T SEND AN E-MAIL                    
*                                   (USER HIT "SUB" TWICE BY MISTAKE)           
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAINI',0)    ATTACH AND INIT. JESMAIL         
*                                                                               
         IF    (CLC,RETCODE,EQ,=F'8')  SET E-MAIL SUBJECT BASED ON RC           
           MVC   SBJHDR,FATALHDR       FATAL ERROR                              
         ELSE  ,                       RETURN CODE MIGHT = 4                    
           MVC   SBJHDR,WARNHDR        WARNING                                  
         ENDIF ,                                                                
*                                                                               
         MVC   SBJMR#,DESNUMB      PUT MR# IN EMAIL SUBJECT                     
*                                                                               
         MVC   WORK,SPACES         BUILD RECIPIENT LIST IN WORK                 
         LA    RF,WORK                                                          
         LA    RE,ITMFWHO          ITMF TICKET SUBMITTER'S USERID               
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&UK                                                                           
         MVC   0(4,RF),=C'DDLO'                                                 
         LA    RF,4(RF)                                                         
*&&                                                                             
         MVI   0(RF),C','          DELIMITER BETWEEN RECIPIENTS                 
         LA    RF,1(RF)                                                         
*                                                                               
         CLI   CC_OPS,C'Y'                                                      
         BNE   SNDEM05                                                          
         CLC   ENV,=C'SBX'                                                      
         BE    SNDEM05             DON'T CC OPS FOR SANDBOX MR'S                
*&&US                                                                           
         MVC   0(18,RF),=C'US-ComputerRoomNY,'                                  
         LA    RF,18(RF)                                                        
*&&                                                                             
*&&UK                                                                           
         MVC   0(13,RF),=C'UK-Operators,'                                       
         LA    RF,13(RF)                                                        
*&&                                                                             
SNDEM05  DS    0H                                                               
         LA    RE,DESADDID         MR OWNER (SMTP WILL SUPPRESS DUPS)           
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&UK*&& MVC   0(4,RF),=C'DDLO'                                                 
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPRS',WORK),('SUBJCTLQ',SUBJECT)             
*                                                                               
         MVI   P,0                 PRINT THE E-MAIL CONTENTS                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'WORK),WORK                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(SUBJCTLQ),SUBJECT                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   MSGMR#,DESNUMB          SET MR# IN E-MAIL                        
         MVC   MSGHDR,SBJHDR           SET SUBJECT                              
         IF    (CLC,RETCODE,EQ,=F'8')  RC = 8                                   
           MVC   MSGMSG,BADMSG                                                  
         ELSE  ,                       RC = 4                                   
           MVC   MSGMSG,WARNMSG                                                 
         ENDIF ,                                                                
*                                                                               
         LA    R2,MESSAGE                                                       
SNDEM10  DS    0H                  PUT TOP PORTION OF ERROR E-MAIL              
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
         MVC   P(80),0(R2)                                                      
         GOTO1 =V(PRINTER)                                                      
         AHI   R2,80                                                            
         CLI   0(R2),X'FF'                                                      
         BNE   SNDEM10                                                          
*                                                                               
SNDEM20  DS    0H                  PUT ERROR-SPECIFIC PORTION OF E-MAIL         
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R3))                                  
         MVC   P(80),0(R3)                                                      
         GOTO1 =V(PRINTER)                                                      
         AHI   R3,80                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   SNDEM20                                                          
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPASND',0)   SEND THE BUFFERED E-MAIL          
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAEND',0)   DETACH JESMAIL                    
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
GETITMF  NTR1  ,                                                                
*                                                                               
* THE APIK200 SKELETON HAS POPULATED THE ITMFFLDS DATASET WITH                  
* PARSED, EXTRACTED FIELDS FROM A CALL TO THE JIRA API. WE USE THE API          
* TO DIG OUT SPECIFIC FIELDS FROM THE ITMF TICKET.                              
* RECORDS ARE FB, LRECL=80. KEYWORD IN COLUMN 1, VALUE IN COLUMN 21.            
*                                                                               
       OPEN  ITMFFLDS            EXTRACTED FIELDS FROM ITMF ISSUE               
       LTR   RF,RF                                                              
       JNZ   *+2                 CAN'T OPEN THE FILE ?!?                        
                                                                                
       GET   ITMFFLDS,WORK       THERE'S ALWAYS AT LEAST ONE RECORD             
       CLC   =C'ITMF TICKET ',WORK                                              
       JNE   *+2                 1ST RECORD MUST HAVE ITMF TKT # !              
                                                                                
       MVC   P(35),=C'EXTRACTED FIELDS FROM ITMF TICKET: '                      
       MVC   P+35(L'JIRA_ITMFTKT),WORK+20                                       
       GOTO1 =V(PRINTER)                                                        
                                                                                
       MVC   JIRA_ITMFTKT,WORK+20  GET THE ITMF ISSUE #                         
                                                                                
       GET   ITMFFLDS,WORK                                                      
       MVC   P(L'WORK),WORK                                                     
       GOTO1 =V(PRINTER)                                                        
       CLC   =C'RESPONSE CODE ',WORK                                            
       JNE   *+2                 2ND RECORD MUST HAVE RESPONSE CODE!            
                                                                                
       MVC   JIRA_RESPONSE,WORK+20                                              
       MVC   JIRARESP,WORK+20    SET IN ERROR MESSAGE                           
                                                                                
       IF (CLC,JIRA_RESPONSE,EQ,SPACES)  EMPTY RESPONSE RECEIVED?               
         MVC   JIRARSP#,=F'-2'    -2 = RESPONSE RECEIVED, BUT "EMPTY"           
         MVC   JIRARESP,=CL20'<BLANKS>'                                         
       ELSE  ,                                                                  
         GOTO1 =V(NUMVAL),DMCB,(0,JIRA_RESPONSE),(X'02',0)                      
         CLI   DMCB,0              RESPONSE CODE IS VALID NUMERIC?              
         JNE   *+2                 NO ?!?                                       
         MVC   JIRARSP#,DMCB+4     SAVE BINARY RESPONSE CODE                    
       ENDIF ,                                                                  
                                                                                
       IF (CLC,JIRARSP#,EQ,=F'200')    JIRA ISSUE FOUND?                        
                                                                                
         DO    INF                 UNTIL EOF                                    
                                                                                
           GET   ITMFFLDS,WORK       GET AND PRINT EACH KEYWORD/VALUE           
           MVC   P(L'WORK),WORK                                                 
           GOTO1 =V(PRINTER)                                                    
                                                                                
           SELECT CLC,WORK(20),EQ    SET APPROPRIATE FIELD IN W/S               
             WHEN (=CL20'MR#')                                                  
               MVC JIRA_MR#,WORK+20                                             
             WHEN (=CL20'DIRECTION')                                            
               MVC JIRA_DIRECTION,WORK+20                                       
             WHEN (=CL20'LEVEL')                                                
               MVC JIRA_LEVEL,WORK+20                                           
             WHEN (=CL20'COUNTRY')                                              
               MVC JIRA_COUNTRY,WORK+20                                         
             WHEN (=CL20'JIRASPEC')                                             
               MVC JIRA_SPECTKT,WORK+20                                         
             WHEN (=CL20'USERID')                                               
               MVC JIRA_USERID,WORK+20                                          
           ENDSEL ,                                                             
                                                                                
         ENDDO ,                   WE FALL OUT AT EOF                           
                                                                                
       ENDIF ,                                                                  
                                                                                
       MVI   P,0                                                                
       GOTO1 =V(PRINTER)                                                        
                                                                                
ITMFEOF DS   0H                                                                 
       CLOSE ITMFFLDS                                                           
                                                                                
       J     XIT                                                                
*                                                                               
       DROP  R5                                                                 
       EJECT                                                                    
         LTORG                                                                  
         SPACE 3                                                                
DMCB     DS    6F                                                               
RETCODE  DC    F'0'                RETURN CODE                                  
JIRARSP# DC    F'-1'               JIRA RESPONSE CODE (BINARY)                  
WORK     DS    CL80                                                             
TODAY    DS    CL8                 YYYYMMDD                                     
SUBMITTR DC    CL8' ',C' '                                                      
ITMFWHO  DC    CL8' ',C' '                                                      
WHENFLAG DC    C' '                C'P': CALLED BY PAPTSTC STARTED TASK         
ENV      DC    CL3'DDS'            'DDS' OR 'SBX' (ASSUME PRODUCTION)           
CC_OPS   DC    C'Y'                C'Y': CC COMPUTER ROOM ON E-MAILS            
LPAR     DS    CL4                 LPAR (SYSTEM ID)                             
FATALHDR DC    CL(L'SBJHDR)'*** FATAL ERROR ***'                                
WARNHDR  DC    CL(L'SBJHDR)'***** WARNING *****'                                
*                                                                               
* EXTRACTED VALUES FROM JIRA ISSUE                                              
JIRA_ITMFTKT   DC CL16' '          JIRA ITMF ISSUE (E.G.: ITMF-12345)           
JIRA_RESPONSE  DC CL20' '          RESPONSE CODE FROM CURL                      
JIRA_MR#       DC CL6' '           MOVE REQUEST #                               
JIRA_DIRECTION DC CL10' '          'PROMOTE' OR 'BACKOUT'                       
JIRA_LEVEL     DC CL4' '           E.G., 'STGE', 'PROD'                         
JIRA_COUNTRY   DC CL2' '           US/UK                                        
JIRA_SPECTKT   DC CL20' '          JIRA SPEC ISSUE (E.G.: SPEC-1234)            
JIRA_USERID    DC CL8' '           REQUESTING DEVELOPER'S USERID                
*                                                                               
SUBJECT  DC    C'PanAPT MR# '                                                   
SBJMR#   DC    C'NNNNNN'                                                        
         DC    C' '                                                             
SBJHDR   DC    C'                   '                                           
SUBJCTLQ EQU   *-SUBJECT                                                        
         EJECT                                                                  
MESSAGE  DS    0D                                                               
         DC    CL20' '                                                          
MSGHDR   DC    CL(L'SBJHDR)' '     MESSAGE HEADER (EYE-CATCHER)                 
         DC    CL(80-20-L'SBJHDR)' '                                            
*                                                                               
         DC    CL80' '             BLANK LINE                                   
*                                                                               
         DC    CL20'PanAPT Move Request '                                       
MSGMR#   DS    CL6                                                              
         DC    CL(80-20-L'MSGMR#)' was submitted using action "SUB".'           
MSGMSG   DC    CL80' '                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
BADMSGMT DS    0D                                                               
 DC C'The Move Request has an invalid Move Type of '                            
BADMRTYP DS    C                                                                
 DC CL(80-(*-BADMSGMT))'. This condition'                                       
 DC CL80'should never arise. Please contact your PanAPT administrator.'         
         DC    X'FF'                                                            
         SPACE 2                                                                
NOTAUTH  DS    0D                                                               
 DC CL80'You are not authorized to SUB a Move Request. Please create'           
 DC CL80'a JIRA ITMF Move Request ticket for Ops. If this is a genuine'         
 DC CL80'emergency, and/or if JIRA is unavailable, then call the'               
 DC CL80'computer room at x5060 and ask the Operator to perform an'             
 DC CL80'EMERGENCY Move Request SUB. You are required to document this'         
 DC CL80'activity in JIRA as soon as possible.'                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
BADMSGND DS    0D                                                               
 DC C'The Move Request "Next Move Date" of '                                    
*&&US                                                                           
BADNMDTM DS    CL2                 MM                                           
         DC    C'/'                                                             
BADNMDTD DS    CL2                 DD                                           
*&&                                                                             
*&&UK                                                                           
BADNMDTD DS    CL2                 DD                                           
         DC    C'/'                                                             
BADNMDTM DS    CL2                 MM                                           
*&&                                                                             
         DC    C'/'                                                             
BADNMDTC DS    CL2                 CC                                           
BADNMDTY DS    CL2                 YY                                           
 DC CL(80-(*-BADMSGND))' has not yet been'                                      
 DC CL80'reached. This is almost certainly because the PanAPT "DAT"'            
 DC CL80'command was used to deliberately prevent this MR from being'           
 DC CL80'promoted to its next Level prior to the specified date.'               
 DC CL80' '                                                                     
 DC CL80'If you wish to promote this MR today, then use the DAT'                
 DC CL80'command to change the "Next Move Date" to today.'                      
 DC CL80' '                                                                     
*&&US                                                                           
 DC CL80'(Note: in the U.S., the mainframe date changes at 6am!)'               
*&&                                                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
BADMSGFD DS    0D                                                               
 DC C'The Move Request "Final Move Date" of '                                   
*&&US                                                                           
BADFMDTM DS    CL2                 MM                                           
         DC    C'/'                                                             
BADFMDTD DS    CL2                 DD                                           
*&&                                                                             
*&&UK                                                                           
BADFMDTD DS    CL2                 DD                                           
         DC    C'/'                                                             
BADFMDTM DS    CL2                 MM                                           
*&&                                                                             
         DC    C'/'                                                             
BADFMDTC DS    CL2                 CC                                           
BADFMDTY DS    CL2                 YY                                           
 DC CL(80-(*-BADMSGFD))' has not yet been'                                      
 DC CL80'reached. This is almost certainly because the PanAPT "DAT"'            
 DC CL80'command was used to deliberately prevent this MR from being'           
 DC CL80'promoted to PROD prior to the specified date.'                         
 DC CL80' '                                                                     
 DC CL80'If you wish to promote this MR today, then use the DAT'                
 DC CL80'command to change the "Final Move Date" to today.'                     
 DC CL80' '                                                                     
*&&US                                                                           
 DC CL80'(Note: in the U.S., the mainframe date changes at 6am!)'               
*&&                                                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
BADMSGST DS    0D                                                               
 DC C'The current status of the Move Request is '                               
BADSTAT  DS    CL6                                                              
 DC CL(80-(*-BADMSGST))', which is not an'                                      
 DC CL80'eligible status for promotion or backout. Eligible statuses ar+        
               e:'                                                              
 DC CL80' '                                                                     
 DC CL80'  "APS"    (Approved for promotion to STGE)'                           
 DC CL80'  "APPS"   (Approved for promotion to PRKS)'                           
 DC CL80'  "APP"    (Approved for promotion to PROD)'                           
 DC CL80'  "APS-B"  (Approved for backout from STGE)'                           
 DC CL80'  "APPS-B" (Approved for backout from PRKS)'                           
 DC CL80'  "APP-B"  (Approved for backout from PROD)'                           
         DC    X'FF'                                                            
         SPACE 2                                                                
MISVERAP DS    0D                                                               
 DC CL80'All required Verifications and Approvals for this Move'                
 DC CL80'Request have not yet been obtained.'                                   
         DC    X'FF'                                                            
         SPACE 2                                                                
JIRA_NO_RESPONSE_MSG DS 0D                                                      
 DC CL80'No response to the REST call invoking the JIRA API.'                   
         DC    X'FF'                                                            
         SPACE 2                                                                
JIRA_ISSUE_NOT_FOUND DS 0D                                                      
 DC C'JIRA issue '                                                              
JIRA_ISSUE_NOT_FOUND_ISSUE# DS CL16                                             
 DC CL(80-(*-JIRA_ISSUE_NOT_FOUND))' was not found.'                            
         DC    X'FF'                                                            
         SPACE 2                                                                
JIRA_SERVER_UNAVAILABLE_MSG DS 0D                                               
 DC CL80'JIRA server unavailable.'                                              
         DC    X'FF'                                                            
         SPACE 2                                                                
JIRA_MR#_MISMATCH DS 0D                                                         
 DC C'A "SUB" command was issued for Move Request number '                      
JIRA_MR#_MISMATCH_PANAPT DS CL6                                                 
 DC CL(80-(*-JIRA_MR#_MISMATCH))','                                             
 DC C'but the JIRA ITMF issue specifies Move Request number '                   
JIRA_MR#_MISMATCH_ITMF   DS CL6                                                 
 DC CL(80-(*-(JIRA_MR#_MISMATCH+80)))'.'                                        
 DC CL80'This implies that one of the following errors has occurred:'           
 DC CL80'1) The wrong Move Request has been submitted, or'                      
 DC CL80'2) The wrong ITMF ticket number was entered in the SUB panel, +        
               or'                                                              
 DC CL80'3) The developer has entered an incorrect MR# in the ITMF tick+        
               et.'                                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
MR_DIRECTION_LEVEL_MISMATCH DS 0D                                               
 DC C'The ITMF ticket requests a '                                              
MR_DIRECTION DS CL12               "Promotion to" or "Backout from"             
 DC C' Level '                                                                  
MR_DIRECTION_LEVEL_MISMATCH_ITMF DS CL4                                         
 DC CL(80-(*-MR_DIRECTION_LEVEL_MISMATCH))','                                   
 DC C'which is incompatible with the current MR Status of '                     
MR_DIRECTION_LEVEL_MISMATCH_STATUS DS CL6                                       
 DC CL(80-(*-(MR_DIRECTION_LEVEL_MISMATCH+80)))'.'                              
         DC    X'FF'                                                            
         SPACE 2                                                                
JIRA_UNEXPECTED_RESPONSE_MSG DS 0D                                              
 DC CL36'Unexpected response code from JIRA: '                                  
JIRARESP DS    CL20                EXPECTING CL3, BUT WE'VE SEEN LONGER         
         DC    CL(80-36-L'JIRARESP)' '                                          
         DC    X'FF'                                                            
*&&DO                                                                           
BADLPAR  DS    0D                                                               
 DC CL80'The "SUB" was issued from an invalid LPAR (typically, Move'            
 DC CL80'Requests must be submitted from SY7).'                                 
         DC    X'FF'                                                            
*&&                                                                             
BADMSG   DC    CL80'The SUB failed because the Move Request is ineligib+        
               le for processing.'                                              
WARNMSG  DC    CL80'The Move Request is eligible for processing, but wi+        
               th one or more warnings.'                                        
*                                                                               
MRFILE   DCB   DDNAME=INFILE,DSORG=PS,MACRF=GM                                  
ITMFFLDS DCB   DDNAME=ITMFFLDS,DSORG=PS,MACRF=GM,LRECL=80,             +        
               EODAD=ITMFEOF                                                    
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*MRREC**'                                                      
MRREC    DS    2000C                                                            
         SPACE 3                                                                
         DS    0D                                                               
         DC    CL16'*****SSB********'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
         PRINT GEN                                                              
MMMDES   APAMMDES DSECT=YES                                                     
         PRINT NOGEN                                                            
         SPACE 3                                                                
         IHAPSA                                                                 
         CVT   DSECT=YES                                                        
         EJECT                                                                  
       ++INCLUDE DDOPERLSTD                                                     
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
* ++INCLUDE DDSMTPD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034DDPAPTMRV 07/15/20'                                      
         END                                                                    
