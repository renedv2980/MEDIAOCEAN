*          DATA SET ACCHPCARD  AT LEVEL 006 AS OF 10/27/17                      
*PHASE ACPCARDA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SMTP                                                                   
*INCLUDE CARDS                                                                  
         TITLE ' VALIDATE CHASE PAYMENT RESPONSE FOR ERROR TRAILER  '           
***********************************************************************         
*                                                                     *         
*  AUTHOR :-   NEERAJ MALIK   [ NMAL ]                                *         
*  DATE   :-   23 MARCH 2017                                          *         
*                                                                     *         
* DESCRIPTION :-                                                      *         
* READ MERGED FILE FOR FULL DAY AND VALIDATE ALL TRAILER RECORD TO    *         
* SEE IF THERE ARE ANY ERROR IN PAYMENT PROCESSING BY CHASE           *         
*                                                                     *         
* IF THERE ARE ANY PAYMENT ERROR WE NEED TO TRIGGER                   *         
*  E-MAIL NOTIFICATION TO RELATED STAKE HOLDERS                       *         
*                                                                     *         
*  BELOW ITEMS WILL BE SENT IN EMIAL NOTIFICATION                     *         
* 1. PHYSICAL FILE NAME FOR FURHTER ANALYIS                           *         
* 2. TRAILER REOCRD WITH ERROR COUNT >0.                              *         
*   (MAXIMUM 5 TRAILER REOCRDS ARE SENT IN E-MAIL)                    *         
* 3. COUNT FOR TRAILER WITH ERROR RECORD                              *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
ACPCARD  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,ACPCARD,=V(REGSAVE)                                            
*                                                                               
MAIN10   DS    0C                                                               
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'    READ INPUT RECORD                
         XC    RUNPARM,RUNPARM                                                  
         CLC   RUNCARD,=C'RUN=PROD'   | BASED ON PARM SET RUNPARM               
         BNE   *+8                    | TO PROD OR TEST                         
         OI    RUNPARM,RPROD          | THIS PARM WILL DECIDE MAILING           
*        B     *+8                    | LIST IN CASE OF ERROR IN                
*        OI    RUNPARM,RTEST          | PAYMENT FILE                            
*                                                                               
         BAS   RE,NAME         GET PHYSICAL FILE NAME                           
*                                                                               
         BAS   RE,PROCESS      READ AND PROCESS INFILE REOCRDS                  
*                                                                               
         CP    ERRCNT,=P'0'    CHECK IF THERE ARE ANY ERROR RECORD              
         BE    PGMEND          IF NO ERROR TRAILER, THEN SKIP E-MAIL            
*                                                                               
         EDIT  ERRCNT,OUTCNT,ZERO=BLANK                                         
         BAS   RE,ALERT        BUILD AND SEND ALERT E-MAIL NOTIFICATION         
*                                                                               
         B     PGMEND          END OF THE PROGRAM                               
*                                                                               
********************************************************************            
*                                                                               
* PHYSICAL DATASET NAME EXTRACT FOR DDNAME INFILE                               
*                                                                               
********************************************************************            
*                                                                               
NAME     NTR1                                                                   
*                                                                               
         RDJFCB INFILE                                                          
         LTR   RF,RF                                                            
         BNZ   NAMEX                                                            
         LA    R3,JFCBAREA                                                      
         USING INFMJFCB,R3                                                      
         MVC   TMPNAME(L'JFCBDSNM),JFCBDSNM                                     
*                                                                               
NAMEX    B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
********************************************************************            
*                                                                               
* READ FILE AND CHECK FOR TRAILER WITH ERROR COUNT                              
*                                                                               
********************************************************************            
*                                                                               
         USING RECD,R3                                                          
*                                                                               
PROCESS  NTR1                                                                   
         OPEN  INFILE           OPEN INPUT FILE                                 
         L     R3,=A(IOAREA)    IO AREA FOR READING RECORD                      
         LA    R4,LINE          WILL BUILD ERROR DATA IN LINE                   
         ZAP   ERRCNT,=P'0'     INITIALIZE ERROR COUNTER                        
READ     GET   INFILE,(R3)      READ INPUT FILE                                 
*                                                                               
         CLI   COMMA1,C','      CHECK FOR COMMA                                 
         BNE   READ             NO, THEN READ NEXT RECORD                       
*                                                                               
         TRT   RECREC,TABNUM    CHECK IF STRING HAVE ANY NON NUMERIC            
         BNZ   READ             IF NOT NUMERIC, READ NEXT RECORD                
*                                                                               
         TRT   RECERR,TABNUM    CHECK IF ERROR COUNT VALUE IS NUMERIC           
         BNZ   READ             IF NOT NUMERIC, READ NEXT RECORD                
*                                                                               
         CLC   RECERR,ZEROS     CHECK IF ERROR COUNT IN TRAILER IS ZERO         
         BE    READ             IF ZERO, THEN READ NEXT RECORD                  
*                                                                               
         AP    ERRCNT,=P'1'     ADD ONE TO ERROR COUNTER                        
         CP    ERRCNT,TRLCNTP   CHECK IF ERROR COUNT > 5                        
         BH    READ             IF ERROR COUNT >5 THEN GO BACK TO READ          
         MVC   0(RECDQ,R4),RECD ELSE WRITE ERROR TRAILER FOR MAIL               
         LA    R4,RECDQ(R4)                                                     
         B     READ                                                             
ENDREAD  EQU   *                                                                
         CLOSE INFILE           CLOSE INPUT FILE                                
*                                                                               
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
********************************************************************            
*                                                                               
* BUILD AND SEND ALERT E-MAIL NOTIFICATION TO ALL STAKE HOLDERS                 
*                                                                               
********************************************************************            
*                                                                               
ALERT    NTR1                                                                   
*                                                                               
****  SET ALERT E-MAIL NOTIFICATION                                             
         GOTO1 =V(SMTP),DMCB,('SMTPAINI',0)       INITIALIZE                    
*                                                                               
*        WORK=EMAIL ADDRESSES : TEXT=SUBJECT LINE                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         TM    RUNPARM,RPROD                    |                               
         JO    *+14                             |                               
         MVC   WORK(L'EMAILADT),EMAILADT        | TEST SUPPORT ADDRESS          
         J     *+10                             |                               
         MVC   WORK(L'EMAILADD),EMAILADD        | PROD EMAIL ADDRESS            
         MVI   MAILTEXT,C' '                      MAILTEXT                      
         MVC   MAILTEXT+1(L'MAILTEXT-1),MAILTEXT                                
         MVC   MAILTEXT(L'EMLMSUB),EMLMSUB        SUBJECT LINE                  
         GOTO1 =V(SMTP),DMCB,('SMTPAPRS',WORK),(L'MAILTEXT,MAILTEXT)            
*                                                                               
*  MOVE FILE NAME TO E-MAIL BODY                                                
         MVI   MAILTEXT,C' '                      MAILTEXT                      
         MVC   MAILTEXT+1(L'MAILTEXT-1),MAILTEXT                                
         MVC   MAILTEXT(L'TMPFILE),TMPFILE        PHYSICAL FILE NAME            
         MVC   MAILTEXT+L'TMPFILE(L'JFCBDSNM),TMPNAME                           
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MAILTEXT)                              
*                                                                               
*  MOVE TRAILER RECORD HEADER IN E-MAIL BODY                                    
         MVI   MAILTEXT,C' '                      MAILTEXT                      
         MVC   MAILTEXT+1(L'MAILTEXT-1),MAILTEXT                                
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MAILTEXT)                              
         MVC   MAILTEXT(L'TRLRHDR),TRLRHDR        TRAILER HEADER                
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MAILTEXT)                              
*                                                                               
*  MOVE ERROR TRAILER LINE TO E-MAIL BODY                                       
         LA    R5,TRLCNT                          MAX TRAILER TO PRINT          
         LA    R4,LINE                            TRAILER REC ADDRESS           
ERRLINE  EQU   *                                                                
         MVI   MAILTEXT,C' '                      MAILTEXT                      
         MVC   MAILTEXT+1(L'MAILTEXT-1),MAILTEXT                                
         MVC   MAILTEXT+3(RECDQ),0(R4)            TRAILER RECORD                
         OC    MAILTEXT,MAILTEXT                  CHECK IF NO MORE ERR          
         BZ    ERRLINX                            COME OUT OF LOOP              
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MAILTEXT)                              
         LA    R4,RECDQ(R4)                       NEXT RECORD ADDRESS           
         BCT   R5,ERRLINE                         DECREMENT FOR LOOP            
ERRLINX  EQU   *                                                                
*  NUMBER OF TRAILER WITH ERROR COUNT IN E-MAIL BODY                            
         MVI   MAILTEXT,C' '                       MAILTEXT                     
         MVC   MAILTEXT+1(L'MAILTEXT-1),MAILTEXT                                
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MAILTEXT) PRINT BLANK LINE             
         MVC   MAILTEXT(L'TRLRCNT),TRLRCNT                                      
         MVC   MAILTEXT+L'TRLRCNT(L'OUTCNT),OUTCNT TRAILER ERR COUNT            
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MAILTEXT)                              
*  MAXIMUM FIVE ERROR TRAILER DISPLAYED                                         
         CP    ERRCNT,TRLCNTP       CHECK IF TRAILER COUNT IN ERROR<5           
         BL    SNDMAIL              THEN SKIP THE WARNING                       
         MVI   MAILTEXT,C' '                       MAILTEXT                     
         MVC   MAILTEXT+1(L'MAILTEXT-1),MAILTEXT                                
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MAILTEXT) PRINT BLANK LINE             
         MVC   MAILTEXT(L'TRLRMAX),TRLRMAX         WARNING MESSAGE              
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MAILTEXT)                              
*                                                                               
SNDMAIL  GOTO1 =V(SMTP),DMCB,('SMTPASND',0)        SEND EMAIL                   
         GOTO1 =V(SMTP),DMCB,('SMTPAEND',0)        DETACH FROM JESMAIL          
*                                                                               
         B     EXIT                                                             
*****                                                                           
EXIT     XIT1                                                                   
*                                                                               
PGMEND   XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
***********************************************************                     
*                                                         *                     
* LITERAL AND STORAGE DEFINATIONS                         *                     
*                                                         *                     
***********************************************************                     
L3K      EQU   3000                                                             
TRLCNT   EQU   5                                                                
CARD     DS    CL80                                                             
         ORG   CARD                                                             
RUNCARD  DS    CL8                                                              
         ORG                                                                    
TRLCNTP  DC    P'5'                                                             
ZEROS    DC    10C'0'                                                           
DUB      DS    D                                                                
ERRCNT   DS    PL2                                                              
OUTCNT   DS    CL3                                                              
RUNPARM  DS    XL1         TO SEE IF RUNNING WITH TEST OR PROD PARM             
RTEST    EQU   0             RUN=TEST       DEFAULT IS TEST                     
RPROD    EQU   4             RUN=PROD                                           
DMCB     DS    6F                                                               
EMAILADD DC    C'US-ACC_TEAM_NY,INT_PROD:'                                      
EMAILADT DC    C'NMALIK@MEDIAOCEAN.COM,MNASCA@MEDIAOCEAN.COM:'                  
EMLMSUB  DC    C'** URGENT ** : ERROR IN PAYMENT PROCESSING CHASE '             
TRLRHDR  DC    C'   TRAILER RECORD WITH ERROR RECORD              '             
TRLRCNT  DC    C'   NUMBER OF TRAILER WITH ERROR REC - '                        
TRLRMAX  DC    C'** WARNING ** MAX 5 TRAILER REC IN ERROR DISPLAYED'            
TMPFILE  DC    C'   FILE NAME- '        | KEEP THEM                             
TMPNAME  DC    CL60' '                  |  TOGETHER                             
MAILTEXT DC    CL80' '                                                          
WORK     DC    XL64'00'                                                         
LINE     DS    CL200                    TO STORE ERROR TRAILER RECORD           
*                                                                               
*******************************************                                     
*      TABLE TO NUMERIC CHECK IN STRING   *                                     
*******************************************                                     
TABNUM   DC    256X'FF'                                                         
         ORG  TABNUM+C'0'                                                       
         DC   X'00000000000000000000'                                           
*               F0F1F2F3F4F5F6F7F8F9                                            
         ORG                                                                    
*******************************************                                     
*      INPUT FILE STRUCTURE DECLARATION   *                                     
*******************************************                                     
INFILE   DCB   DDNAME=INFILE,DSORG=PS,                                 \        
               RECFM=FB,MACRF=GM,LRECL=L3K,                            \        
               EODAD=ENDREAD,EXLST=JFCBPTR                                      
*                                                                               
*******************************************                                     
*      JFCB CONTROL BLOCK POINTER         *                                     
*******************************************                                     
JFCBPTR  DC    0F                       | ADDRESS POINTER FOR                   
         DC    AL1(EXLLASTE+EXLRJFCB)   | JFCB CONTROL BLOCK                    
         DC    AL3(JFCBAREA)            |                                       
JFCBAREA DS    0F,176C                    MEMORY FOR BLOCK CONTENT              
*                                                                               
*******************************************                                     
*      IO AREA FOR READING FILE RECRD     *                                     
*******************************************                                     
IOAREA   DS    CL(L3K)                                                          
*                                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
*                                                                               
         IEFJFCBN                                                               
         IHAEXLST                                                               
*                                                                               
RECD     DSECT                                                                  
*                                                                               
RECREC   DS      CL10        NUMBER OF REC RECEIVED                             
COMMA1   DS      CL1                                                            
RECPRO   DS      CL10        NUMBER OF REC PROCESSED                            
COMMA2   DS      CL1                                                            
RECERR   DS      CL10        NUMBER OF REC IN ERROR                             
RECDQ    EQU     *-RECD      LENGTH VALUE                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACCHPCARD 10/27/17'                                      
         END                                                                    
