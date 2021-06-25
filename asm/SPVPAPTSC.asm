*          DATA SET SPVPAPTSC  AT LEVEL 020 AS OF 11/08/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE SECXAPTA                                                                 
***********************************************************************         
*                                                                     *         
* This is the PanAPT Security Exit. The load module name is specified *         
* in the PanAPT Control File (accessible via the administrator        *         
* panels). The load module itself resides in 'DDS.LOADLIB' (i.e., it  *         
* does not need to reside in a dedicated PanAPT load library).        *         
*                                                                     *         
* Documentation for this exit is in the PanAPT Administrator Guide.   *         
*                                                                     *         
* Upon entry to this program, the value of the pass/fail indicator is *         
* set to "C" or "F" based on the custom authorization rules set in    *         
* the PanAPT Control File for the given Activity and User. This exit  *         
* can override the pre-determined pass/fail indicator.                *         
*                                                                     *         
* This program must run above the line, because PanAPT passes it      *         
* 31-bit addresses in its parameter list. As a result, any object     *         
* module which is linked into this load module must be 31-bit         *         
* compatible.                                                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
* Set the &&LOCK flag to 'Y' to restrict access to hard-coded userids.          
* This is typically done only during a PanAPT database upgrade (or              
* software upgrade).                                                            
*&&LOCK  SET   N                   Y = LOCK EVERYONE OUT OF PANAPT              
*                                   EXCEPT THE HARD-CODED USERIDS BELOW         
*&&DEBUG SET   N                   Y = WRITE TRACE MESSAGES TO TERMINAL         
*                                                                               
         TITLE 'PANAPT SECURITY EXIT'                                           
SECEXAPT CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
SECEXAPT AMODE ANY                                                              
*                                                                               
         USING (SECEXAPT,SECEXAPTX),RC  ESTABLISH BASE ADDRESSABILITY           
         REQUS                                                                  
         SAVE  (14,12),,SECEXAPT-&SYSDATE-&SYSTIME                              
         LR    RC,RF               LOAD BASE ADDRESS                            
         LARL  RF,SAVEAREA         FETCH POINTER TO SAVEAREA                    
         ST    RD,4(RF)            SAVE LINKAGE TO CALLING PROGRAM              
         ST    RF,8(RD)            CHAIN SAVEAREAS                              
         LR    RD,RF               POINT TO CURRENT SAVEAREA                    
         USING SAVEAREA,RD                                                      
*                                                                               
         LM    R3,R8,0(R1)         FETCH PARM POINTERS                          
         USING APAMSXEV_DSECT,R3   EVENT BLOCK                                  
         USING APAMSXEN_DSECT,R4   ENVIRONMENT BLOCK                            
         USING APAMMDES,R5         MOVE REQUEST BLOCK                           
         USING APAMMMBR_DSECT,R6   MEMBER BLOCK                                 
         USING APAMSXAP_DSECT,R7   APPROVAL BLOCK                               
         USING APAMDIB2_DSECT,R8   INVENTORY BLOCK                              
*                                   (The PanAPT Administrator Guide             
*                                    says that the 6th parameter is             
*                                    covered by APAMLIB2, but we have           
*                                    empirically observed that this             
*                                    must be a typo, and that they              
*                                    really mean APAMDIB2.)                     
*                                                                               
*======================================================================         
*                                                                               
* ****    N E V E R    R E M O V E    T H I S    C O D E    ****                
*                                                                               
* The Security Exit is invoked even for users who are designated as             
* PanAPT Administrators. Therefore, if code is ever present in this             
* exit to restrict PanAPT access only to specified users, it is                 
* absolutely imperative that the Administrator userid(s) remain                 
* authorized. Otherwise, we could end up in a situation where *ALL*             
* users, including Administrators, are locked out of PanAPT!                    
*                                                                               
         CLC   =C'APT',MSXENUSR       *NEVER* DENY AN ADMINISTRATOR...          
         BE    USER_OK                ...ACCESS TO PANAPT!                      
*                                                                               
*======================================================================         
*                                                                               
*&&LOCK                                                                         
*                                                                               
* Test for each individual userid that should be permitted access               
* during a general lockout.                                                     
*                                                                               
         CLC   MSXENUSR,=C'DEIS    '                                            
         BE    USER_OK                                                          
*********CLC   MSXENUSR,=C'DEIS2   '                                            
*********BE    USER_OK                                                          
*                                                                               
* Everyone else is unconditionally locked out of PanAPT.                        
*                                                                               
         MVI   MSXEVIND,FAIL       FORCE SECURITY EXIT "FAIL"                   
*                                                                               
* Write our own lockout message to user's terminal                              
*                                                                               
         TPUT  MSGASTRS,L'MSGASTRS,EDIT                                         
         TPUT  MSGASTR1,L'MSGASTR1,EDIT                                         
         TPUT  MSG1,L'MSG1,EDIT                                                 
         TPUT  MSGASTR1,L'MSGASTR1,EDIT                                         
         TPUT  MSG2,L'MSG2,EDIT                                                 
         TPUT  MSG3,L'MSG3,EDIT                                                 
         TPUT  MSG4,L'MSG4,EDIT                                                 
         TPUT  MSGASTR1,L'MSGASTR1,EDIT                                         
         TPUT  MSG5,L'MSG5,EDIT                                                 
         TPUT  MSG6,L'MSG6,EDIT                                                 
         TPUT  MSGASTR1,L'MSGASTR1,EDIT                                         
         TPUT  MSG7,L'MSG7,EDIT                                                 
         TPUT  MSG8,L'MSG8,EDIT                                                 
         TPUT  MSGASTR1,L'MSGASTR1,EDIT                                         
         TPUT  MSGASTRS,L'MSGASTRS,EDIT                                         
*                                                                               
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
USER_OK  DS    0H                                                               
*&&DEBUG                                                                        
         TPUT  =C'EVENT BLOCK:',12,EDIT                                         
         TPUT  APAMSXEV,APAMSXEV_LEN,EDIT                                       
         TPUT  =C'ENVIRONMENT BLOCK:',18,EDIT                                   
         TPUT  APAMSXEN,APAMSXEN_LEN,EDIT                                       
*                                                                               
         IF (CLI,MSXEVBL1,EQ,C'Y')     SHOW MR BLOCK IF PRESENT                 
           TPUT  =C'MOVE REQUEST BLOCK:',19,EDIT                                
           TPUT  APAMMDES,DESEXMAX-APAMMDES,EDIT                                
         ENDIF ,                                                                
         IF (CLI,MSXEVBL2,EQ,C'Y')     SHOW MEMBER BLOCK IF PRESENT             
           TPUT  =C'MEMBER BLOCK:',13,EDIT                                      
           TPUT  MRMBR,MRMBRL,EDIT                                              
         ENDIF ,                                                                
         IF (CLI,MSXEVBL3,EQ,C'Y')     SHOW APPROVAL BLOCK IF PRESENT           
           TPUT  =C'APPROVAL BLOCK:',15,EDIT                                    
           TPUT  APAMSXAP,APAMSXAP_LEN,EDIT                                     
         ENDIF ,                                                                
         IF (CLI,MSXEVBL4,EQ,C'Y')     SHOW INVENTORY BLOCK IF PRESENT          
           TPUT  =C'INVENTORY BLOCK:',16,EDIT                                   
           TPUT  INVREC,D2RECLNG,EDIT                                           
         ENDIF ,                                                                
*&&                                                                             
*                                                                               
         CLC   MSXEVEVT,=C'INIT    '   PANAPT INITIALIZATION EVENT?             
         BE    XIT                 YES: NOTHING TO DO                           
*                                                                               
         CLC   MSXEVEVT,=C'TERM    '   PANAPT TERMINATION EVENT?                
         BE    XIT                 YES: NOTHING TO DO                           
*                                                                               
         CLC   MSXEVEVT,=C'ACTIVITY'   PANAPT ACTIVITY EVENT?                   
         BNE   XIT                 UNKNOWN EVENT: JUST GET OUT!                 
*                                                                               
         CLC   =C'MOVEREQ/APB',MSXENACT   APPROVAL FOR BACKOUT?                 
         BE    MAIN10              YES: NO VERIFICATIONS TO CHECK               
         CLC   =C'MOVEREQ/APM',MSXENACT   APPROVAL FOR MOVE (PROMOTE)?          
         BNE   XIT                 HOW DID WE GET HERE?                         
*                                                                               
* Ensure that all required Verifications have completed successfully.           
*                                                                               
         IF (CLI,MSXEVBL1,NE,C'Y')                                              
           ABEND 2021              MOVE REQUEST BLOCK MUST BE PRESENT!          
         ENDIF ,                                                                
         CLC   DESCLVNF(20),DESCLVSF  RVP'S NEEDED VS. RVP'S SO FAR             
         BE    MAIN10              ALL HAVE COMPLETED SUCCESSFULLY              
         MVI   MSXEVIND,FAIL       NO APPROVAL UNTIL RVP'S ARE DONE             
         B     XIT                                                              
*                                                                               
MAIN10   DS    0H                                                               
         CLI   MSXEVIND,CONTINUE   DOES PANAPT SAY USER CAN APPROVE?            
         BNE   XIT                 NO: NOTHING ELSE TO DO                       
*                                                                               
* Note: we used to send an e-mail (via SMTP) as an "Approved"                   
* notification to the MR Owner. That code is no longer needed now that          
* we have transitioned from Lotus Notes to JIRA.                                
*                                                                               
XIT      DS    0H                                                               
         L     RD,4(,RD)           RESTORE LINKAGE TO CALLING PROGRAM           
*                                                                               
*                                  NOTE: RETURN CODE NOT SET                    
         RETURN (14,12)            RETURN TO CALLING PROGRAM                    
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
SECEXAPTX EQU *                                                                 
         EJECT                                                                  
SAVEAREA DC    18F'0'              LOCAL SAVEAREA                               
         SPACE 3                                                                
MSGASTRS DC    C'*****************************************************'         
MSGASTR1 DC    C'*                                                   *'         
MSG1     DC    C'*          *** PANAPT IS UNAVAILABLE ***            *'         
MSG2     DC    C'* PanAPT is currently unavailable while essential   *'         
MSG3     DC    C'* maintenance is performed on the PanAPT database   *'         
MSG4     DC    C'* and/or software base.                             *'         
MSG5     DC    C'* Emergency DDS.LOADLIB updates may be performed    *'         
MSG6     DC    C'* via ISPF option =R.                               *'         
MSG7     DC    C'* Please call the computer room if you have any     *'         
MSG8     DC    C'* questions.                                        *'         
         SPACE 3                                                                
FAIL     EQU   C'F'                SECURITY CHECK *FAILED*: FAIL                
CONTINUE EQU   C'C'                SECURITY CHECK *PASSED*: CONTINUE            
*                                                                               
         EJECT                                                                  
APAMSXEV_DSECT DSECT                                                            
         COPY APAMSXEV                                                          
APAMSXEV_LEN EQU *-APAMSXEV                                                     
         EJECT                                                                  
APAMSXEN_DSECT DSECT                                                            
         COPY APAMSXEN                                                          
APAMSXEN_LEN EQU *-APAMSXEN                                                     
         EJECT                                                                  
APAMSXAP_DSECT DSECT                                                            
         COPY APAMSXAP                                                          
APAMSXAP_LEN EQU *-APAMSXAP                                                     
         EJECT                                                                  
         APAMMDES                                                               
         EJECT                                                                  
APAMMMBR_DSECT DSECT                                                            
         PUSH  PRINT                                                            
         PRINT GEN                                                              
         APAMMMBR                                                               
         POP   PRINT                                                            
         EJECT                                                                  
APAMDIB2_DSECT DSECT                                                            
         COPY APAMDIB2                                                          
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPVPAPTSC 11/08/18'                                      
         END                                                                    
