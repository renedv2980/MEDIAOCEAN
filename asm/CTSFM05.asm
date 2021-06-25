*          DATA SET CTSFM05    AT LEVEL 012 AS OF 05/01/02                      
*PHASE TA0A05A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM05 -- GLOBAL EXCHANGE RATE MAINTENANCE          *         
*                                                                     *         
*  COMMENTS:     MAINTAINS RATE RECORDS ON CTFILE.                    *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMF5 (MAINTENANCE)                        *         
*                        CTSFME5 (LIST)                                         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER POINTER/WORK               *         
*                R3 -- KEY/WORK                                       *         
*                R4 -- GETEL/WORK                                     *         
*                R5 -- UNUSED                                         *         
*                R6 -- UNUSED                                         *         
*                R7 -- UNUSED                                         *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- BASE                                                     
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A05 - RATE RECORD MAINTENANCE'                               
TA0A05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A05                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         EJECT                                                                  
*              KEY VALIDATION ROUTINE                                           
         SPACE 2                                                                
         USING CTRTREC,R3                                                       
         USING CTRATED,R4                                                       
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   RTE10                                                            
         LA    R2,RTECTRYH                                                      
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,RLSCTRYH                                                      
         GOTO1 ANY                 COUNTRY REQUIRED                             
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   CTRTKTYP,CTRTEQU    RATE RECORD TYPE                             
         SPACE 1                                                                
         GOTO1 GETFACT,DMCB,0      GET GETFACT INFO                             
         L     RF,DMCB                                                          
         USING FACTSD,RF                                                        
         L     RF,FAACTRY          WHICH HAS A(COUNTRY TABLE)                   
         USING CTRYTABD,RF                                                      
         LH    R0,0(RF)            R0=L'TABLE ENTRY                             
         LA    RF,6(RF)            BUMP TO FIRST ENTRY                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                R1=L'INPUT-1                                 
RTE4     CLI   0(RF),X'FF'                                                      
         BE    FLDINV                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTRYNAM(0),WORK     MATCH ON ENGLISH COUNTRY NAME                
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     RTE4                                                             
         MVC   CTRTCTRY,CTRYCODE   BINARY EQUATE TO KEY                         
         MVC   COUNTRYC,CTRYCODE   AND SAVE LOCALLY                             
         SPACE 1                                                                
         MVC   8(L'RTECTRY,R2),CTRYNAM AND MOVE EXPANSION TO SCREEN             
         OI    6(R2),X'80'                                                      
         MVC   COUNTRYN,CTRYNAM    AND SAVE LOCALLY                             
         SPACE 1                                                                
         CLI   ACTNUM,ACTLIST      FINISHED IF LISTING                          
         BE    XIT                                                              
         LA    R2,RTEDATEH         VALIDATE EFFECTIVE DATE                      
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    DATINV                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,PDATE)                                   
         SPACE 1                                                                
         GOTO1 HIGH                SEE IF RECORD ALREADY ON FILE                
         CLC   CTRTKEY,KEYSAVE                                                  
         BE    *+14                                                             
         MVC   KEY,KEYSAVE         IF NOT, RESTORE KEY                          
         B     XIT                 AND GET OUT                                  
         SPACE 1                                                                
         BAS   RE,GETRATE          LOOK FOR EXISTING RATE ELEMENT               
         BNE   RTE8                                                             
         CLI   ACTNUM,ACTADD       IF WE'RE ADDING                              
         BE    RECINV              DUPLICATE RECORD                             
         B     XIT                                                              
         SPACE 1                                                                
RTE8     CLI   ACTNUM,ACTADD       IF WE'RE NOT ADDING                          
         BNE   NTFOUND             THEN SHOULD HAVE MATCH                       
         L     R3,AIO                                                           
         CLC   CTRTKEY(CTRTSEQ-CTRTKEY),KEYSAVE DID WE GO TO END                
         BE    RTE8D                                                            
         MVC   KEY,KEYSAVE         YES - RESTORE PREVIOUS KEY                   
         GOTO1 HIGH                                                             
         B     RTE9                AND CONTINUE                                 
         SPACE 1                                                                
RTE8D    CLI   CTRAEL,X'B3'        ARE WE POINTING TO A RATE EL.                
         BNE   RTE9                                                             
         CLC   PDATE,CTRADATE      TRYING TO ADD A PREVIOUS EL.                 
         BNL   RTE9                                                             
         XR    RF,RF               INSURE IT WILL FIT                           
         ICM   RF,3,CTRTLEN                                                     
         LA    R1,CTRALNQ                                                       
         AR    RF,R1                                                            
         CH    RF,=H'1000'                                                      
         BNL   CANTADD                                                          
         SPACE 1                                                                
RTE9     OI    WHENOK,X'01'        TELL GENCON TO LET ME HANDLE IT              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD A RECORD                                        
         SPACE 2                                                                
RTE10    CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   RTE50                                                            
         LA    R2,RTERATEH         VALIDATE RATE                                
         GOTO1 ANY                                                              
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(RF) UP TO 4 DECIMAL PLACES               
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         TM    4(R1),X'80'         NEGATIVE NUMBERS NOT ALLOWED                 
         BO    FLDINV                                                           
         MVC   FULL,4(R1)          SAVE RATE IN FULL                            
         SPACE 1                                                                
         MVI   ELCODE,X'10'        DELETE NAME ELEMENT IF AROUND                
         GOTO1 REMELEM                                                          
         CLI   ACTNUM,ACTADD       IF WE'RE NOT ADDING                          
         BE    RTE20                                                            
         BAS   RE,GETRATE          FIND EXISTING RATE ELEMENT                   
         BE    *+6                                                              
         DC    H'0'                IT HAS DISAPPEARED                           
         MVI   0(R4),X'FF'         SET TO DELETE IT                             
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         USING CTRTREC,R3                                                       
         USING CTDATD,R4                                                        
RTE20    L     R3,AIO                                                           
         LA    R4,ELEMENT          BUILD NAME ELEMENT                           
         XC    ELEMENT,ELEMENT                                                  
         MVC   CTDATEL(2),=X'100D' LENGTH=NAME+2                                
         MVC   CTDATA(11),COUNTRYN                                              
         GOTO1 ADDELEM             AND ADD IT                                   
         SPACE 1                                                                
         USING CTRATED,R4                                                       
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   CTRAEL,X'B3'                                                     
         MVI   CTRALEN,CTRALNQ                                                  
         MVC   CTRADATE,PDATE                                                   
         MVC   CTRARATE,FULL                                                    
         SPACE 1                                                                
         XR    RF,RF               INSURE RECORD DOESN'T GET TOO LARGE          
         ICM   RF,3,CTRTLEN                                                     
         LA    R1,CTRALNQ                                                       
         AR    RF,R1                                                            
         CH    RF,=H'1000'                                                      
         BL    RTE30                                                            
         GOTO1 SEQ                 LOOK FOR ANOTHER RECORD                      
         CLC   CTRTKEY(CTRTSEQ-CTRTKEY),KEYSAVE                                 
         BE    RTE30               IF WE FOUND IT, ADD IT TO THIS ONE           
         MVC   KEY,KEYSAVE         ELSE SET TO ADD NEW RECORD                   
         MVC   CTRTKEY,KEY                                                      
         ZIC   R1,CTRTSEQ          BUMP SEQUENCE NUMBER                         
         AH    R1,=H'1'                                                         
         STC   R1,CTRTSEQ                                                       
         MVC   CTRTLEN,DATADISP                                                 
         XC    CTRTSTAT(5),CTRTSTAT                                             
         OI    STATUS,ADDIT                                                     
RTE30    GOTO1 ADDELEM             ADD RATE ELEMENT                             
         SPACE 1                                                                
         TM    WHENOK,X'01'        WE'RE DOING OUR OWN MAINTENANCE              
         BZ    XIT                                                              
         MVC   CONHEAD(L'ADDMSG),ADDMSG GIVE USER ADD MESSAGE                   
         TM    STATUS,ADDIT        DO WE NEED TO ADD                            
         BZ    RTE40                                                            
         XI    STATUS,ADDIT                                                     
         GOTO1 ADD                 YES                                          
         B     RTE52                                                            
RTE40    GOTO1 WRITE               ELSE WRITE THE RECORD                        
         B     RTE52               RE-DISPLAY                                   
         EJECT                                                                  
*              ROUTINE TO DISPLAY A RATE                                        
         SPACE 1                                                                
RTE50    CLI   MODE,DISPREC                                                     
         BE    RTE52                                                            
         CLI   MODE,XRECADD                                                     
         BE    RTE52                                                            
         CLI   MODE,XRECPUT                                                     
         BNE   RTE60                                                            
RTE52    TWAXC RTEDATEH            CLEAR UNPROTECTED FIELDS                     
         BAS   RE,GETRATE                                                       
         BE    *+6                                                              
         DC    H'0'                MISSING THIS RATE ELEMENT                    
         GOTO1 DATCON,DMCB,(1,CTRADATE),(8,RTEDATE)                             
         EDIT  (4,CTRARATE),(9,RTERATE),4,ALIGN=LEFT                            
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO DELETE A RATE                                         
         SPACE 1                                                                
RTE60    CLI   MODE,RECDEL         HANDLE DELETES                               
         BNE   RTE70                                                            
         BAS   RE,GETRATE          GET THE ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                IT HAS DISAPPEARED                           
         MVI   0(R4),X'FF'         SET TO DELETE IT                             
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         GOTO1 WRITE               WRITE BACK RECORD OURSELVES                  
         MVC   CONHEAD(L'DELMSG),DELMSG                                         
         B     MYEND                                                            
         EJECT                                                                  
*              DISPLAY KEY ROUTINE                                              
         SPACE 1                                                                
RTE70    CLI   MODE,DISPKEY                                                     
         BNE   RTE80                                                            
         MVC   RTECTRY,COUNTRYN    COUNTRY NAME                                 
         OI    RTECTRYH+6,X'80'                                                 
         SPACE 1                                                                
         ZIC   R1,SELLISTN         FIND TABLE ENTRY FOR ELEMENT                 
         MH    R1,=H'3'                                                         
         LA    R1,RATETAB(R1)                                                   
         MVC   PDATE,0(R1)         AND SAVE DATE FOR DISPLAY                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LIST RATES                                            
         SPACE 1                                                                
RTE80    CLI   MODE,LISTRECS                                                    
         BNE   *+16                                                             
         MVI   NLISTS,NLSTS                                                     
         LA    R2,LISTAR                                                        
         B     RTE82                                                            
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,P                                                             
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING CTRTREC,R3                                                       
         USING CTRATED,R4                                                       
RTE82    OC    KEY,KEY             DO WE HAVE KEY ALREADY                       
         BNZ   RTE84                                                            
         LA    R3,KEY              NO, BUILD IT                                 
         MVI   CTRTKTYP,CTRTEQU    RATE RECORD TYPE                             
         MVC   CTRTCTRY,COUNTRYC   BINARY EQUATE TO KEY                         
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,X'B3'                                                     
RTE83    BAS   RE,GETEL            R4=A(FIRST ELEMENT)                          
         BE    RTE86                                                            
         B     XIT                                                              
         SPACE 1                                                                
RTE84    LA    RF,RATETAB+L'RATETAB-3 HAVE KEY - START AT NEXT EL.              
         MVC   PDATE,0(RF)                                                      
         BAS   RE,GETRATE          FIND LAST ONE                                
         BNE   XIT                                                              
         BAS   RE,NEXTEL           AND START WITH NEXT                          
         BE    RTE86                                                            
         GOTO1 SEQ                 TRY FOR ANOTHER RECORD                       
         L     R4,AIO                                                           
         CLC   0(CTRTSEQ-CTRTKEY,R4),KEYSAVE                                    
         BE    RTE83                                                            
         B     XIT                                                              
         SPACE 1                                                                
RTE86    XC    RATETAB,RATETAB     CLEAR RATE TABLE                             
         LA    R3,RATETAB                                                       
         B     RTE89                                                            
         SPACE 1                                                                
RTE88    BAS   RE,NEXTEL           GET NEXT ONE                                 
         BE    RTE89                                                            
RTE85    GOTO1 SEQ                 TRY FOR ANOTHER RECORD                       
         L     R4,AIO                                                           
         CLC   0(CTRTSEQ-CTRTKEY,R4),KEYSAVE                                    
         BNE   XIT                                                              
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         BNE   XIT                                                              
         SPACE 1                                                                
RTE89    MVC   0(3,R3),CTRADATE    SAVE DATE IN TABLE                           
         GOTO1 DATCON,DMCB,(1,CTRADATE),(8,LINDATE) AND DISPLAY                 
         EDIT  (4,CTRARATE),(9,LINRATE),4                                       
         SPACE 1                                                                
         TM    WHEN,X'7F'          IF SPOOLING                                  
         BZ    RTE90                                                            
         GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         B     RTE88                                                            
RTE90    GOTO1 LISTMON             ELSE LET GENCON CONTROL                      
         LA    R3,3(R3)            AND BUMP TABLE POINTER                       
         B     RTE88                                                            
         EJECT                                                                  
*              ROUTINE TO FIND SPECIFIC RATE ELEMENT                            
         SPACE 1                                                                
         USING CTRATED,R4                                                       
GETRATE  NTR1                                                                   
         MVI   ELCODE,X'B3'                                                     
GETR2    L     R4,AIO                                                           
         BAS   RE,GETEL            LOOK FOR MATCHING DATES                      
         B     *+8                                                              
GETR4    BAS   RE,NEXTEL                                                        
         BNE   GETR6                                                            
         CLC   PDATE,CTRADATE                                                   
         BNH   GETRX                                                            
         B     GETR4                                                            
         SPACE 1                                                                
         USING CTRTREC,R3                                                       
GETR6    GOTO1 SEQ                 LOOK FOR ANOTHER RECORD                      
         L     R3,AIO                                                           
         CLC   CTRTKEY(CTRTSEQ-CTRTKEY),KEYSAVE                                 
         BNE   GETRX                                                            
         MVC   KEYSAVE,KEY         SAVE LAST GOOD KEY                           
         B     GETR2                                                            
         SPACE 1                                                                
GETRX    XIT1  REGS=(R4)           RETURN CC                                    
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
*              HEADLINE ROUTINES FOR SPOOLING                                   
         SPACE 2                                                                
HOOK     NTR1                                                                   
         MVC   H3+8(11),COUNTRYN                                                
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE 1                                                                
DATINV   MVI   ERROR,INVDATE                                                    
         B     THEEND                                                           
         SPACE 1                                                                
NTFOUND  MVI   ERROR,NOTFOUND                                                   
         LA    R2,RTEDATEH                                                      
         B     THEEND                                                           
         SPACE 1                                                                
RECINV   MVI   ERROR,RECEXIST                                                   
         LA    R2,RTEDATEH                                                      
         B     THEEND                                                           
         SPACE 1                                                                
CANTADD  MVC   CONHEAD(L'CANTMSG),CANTMSG                                       
         LA    R2,RTEDATEH                                                      
         B     MYEND                                                            
         SPACE 1                                                                
THEEND   GOTO1 ERREX               SYSTEM ERRORS                                
MYEND    GOTO1 ERREX2              MY ERRORS                                    
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
ADDMSG   DC    C'NEW RECORD HAS BEEN ADDED TO FILE'                             
DELMSG   DC    C'RECORD HAS BEEN DELETED - ENTER NEXT REQUEST'                  
CANTMSG  DC    C'** ERROR ** CANNOT ADD PREVIOUS RATE'                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
MYSPECS  DS    0H                  SPECS FOR SPOOLING                           
         SPROG 0                                                                
         SSPEC H1,1,RUN                                                         
         SSPEC H1,29,C'EXCHANGE RATE LISTING'                                   
         SSPEC H2,29,C'---------------------'                                   
         SSPEC H1,61,REQUESTOR                                                  
         SSPEC H3,1,C'COUNTRY'                                                  
         SSPEC H2,71,PAGE                                                       
         SSPEC H5,1,C'EFFECTIVE DATE    EXCHANGE RATE'                          
         DC    X'00'                                                            
         EJECT                                                                  
* FAFACTS                                                                       
* FACTRY                                                                        
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* CTSFMWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FACTRY                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE CTSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
STATUS   DS    XL1                                                              
ADDIT    EQU   X'80'               ADD NEW RECORD INSTEAD OF WRITE              
COUNTRYC DS    XL1                 COUNTRY CODE                                 
COUNTRYN DS    CL11                COUNTRY NAME                                 
PDATE    DS    PL3                 PWOS EFFECTIVE DATE                          
RATETAB  DS    CL(3*NLSTS)         TABLE OF DATES FOR LISTS                     
NLSTS    EQU   ((RLSLAST-RLSSELH)/(L'RLSDATA+3+8+8))                            
         SPACE 3                                                                
*              DSECT TO COVER LIST LINE                                         
         SPACE 1                                                                
LINED    DSECT                                                                  
LINDATE  DS    CL8                 EFFECTIVE DATE                               
         DS    CL14                                                             
LINRATE  DS    CL9                 EXCHANGE RATE                                
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMF5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFME5D                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012CTSFM05   05/01/02'                                      
         END                                                                    
