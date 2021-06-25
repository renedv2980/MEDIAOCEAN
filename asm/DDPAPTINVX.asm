*          DATA SET DDPAPTINVX AT LEVEL 007 AS OF 09/19/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTINXA                                                                 
         TITLE 'PANAPT INVENTORY EDIT EXIT'                                     
***********************************************************************         
*                                                                     *         
* THIS IS THE PANAPT INVENTORY EDIT EXIT. THE LOAD MODULE NAME IS     *         
* SUPPLIED IN THE LIBCODE DEFINITION(S) IN THE PANAPT CONTROL FILE.   *         
*                                                                     *         
* THIS ASSEMBLER MODULE WAS DERIVED FROM THE PANAPT-DELIVERED COBOL   *         
* SOURCE MODULE APCS0304.                                             *         
*                                                                     *         
* DESCRIPT: INVENTORY VERIFICATION EXIT.                              *         
*                                                                     *         
* FUNCTION:   TO REVIEW INVENTORY FILE MODIFICATIONS.                 *         
*                                                                     *         
* ENTRY COND:                                                         *         
*                                                                     *         
*   LINKAGE : STANDARD OS LINKAGE                                     *         
*   PARMS :   (USAGE IS IN=INPUT, OUT=OUTPUT, MOD=MODIFIED)           *         
*             (TYPE IS STRUCTURE = A COLLECTION OF RELATED DATA       *         
*              FIELDS USED TOGETHER AS A DATA STRUCTURE)              *         
*                                                                     *         
*    PARAMETER  USAGE   TYPE           DESCRIPTION                    *         
*    --------- ------ ----------- ------------------------------      *         
*    APAMINV     I    STRUCTURE    PANEL APIP610 INPUT DATA.          *         
*    APAMDIB2    I    STRUCTURE    INVENTORY FILE RECORD.             *         
*    APAMLIB2    I    STRUCTURE    LIBRARY CODE FILE RECORD.          *         
*    APCCENVM    I    STRUCTURE    PANAPT ENVIRONMENTAL DATA.         *         
*                                                                     *         
*   EXIT COND.                                                        *         
*                                                                     *         
*     RETURN : 0 <===> DATA IN APCCINV1 HAS BEEN VERIFIED, AND        *         
*      CODES :         IS TO REPLACE THE CORRESPONDING DATA IN        *         
*                      THE CURRENT INVENTORY RECORD.                  *         
*                                                                     *         
*              4 <===> DATA IN APCCINV1 HAS BEEN VERIFIED AND         *         
*                      THERE SHOULD BE NO REPLACEMENT OF DATA         *         
*                      IN THE CURRENT INVENTORY RECORD.               *         
*                                                                     *         
*              8 <===> INVALID ACTION SPECIFIED.                      *         
*                                                                     *         
* THIS ROUTINE ALLOWS THE USER THE OPPORTUNITY TO REVIEW PENDING      *         
* CREATION OR MODIFICATIONS TO THE INVENTORY FILE. THE DATA           *         
* ENTERED ON PANEL APIP610 AS WELL AS EXISTING DATA IS AVAILABLE      *         
* FOR INSPECTION.                                                     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
PAPTINX  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
PAPTINX  AMODE ANY                                                              
         USING (PAPTINX,PAPTINXX),RC   ESTABLISH BASE ADDRESSABILITY            
         REQUS                                                                  
         SAVE  (14,12),,PAPTINX-&SYSDATE-&SYSTIME                               
         LR    RC,RF               LOAD BASE ADDRESS                            
         LARL  RF,SAVEAREA         FETCH POINTER TO SAVEAREA                    
         ST    RD,4(RF)            SAVE LINKAGE TO CALLING PROGRAM              
         ST    RF,8(RD)            CHAIN SAVEAREAS                              
         LR    RD,RF               POINT TO CURRENT SAVEAREA                    
         USING SAVEAREA,RD                                                      
*                                                                               
         LM    R3,R6,0(R1)         FETCH PARM POINTERS                          
         USING PANEL_APAMINV_DSECT,R3    INVENTORY DATA FROM PANELS...          
*                                        ...APIP610, APIP611, APIP612           
         USING APAMDIB2_DSECT,R4   INVENTORY RECORD LAYOUT                      
         USING APAMLIB2,R5         LIBCODE BLOCK                                
         USING APAMENVM_DSECT,R6   ENVIRONMENT BLOCK                            
*                                                                               
         SR    RF,RF               DEFAULT RETURN CODE IS ZERO                  
*                                                                               
         MVC   EUVMSMG,SPACES      INITIALIZE RETURN MESSAGE                    
*                                                                               
*======================================================================         
*                                                                               
* ****    N E V E R    R E M O V E    T H I S    C O D E    ****                
* WE NEED TO MAKE SURE THAT NO MATTER WHAT ELSE HAPPENS, A PANAPT               
* ADMINISTRATOR USERID CAN DO WHATEVER HE/SHE WANTS WITH AN INVENTORY           
* RECORD.                                                                       
*                                                                               
         L     RE,PSAAOLD-PSA(,0)       GET CURRENT/HOME ASCB                   
         L     RE,(ASCBASXB-ASCB)(,RE)  GET ASXB ADDRESS                        
         L     RE,(ASXBSENV-ASXB)(,RE)  GET ACEE ADDRESS                        
         CLC   =C'ACEE',0(RE)           VALID ACEE?                             
         BE    *+6                      YES: EXTRACT RACF USERID                
         DC    H'0'                     NO: IMPOSSIBLE                          
         MVC   USERID,(ACEEUSRI-ACEE)(RE)  SAVE THE USERID                      
         CLC   =C'APT',USERID           *NEVER* DENY AN...                      
         BE    XIT                      ...ADMINSTRATOR PERMISSION!             
*                                                                               
*======================================================================         
         EJECT                                                                  
*                                  MAKE SURE CALLED ACTION IS VALID             
         CLC   ENVMACT,=C'ADD     '                                             
         BE    CHKPANEL                                                         
         CLC   ENVMACT,=C'CHG     '                                             
         BE    CHKPANEL                                                         
         CLC   ENVMACT,=C'APP     '                                             
         BE    CHKPANEL                                                         
         CLC   ENVMACT,=C'AUTOADD '                                             
         BE    CHKPANEL                                                         
*                                                                               
         MVC   EUVMSMG(27),=C'PAPTINX-01: Invalid action '                      
         MVC   EUVMSMG+27(8),ENVMACT                                            
         MVC   EUVMSMG+35(15),=C' was specified.'                               
         LHI   RF,8                SET RC = 8                                   
         B     XIT                                                              
*                                                                               
CHKPANEL DS    0H                                                               
         CLC   P$INVPNLID,=C'APIP610'   MAIN INVENTORY MAINT PANEL              
         BE    VAL610                                                           
         CLC   P$INVPNLID,=C'APIP611'   USER DATA MAINTENANCE PANEL             
         BE    VAL611                                                           
         CLC   P$INVPNLID,=C'APIP612'   COMPILE/LINK OPTIONS PANEL              
         BE    VAL612                                                           
*                                                                               
         MVC   EUVMSMG,=CL79'PAPTINX-03: Exit reached by unknown panel.+        
               '                                                                
         LHI   RF,8                SET RC = 8                                   
         B     XIT                                                              
         EJECT                                                                  
VAL610   DS    0H                                                               
         CLC   P$INVOWNER,=C'        '  NO INVENTORY OWNER PERMITTED            
         BE    *+18                IT'S BLANK                                   
         MVC   EUVMSMG,=CL79'PAPTINX-02: "Owned by" field must be blank+        
               .'                                                               
         LHI   RF,4                SET RC: INVALID INVENTORY DATA               
         B     VAL610X                                                          
*                                                                               
         CLI   P$INVDCFLG,C'Y'     COMPILE/LINK OPTIONS FLAG ON?                
         BNE   *+18                                                             
         MVC   EUVMSMG,=CL79'PAPTINX-05: Compile/Link Options are not s+        
               upported. Field must be "N".'                                    
         LHI   RF,4                SET RC: INVALID INVENTORY DATA               
         B     VAL610X                                                          
*                                                                               
* WE NEED TO PREVENT UNAUTHORIZED USERS FROM SETTING CERTAIN USER               
* DATA FIELDS (E.G., STGE-BYPASS). UNFORTUNATELY, PANAPT DOESN'T PASS           
* ANY USER IDENTIFICATION OR AUTHORIZATION DATA TO THIS EXIT (LIKE IT           
* DOES TO THE SECURITY EXIT). SO WHILE WE CAN DIG THE USERID OUT OF THE         
* ACEE BLOCK, WE CAN'T KNOW IF THE USER IS AUTHORIZED TO MAKE CHANGES.          
* SO WE WOULD NEED TO ADD HARD CODE (OR TABLES) TO THIS PROGRAM TO              
* ENFORCE THIS.                                                                 
*                                                                               
* AS THIS WOULD BE RATHER ONEROUS, WE DO IT BY SETTING THE INVENTRY/CHG         
* ACTIVITY RECORD IN THE PANAPT CONTROL FILE TO ONLY ALLOW AUTHORIZED           
* USERS TO CHANGE AN INVENTORY RECORD. I.E., ANYONE CAN DO AN AUTOADD,          
* BUT ONLY AUTHORIZED USERS CAN CHANGE AN INVENTORY RECORD ONCE IT'S            
* BEEN ADDED.                                                                   
*                                                                               
* IF WE EVER NEED TO HAVE DIFFERENT LEVELS OF SECURITY FOR DIFFERENT            
* USER DATA FIELDS, WE'LL NEED TO FIGURE OUT ANOTHER SOLUTION.                  
*                                                                               
         CLC   ENVMACT,=C'CHG     '  IF THE RECORD IS BEING CHANGED...          
         BE    VAL610X               ...LET PANAPT'S SECURITY HANDLE IT         
         CLI   P$INVDUFLG,C'Y'     USER DATA FIELDS BEING UPDATED?              
         BNE   VAL610X             NO: OKAY                                     
         MVC   EUVMSMG,=CL79'PAPTINX-04: User data may only be modified+        
                from action CHG. Field must be "N".'                            
         LHI   RF,4                SET RC: INVALID INVENTORY DATA               
         B     VAL610X                                                          
         EJECT                                                                  
VAL611   DS    0H                  VALIDATE USER DATA OPTIONS HERE              
*                                                                               
         CLC   P$INVUSE01,SPACES   DEFAULT                                      
         BE    USER03                                                           
         CLC   =C'Y ',P$INVUSE01   YES                                          
         BE    USER03                                                           
         CLC   =C'N ',P$INVUSE01   NO                                           
         BE    USER03                                                           
*                                                                               
         MVC   EUVMSMG,=CL79'PAPTINX-06: Invalid "Critical module" valu+        
               e. Specify "Y", "N", or blank.'                                  
         LHI   RF,4                SET RC: INVALID INVENTORY DATA               
         B     VAL610X                                                          
*                                                                               
USER03   DS    0H                                                               
         CLC   P$INVUSE03,SPACES   DEFAULT                                      
         BE    USER04                                                           
         CLC   =C'Y ',P$INVUSE03   YES                                          
         BE    USER04                                                           
         CLC   =C'N ',P$INVUSE03   NO                                           
         BE    USER04                                                           
*                                                                               
         MVC   EUVMSMG,=CL79'PAPTINX-07: Invalid "STGE Bypass OK" value+        
               . Specify "Y", "N", or blank.'                                   
         LHI   RF,4                SET RC: INVALID INVENTORY DATA               
         B     VAL610X                                                          
*                                                                               
USER04   DS    0H                                                               
         CLC   P$INVUSE04,SPACES   DEFAULT                                      
         BE    USER04E                                                          
         CLC   =C'Y ',P$INVUSE04   YES                                          
         BE    USER04E                                                          
         CLC   =C'N ',P$INVUSE04   NO                                           
         BE    USER04E                                                          
*                                                                               
         MVC   EUVMSMG,=CL79'PAPTINX-08: Invalid "Test module only" val+        
               ue. Specify "Y", "N", or blank.'                                 
         LHI   RF,4                SET RC: INVALID INVENTORY DATA               
         B     VAL610X                                                          
*                                                                               
USER04E  DS    0H                                                               
         IF (CLC,P$INVUSE06,NE,SPACES),AND,                                     
            (CLC,=C'Y ',NE,P$INVUSE06),AND,                                     
            (CLC,=C'N ',NE,P$INVUSE06)                                          
           MVC   EUVMSMG,=CL79'PAPTINX-08: Invalid "Record DSECT?" valu+        
               e. Specify "Y", "N", or blank.'                                  
           LHI   RF,4                SET RC: INVALID INVENTORY DATA             
           B     VAL610X                                                        
         ENDIF ,                                                                
*                                                                               
*&&DO                                                                           
* ENABLE THIS CODE IF WE EVER GET PARANOID, AND WANT TO PREVENT ANYONE          
* EXCEPT A PANAPT ADMINISTRATOR FROM SETTING THE FLAG TO "Y".                   
         CLC   =C'Y ',P$INVUSE04   USER ENTERED A 'Y' ?                         
         BNE   VAL610X             NO: ANYONE CAN TURN THE FLAG *OFF*           
         CLC   =C'APT',USERID      ONLY AN ADMINISTRATOR CAN SET...             
         BE    VAL610X             ...A MEMBER TO "PERMANENT TEST"              
         CLC   =C'ENV=SBX',L2IPARM UNLESS WE'RE IN SBX                          
         BE    VAL610X                                                          
         MVC   EUVMSMG,=CL79'PAPTINX-09: Only a PanAPT Administrator ma+        
               y set "Test module only" to "Y".'                                
         LHI   RF,4                SET RC: INVALID INVENTORY DATA               
*&&                                                                             
         CLC   P$INVUSE11,SPACES   VALIDATE NOTIFICATION GROUP NAME(S)          
         BE    VAL610X             NO GROUPS: THAT'S OKAY                       
*                                                                               
         LA    R7,P$INVUSE11       PREPARE TO PARSE THIS FIELD                  
USER11B  DS    0H                                                               
         MVC   WORK,SPACES         PRESET VALUE TO SPACES                       
         LA    R1,WORK                                                          
         SR    R0,R0               COUNT NUMBER OF CHARACTERS                   
USER11D  CLI   0(R7),C' '          GROUP WILL END WITH COMMA OR BLANK           
         BE    USER11F                                                          
         CLI   0(R7),C','                                                       
         BE    USER11F                                                          
         MVC   0(1,R1),0(R7)       BUILD GROUP ONE CHARACTER AT A TIME          
         LA    R1,1(R1)                                                         
         LA    R7,1(R7)                                                         
         AHI   R0,1                                                             
         CHI   R0,NTFYGRPLQ                                                     
         BNH   USER11D                                                          
         B     USER11X             GROUP NAME IS TOO MANY CHARACTERS            
*                                                                               
USER11F  DS    0H                                                               
         LA    R1,NOTIFICATION_GROUPS                                           
USER11H  DS    0H                                                               
         CLI   0(R1),X'FF'         EOT?                                         
         BE    USER11X             YES: INVALID GROUP NAME                      
         CLC   WORK(NTFYGRPLQ),0(R1)  CHECK GROUP NAME VALIDITY                 
         BE    USER11J             IT'S IN THE TABLE: THIS ONE IS OKAY          
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FE'         SCAN FORWARD TO END OF E-MAIL LIST           
         BNE   *-8                                                              
         LA    R1,1(R1)            BUMP PAST EOL INDICATOR                      
         B     USER11H             TRY NEXT GROUP                               
*                                                                               
USER11J  DS    0H                                                               
         CLI   0(R7),C' '          WAS THIS THE LAST GROUP IN THE LIST?         
         BE    VAL610X             YES: WE'RE DONE                              
         LA    R7,1(R7)            NO: BUMP PAST COMMA, GET NEXT GROUP          
         B     USER11B                                                          
*                                                                               
USER11X  DS    0H                                                               
         MVC   EUVMSMG,=CL79'PAPTINX-10: Invalid "Notification group" v+        
               alue.'                                                           
         LHI   RF,4                SET RC: INVALID INVENTORY DATA               
         B     VAL610X                                                          
*                                                                               
VAL610X  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
VAL612   DS    0H                  VALIDATE COMPILE/LINK OPTIONS HERE           
         B     XIT                                                              
*                                                                               
XIT      DS    0H                                                               
         L     RD,4(,RD)           RESTORE LINKAGE TO CALLING PROGRAM           
*                                                                               
         RETURN (14,12),RC=(15)    RETURN TO CALLING PROGRAM                    
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
PAPTINXX EQU   *                                                                
         EJECT                                                                  
SAVEAREA DC    18F'0'              LOCAL SAVEAREA                               
WORK     DS    CL80                                                             
USERID   DS    CL8                 THIS TSO USER                                
SPACES   DC    CL80' '                                                          
         EJECT                                                                  
       ++INCLUDE DDPAPTNTFY                                                     
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* THIS DSECT WAS ORIGINALLY TAKEN FROM CA-DELIVERED MEMBER APAMINV.   *         
* IT APPEARS THAT AT ONE TIME, IT WAS *SUPPOSED* TO BE A STRUCTURE    *         
* REPRESENTING THE FIELDS PRESENT ON THE INVENTORY MAINTENANCE        *         
* PANELS, TO BE USED BY THIS EXIT FOR VALIDATION. IN OTHER WORDS, IT  *         
* WAS SUPPOSED TO BE AN ASSEMBLER DSECT EQUIVALENT TO THE COBOL       *         
* MEMBER APCCINV1. HOWEVER, DEIS DISCOVERED TWO THINGS:               *         
*    1. THERE WERE SYMBOL COLLISIONS BETWEEN MEMBERS APAMINV AND      *         
*        APAMDIB2, WHICH IS ALSO PASSED AS A PARAMETER TO THIS        *         
*        MODULE.                                                      *         
*    2. AT SOME POINT, APCCINV1 AND APAMINV START TO DIFFER, AND      *         
*        EMPIRICAL OBSERVATION SHOWS THAT THE COBOL MEMBER IS         *         
*        CORRECT, NOT APAMINV.                                        *         
*    3. THE PANAPT DOCUMENTATION SAYS THAT APCCDIB2 AND APAMINV ARE   *         
*        EQUIVALENT, WHICH IS OBVIOUSLY NOT TRUE IF WE JUST LOOK AT   *         
*        THEM.                                                        *         
* IN THE END, THE FOLLOWING DSECT WAS CONSTRUCTED BASED UPON MEMBER   *         
* APCCINV1.                                                           *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
* NAME       : APAMINV                                                *         
* PRODUCT    : PANAPT                                                 *         
* TYPE       : ASSEMBLER COPYBOOK                                     *         
*                                                                     *         
* DESCRIPT.  : APAMINV  INVENTORY RECORD LAYOUT.                      *         
*                                                                     *         
* NOTICES    : THIS MODULE IS PART OF THE DISTRIBUTED SOURCE          *         
*              CODE FOR PANAPT.                                       *         
*                                                                     *         
* FUNCTION   : TO PROVIDE A COMMON RECORD DESCRIPTION FOR THE         *         
*              INVENTORY RECORD PASSED TO THE MEMBER EXISTENCE        *         
*              EXITS.                                                 *         
*                                                                     *         
* RELATED TO : APCCINV1 MUST REFLECT ANY CHANGES TO THIS MEMBER.      *         
*                                                                     *         
* COMMENTS   : NONE.                                                  *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
PANEL_APAMINV_DSECT DSECT                                                       
P$INVREC DS    0H            BEGINNING OF COMMON INVENTORY DATA LAYOUT          
P$INRECLNG EQU (P$INRECEND-P$INVREC)   L'(COMMON INVENTORY DATA LAYOUT)         
*---------------------------------------------------------------------          
*                 GENERAL INVENTORY DATA (FROM APIP610)                         
*---------------------------------------------------------------------          
P$INVLBCDE DS    CL04              LIBRARY CODE                                 
P$INVLBSUB DS    CL03              LIBRARY SUBCODE                              
P$INVQUAL  DS    CL08              INVENTORY QUALIFIER                          
P$INVMEM   DS    CL10              MEMBER NAME                                  
P$INVOWNER DS    CL08              OWNER ID                                     
P$INVAPFLG DS    CL01              APPROVED FLAG                                
P$INVLSTMR DS    CL06              LAST MOVED BY MOVE REQUEST NUMBER            
P$INVDESCR DS    CL55              DESCRIPTION                                  
P$INVENVIR DS    CL08              ENVIRONMEMT                                  
P$INVAPPL  DS    CL08              APPLICATION                                  
P$INVLANG  DS    CL08              LANGUAGE                                     
P$INVCOMM  DS    CL55              COMMENTS                                     
P$INVPNLID DS    CL7               INV MAINTENANCE PANEL ID                     
P$INVDCFLG DS    CL1               COMPILE/LINK OPTIONS: Y/N SWITCH             
P$INVDUFLG DS    CL1               USER DATA: Y/N SWITCH                        
P$INVASFLG DS    CL01              ASSIGNED FLAG                                
P$INVASUSR DS    CL08              ASSIGNED TO USER                             
P$INVASDTE DS    0CL08             ASSIGNMENT DATE INFORMATION                  
P$INVASDCC DS    CL02              CENTURY                                      
P$INVASDYY DS    CL02              YEAR                                         
P$INVASDMM DS    CL02              MONTH                                        
P$INVASDDD DS    CL02              DAY OF MONTH                                 
P$INVASTME DS    0CL06             ASSIGNMENT TIME INFORMATION                  
P$INVASTHH DS    CL02              HOUR                                         
P$INVASTMM DS    CL02              MINUTE                                       
P$INVASTSS DS    CL02              SECOND                                       
P$INVASSMR DS    CL06              ASSIGNED TO MOVE REQUEST                     
*---------------------------------------------------------------------          
*                 USER DATA INFORMATION TABLES (FROM APIP611)                   
*---------------------------------------------------------------------          
P$INVUSE01 DS    CL8               USER DATA FIELD 01                           
P$INVUSE02 DS    CL8               USER DATA FIELD 02                           
P$INVUSE03 DS    CL8               USER DATA FIELD 03                           
P$INVUSE04 DS    CL8               USER DATA FIELD 04                           
P$INVUSE05 DS    CL8               USER DATA FIELD 05                           
P$INVUSE06 DS    CL16              USER DATA FIELD 06                           
P$INVUSE07 DS    CL16              USER DATA FIELD 07                           
P$INVUSE08 DS    CL16              USER DATA FIELD 08                           
P$INVUSE09 DS    CL16              USER DATA FIELD 09                           
P$INVUSE10 DS    CL16              USER DATA FIELD 10                           
P$INVUSE11 DS    CL50              USER DATA FIELD 11                           
P$INVUSE12 DS    CL50              USER DATA FIELD 12                           
P$INVUSE13 DS    CL50              USER DATA FIELD 13                           
P$INVUSE14 DS    CL50              USER DATA FIELD 14                           
P$INVUSE15 DS    CL50              USER DATA FIELD 15                           
P$INVUSE16 DS    CL50              USER DATA FIELD 16                           
P$INVUSE17 DS    CL50              USER DATA FIELD 17                           
P$INVUSE18 DS    CL50              USER DATA FIELD 18                           
P$INVUSE19 DS    CL50              USER DATA FIELD 19                           
P$INVUSE20 DS    CL50              USER DATA FIELD 20                           
*---------------------------------------------------------------------          
*                 COMPILE/LINK DATA (FROM APIP612)                              
*---------------------------------------------------------------------          
P$INV_COMP_OPT DS    CL60          COMPLIER OPTIONS                             
P$INV_LINK_OPT DS    CL60          LINK OPTIONS                                 
P$INV_DB_OPT   DS    CL60          DATA BASE PRECOMPILE                         
P$INV_CICS_OPT DS    CL60          CICS PRECOMPILE                              
P$INV_LINK_STREAM DS CL10          LINKAGE EDITOR CONTROL STMT MEMBRNAM         
*                                                                               
* FOR OVERRIDE FIELDS, Y MEANS THE LIST, OBJ, OR LOAD MUST BE KEPT. IF          
* THE LIBRARY CODE IS NO LONGER SETUP TO KEEP THE OUTPUT, A MODELLING           
* ERROR SHOULD BE GENERATED BY THE MODEL.  N MEANS DON'T KEEP THE               
* OUTPUT, EVEN IF THE LIBRARY CODE IS SETUP TO KEEP IT. BLANK MEANS             
* SAVE IT IF THE LIBRARY CODE IS SETUP TO SAVE IT, OTHERWISE DON'T              
* SAVE IT.                                                                      
P$INV_LIST_OVERRIDE   DS CL1                                                    
P$INV_OBJECT_OVERRIDE DS CL1                                                    
P$INV_LOAD_OVERRIDE   DS CL1                                                    
* LIBCODE SUPPORT                                                               
P$INV_LIST_SUPPORT    DS CL1                                                    
P$INV_OBJECT_SUPPORT  DS CL1                                                    
P$INV_LOAD_SUPPORT    DS CL1                                                    
*                                                                               
P$INRECEND EQU   *                 END OF COMMON INVENTORY DATA LAYOUT          
         EJECT                                                                  
APAMDIB2_DSECT DSECT                                                            
         COPY APAMDIB2                                                          
         EJECT                                                                  
         COPY APAMLIB2                                                          
         EJECT                                                                  
APAMENVM_DSECT DSECT                                                            
         COPY APAMENVM                                                          
         EJECT                                                                  
         IHAPSA                                                                 
         IHAACEE                                                                
         IHAASCB                                                                
         IHAASXB                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DDPAPTINVX09/19/19'                                      
         END                                                                    
