*          DATA SET PRWRI01    AT LEVEL 128 AS OF 10/09/20                      
*PHASE T40501A,*                                                                
*INCLUDE PUBEDIT                                                                
*        TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - HISTORY'                 
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - HISTORY'                 
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-50579  10/09/20 SPECIAL FILE NAMES FOR ACTIVE             *         
* AKAT SPEC-31455  01/23/19 INCREASE CLEARANCE BUFFER                 *         
* AKAT SPEC-13756  06/20/17 FIX MEMORY LEAK WHEN REPORTING CLEARANCES *         
***********************************************************************         
*                                                                     *         
* AKAT 02/23/12 SC JOHNSON ESTIMATE FILE SUPPORT                      *         
*                                                                     *         
* BOBY 01/16/09 INCREASE SIZE OF CLRST TABLE                          *         
*                                                                     *         
* BOBY 11/20/07 SUPPORT OUTPUT TO FILE                                *         
*                                                                     *         
* BOBY 02/20/04 STOP DEFAULT LOAD OF OUTPUT WITH 'DOWN'               *         
*                                                                     *         
* BOBY 11/15/94 ADD SECOND REPORT AFTER MAIN REPORT                   *         
*                                                                     *         
* ROSA 4/26/91 ALLOW AOR RECORD TO BE IN CORE FOR DRIVER USE          *         
*                                                                     *         
* ROSA 4/2/91  INCREASE SIZE OF CONTRACT SAVE AREA FROM 1000>1500     *         
*                                                                     *         
* ROSA 7/12/90  ALWAYS LOAD WORK AREA IF OFF LINE                     *         
*                                                                     *         
* ROSA 6/14/90  INCREASE SIZE OF WORK AREA .. NEEDED BY T40511        *         
*                                                                     *         
* ROSA 5/14/90  TABLE STORAGE MANAGEMENT-- IF ESTIMATES ARE NOT NEEDED*         
* ****L0A****   USE ESTIMATE AREA// IF DRD NOT RQSTED USE THAT AREA   *         
*                                                                     *         
* BPLA 5/4/90   MORE GF TAPE LOGIC                                    *         
*                                                                     *         
* ROSA 4/25/90  ADD GF TAPE LOGIC                                     *         
*                                                                     *         
***********************************************************************         
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - INIT'                    
**********************************************************************          
*                                                                    *          
*        PROGRAM INITIALIZATION                                      *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
T40501   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40501,RA,RR=R2                                                
*                                                                               
         L     RC,0(R1)            ESTABLISH WORKING STORAGE                    
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         ST    R9,SAVEREG9                                                      
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         ST    R2,RELO01                                                        
*                                                                               
         ST    RD,SAVERD01         SAVE A(REGISTER SAVE AREA)                   
*                                                                               
*        SET MORE SYSTEM ADDRESSES                                              
*                                                                               
         LA    RE,DRIVGEN                                                       
         ST    RE,ADRIVER          A(DRIVER CALLING ROUTINE)                    
*                                                                               
         L     R1,TWADCONS                                                      
         L     R1,TSPFUSER-TWADCOND(R1)                                         
         ST    R1,PBPLEDCB         PASS PUBERR DCB ADDRESS                      
*                                                                               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - GENCALL'                 
**********************************************************************          
*                                                                    *          
*        DETERMINE GENCON CALL                                       *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         CLI   MODE,RUNFRST        RUNFIRST                       L01           
         BE    FRST                                                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
*                                                                               
         CLI   MODE,VALREC         VALIDATE REQUEST PARAMETERS                  
         BE    VREQ                                                             
*                                                                               
         CLI   MODE,ERRHOOK        HANDLE ERROR                                 
         BNE   *+12                                                             
         BRAS  RE,MQRPTERR                                                      
         J     XIT                                                              
*                                                                               
         CLI   MODE,RUNLAST        RUNLAST                                      
         BE    LST                                                              
*                                                                               
         B     PRT                                                              
*                                                                               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - VKEY'                    
**********************************************************************          
*                                                                    *          
*        VALIDATE HEADER FIELDS                                      *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VKEY     DS    0H                                                               
*                                                                               
*        READ W0 PROFILE                                                        
*                                                                               
         XC    DUB,DUB             PARAMETERS FOR GETPROF                       
*                                                                               
         MVI   DUB,C'P'            SET SYSTEM                                   
         MVC   DUB+2(2),=C'W0'     W0 - WRITER PROFILE                          
         MVC   DUB+4(2),PBQAGY     AGENCY                                       
*                                                                               
         GOTO1 GETPROF,DMCB,DUB,PBQPRFW0,DATAMGR                                
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    VKEYFLTX                                                         
*                                                                               
         L     R2,EFHTAG           YES-LOCATE FILTER FIELD                      
         SR    R0,R0                                                            
         LA    RE,3                                                             
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    *+8                  YES                                         
         LA    RE,6                 NO - FILTER FIELD DIFFERENT!                
*                                                                               
         IC    R0,0(R2)            POINT TO FILTER FIELD                        
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
         CLI   PBQPRFW0+8,C'Y'     IF  FILTER REQUIRED                          
         BNE   VKEYFLTN                                                         
*                                                                               
         TM    AUTH,X'40'          IF AUTHORIZED                                
         BO    *+12                                                             
         CLI   DDS,C'Y'            IF DDS TERMINAL                              
         BNE   *+16                                                             
         NI    1(R2),X'FF'-X'0C'      TURN ON FIELD                             
         OI    6(R2),X'80'            FORCE RE-TRANSMISSION                     
         B     VKEYFLT3               DON'T VALIDATE, JUST ACCEPT               
*                                                                               
         TM    1(R2),X'0C'         IF FILTER HAS NON-ZERO INTENSITY             
         BO    *+12                                                             
         OI    1(R2),X'0C'            TURN OFF FIELD                            
         OI    6(R2),X'80'            FORCE RE-TRANSMISSION                     
*                                                                               
         CLI   5(R2),4             TEST AT LEAST 4 FILTER CHARACTERS            
         BL    VKEYFLTE                                                         
*                                                                               
         ZIC   RE,5(R2)                                                         
         LA    R1,8(R2)            CHARACTERS * AND - NOT ALLOWED               
*                                                                               
         CLI   0(R1),C'*'                                                       
         BE    *+8                                                              
         CLI   0(R1),C'-'                                                       
         BE    VKEYFLTE                                                         
         LA    R1,1(R1)                                                         
         BCT   RE,*-20                                                          
*                                                                               
VKEYFLT3 DS    0H                                                               
*                                                                               
         MVC   FILTER,8(R2)        SAVE VALID FILTER                            
*                                                                               
         B     VKEYFLTX                                                         
*                                                                               
VKEYFLTN DS    0H                  FILTER NOT REQUIRED                          
*                                                                               
         TM    1(R2),X'0C'         IF FILTER HAS ZERO INTENSITY                 
         BNO   *+12                                                             
         NI    1(R2),X'FF'-X'0C'      TURN ON FIELD                             
         OI    6(R2),X'80'            FORCE RE-TRANSMISSION                     
*                                                                               
VKEYFLTX DS    0H                                                               
*                                                                               
VKEYX    B     XIT                                                              
*                                                                               
VKEYFLTE DS    0H                  INVALID FILTER                               
*                                                                               
         MVI   ERROR,PWEFLTNV      INVALID FILTER                               
         GOTO1 CURSERR                                                          
*                                                                               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - DREC'                    
**********************************************************************          
*                                                                    *          
*        DISPLAY WRITER FORMAT                                       *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
DREC     DS    0H                                                               
*                                                                               
*        READ W0 PROFILE                                                        
*                                                                               
         OC    PBQPRFW0,PBQPRFW0   SKIP IF ALREADY READ                         
         BNZ   DRECPRFX                                                         
*                                                                               
         XC    DUB,DUB             PARAMETERS FOR GETPROF                       
*                                                                               
         MVI   DUB,C'P'            SET SYSTEM                                   
         MVC   DUB+2(2),=C'W0'     W0 - WRITER PROFILE                          
         MVC   DUB+4(2),PBQAGY     AGENCY                                       
*                                                                               
         GOTO1 GETPROF,DMCB,DUB,PBQPRFW0,DATAMGR                                
*                                                                               
DRECPRFX DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    DRECFLTX                                                         
*****    B     DRECFLTX                                                         
*                                                                               
         L     R2,EFHTAG           YES-LOCATE FILTER FIELD                      
         SR    R0,R0                                                            
         LA    RE,3                                                             
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    *+8                  YES                                         
         LA    RE,6                 NO - FILTER FIELD DIFFERENT!                
*                                                                               
         IC    R0,0(R2)            POINT TO FILTER FIELD                        
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
         CLI   PBQPRFW0+8,C'Y'     IF  FILTER REQUIRED                          
         BNE   DRECFLTN                                                         
*                                                                               
         CLI   DDS,C'Y'            IF DDS TERMINAL                              
         BNE   *+16                                                             
         NI    1(R2),X'FF'-X'0C'      TURN ON FIELD                             
         OI    6(R2),X'80'            FORCE RE-TRANSMISSION                     
         B     DRECFLT3               DON'T VALIDATE, JUST ACCEPT               
*                                                                               
         TM    1(R2),X'0C'         IF FILTER HAS NON-ZERO INTENSITY             
         BO    *+12                                                             
         OI    1(R2),X'0C'            TURN OFF FIELD                            
         OI    6(R2),X'80'            FORCE RE-TRANSMISSION                     
*                                                                               
*        ON DISPLAY GEGENPRG DOESN'T SET FILTER FIELD LENGTH                    
*        SO WE HAVE TO DETERMINE LENGTH BY FINDING LAST NON-BLANK               
*        ALSO LIST SCREEN FIELD LENGTH IS LONGER AND MUST                       
*        ALWAYS BE KEYED IN. (LIST SCREEN SHOULD NEVER GET HERE)                
*                                                                               
         LA    RE,L'WRIRFL         MAX FILTER LENGTH                            
         CLM   RE,1,5(R2)          DEFAULT TO HIGHER LENGTH                     
         BNL   *+8                                                              
         ICM   RE,1,5(R2)                                                       
*                                                                               
         LA    R1,8(RE,R2)         LAST BYTE OF FILTER                          
         BCTR  R1,0                                                             
*                                                                               
         CLI   0(R1),C' '          FIND LAST OF FILTER                          
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RE,*-10                                                          
*                                                                               
         CH    RE,=H'4'            FILTER MUST BE 4 LONG                        
         BL    DRECFLTE                                                         
*                                                                               
*                                  RE HAS FILTER LENGTH                         
         LA    R1,8(R2)            CHARACTERS * AND - NOT ALLOWED               
*                                                                               
         CLI   0(R1),C'*'                                                       
         BE    *+8                                                              
         CLI   0(R1),C'-'                                                       
         BE    DRECFLTE                                                         
         LA    R1,1(R1)                                                         
         BCT   RE,*-20                                                          
*                                                                               
         CLC   FILTER,8(R2)        MUST MATCH FILTER FOUND IN VKEY              
         BNE   DRECFL1E                                                         
*                                                                               
DRECFLT3 DS    0H                                                               
*                                                                               
         MVC   FILTER,8(R2)        SAVE VALID FILTER                            
*                                                                               
         B     DRECFLTX                                                         
*                                                                               
DRECFLTN DS    0H                  FILTER NOT REQUIRED                          
*                                                                               
         TM    1(R2),X'0C'         IF FILTER HAS ZERO INTENSITY                 
         BNO   *+12                                                             
         NI    1(R2),X'FF'-X'0C'      TURN ON FIELD                             
         OI    6(R2),X'80'            FORCE RE-TRANSMISSION                     
*                                                                               
DRECFLTX DS    0H                                                               
*                                                                               
DRECX    B     XIT                                                              
*                                                                               
DRECFL1E DS    0H                  SECURITY LOCKOUT                             
         MVI   ERROR,SECLOCK       ERROR CODE                                   
*                                                                               
         MVC   8(L'FILTER,R2),FILTER   RE-DISPLAY ENTERED FILTER                
         MVI   5(R2),L'FILTER          FIELD LENGTH                             
         OI    6(R2),X'80'             FORCE RE-TRANSMISSION                    
*                                                                               
         TWAXC WRIMEDH             CLEAR SCREEN                                 
*                                                                               
         B     DRECERR                                                          
*                                                                               
DRECFLTE DS    0H                  INVALID FILTER                               
*                                                                               
         MVI   ERROR,PWEFLTNV      INVALID FILTER                               
*                                                                               
DRECERR  DS    0H                  INVALID FILTER                               
*                                                                               
         GOTO1 CURSERR                                                          
*                                                                               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - VREQ'                    
**********************************************************************          
*                                                                    *          
*        VALIDATE REQUEST PARAMETERS                                 *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VREQ     DS    0H                                                               
*                                                                               
*        READ W0 PROFILE                                                        
*                                                                               
         OC    PBQPRFW0,PBQPRFW0   SKIP IF ALREADY READ                         
         BNZ   VREQPRFX                                                         
*                                                                               
         XC    DUB,DUB             PARAMETERS FOR GETPROF                       
*                                                                               
         MVI   DUB,C'P'            SET SYSTEM                                   
         MVC   DUB+2(2),=C'W0'     W0 - WRITER PROFILE                          
         MVC   DUB+4(2),PBQAGY     AGENCY                                       
*                                                                               
         GOTO1 GETPROF,DMCB,DUB,PBQPRFW0,DATAMGR                                
*                                                                               
VREQPRFX DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    VREQFLTX                                                         
******   B     VREQFLTX                                                         
*                                                                               
         L     R2,EFHTAG           YES-LOCATE FILTER FIELD                      
         SR    R0,R0                                                            
         LA    RE,3                                                             
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    *+8                  YES                                         
         LA    RE,6                 NO - FILTER FIELD DIFFERENT!                
*                                                                               
         IC    R0,0(R2)            POINT TO FILTER FIELD                        
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
         CLI   PBQPRFW0+8,C'Y'     IF  FILTER REQUIRED                          
         BNE   VREQFLTN                                                         
*                                                                               
         CLI   DDS,C'Y'            IF DDS TERMINAL                              
         BNE   *+16                                                             
         NI    1(R2),X'FF'-X'0C'      TURN ON FIELD                             
         OI    6(R2),X'80'            FORCE RE-TRANSMISSION                     
         B     VREQFLT3               VALIDATE                                  
*                                                                               
         TM    1(R2),X'0C'         IF FILTER HAS NON-ZERO INTENSITY             
         BO    *+12                                                             
         OI    1(R2),X'0C'            TURN OFF FIELD                            
         OI    6(R2),X'80'            FORCE RE-TRANSMISSION                     
*                                                                               
         LA    RE,L'WRIRFL         MAX FILTER LENGTH                            
*                                                                               
         LA    R1,8(RE,R2)         LAST BYTE OF FILTER                          
         BCTR  R1,0                                                             
*                                                                               
         CLI   0(R1),C' '          FIND LAST OF FILTER                          
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RE,*-10                                                          
*                                                                               
         CHI   RE,4                FILTER MUST BE 4 LONG                        
         BL    VREQFLTE                                                         
*                                                                               
*                                  RE HAS FILTER LENGTH                         
         LA    R1,8(R2)            CHARACTERS * AND - NOT ALLOWED               
*                                                                               
         CLI   0(R1),C'*'                                                       
         BE    *+8                                                              
         CLI   0(R1),C'-'                                                       
         BE    VREQFLTE                                                         
         LA    R1,1(R1)                                                         
         BCT   RE,*-20                                                          
*                                                                               
VREQFLT3 DS    0H                                                               
*                                                                               
         MVC   FILTER,8(R2)        SAVE VALID FILTER                            
*                                                                               
         B     VREQFLTX                                                         
*                                                                               
VREQFLTN DS    0H                  FILTER NOT REQUIRED                          
*                                                                               
         TM    1(R2),X'0C'         IF FILTER HAS ZERO INTENSITY                 
         BNO   *+12                                                             
         NI    1(R2),X'FF'-X'0C'      TURN ON FIELD                             
         OI    6(R2),X'80'            FORCE RE-TRANSMISSION                     
*                                                                               
         B     VREQFLTX                                                         
*                                                                               
VREQFLTE DS    0H                  INVALID FILTER                               
*                                                                               
         MVI   ERROR,PWEFLTNV      INVALID FILTER                               
*                                                                               
         GOTO1 CURSERR                                                          
*                                                                               
VREQFLTX DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFF-LINE                         
         BNE   VAL10                                                            
*                                                                               
*        OFF-LINE - GET BUFFER                                                  
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(30*1024)        30K BUFFER                                   
*                                                                               
         OC    AOV1WRK,AOV1WRK     CHECK FOR MULTIPLE ENTRIES                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,AOV1WRK          BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
VAL10    DS    0H                                                               
*                                                                               
         GOTO1 =A(VREC),DMCB,(RC),RR=RELO01                                     
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - PRT'                     
**********************************************************************          
*                                                                    *          
*        PRINT REPORT                                                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
PRT      DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'    NO NEED TO GO THRU PREP OR         L0A           
         BNE   XIT             SUBSEQUENT LOGIC IF ON LINE        L0A           
*                                                                               
         MVI   PRNTSW,0            INDICATE NO LINES PRINTED YET                
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
*                                                                               
         CLI   RPTOVLY,0           SKIP IF NOT WRITER                           
         BNE   PRT10                                                            
*                                                                               
         L     R3,PBPLEDCB         POINT TO PUBLINK ERROR DCB                   
*                                                                               
         OPEN  ((R3),OUTPUT)       OPEN PUB ERROR FILE                          
*                                                                               
         LTR   RF,RF               TEST RETURN CODE                             
         BZ    PRT10                                                            
*                                                                               
         CLOSE ((R3))              CLOSE - PROBABLY OPEN FROM ERROR             
*                                    ON LAST REQUEST                            
         OPEN  ((R3),OUTPUT)       RE-OPEN AND NO ERRORS ALLOWED NOW            
*                                                                               
         LTR   RF,RF               TEST RETURN CODE                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRT10    DS    0H                                                               
*                                                                               
*        GET CLEARANCE STATUS AND BILL HEADER BUFFERS IF NEEDED                 
*                                                                               
         TM    PBBYOPTS,PBBYCLSQ   SKIP IF CLEARANCE STATUS NOT WANTED          
         BNO   PRTCLSX                                                          
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
***      DC    AL4(18000*PPCLELLN)      BUFFER                                  
         DC    AL4(36000*PPCLELLN)      BUFFER                                  
*                                                                               
         OC    PBACLSBF,PBACLSBF   CHECK FOR MULTIPLE ENTRIES                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,PBACLSBF         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         ST    R3,PBLCLSBF         SAVE RETURNED BUFFER LENGTH                  
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
PRTCLSX  DS    0H                                                               
*                                                                               
         TM    PBBYOPTS,PBBYBHTQ   SKIP IF BILL HEADER TABLE NOT WANTED         
         BNO   PRTBHDX                                                          
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(600*150)             45K BUFFER                              
*                                                                               
         OC    PBABHDBF,PBABHDBF   CHECK FOR MULTIPLE ENTRIES                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,PBABHDBF         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         ST    R3,PBLBHDBF         SAVE RETURNED BUFFER LENGTH                  
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
PRTBHDX  DS    0H                                                               
*                                                                               
         TM    STACKSW,STCKBUFQ    SKIP IF NO STACK BUFFER NEEDED               
         BNO   PRTSVBX                                                          
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(60000)               60K BUFFER                              
*                                                                               
         OC    PBASVBBF,PBASVBBF   CHECK FOR MULTIPLE ENTRIES                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,PBASVBBF         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         ST    R3,PBLSVBBF         SAVE RETURNED BUFFER LENGTH                  
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         L     R4,PBASVBBF         BUFFER ADDRESS SAVEAREA                      
         USING STKVLBFD,R4         ESTABLISH BUFFER                             
*                                                                               
         LA    RF,SVBFSTRT         SET BUFFER START ADDRESS                     
         ST    RF,SVBFSTA                                                       
*                                                                               
         XC    SVBFLEN,SVBFLEN       INIT ENTRY LENGTH                          
*                                                                               
         BCTR  RF,0                                                             
         ST    RF,SVBFENDA         INDICATE BUFFER IS EMPTY                     
*                                                                               
         DROP  R4                                                               
*                                                                               
PRTSVBX  DS    0H                                                               
*                                                                               
*        PRINT THE REPORT                                                       
*                                                                               
         BAS   RE,PREP                                                          
*                                                                               
         CLI   RPTOVLY,0           SKIP IF NOT WRITER                           
         BNE   PRT20                                                            
*                                                                               
         L     R3,PBPLEDCB         POINT TO PUBLINK ERROR DCB                   
*                                                                               
         CLOSE ((R3))              CLOSE PUB ERROR FILE                         
*                                                                               
         LTR   RF,RF               TEST RETURN CODE                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRT20    DS    0H                                                               
*                                                                               
         CLI   PRNTSW,0           SKIP IF NO LINES PRINTED                      
         BE    LASTCOMS                                                         
*                                                                               
         CLC   FOOTCOM,SPACES     SKIP IF THERE ARE NO FOOTINGS                 
         BNH   FOOTX                                                            
*                                                                               
         MVI   FORCEFUT,C'Y'       FORCE PRINTING OF FOOTINGS                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE PRINTING OF HEADLINES                  
         XC    FOOTHOOK,FOOTHOOK   NO MORE FOOT COMMENTS                        
*                                                                               
         B     LASTCOMS                                                         
*                                                                               
FOOTX    DS    0H                                                               
*                                                                               
         CLI   DOWNOPT,C'Y'       SKIP BOX CLOSE IF DOWNLOADING                 
         BE    LASTCOMS                                                         
*                                                                               
         L     RF,ABOX             ESTABLISH BOX CONTROL BLOCK                  
         USING BOXD,RF                                                          
*                                                                               
         CLI   BOXYORN,C'Y'       SKIP BOX CLOSE IF NO BOXES USED               
         BNE   LASTCOMS                                                         
*                                                                               
         MVI   BOXREQ,C'C'         CLOSE LAST BOX                               
*                                                                               
         L     R1,VGENHD           RESET HOOK ADDRESS IN SPOOL BLOCK            
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
         EJECT                                                                  
*=============================================================                  
*                                                                               
*        END OF REPORT                                                          
*                                                                               
*      INCLUDE ANY SUB-ROUTINES TO PRINT DATA OUTSIDE OF BOX                    
*         AT END OF REPORT (STANDARD COMMENT 1 & 2                              
*                   =====HERE=========                                          
*=============================================================                  
*                                                                               
*        PRINT ANY TRAILING STANDARD COMMENTS                                   
*                                                                               
LASTCOMS DS    0H                                                               
*                                                                               
         TM    PBQCOM1P,PBQCOMLQ   TEST FOR ANY TRAILING STANDARD               
         BO    *+8                   COMMENTS                                   
         TM    PBQCOM2P,PBQCOMLQ                                                
         BNO   LASTCOMX                                                         
*                                                                               
         CLI   OFFLINE,C'Y'        MUST BE OFF-LINE                             
         BNE   LASTCOMX                                                         
*                                                                               
         L     R1,VGENHD           RESET HOOK ADDRESS IN SPOOL BLOCK            
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 =A(COMPRT),DMCB,(RC),PBQCOMLQ,RR=RELO01 LAST COMMENTS            
*                                                                               
LASTCOMX DS    0H                                                               
*                                                                               
         TM    PBQSWTCH,PBQXCLTQ   PRINT CLIENT DROPPED MESSAGE                 
         BNO   PRXCLTX                                                          
*                                                                               
         CLI   DOWNOPT,C'Y'       SKIP IF DOWNLOADING                           
         BE    PRXCLTX                                                          
*                                                                               
         CLI   OFFLINE,C'Y'        MUST BE OFF-LINE                             
         BNE   PRXCLTX                                                          
*                                                                               
         L     R1,VGENHD           RESET HOOK ADDRESS IN SPOOL BLOCK            
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 =A(COMPRT),DMCB,(RC),PBQXCLPQ,RR=RELO01 XCLUDED CLIENT           
*                                                                               
PRXCLTX  DS    0H                                                               
*                                                                               
         CLI   PAOPLESW,C'Y'       SKIP IF NO PUB LINK ERRORS                   
         BNE   PUBLINKX                                                         
*                                                                               
         GOTO1 =A(PUBLNKER),DMCB,(RC),RR=RELO01 PRINT PUB LINK ERR RPT          
*                                                                               
PUBLINKX DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   LST10                                                            
*                                                                               
*        OFF-LINE - FREE BUFFER                                                 
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(30*1024)        30K BUFFER                                   
         LA    R4,AOV1WRK          BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  RELEASE CORE                               
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         XC    AOV1WRK,AOV1WRK     RESET ADDRESS                                
*                                                                               
         TM    PBBYOPTS,PBBYCLSQ   SKIP IF CLEARANCE STATUS NOT WANTED          
         BNO   PRTCLSCX                                                         
*                                                                               
         ICM   R3,15,PBLCLSBF      LENGTH OF CLEARANCE STATUS BUFFER            
         BZ    PRTCLSCX            NONE USED SKIP                               
*                                                                               
         LA    R4,PBACLSBF         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  RELEASE CORE                               
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         XC    PBACLSBF,PBACLSBF   RESET ADDRESS                                
*                                                                               
PRTCLSCX DS    0H                                                               
*                                                                               
         TM    PBBYOPTS,PBBYBHDQ   SKIP IF BILL HEADER NOT WANTED               
         BO    PRTBHDCX                                                         
*                                                                               
         ICM   R3,15,PBLBHDBF      LENGTH OF BUFFER                             
         BZ    PRTBHDCX            NONE USED SKIP                               
*                                                                               
         LA    R4,PBABHDBF         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  RELEASE CORE                               
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         XC    PBABHDBF,PBABHDBF   RESET ADDRESS                                
*                                                                               
PRTBHDCX DS    0H                                                               
*                                                                               
         ICM   R3,15,PBLSVBBF      LENGTH OF BUFFER                             
         BZ    PRTSVBCX            NONE USED SKIP                               
*                                                                               
         LA    R4,PBASVBBF         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  RELEASE CORE                               
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         XC    PBASVBBF,PBASVBBF   RESET ADDRESS                                
*                                                                               
PRTSVBCX DS    0H                                                               
*                                                                               
LST10    DS    0H                                                               
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
SAVEREG9 DS    F                                                                
*                                                                               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - FRST'                    
**********************************************************************          
*                                                                    *          
*        RUN FIRST                                                   *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
FRST     CLI   OFFLINE,C'Y'        TEST OFFLINE                   L01           
         BNE   FRSTX                                                            
*                                                                               
         CLI   RPTOVLY,0           SKIP IF WRITER                               
         BE    *+8                                                              
         MVI   TWAFIRST,2          YES-GET RUNLAST MODE ALSO      L01           
*                                                                               
         CLI   RPTOVLY,0           SKIP IF NOT WRITER                           
         BNE   FRSTX                                                            
*                                                                               
         L     R3,PBPLEDCB         POINT TO DCB SAVEAREA IN SPUSER              
         L     RF,=A(PUBERR)       POINT TO DCB                                 
         A     RF,RELO01           RE-LOCATE ADDRESS JUST IN CASE               
         MVC   0(128,R3),0(RF)     MOVE DCB TO SAVEAREA                         
*                                                                               
FRSTX    XIT1                                                     L01           
*                                                                               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - LST'                     
**********************************************************************          
*                                                                    *          
*        RUN LAST                                                    *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LST      DS    0H                                                               
*                                                                               
         CLI   RPTOVLY,0           SKIP IF NOT WRITER                           
         BNE   LST1                                                             
*                                                                               
         L     R3,PBPLEDCB         POINT TO PUBERR DCB                          
*                                                                               
         CLOSE ((R3))              CLOSE FILE                                   
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         FREEPOOL (R3)                                                          
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LST1     DS    0H                                                               
*                                                                               
         CLI   RPTOVLY,0           SKIP IF WRITER                               
         BE    LSTX                                                             
*                                                                               
         GOTO1 =A(RPT),DMCB,('RPRUNLST',(RC)),RR=RELO01 PASS TO USER            
*                                                                               
LSTX     DS    0H                                                               
*                                                                               
         XIT1                                                     L01           
*                                                                               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - SPHOOK'                  
**********************************************************************          
*                                                                    *          
*        SUBSTITUTE HOOK ROUTINE FOR SPOOL WHICH CALLS APPLICATION   *          
*        HOOK ROUTINE                                                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
SPHOOK   NTR1                                                                   
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         L     RF,GLAHOOK          RF=A(APPLICATION HOOK)                       
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)        RESTORE APPLICATION REGISTERS                
         BASR  RE,RF                                                            
         XIT1  ,                                                                
*                                                                               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - PREP'                    
**********************************************************************          
*                                                                    *          
*        PRINT REPORT                                                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
PREP     NTR1  WORK=(R1,500)                                                    
*                                                                               
         ST    R1,PBCLTADR            TEMP SAVE OF IOAREA                       
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   PR10                                                             
*                                                                               
         GOTO1 VINIDRIV            INITIALIZE DRIVER                            
*                                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LA    R1,DRHOOK           APPLICATION HOOK                             
         ST    R1,GLAHOOK                                                       
*                                                                               
         MVI   GLHOOK,GLINIT                                                    
         GOTO1 =A(RPT),DMCB,('RPDRHOOK',(RC)),RR=RELO01 PASS TO USER            
*                                                                               
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
PR10     LA    R1,IOHOOK           PREPARE FOR PRNTIO                           
         ST    R1,PBIOHOOK                                                      
         MVC   PBCOMFAC,ACOMFACS                                                
         MVC   PBAIO1(12),AIO1                                                  
         MVC   PBVMINIO,VMINIO     PASS V(MINIO)                                
         MVC   PBVOFFCR,OFFICER                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),PBQTODAY                                  
         MVC   PBPRINT,VPRINT                                                   
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         LA    R1,RPTSPECS                                                      
         ST    R1,SPECS                                                         
*                                                                               
         MVC   PBQDOWN,DOWNOPT     PASS ALONG DOWNLOAD OPTION                   
*                                                                               
         EJECT                                                                  
*****    CREATE TABLE OF ESTIMATES // ESTIMATE NAME NEEDED                      
* NOTE ----- DATAIND IS LOADED BY PRWRITER .. BY ANY VALUE ENTERED              
*            IN THE ROUTLIST TABLE BYTE+12... TABLE CONSISTS OF 16              
*            BYTES 8 ENTRY NAME (4) ROUTINE ADDRESS AND 4 SPARE BYTES           
*            THE FIRST BYTE CAN INDICATE SPECIAL CHARACTERISTICS OF             
*            THAT FIELD.. E.G. ENTRY "IEST" NEEDS TO THE ESTNAME AND            
*            THE EST BUFFER SHOULD BE CREATED..                                 
*                                                                               
         TM    DATAIND,DIEST       TEST ESTIMATE NAMES NEEDED                   
         BO    PR20                                                             
         OC    PBQBLLDT,PBQBLLDT   OR BILL DATE FILTER                          
         BNZ   PR20                                                             
         TM    DATAIND,DIESTREC    OR ESTREC NEEDE                              
         BO    PR20                                                             
         TM    DATAIND,DPROFDEF     PROD/EST BILLING DEFAULTS NEEDED            
         BO    PR20                                                             
         CLC   PBQESTX,SPACES      OR ESTIMATE FILTERS                          
         BE    PR12                                                             
         CLI   PBQEST,C'0'                                                      
         BL    PR20                                                             
*                                                                               
PR12     DS    0H                                                               
*                                                                               
PR20     DS    0H                                                               
         L     RF,PBCLTADR         START OF IO SAVE AREA                        
         LA    R1,4000(RF)         END OF AREA                                  
         XC    PBCLTADR,PBCLTADR   THIS WAS A TEMP ADDRESS AREA                 
*                                                                               
         MVC   0(16,RF),=C'**T40501-AGYH***' AGENCY HEADER MARKER               
         LA    RF,16(RF)                                                        
         ST    RF,PBAGYADR         AGENCY HEADER                                
         MVC   PBAGYLEN,=F'300'                                                 
         A     RF,PBAGYLEN                                                      
         CR    RF,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                TABLE M/B ENLARGED                           
*                                                                               
*****    TM    DATAIND,DICLT                                                    
*****    BNO   PRDRQSTD                                                         
         MVC   0(16,RF),=C'**T40501-CLIH***'                                    
         LA    RF,16(RF)                                                        
         ST    RF,PBCLTADR         MOVE CLIENT HEADER        CLIENT             
         MVC   PBCLTLEN,=F'500'                                                 
         A     RF,PBCLTLEN                                                      
         CR    RF,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                TABLE M/B ENLARGED                           
PRDRQSTD TM    DATAIND,DIPRD       IS THERE A NEED TO HAVE PROD IN CORE         
         BNO   ESTRQSTD                                                         
         MVC   0(16,RF),=C'**T40501-PROD***'                                    
         LA    RF,16(RF)                                                        
         ST    RF,PBPRDADR                                   PRODUCT            
         MVC   PBPRDLEN,=F'500'                                                 
         A     RF,PBPRDLEN                                                      
         CR    RF,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                TABLE M/B ENLARGED                           
ESTRQSTD TM    DATAIND,DIESTREC                                                 
         BNO   ANYRQSTD                                                         
         MVC   0(16,RF),=C'**T40501-ESTH***'                                    
         LA    RF,16(RF)                                                        
         ST    RF,PBESTADR                                   ESTIMATE           
         MVC   PBESTLEN,=F'500'                             HEADERS             
         A     RF,PBPRDLEN                                                      
         CR    RF,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                TABLE M/B ENLARGED                           
ANYRQSTD TM    DATAIND,DPROFDEF     PROD/EST BILLING DEFAULTS NEEDED            
         BNO   ANYRQSTX                                                         
         MVC   0(16,RF),=C'**T40501-PDEFBL*'                                    
         LA    RF,16(RF)                                                        
         ST    RF,PBPRDFLT        ADDRESS OF PRODUCT DEFAULT BILLING            
         LA    RF,37(RF)             PROFILE                                    
         MVC   0(16,RF),=C'**T40501-ESTDBL*'                                    
         LA    RF,16(RF)                                                        
         ST    RF,PBESTFLT                   ESTIMATE                           
         LA    RF,37(RF)                                                        
         CR    RF,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                TABLE M/B ENLARGED                           
ANYRQSTX DS    0H                                                               
         TM    DATAIND,DIPUBLSH                                                 
         BNO   ANYRQSTY                                                         
         MVC   0(16,RF),=C'**T40501-PUBLSHR'                                    
         LA    RF,16(RF)                                                        
         ST    RF,PBPBLSHA                                                      
         MVC   PBPBLSLN,=F'1000'                                                
         A     RF,PBPBLSLN                                                      
         CR    RF,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                TABLE M/B ENLARGED                           
ANYRQSTY DS    0H                                                  L03          
         TM    DATAIND,DIAORREC                                    L03          
         BNO   ANYRQSYY                                            L03          
         MVC   0(16,RF),=C'**T40501-A-O-R  '                       L03          
         LA    RF,16(RF)                                           L03          
         ST    RF,AORECORD                                         L03          
         MVC   PBPBLSLN,=F'1000'                                   L03          
         A     RF,PBPBLSLN                                         L03          
         CR    RF,R1                                               L03          
         BNH   *+6                                                 L03          
         DC    H'0'                TABLE M/B ENLARGED              L03          
*                                                                               
ANYRQSYY DS    0H                                                               
*                                                                               
         OC    PBQCOM1(4),PBQCOM1     ANY STD COMMENTS                          
         BNZ   *+10                                                             
         OC    PBQCOM2(4),PBQCOM2                                               
         BZ    ANYRQSTZ                                                         
*                                                                               
         L     RF,AOV1WRK                                                       
         LA    RF,16(RF)                                                        
         MVC   0(16,RF),=C'**HEADING SAVE**'                                    
         LA    RF,16(RF)                                                        
         ST    RF,PBASVHED                                                      
         LA    RF,10*198(RF)       SAVE MAX 10 HEADLLINES                       
*                                                                               
ANYRQSTZ DS    0H                                                               
*                                                                               
         ST    RF,PBT2PTR   ADDRESS OF NEXT AVAIL PIECE OF SAVE AREA            
*                                                                               
****     TM    DATAIND,DIREPB  NEED REP BUFFER                                  
****     BNO   BLDCONA                                                          
*                                                                               
****     L     RF,PBAREPBF         POINT TO REP BUFFER                          
****     MVC   0(8,RF),=C'*REPBUF*'                               L0A           
****     MVC   8(4,RF),REPSIZE                                    L0A           
****     LA    RF,12(RF)                                          L0A           
****     ST    RF,PBAREPBF                                        L0A           
*                                                                               
*   SEE IF A PERIOD TOTAL REQTD (INSDATE,MQ) AND IF SO ELIMINATE                
*        ANY SPACING BETWEEN TOTALS                                             
*                                                                               
BLDCONA  TM    PBQPTLVL,PBQPTL2Q                                                
         BNO   NOPTOT                                                           
         L     RF,GLADTAB          START SEARCH FROM BEGINING                   
         USING DRFLD,RF            OF DRIVE TABLE                               
CLI40    CLI   0(RF),0                                                          
         BE    NOPTOT                                                           
         CLI   0(RF),X'44'                                                      
         BE    YES44                                                            
         CLI   0(RF),X'48'                                                      
         BE    YES44                                                            
ZIC1F    ZIC   RE,1(RF)                                                         
         CH    RE,=H'0'                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AR    RF,RE                                                            
         B     CLI40                                                            
*                                                                               
YES44    MVI   DRFLSPAC,0                                                       
         B     ZIC1F                                                            
*                                                                               
*                                                                               
         DROP  RF                                                               
NOPTOT   CLI   PBFLAVOR,C'B'    BUY FLAVOR                                      
         BNE   GOTOIOR                                                          
         TM    PBQSPLOP,X'80'      CONTRACT ITEMS SELECTED                      
         BNO   GOTOIOR                                                          
******                                                                          
******                                                                          
         L     RF,PBAOFFBF       -- CREATE CONTRACT TABLE ---                   
         LHI   RE,CONTNLEN                                                      
         MH    RE,=H'1500' WAS 1000 4/1/91              BUG03                   
         AR    RE,RF                                                            
         ST    RE,PBAOFFBF                                                      
         ST    RF,PBACONTB       ADDRESS OF CONTRACT BUFFER                     
*                                                                               
GOTOIOR  GOTO1 PRNTIO,DMCB,PBLOCK,RR=RB                                         
*                                                                               
         CLI   OFFLINE,C'Y'        ++TEST                                       
         BNE   PRX                                                              
*                                                                               
PR40     MVI   GLHOOK,GLOUTPUT                                                  
         GOTO1 =A(RPT),DMCB,('RPDRHOOK',(RC)),RR=RELO PASS TO USER              
*                                                                               
         TM    PBQFLOPT,PBQFLOUT   SEND OUTPUT DATA TO FILE?                    
         BNO   PR42                 NO                                          
*                                                                               
** MQFILENM IS ORG'D OVER MAAORLK (ONLY USED DURING INPUT)                      
*                                                                               
         LAY   R2,MQFILENM         POINT TO FILE NAME BUILD AREA                
         USING FILNAMD,R2                                                       
*                                                                               
         MVC   0(L'MQFILENM,R2),SPACES INIT AREA                                
*                                                                               
         MVC   FNPFIX,=C'SFTPDISK.PROD.'                                        
*                                                                               
         BRAS  RE,TESTRUN                                                       
         BNE   *+10                                                             
         MVC   9(4,R2),=C'TEST'                                                 
*                                                                               
         BRAS  RE,SPCLSTUP         SPECIAL FILE NAMING REQUIRED?                
         BE    PR41A               YES - SKIP CONVENTION                        
*                                                                               
         MVC   FNPRG(FILNAMX-FNPRG),=CL30'BUY.PR.SJSJ.DYYMMDD.THHMMSST'         
*                                                                               
         L     RF,TWAMASTC                                                      
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   FNAGYLBL,MCAGYCOD-MCEXTRA(RF)                                    
         TM    PBQFLOPT,PBQFLSCJ   SC JOHNSON ESTIMATE FILE?                    
         BZ    PR41                NO                                           
         MVC   FNPRG(3),=C'EST'    YES - CREATE ESTIMATE FILE                   
         L     RF,TWAMASTC                                                      
         MVC   FNAGYLBL(2),MCUSER-MASTD(RF)                                     
         MVC   FNAGYLBL+2(2),=C'SP'                                             
         CLI   DIGITAL,C'Y'        DIGITAL?                                     
         BNE   PR41                NO                                           
         MVI   FNAGYLBL+3,C'D'     YES                                          
*                                                                               
PR41     GOTO1 DATCON,DMCB,(3,BTODAY),(X'20',FNYYMMDD)                          
*                                                                               
         TIME  DEC                 R0=HHMMSSHH                                  
*                                                                               
         ST    R0,FULL                                                          
*                                                                               
         GOTO1 HEXOUT,DMCB,FULL,FNTIME,4                                        
         MVI   FNTIME+7,C' '       GET RID OF HENDREDTHS                        
*                                                                               
PR41A    LAY   RE,MQFILENM                                                      
         ST    RE,GLAOUTP          TELL DRIVER FILE NAME                        
*                                                                               
PR42     DS    0H                                                               
*                                                                               
         MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
         TM    PBQFLOPT,PBQFLOUT   DID WE SEND OUTPUT DATA TO FILE?             
         BNO   PR43                 NO                                          
*                                                                               
* SEND MQ MESSAGE WITH FILE NAME                                                
*                                                                               
MQ       USING MQMSGD,ELEM                                                      
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         LAY   R2,MQFILENM                                                      
*                                                                               
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
*                                                                               
         MVC   MQ.MQHID,=CL6'DANOT1'                                            
         MVC   MQ.MQSYS,=CL3'PRT'                                               
*                                                                               
         L     RF,TWAMASTC                                                      
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   MQ.MQAGYID,MCAGYCOD-MCEXTRA(RF)                                  
         TM    PBQFLOPT,PBQFLSCJ   SC JOHNSON ESTIMATE FILE?                    
         BZ    *+14                NO                                           
         MVC   MQ.MQQUAL(11),SCJTIT+16 YES - USE TITLE FROM SCJE SCREEN         
         B     *+10                                                             
         MVC   MQ.MQQUAL,WRITIT+16                                              
         OC    MQ.MQQUAL,SPACES                                                 
                                                                                
*                                                                               
         CLC   MQ.MQAGYID,=C'AINY' AGENCY ACTIVE?                               
         BNE   MQ10                NO                                           
         LA    RE,WRITIT+40        FOLDER NAME                                  
         CLC   =C'BILLING',0(RE)   BILLING FOLDER?                              
         BNE   MQ05                NO                                           
         MVI   MQ.MQAGYID+3,C'1'   YES - SEND AIN1 AS 4-CHARACTER CODE          
         B     MQ06                GO SET DATE AND TIME                         
*                                                                               
MQ05     CLC   =C'PNL',0(RE)       PNL FOLDER?                                  
         BNE   MQ10                NO - LEAVE AINY AS IS                        
         MVI   MQ.MQAGYID+3,C'2'   YES - SEND AIN2 AS 4-CHARACTER CODE          
*                                                                               
MQ06     LA    RE,FNPRG            FILE NAME                                    
*                                                                               
MQ06A    CLC   =C'.D',0(RE)        DATE STAMP?                                  
         BE    MQ07                YES                                          
         LA    RE,1(RE)            BUMP FILE NAME POINTER                       
         B     MQ06A               KEEP CHECKING FOR DATE STAMP                 
*                                                                               
MQ07     MVC   MQ.MQDATE,2(RE)     DATE STAMP YYMMDD                            
         MVC   MQ.MQTIME,10(RE)    TIME STAMP HHMMSS                            
         B     MQ20                DATE & TIME SET                              
*                                                                               
MQ10     MVC   MQ.MQDATE,FNYYMMDD                                               
         MVC   MQ.MQTIME,FNTIME                                                 
*                                                                               
MQ20     MVC   MQ.MQDATA1(L'WRIDESC),WRIDESC                                    
         OC    MQ.MQDATA1,SPACES                                                
         MVC   MQ.MQFILE(FILNAMX-FNPRG),FNPRG                                   
*                                                                               
         GOTO1 PBAMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 PBAMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                
         CLI   DMCB+8,0                                                         
         BE    PR43                                                             
         DCHO                                                                   
*                                                                               
         DROP  MQ                                                               
*                                                                               
* NOTE: THE 'ROUTE' IS PASSED IN THE MQOPEN AS THE HEADER                       
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM=PRT                                   
MQAGYID  DS    CL4                 AGENCY ID                                    
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 DATE                                         
MQTIME   DS    CL6                 TIME HHMMSS                                  
MQDATA1  DS    CL32                                                             
MQDATA2  DS    CL32                                                             
MQFILE   DS    CL64                DSN                                          
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
FILNAMD  DSECT                                                                  
FNPFIX   DS    CL14                PREFIX (SFTPDISK.PROD)                       
FNPRG    DS    CL3                 BUY                                          
         DS    CL1                 .                                            
FNSYS    DS    CL2                 PR                                           
         DS    C                   .                                            
FNAGYLBL DS    CL4                 AGY LABEL (CTAGCCOD)                         
         DS    CL2                 .D                                           
FNYYMMDD DS    CL6                 YYMMDD                                       
         DS    CL2                 .T                                           
FNTIME   DS    CL7                 TIME HHMMSST                                 
FILNAMDQ EQU   *-FILNAMD                                                        
FILNAMX  EQU   *                                                                
*                                                                               
T40501   CSECT                                                                  
*                                                                               
*                                                                               
PR43     DS    0H                                                               
*                                                                               
*        FREE UP CLIENT BUFFER CORE                                             
*                                                                               
         L     R3,PBACLTBF                                                      
         L     R5,PBLCLTBF                                                      
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
*                                                                               
*        FREE UP PRODUCT BUFFER CORE                                            
*                                                                               
         L     R3,PBAPRDBF                                                      
         L     R5,PBLPRDBF                                                      
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
*                                                                               
*        FREE UP REP BUFFER CORE                                                
*                                                                               
         L     R3,PBAREPBF                                                      
         SH    R3,=H'16'           BACK UP TO START OF HEADER                   
         L     R5,12(R3)           BUFFER LENGTH                                
         AH    R5,=H'16'           BUFFER HEADER LENGTH                         
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
*                                                                               
*        FREE UP ESTIMATE BUFFER CORE                                           
*                                                                               
         L     R3,PBAESTBF                                                      
         L     R5,PBLESTBF                                                      
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
*                                                                               
*        FREE UP AD BUFFER CORE                                                 
*                                                                               
         L     R3,PBAADBF                                                       
         L     R5,PBLADBF                                                       
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
*                                                                               
*        FREE UP DRD BUFFER CORE                                                
*                                                                               
         L     R3,PBADRDBF                                                      
         L     R5,PBLDRDBF                                                      
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
*                                                                               
*        FREE UP PQINDEX TABLE                                                  
*                                                                               
         ICM   R3,15,APQINDEX      POINT TO PQINDEX TABLE                       
         BZ    PREPINXX            NOT THERE                                    
*                                                                               
         AHI   R3,-4               BACK UP TO START OF BUFFER                   
         L     R5,0(R3)            GET BUFFER LENGTH                            
*                                                                               
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
*                                                                               
PREPINXX DS    0H                                                               
*                                                                               
*        FINAL CALL TO USER APPLICATION                                         
*                                                                               
         GOTO1 =A(RPT),DMCB,('RPFINAL',(RC)),RR=RELO01 PASS TO USER             
*                                                                               
PRX      B     XIT                                                              
REPSIZE  DC    A(2000*REPBUFLN)        SIZE OF OFFLINE REP BUFFER               
         TITLE 'PRWRI01 - PRINT WRITER REPORT MASTER - IOHOOK'                  
**********************************************************************          
*                                                                    *          
*        I/OHOOK                                                     *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
IOHOOK   NTR1                                                                   
*                                                                               
         GOTO1 =A(RPT),DMCB,('RPINPUT',(RC)),RR=RELO01 CALL USER APPL           
*                                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*===================================================                            
*              PBMODE SET BY PRNTIO                *                            
*===================================================                            
         CLI   PBMODE,PBPROCCL     CLIENT FIRST                                 
         BE    CLIENT                                                           
         CLI   PBMODE,PBPROCBY     PROCESS BUY RECORD                           
         BE    BUY                                                              
         CLI   PBMODE,PBPROCBL     PROCESS BILL RECORD                          
         BE    BILL                                                             
         CLI   PBMODE,PBPROCBK     PROCESS BUCKET RECORD                        
         BE    BUCK                                                             
         CLI   PBMODE,PBPROCCO     PROCESS  CONTRACT                            
         BE    CONT                                                             
         CLI   PBMODE,PBPROCIS     PROCESS  ISSUE RECORD                        
         BE    ISSUE                                                            
         CLI   PBMODE,PBPROCES     PROCESS ESTIMATE RECORD                      
         BE    ESTIMATE                                                         
         CLI   PBMODE,PBPROCPB     PROCESS PUB      RECORD                      
         BE    PUB                                                              
         CLI   PBMODE,PBPROCRP     PROCESS REP      RECORD                      
         BE    REP                                                              
         CLI   PBMODE,PBPROCIV     PROCESS INVOICE  RECORD                      
         BE    INV                                                              
         CLI   PBMODE,PBPROCNV     PROCESS NEW INVOICE  RECORD                  
         BE    PNV                                                              
*                                                                               
HKX      B     XIT                                                              
         EJECT                                                                  
CLIENT   DS    0H                  ** CLIENT FIRST **                           
*                                                                               
CLX      B     HKX                                                              
         EJECT                                                                  
BUY      CLI   OFFLINE,C'Y'        ** PROCESS A BUY **                          
         BE    BUY09                                                            
         L     R2,PBAIO1 |                                                      
         USING PBUYRECD,R2 |                                                    
         MVC   P(9),=C'*BUY KEY*' |                                             
         MVC   P+10(25),PBUYKEY |                                               
         GOTO1 HEXOUT,DMCB,PBUYKEY,P2+10,25,=C'SEP' |                           
         MVC   P3+10(25),P2+35 |                                                
         MVC   P2+35(25),SPACES |                                               
         GOTO1 SPOOL,DMCB,(R8) |                                                
         BASR  RE,RF |                                                          
         B     BYX |                                                            
*============================================================|===               
*    CHANGE BRANCH TO BE BUY09-->----------------------------|                  
*================================================================               
BUY09    ICM   RF,15,PBALTLEL    SEE IF ANY DRD SPLIT NECESSARY                 
         BZ    BY10              PBALTLEL SET UP IN I/O                         
         LR    R5,RF             R5 WILL BE BUMPED UP AS NEEDED                 
         MVC   SVGROSS,PBGRS     SAVE GROSS RDSPLIT WILL DESTROY                
         GOTO1 =A(RDSPLIT),DMCB,(R5),(RC),RR=RELO01                             
         CLI   4(R1),C'Y'        SUCCESSFUL REQUEST FILERING                    
         BE    BY10                                                             
         BAS   RE,NXTLTL        SEE IF SUCCESS ON NEXT LITTLE RECORD            
         BNE   XCPBAL                                                           
         L     RF,PBALTLEL      SEE IF SAME CLI/DIV                             
         CLC   2(6,RF),2(R5)                                                    
         BNE   XCPBAL            EXIT                                           
*                                                                               
*================================================================               
*                                                                               
BY10     BAS   RE,DRIVIN                                                        
*                                                                               
*================================================================               
*                                                                               
         ICM   RF,15,PBALTLEL                                                   
         BZ    BYX                 TO I/O                                       
DOSPLAA  BAS   RE,NXTLTL                                                        
         BNE   XCPBAL                                                           
         L     RF,PBALTLEL      SEE IF SAME CLI/DIV                             
         CLC   2(6,RF),2(R5)                                                    
         BNE   XCPBAL            EXIT                                           
         MVC   PBGRS(64),SVGROSS     SAVE GROSS RDSPLIT WILL DESTROY            
         GOTO1 =A(RDSPLIT),DMCB,(R5),(RC),RR=RELO01                             
         CLI   4(R1),C'Y'        SUCCESSFUL REQUEST FILERING                    
         BE    BY10                                                             
*********XC    RDSPLITX(8),RDSPLITX FOR TESTING                                 
         B     DOSPLAA PROCESS NEXT                                             
****                                                                            
XCPBAL   XC    PBALTLEL,PBALTLEL       CLEAR FOR NEXT RECORD                    
*                                                                               
BYX      B     HKX                                                              
         SPACE 3                                                                
NXTLTL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    NXTLTL2                                                          
         CLI   0(R5),X'71'                                                      
         BER   RE                                                               
         B     NXTLTL+2                                                         
NXTLTL2  LTR   R5,R5                                                            
         BR    RE                                                               
         EJECT                                                                  
CONT     CLI   OFFLINE,C'Y'        ** PROCESS A CONTRACT ** (SPECIAL)           
         BE    BY10C                                                            
         L     R2,PBAIO2                                                        
         USING PCONRECD,R2                                                      
         MVC   P(9),=C'*CON KEY*'                                               
         MVC   P+10(25),PCONKEY                                                 
         GOTO1 HEXOUT,DMCB,PCONKEY,P2+10,25,=C'SEP'                             
         MVC   P3+10(25),P2+35                                                  
         MVC   P2+35(25),SPACES                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BASR  RE,RF                                                            
         B     BYXX                                                             
*                                                                               
BY10C    BAS   RE,DRIVIN                                                        
*                                                                               
BYXX     B     HKX                                                              
         EJECT                                                                  
BILL     CLI   OFFLINE,C'Y'        ** PROCESS A BILL **                         
         BE    BL10                                                             
         L     R2,PBAIO1                                                        
         USING PBILLRCD,R2                                                      
         MVC   P(10),=C'*BILL KEY*'                                             
         MVC   P+12(25),PBILLKEY                                                
         GOTO1 HEXOUT,DMCB,PBILLKEY,P2+12,25,=C'SEP'                            
         MVC   P3+12(25),P2+35                                                  
         MVC   P2+37(25),SPACES                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BASR  RE,RF                                                            
         B     BLX                                                              
*                                                                               
BL10     BAS   RE,DRIVIN                                                        
*                                                                               
BLX      B     HKX                                                              
         SPACE 3                                                                
         EJECT                                                                  
BUCK     DS    0H                  ** PROCESS ESTIMATE BUCKET RECORD            
         CLI   OFFLINE,C'Y'                                                     
         BE    BK01                                                             
         L     R2,PBAIO2                                                        
         USING PBKRECD,R2                                                       
         MVC   P(10),=C'*BUCK KEY*'                                             
         MVC   P+12(25),PBKKEY                                                  
         GOTO1 HEXOUT,DMCB,PBKKEY,P2+12,25,=C'SEP'                              
         MVC   P3+12(25),P2+35                                                  
         MVC   P2+37(25),SPACES                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BASR  RE,RF                                                            
         DROP  R2                                                               
         B     BKX                                                              
*                                                                               
BK01     L     R2,PBAIO2                                                        
         USING PBKRECD,R2                                                       
BK2      LA    R5,PBKKEY+33        SEARCH FOR ELEMNTS IN REQUEST PERIOD         
         USING BKELEMD,R5                                                       
*                                                                               
BK10     CLI   0(R5),0                                                          
         BE    BKX                                                              
         CLI   0(R5),X'22'                                                      
         BE    BK20                                                             
*                                                                               
BK15     ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     BK10                                                             
*                                                                               
BK20     CLC   BKYM,PBQBST      CHECK PERIOD IS IN REQUEST PERIOD               
         BL    BK15                                                             
         CLC   BKYM,PBQBEND                                                     
         BH    BK15                                                             
*                                                                               
BK22     ST    R5,PBGRS            USE PBGRS FOR ELEM ADDRESS                   
         BAS   RE,DRIVIN                                                        
         B     BK15                NEXT ELEMENT                                 
*                                                                               
BKX      B     HKX                                                              
         EJECT                                                                  
ISSUE    DS    0H                  ** PROCESS ISSUE RECORD                      
         CLI   OFFLINE,C'Y'                                                     
         BE    IS01                                                             
*                                                                               
         L     R2,PBAIO1                                                        
         USING PISSREC,R2                                                       
*                                                                               
         MVC   P(11),=C'*ISSUE KEY*'                                            
         MVC   P+13(25),PISSKEY                                                 
         GOTO1 HEXOUT,DMCB,PISSKEY,P2+13,25,=C'SEP'                             
         MVC   P3+13(25),P2+36                                                  
         MVC   P2+37(25),SPACES                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BASR  RE,RF                                                            
         DROP  R2                                                               
         B     ISX                                                              
*                                                                               
IS01     L     R2,PBAIO1                                                        
         USING PESTREC,R2                                                       
*                                                                               
         BAS   RE,DRIVIN                                                        
*                                                                               
ISX      B     HKX                                                              
*                                                                               
         EJECT                                                                  
ESTIMATE DS    0H                  ** PROCESS ESTIMATE RECORD                   
         CLI   OFFLINE,C'Y'                                                     
         BE    ES01                                                             
*                                                                               
         L     R2,PBAIO1                                                        
         USING PESTREC,R2                                                       
*                                                                               
         MVC   P(11),=C'*EST   KEY*'                                            
         MVC   P+13(25),PESTKEY                                                 
         GOTO1 HEXOUT,DMCB,PESTKEY,P2+13,25,=C'SEP'                             
         MVC   P3+13(25),P2+36                                                  
         MVC   P2+37(25),SPACES                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BASR  RE,RF                                                            
         DROP  R2                                                               
         B     ESX                                                              
*                                                                               
ES01     L     R2,PBAIO1                                                        
         USING PESTREC,R2                                                       
*                                                                               
         BAS   RE,DRIVIN                                                        
*                                                                               
ESX      B     HKX                                                              
         EJECT                                                                  
PUB      DS    0H                  ** PROCESS PUB      RECORD                   
         CLI   OFFLINE,C'Y'                                                     
         BE    PB01                                                             
*                                                                               
         L     R2,PBAIO1                                                        
         USING PUBRECD,R2                                                       
*                                                                               
         MVC   P(11),=C'*PUB   KEY*'                                            
         MVC   P+13(25),PUBKEY                                                  
         GOTO1 HEXOUT,DMCB,PUBKEY,P2+13,25,=C'SEP'                              
         MVC   P3+13(25),P2+36                                                  
         MVC   P2+37(25),SPACES                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BASR  RE,RF                                                            
         DROP  R2                                                               
         B     PBX                                                              
*                                                                               
PB01     L     R2,PBAIO3                                                        
         USING PUBRECD,R2                                                       
*                                                                               
         BAS   RE,DRIVIN                                                        
*                                                                               
PBX      B     HKX                                                              
         EJECT                                                                  
REP      DS    0H                  ** PROCESS REP      RECORD                   
         CLI   OFFLINE,C'Y'                                                     
         BE    RP01                                                             
*                                                                               
         L     R2,PBAIO1                                                        
         USING PREPRECD,R2                                                      
*                                                                               
         MVC   P(11),=C'*REP   KEY*'                                            
         MVC   P+13(25),PREPKEY                                                 
         GOTO1 HEXOUT,DMCB,PREPKEY,P2+13,25,=C'SEP'                             
         MVC   P3+13(25),P2+36                                                  
         MVC   P2+37(25),SPACES                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BASR  RE,RF                                                            
         DROP  R2                                                               
         B     RPX                                                              
*                                                                               
RP01     L     R2,PBAIO1                                                        
         USING PREPREC,R2                                                       
*                                                                               
         BAS   RE,DRIVIN                                                        
*                                                                               
RPX      B     HKX                                                              
*                                                                               
*              PROCESS INVOICE RECORD                                           
*                                                                               
INV      DS    0H                                                               
*                                                                               
         ICM   R2,15,PBIKYELA      POINT TO RECORD KEY                          
*                                                                               
         BAS   RE,DRIVIN                                                        
*                                                                               
INVX     DS    0H                                                               
         B     HKX                                                              
*                                                                               
PNV      DS    0H                                                               
*                                                                               
         ICM   R2,15,PBIKYELA      POINT TO RECORD KEY                          
*                                                                               
         BAS   RE,DRIVIN                                                        
*                                                                               
PNVX     DS    0H                                                               
         B     HKX                                                              
*                                                                               
         EJECT                                                                  
* DRIVER INPUT ROUTINE                                                          
*                                                                               
         USING GLOBALD,R4                                                       
DRIVIN   LR    R0,RE                                                            
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* GENERAL DRIVER CALLING ROUTINE                                                
*                                                                               
DRIVGEN  LR    R0,RE                                                            
         GOTO1 DRIVER,DMCB,AGLOBAL                                              
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   NTR1                                                                   
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLLAST       LASTS                                        
         BE    LASTS                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEADHK                                                           
*                                                                               
DRHOOKX  DS    0H                                                               
         GOTO1 =A(RPT),DMCB,('RPDRHOOK',(RC)),RR=RELO01 TRY USER APPL           
         B     XIT                                                              
         EJECT                                                                  
* RESOLVE ROUTINE ADDRESSES                                                     
*                                                                               
RESOLVE  LA    R1,ROUTLIST                                                      
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         BE    RESOLVEX                                                         
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       RETURN ADDRESS                               
         B     XIT                                                              
*                                                                               
RESOLVEX GOTO1 =A(RPT),DMCB,('RPDRHOOK',(RC)),RR=RELO01 TRY USER OVLY           
         B     XIT                                                              
         SPACE 1                                                                
ROUTLIST DS    0F                  ROUTINE ADDRESS LIST                         
         DC    X'FF'                                                            
         EJECT                                                                  
* EXECUTING ROUTINES                                                            
*                                                                               
EXEC     LA    R1,ROUTLIST         TEST ROUTINE IS IN THIS OVERLAY              
EXEC2    CLI   0(R1),FF                                                         
         BE    EXECX                                                            
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     EXEC2                                                            
         L     R2,GLAIFLD          YES-R2=A(INPUT)                              
         L     R3,GLAOFLD              R3=A(OUTPUT)                             
         L     RF,GLAROUT              BRANCH TO ROUTINE                        
         BASR  RE,RF                                                            
         B     XIT                                                              
*                                  NO-ROUTINE IS IN USER REPORT OVERLAY         
EXECX    GOTO1 =A(RPT),DMCB,('RPDRHOOK',(RC)),RR=RELO01 TRY USER OVLY           
         B     XIT                                                              
*                                                                               
         TITLE 'PRWRI01 - PRINT A LINE - PRINT'                                 
***********************************************************************         
*                                                                     *         
*        PRINT A LINE                                                 *         
*                                                                     *         
*NTRY    SORTSW   C'X' - DON'T PRINT THE LINE                         *         
*        PRNTSW   C'Y' - AT LEAST ONE LINE PRINTED                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRINT    DS    0H                                                               
*                                                                               
         CLI   SORTSW,C'X'         IF NO PRINT SWITCH IS ON                     
         BNE   *+12                                                             
         MVI   GLHOOK,GLDONT          DON'T PRINT LINE                          
         B     PRINTX                                                           
*                                                                               
         MVI   SORTSW,0            CLEAR SWITCH                                 
*                                                                               
*        PRINT EDICT CONTROL CARDS BEFORE FIRST LINE IF NEEDED                  
*                                                                               
         CLI   PRNTSW,C'Y'         SKIP IF A LINE ALREADY PRINTED               
         BE    PREDCTX                                                          
*                                                                               
         CLC   =C'TRANSMIT',CONREC SKIP IF NOT TRANSMITTING REPORT              
         BNE   PREDCTX                                                          
*                                                                               
         LA    R2,KEY              READ DESTINATION ID RECORD                   
         USING CTIREC,R2           ESTABLISH ID RECORD KEY                      
*                                                                               
         XC    CTIKEY,CTIKEY       INIT KEY                                     
         MVI   CTIKTYP,CTIKTYPQ    SET RECORD ID                                
*                                                                               
         L     R1,TWAMASTC                                                      
         MVC   CTIKNUM,MCDESTID-MASTD(R1)   SET DESTINATION ID                  
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'        SET TO READ CONTROL FILE            
*                                                                               
         IC    R0,USEIO            SAVE CURRENT USEIO                           
         MVI   USEIO,C'Y'          SET WE WILL DO IO                            
*                                                                               
         MVC   AIO,AIO1            READ INTO IOAAREA 1                          
*                                                                               
         GOTO1 HIGH                READ ID RECORD                               
*                                                                               
         STC   R0,USEIO            RESTORE IO OPTION                            
*                                                                               
         XC    FILENAME,FILENAME   RESET FILENAME TO DEFAULT                    
*                                                                               
         CLC   KEY(L'CTIKEY),KEYSAVE  MUST FIND RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        FIND DESCRIPTION ELEMENT                                               
*                                                                               
         L     R2,AIO1             POINT TO FOUND RECORD                        
         SR    R0,R0               FIND DESCRIPTION ELEMENT                     
         LA    R1,CTIDATA          POINT TO START OF ELEMENTS IN REC            
*                                                                               
PREDCTLP DS    0H                                                               
*                                                                               
         CLI   0(R1),0             ERROR IF END OF RECORD REACHED               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),CTDSCELQ      LOOKING FOR DESCRIPTION ELEMENT              
         BNE   PREDCTCN                                                         
*                                                                               
         CLI   1(R1),12            ELEMENT MUST BE 12 LONG                      
         BNE   PREDCTCN                                                         
*                                                                               
         MVC   WORK(10),CTDSC-CTDSCD(R1)  EXTRACT DESTINATION ID NAME           
*                                                                               
         B     PREDCTDN                                                         
*                                                                               
PREDCTCN IC    R0,1(R1)             BUMP TO NEXT ELEMENT                        
         AR    R1,R0                                                            
         B     PREDCTLP                                                         
*                                                                               
PREDCTDN DS    0H                                                               
*                                                                               
*        BUILD EDICT HEADER LINE                                                
*                                                                               
         MVC   P,SPACES            *HDR* CARD                                   
         LA    R1,P                                                             
*                                                                               
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(6,R1),=C'EDICT='                                               
         MVC   15(10,R1),WORK      DESTINATION                                  
         MVI   34(R1),C'W'         132 CHARS WIDE                               
******   MVI   36(R1),C'S'         STRIP DOUBLEQUOTES - NOT ACTIVE              
         MVC   38(10,R1),WORK      FORMATTED DESTINATION NAME                   
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         MVC   P,SPACES            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS PRW  TRN'                                      
         MVC   P+9(2),PBQAGY                                                    
*                                                                               
         LA    R1,P+15                                                          
         USING PPEDICTD,R1                                                      
*                                                                               
         MVI   PPWRTYPE,PPWRDATQ                                                
         MVC   PPWRNAME,PBQAGY                                                  
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES            ++DDS FTP CARD                               
         MVC   P(14),=CL14'++DDS      FTP'                                      
*                                                                               
         MVC   P+15(8),WRINAM      PROVIDE FORMAT NAME                          
*                                                                               
         CLC   WRINAM,SPACES       IF NO FORMAT NAME                            
         BH    *+16                                                             
         MVC   P+15(3),REMUSER        REPLACE WITH REQUESTOR INITIALS           
         MVC   P+18(5),SPACES         CLEAR END OF FIELD                        
*                                                                               
         OC    P+15(8),SPACES      MAKE UPPERCASE                               
*                                                                               
         SR    R1,R1                                                            
         SR    R2,R2                                                            
*                                                                               
         TRT   P+15(8),TRTTABLE    MAKE SURE ALL CHARS ARE ALLOWED              
         BZ    *+10                OKAY                                         
         MVC   P+15(8),=CL8'BADID'                                              
*                                                                               
         CLI   P+15,C'0'           FIRST CHAR CAN'T BE NUMERIC                  
         BL    *+8                                                              
         MVI   P+15,C'Z'           REPLACE WITH Z                               
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
PREDCTX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        PRINT INDEX HEADERS ON FIRST LINE TO PRINT                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRINX    DS    0H                                                               
*                                                                               
         ICM   R3,15,APQINDEX      POINT TO PQINDEX TABLE                       
         BZ    PRINXX              SKIP IF NONE AVAILABLE                       
*                                                                               
         CLI   PRNTSW,C' '         SKIP IF NOT FIRST LINE OF REPORT             
         BH    PRINXHLX                                                         
*                                                                               
*        IF PQIX=Y, PRINT INDEX HEADER                                          
*                                                                               
         USING PQINDEX,R3          ESTABLISH PQINDEX TABLE ENTRY                
*                                                                               
         MVC   P,SPACES            INIT PRINT LINE                              
         MVC   P2,SPACES                                                        
*                                                                               
         MVC   P(06),=C'<DECL>'    START OF HEADING DECLARATIONS                
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
         LA    R2,P                                                             
*                                                                               
         MVC   0(09,R2),=C'<REQNAME '                                           
         AHI   R2,9                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,WRINAMH+5      LENGTH OF REQUEST NAME                       
         BZ    *+22                NO ENTRY                                     
         AHI   RE,-1               DECREMENT FOR EXECUTE                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WRINAM      PASS REQUEST FORMAT NAME                     
*                                                                               
         LA    R2,1(RE,R2)                                                      
         MVI   0(R2),C'>'                                                       
*                                                                               
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IH '                                                
         LA    R2,4(R2)                                                         
*                                                                               
*        PASS HEADLINES FOR HEADLINES                                           
*                                                                               
         LA    R0,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS  MAX ENTRIES IN TAB          
*                                                                               
PRINXHLP DS    0H                                                               
*                                                                               
         OC    PQKEYWRD,PQKEYWRD   DONE WHEN TABLE EXHAUSTED                    
         BZ    PRINXHDN                                                         
*                                                                               
         CLI   PQPOSO,C'H'         ONLY INTERESTED IN HEADLINES                 
         BNE   PRINXHDN                                                         
*                                                                               
         BRAS  RE,IXFMT            ADD HEADLINE TO STRING                       
*                                                                               
PRINXHCN DS    0H                                                               
*                                                                               
         AHI   R3,PQINDXEQ         BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,PRINXHLP                                                      
*                                                                               
PRINXHDN DS    0H                                                               
*                                                                               
         MVI   0(R2),C'>'          CLOSE EXPRESSION                             
         GOTO1 (RF),(R1)           PRINT HEADLINES                              
*                                                                               
         CLC   P2,SPACES           CHECK IF SECOND LINE NEEDED                  
         BNH   *+12                                                             
         MVC   P,P2                YES - MOVE DATA TO PRINT AREA                
         BASR  RE,RF                                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
         LTR   R0,R0               DONE IF NO ENTRIES LEFT IN TABLE             
         BZ    PRINXHDX                                                         
*                                                                               
*        PASS HEADLINES FOR MIDLINES                                            
*                                                                               
*        CURRENTLY ONLY HEADLINES IN TABLE SO THIS IS MOOT                      
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IM '   INDICATE MIDLINE HEADINGS                    
         LA    R2,4(R2)                                                         
*                                                                               
PRINXMLP DS    0H                                                               
*                                                                               
         OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    PRINXMDN             NO                                          
*                                                                               
         CLI   PQPOSO,C'M'         LOOKING FOR MIDLINES                         
         BE    *+8                                                              
         CLI   PQPOSO,C'R'         LOOKING FOR ROWS                             
         BNE   PRINXMDN            ELSE DONE                                    
*                                                                               
         BRAS  RE,IXFMT            ADD HEADLINES TO EXPRESSION                  
*                                                                               
PRINXMCN DS    0H                                                               
*                                                                               
         AHI   R3,PQINDXEQ         BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,PRINXMLP                                                      
*                                                                               
PRINXMDN DS    0H                                                               
*                                                                               
         MVI   0(R2),C'>'          CLOSE EXPRESSION                             
         GOTO1 (RF),(R1)           PRINT EXPRESSION                             
*                                                                               
         CLC   P2,SPACES           CHECK IF SECOND LINE NEEDED                  
         BNH   *+12                                                             
         MVC   P,P2                YES - MOVE DATA TO PRINT AREA                
         BASR  RE,RF                                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
         LTR   R0,R0               DONE IF NO ENTRIES LEFT IN TABLE             
         BZ    PRINXHDX                                                         
*                                                                               
*        PUT OUT HEADLINES FOR COLUMNS                                          
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IC '   INDICATE COLUMNS NEXT                        
         LA    R2,4(R2)                                                         
*                                                                               
PRINXCLP DS    0H                                                               
*                                                                               
         OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    PRINXCDN             NO                                          
*                                                                               
         BRAS  RE,IXFMT            ADD HEADLINES TO EXPRESSION                  
*                                                                               
PRINXCCN DS    0H                                                               
*                                                                               
         AHI   R3,PQINDXEQ         BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,PRINXCLP                                                      
*                                                                               
PRINXCDN DS    0H                                                               
*                                                                               
         MVI   0(R2),C'>'          CLOSE EXPRESSION                             
         GOTO1 (RF),(R1)           PRINT EXPRESSION                             
*                                                                               
         CLC   P2,SPACES           CHECK IF SECOND LINE NEEDED                  
         BNH   *+12                                                             
         MVC   P,P2                YES - MOVE DATA TO PRINT AREA                
         BASR  RE,RF                                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
PRINXHDX DS    0H                                                               
*                                                                               
         CLI   DOWNOPT,C'Y'        DOWNLOAD?                                    
         BNE   PRINXDNX             NO                                          
*                                                                               
         MVC   P(10),=C'<FMT DATA>'                                             
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         B     PRINXDNN                                                         
*                                                                               
PRINXDNX DS    0H                                                               
*                                                                               
         MVC   P(04),=C'<HL '      NUMBER OF HEADLINES                          
         EDIT  LASTHEAD,(2,P+4),FILL=0                                          
         MVI   P+6,C'>'                                                         
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
PRINXDNN DS    0H                                                               
*                                                                               
         MVC   P(07),=C'</DECL>'   END OF HEADING DECLARATIONS                  
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
         DROP  R3                                                               
*                                                                               
PRINXHLX DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        PRINT DATA FOR LEVEL THAT CHANGED AND ALL LOWER              *         
*                                                                     *         
*        HEADLINES ONLY AT THIS POINT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRINXLV  DS    0H                                                               
*                                                                               
         TM    NEWOPTS,NOPTCHGQ    SKIP IF NO CHANGES AROUND                    
         BNO   PRINXLVX                                                         
*                                                                               
         NI    NEWOPTS,X'FF'-NOPTCHGQ TURN OFF CHANGE INDICATOR                 
*                                                                               
         CLI   DOWNOPT,C'Y'        IF WE ARE DOWNLOADING, NO DATA LINES         
         BE    PRINXLVX                                                         
*                                                                               
         ICM   R3,15,APQINDEX      POINT TO START OF PQINDEX TABLE              
         BZ    PRINXLVX            NO TABLE AVAILABLE                           
         USING PQINDEX,R3          ESTABLISH TABLE ENTRY                        
*                                                                               
         LA    R2,P                                                             
*                                                                               
         MVI   HALF,0                                                           
         LA    R1,1                INIT LEVEL COUNTER                           
         LA    R4,MAXHEADS         INDEX LINES ONLY FOR HEAD CHANGES            
         LA    R2,P                                                             
*                                                                               
PRINXLVL DS    0H                                                               
*                                                                               
         CLI   PQPOSO,C'H'         INDEX LINES ONLY FOR HEAD CHANGES            
         BNE   PRINXLVC                                                         
*                                                                               
         TM    PQSTATUS,PQCHG      HAS THIS KEY CHANGED?                        
         BNO   PRINXLVC             NO                                          
*                                                                               
         NI    PQSTATUS,X'FF'-PQCHG       RESET CHANGE FLAG                     
*                                                                               
         CLI   HALF,0              ANY ENTRIES YET?                             
         BNE   PRINXLV5             YES                                         
*                                                                               
         MVI   HALF,1              SET HAVE ONE NOW                             
*                                                                               
         MVC   0(06,R2),=C'<DATA '                                              
         AHI   R2,6                                                             
         EDIT  (R1),(2,0(R2)),FILL=0   01=                                      
         MVI   2(R2),C'='                                                       
         AHI   R2,3                                                             
*                                                                               
PRINXLV5 DS    0H                                                               
*                                                                               
*        PASS CODE VALUE OF HEADLINE                                            
*                                                                               
         L     RE,PQAOUT           GET A(OUTPUT SORT AREA)                      
         AHI   RE,L'LABLAREA+1     RE=CODE PORTION                              
         LA    RF,L'CODEAREA                                                    
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)       MOVE DATA TO PRINT LINE                      
*                                                                               
         LA    R2,1(RF,R2)         GET LAST USED PRINT POSN                     
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),X'5E'         SEMI-COLON                                   
         AHI   R2,2                                                             
*                                                                               
PRINXLVC DS    0H                                                               
*                                                                               
         AHI   R1,1                BUMP LEVEL COUNTER                           
         AHI   R3,PQINDXEQ         BUMP TO NEXT TABLE ENTRY                     
         BCT   R4,PRINXLVL                                                      
*                                                                               
         DROP  R3                                                               
*                                                                               
PRINXLVD DS    0H                                                               
*                                                                               
         CLI   HALF,1              SKIP IF NOTHING PRINTED                      
         BNE   PRINXLVX                                                         
*                                                                               
         MVI   0(R2),C'>'                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
*                                                                               
PRINXLVX DS    0H                                                               
*                                                                               
PRINXX   DS    0H                                                               
*                                                                               
* FIRST TIME CONTROLS                                                           
*        PRINT ANY LEADING STANDARD COMMENTS ON FIRST TIME IN                   
*                                                                               
         MVI   PRNTSW,C'Y'         INDICATE A LINE WAS PRINTED                  
*                                                                               
         TM    PBQCOM1P,PBQCOMFQ   TEST FOR ANY LEADING STANDARD                
         BO    *+8                   COMMENTS                                   
         TM    PBQCOM2P,PBQCOMFQ                                                
         BNO   PRINT10                                                          
*                                                                               
         CLI   OFFLINE,C'Y'        MUST BE OFF-LINE                             
         BNE   PRINT10                                                          
*                                                                               
         L     R0,HEADHOOK         SAVE CURRENT HEADLINE ROUTINE ADDR           
*                                                                               
         L     R1,VGENHD           RESET HOOK ADDRESS IN SPOOL BLOCK            
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE HEADINGS PRINT                         
*                                                                               
         GOTO1 =A(COMPRT),DMCB,(RC),PBQCOMFQ,RR=RELO01 1ST COMMENTS             
*                                                                               
         ST    R0,HEADHOOK         RESTORE HEAD ROUTINE ADDRESS                 
*                                                                               
         MVI   PRNTSW,C'C'         INDICATE A COVER SHEET WAS PRINTED           
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE HEADINGS PRINT                         
*                                                                               
         NI    PBQCOM1P,X'FF'-PBQCOMFQ   TURN OFF STARTING COMMENTS             
         NI    PBQCOM2P,X'FF'-PBQCOMFQ      INDICATORS                          
*                                                                               
PRINT10  DS    0H                                                               
*                                                                               
         GOTO1 =A(RPT),DMCB,('RPDRHOOK',(RC)),RR=RELO01 TRY USER OVLY           
*                                                                               
PRINTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         TITLE 'PRWRI01 - FIRST TIME CONTROLS - FIRSTS'                         
***********************************************************************         
*                                                                     *         
*        FIRST TIME CONTROLS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FIRSTS   DS    0H                                                               
*                                                                               
*        DETERMINE WHICH KEYWORD IS BREAKING                                    
*                                                                               
FSTINX   DS    0H                                                               
*                                                                               
         ICM   RF,15,APQINDEX      POINT TO PQINDEX TABLE                       
         BZ    FSTINXX             SKIP IF NO TABLE PRESENT                     
*                                                                               
         USING PQINDEX,RF          ESTABLISH TABLE ENTRY                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,GLARGS         GET LEVEL BREAK                              
         BZ    FSTINXX             SKIP LEVEL 0 BREAKS                          
*                                                                               
         AHI   R1,-1               DECREMENT FOR INDEXING                       
         MHI   R1,PQINDXEQ         CALCULATE INDEX INTO TABLE                   
*                                                                               
         AR    RF,R1               POINT TO ENTRY IN PQINDEX TABLE              
*                                                                               
         OI    PQSTATUS,PQCHG      INDICATE THIS ROW HAS CHANGED                
         OI    NEWOPTS,NOPTCHGQ    INDICATE CHANGE SOMEWHERE                    
*                                                                               
         L     RE,GLADTENT         POINT TO OUTPUT DESCRIPTION                  
         USING DROD,RE             ESTABLLISH OUTPUT DESCRIPTION                
         MVC   PQAOUT,DROAPOS      SAVE OUTPUT ADDRESS                          
*                                                                               
         DROP  RE,RF                                                            
*                                                                               
FSTINXX  DS    0H                                                               
*                                                                               
         CLI   CLTLEV,0            TEST CLIENT IS A ROW                         
         BE    FS10                                                             
         CLC   CLTLEV,GLARGS       YES - TEST THIS IS CLIENT FIRST              
         BNE   FS10                                                             
         CLC   PBQCLT,=C'ALL'      YES - TEST MULTI CLIENT REQUEST              
         BE    FS02                                                             
         CLI   PBQCLT,C'*'                                                      
         BE    FS02                                                             
         CLI   PBQCLT,C'$'                                                      
         BNE   FS10                                                             
*                                                                               
FS02     L     RE,PBAPRDBF         YES - CLEAR THE PRODUCT BUFFER               
         L     RF,PBLPRDBF                                                      
         XCEF  ,                                                                
         XC    PBCPRDBF,PBCPRDBF                                                
         XC    PBCLT,PBCLT                                                      
         ICM   R1,15,GLADTENT      EXTRACT THE CLIENT                           
         BZ    FS10                                                             
         L     R1,DROIADD-DROD(R1)                                              
         LTR   R1,R1                                                            
         BZ    FS10                                                             
         LH    R1,DRINDISP-DRIND(R1)                                            
         A     R1,GLATHREC                                                      
         MVC   PBCLT,0(R1)                                                      
*                                                                               
FS10     B     FSX                                                              
*                                                                               
FSX      GOTO1 =A(RPT),DMCB,('RPDRHOOK',(RC)),RR=RELO01 TRY USER OVLY           
         XIT1                                                                   
*                                                                               
         TITLE 'PRWRI01 - HEADHOOK - HEADHK'                                    
***********************************************************************         
*                                                                     *         
*        HEADHOOK                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HEADHK   DS    0H                                                               
*                                                                               
HDHOOKX  DS    0H                                                               
         GOTO1 =A(RPT),DMCB,('RPDRHOOK',(RC)),RR=RELO01 TRY USER OVLY           
         XIT1                                                                   
*                                                                               
         TITLE 'PRWRI01 - LAST TIME CONTROLS - LASTS'                           
***********************************************************************         
*                                                                     *         
*        LAST TIME CONTROLS                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LASTS    DS    0H                                                               
*                                                                               
LSX      DS    0H                                                               
         GOTO1 =A(RPT),DMCB,('RPDRHOOK',(RC)),RR=RELO01 TRY USER OVLY           
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
RPTSPECS SSPEC H1,35,C'PRNTIO TRACE'                                            
         SSPEC H2,35,C'------------'                                            
         DC    X'00'                                                            
         EJECT                                                                  
PATCH    DC    XL32'00'                                                         
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
SVGROSS  DS    XL64'0'                                                          
*                                                                               
ESTBUFF  DC    (LESTBUFF)X'00'                                                  
LESTBUFF EQU   512                                                              
         SPACE 2                                                                
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
*        TRTTABLE FOR VALID DATASET NAME CHARACTERS                             
*                                                                               
TRTTABLE DC    256AL1(*-TRTTABLE)                                               
         ORG   TRTTABLE            ALPHANUMERIC ALLOWED                         
         DC    X'FF'                                                            
         ORG   TRTTABLE+C' '       ALPHANUMERIC ALLOWED                         
         DC    X'00'                                                            
         ORG   TRTTABLE+C'A'       ALPHANUMERIC ALLOWED                         
         DC    9X'00'                                                           
         ORG   TRTTABLE+C'J'       ALPHANUMERIC ALLOWED                         
         DC    9X'00'                                                           
         ORG   TRTTABLE+C'S'       ALPHANUMERIC ALLOWED                         
         DC    8X'00'                                                           
         ORG   TRTTABLE+C'0'       ALPHANUMERIC ALLOWED                         
         DC    10X'00'                                                          
         ORG                                                                    
*                                                                               
         TITLE 'PRWRI01 - FORMAT INDEX DATA - IXFMT'                            
***********************************************************************         
*                                                                     *         
* FORMAT INDEX DATA                                                   *         
*                                                                     *         
*   INPUT    R2 = A(FORMAT AREA)                                      *         
*            R3 = A(PQINDEX ENTRY)                                    *         
*                                                                     *         
*   RETURN   FORMATTED DATA, FOLLOWED BY A SEMI-COLON                 *         
*            R2 = A(NEXT BLANK SPACE)                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IXFMT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PQINDEX,R3          ESTABLISH INDEX TABLE ENTRY                  
*                                                                               
         MVC   0(8,R2),PQKEYWRD    DISPLAY KEYWORD                              
*                                                                               
         AHI   R2,8                POINT TO END OF KEYWORD                      
         CLI   0(R2),C' '          FIND LAST NON-BLANK IN KEYWORD               
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         CLC   PQHEAD1(96),SPACES   SKIP IF NO HEADLINES                        
         BNH   IXFMTDN                                                          
*                                                                               
         CLC   =C'<IC ',P          SKIP IF COLUMNS                              
         BE    IXFMTDN                                                          
*                                                                               
*        PUT OUT HEADLINES                                                      
*                                                                               
         LA    R1,PQHEAD1          POINT TO HEADER                              
         LA    RE,95               EXECUTE LENGTH                               
         MVC   1(2,R2),=C'="'                                                   
         AHI   R2,3                                                             
         LA    R0,4                MAX HEADERS                                  
*                                                                               
IXFMTLP  DS    0H                                                               
*                                                                               
         MVC   0(24,R2),0(R1)      MOVE OUT HEADER                              
         OC    0(24,R2),SPACES     MAKE UPPERCASE                               
*                                                                               
         AHI   R2,24               END OF HEADER                                
*                                                                               
         CLI   0(R2),C' '          BACK UP TO LAST CHAR                         
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         MVI   1(R2),C'"'          CLOSE QUOTE                                  
         AHI   R2,1                                                             
*                                                                               
         BCT   R0,*+8                                                           
         B     IXFMTDN                                                          
*                                                                               
IXFMTCN  DS    0H                                                               
*                                                                               
         AHI   R1,24               NEXT HEADER                                  
         AHI   RE,-24              L'REMAINING HEADERS                          
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES      ANY LEFT?                                    
         BNH   IXFMTDN                                                          
*                                                                               
         MVC   1(2,R2),=C',"'                                                   
         AHI   R2,3                                                             
         B     IXFMTLP                                                          
*                                                                               
IXFMTDN  DS    0H                                                               
*                                                                               
         MVI   1(R2),X'5E'         SEMICOLON                                    
         AHI   R2,2                                                             
*                                                                               
IXFMTX   DS    0H                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
* ROUTINE TO CALL USER REPORT OVERLAY                             L01           
*                                                                               
RPT      NMOD1 0,**#RPT                                           L01           
         ICM   RF,15,ARPTNTRY                                     L01           
         BZ    RPTX                                               L01           
         L     RC,0(R1)                                           L01           
         LA    RC,0(RC)                                                         
         MVC   RPMODE,0(R1)                                       L01           
         GOTO1 (RF),DMCB,(RC)                                     L01           
RPTX     XIT1                                                                   
*                                                                               
         TITLE 'PRWRI01 - PRINT COMMENTS AT END OF REPORT - COMPRT'             
***********************************************************************         
*                                                                     *         
*        PRINT STANDARD COMMENTS AT START AND END OF REPORT           *         
*                                                                     *         
*NTRY - PARM0 ==> WORKAREA                                            *         
*       PARM1+3 = FIRST OR LAST COMMENTS INDICATOR                    *         
*                                                                     *         
***********************************************************************         
         DROP  R4,RA                                                            
         DS    0D                                                               
COMPRT   NMOD1 0,COMPRT                                                         
***      =====          **                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         CLC   CPRTTYPE,7(R1)      EXIT IF SAME TYPE AS LAST TIME               
         BE    COMPRTX                                                          
*                                                                               
         MVC   CPRTTYPE,7(R1)      SAVE COMMENT TYPE INDICATOR                  
*                                                                               
         L     RF,ABOX             ESTABLISH BOXAREA                            
         USING BOXD,RF                                                          
         MVC   CPRTYORN,BOXYORN    SAVE BOX INDICATOR                           
*                                                                               
CPRTPOS  DS    0H                                                               
*                                                                               
*        FIND STARTING PRINT POSITION FOR COMMENTS                              
*                                                                               
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
*                                                                               
         L     RE,GLAP1            A(FIRST DETAIL LINE)                         
*                                                                               
         L     R1,GLAINTD          DRIVER INTERNAL RECORD                       
         USING GLINTD,R1                                                        
*                                                                               
         LH    RF,GLPWIDTH         REPORT WIDTH                   L08           
*                                                                               
         CH    RF,=H'70'           IF REPORT WIDER THAN           L08           
         BNL   CPRTPOS1            STANDARD COMMENT LENGTH        L08           
*                                                                               
         L     RF,PWIDTH           SET TO CENTER ON PAGE                        
*                                                                               
         CLI   LEFTOPT,C'Y'        IF LEFT OPTION ON                            
         BNE   *+8                                                              
         LH    RF,=H'70'           FORCE LEFT SIDE START                        
*                                                                               
         B     CPRTPOS2                                                         
*                                                                               
CPRTPOS1 DS    0H                                                               
*                                                                               
         AH    RE,GLPDISP          POINT TO REPORT LEFT MARGIN                  
*                                                                               
CPRTPOS2 DS    0H                                                               
*                                                                               
         SH    RF,=H'70'           CENTER COMMENTS UNDER REPORT   L08           
         SRL   RF,1                                               L08           
         AR    RE,RF                                              L08           
*                                                                               
CPRTPOSX DS    0H                                                               
*                                                                               
         ST    RE,CPRTAPOS         SAVE STARTING PRINT POSITION                 
*                                                                               
         CLI   CPRTTYPE,PBQXCLPQ   SKIP IF PRINT EXCLT MSG                      
         BE    CPRTXCLT                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
         TM    CPRTTYPE,PBQCOMLQ   IF DOING LAST STANDARD COMMENTS              
         BNO   *+8                                                              
         OI    PBASVHED,X'40'      FORCE TO PRINT SAVED HEADINGS                
*                                                                               
*        READ STANDARD COMMENT RECORD AND PRINT ALL COMMENTS IN IT              
*                                                                               
         LA    R3,PBQCOM1          START WITH FIRST STANDARD COMMENT            
         LA    R4,PBQCOM1P                                                      
*                                                                               
CPRTSTLP DS      0H                                                             
*                                                                               
         MVC   CPRTBYTE,CPRTTYPE   MOVE COMMENT TYPE TO WORKAREA                
         NC    CPRTBYTE,0(R4)      TEST IF THIS COMMENT WANTED NOW              
         BZ    CPRTSTCN            NO                                           
*                                                                               
         OC    0(4,R3),0(R3)       MUST HAVE A DISK ADDRESS                     
         BZ    CPRTSTCN                                                         
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         L     RF,AIO1             SET IOAREA                                   
         ST    RF,AIO                                                           
*                                                                               
         MVC   KEY+27(4),0(R3)     SET DISK ADDRESS                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*        PRINT COMMENT LINES                                                    
*                                                                               
         L     RF,ABOX                                                          
         USING BOXD,RF             ESTABLISH BOXAREA                            
*                                                                               
         MVI   BOXYORN,C'N'        TURN OFF BOXES                               
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  SPACE A LINE BEFORE PRINTING                 
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,33(R2)           POINT TO FIRST ELEMENT IN RECORD             
         SR    R0,R0                                                            
*                                                                               
CPRTPRLP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             EOR                                          
         BE    CPRTPRDN                                                         
*                                                                               
         CLI   0(R2),X'40'         ONLY WANT COMMENT ELEMENTS                   
         BNE   CPRTPRCN                                                         
*                                                                               
         CLC   =C'++START',2(R2)   IF ++START                                   
         BE    CPRTPRCN              SKIP                         L08           
*                                                                               
         CLC   =C'++END',2(R2)     IF ++END                                     
         BE    CPRTPRCN              SKIP                         L08           
*                                                                               
         SR    R5,R5                                                            
         IC    R5,1(R2)            ELEMENT'S LENGTH                             
         SH    R5,=H'2'            COMMENT LENGTH IS 2 LESS THAN ELM'S          
*                                                                               
         LTR   R5,R5                                                            
         BNP   CPRTPRCN            MUST HAVE SOME LENGTH                        
*                                                                               
         LA    R1,2(R2)            POINT TO START OF COMMENT                    
*                                                                               
         L     RE,CPRTAPOS         POINT TO START OF COMMENT PRINTAREA          
*                                                                               
         CLI   0(R1),C'+'          SKIP UNLESS LINES TO BE SKIPPED              
         BNE   CPRTPR30                                                         
*                                                                               
         PACK  DUB,1(1,R1)         NUMBER OF LINES TO SKIP                      
         CVB   RF,DUB                                                           
*                                                                               
         MVI   0(RE),0             FORCE BLANK LINE                             
         A     RE,PWIDTH           POINT TO NEXT OUTPUT LINE                    
         BCT   RF,*-8              LOOP FOR NUMBER OF LINES                     
*                                                                               
         LA    R1,2(R1)            POINT TO BODY OF COMMENT                     
         SH    R5,=H'2'            ADJUST COMMENT LENGTH                        
         BNP   CPRTPR40            MUST HAVE SOMETHING TO PRINT                 
*                                                                               
CPRTPR30 DS    0H                                                               
*                                                                               
         BCTR  R5,0                DECREMENT LENGTH FOR EXECUTE                 
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       MOVE COMMENT TO OUTPUT         L08           
*                                                                               
CPRTPR40 DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
CPRTPRCN DS    0H                                                               
*                                                                               
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     CPRTPRLP                                                         
*                                                                               
CPRTPRDN DS    0H                                                               
*                                                                               
CPRTSTCN DS    0H                                                               
*                                                                               
         LA    RF,PBQCOM2          DONE IF SECOND COMMENT JUST DONE             
         CR    R3,RF                                                            
         BE    CPRTSTDN                                                         
*                                                                               
         LA    R3,PBQCOM2          CONTINUE WITH 2ND STANDARD COMMENT           
         LA    R4,PBQCOM2P                                                      
*                                                                               
         B     CPRTSTLP                                                         
*                                                                               
CPRTSTDN DS    0H                                                               
*                                                                               
         L     RF,ABOX             ESTABLISH BOXAREA                            
         USING BOXD,RF                                                          
         MVC   BOXYORN,CPRTYORN    RESTORE BOX INDICATOR                        
*                                                                               
         B     COMPRTX                                                          
*                                                                               
*        PRINT MESSAGE THAT SOME CLIENTS EXCLUDED FROM THE REPORT               
*        BECAUSE OF SECURITY SETTINGS                                           
*                                                                               
CPRTXCLT DS    0H                                                               
*                                                                               
         CLI   PRNTSW,0           SKIP IF NO LINES PRINTED                      
         BE    CPRTXCLX                                                         
*                                                                               
         OI    PBASVHED,X'40'      FORCE TO PRINT SAVED HEADINGS                
*                                                                               
*        PRINT MESSAGE                                                          
*                                                                               
         L     RF,ABOX                                                          
         USING BOXD,RF             ESTABLISH BOXAREA                            
*                                                                               
         MVI   BOXYORN,C'N'        TURN OFF BOXES                               
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  SPACE A LINE BEFORE PRINTING                 
*                                                                               
         LHI   R5,L'XCLTMSG        MESSAGE LENGTH                               
         L     RE,CPRTAPOS         POINT TO START OF COMMENT PRINTAREA          
*                                                                               
         BCTR  R5,0                DECREMENT LENGTH FOR EXECUTE                 
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),XCLTMSG     MOVE MESSAGE TO OUTPUT                       
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
         L     RF,ABOX             ESTABLISH BOXAREA                            
         USING BOXD,RF                                                          
         MVC   BOXYORN,CPRTYORN    RESTORE BOX INDICATOR                        
*                                                                               
CPRTXCLX DS    0H                                                               
*                                                                               
COMPRTX  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
CPRTAPOS DC    A(0)                STARTING PRINT POSITION FOR COMMENTS         
CPRTTYPE DC    X'00'               TYPE OF STANDARD COMMENTS TO PRINT           
CPRTBYTE DC    X'00'               WORK AREA                                    
CPRTYORN DC    X'00'               BOXES INDICATOR SAVEAREA                     
*                                                                               
XCLTMSG  DC    C'REPORT DOES NOT INCLUDE UNAUTHORIZED CLIENTS'                  
*                                                                               
         SPACE 3                                                                
         DROP  R1,RF                                                            
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI01 - PRINT MISSING PUB LINKS REPORT - PUBLNKER'            
***********************************************************************         
*                                                                     *         
*        PRINT MISSING PUB LINKS                                      *         
*                                                                     *         
*NTRY - PARM0 ==> WORKAREA                                            *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
PUBLNKER NMOD1 0,**#PLERR                                                       
***      =====          **                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         L     RA,PBPLEDCB         POINT TO PUBERR DCB                          
*                                                                               
         OPEN  ((RA),INPUT)        OPEN PUB ERROR FILE                          
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,VGENHD           RESET HOOK ADDRESS IN SPOOL BLOCK            
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVC   TITLE+20(24),=C'MISSING PUB LINKS REPORT'                        
         XC    SUBTITLE,SUBTITLE                                                
*                                                                               
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
*                                                                               
         L     R2,GLAINTD          DRIVER INTERNAL RECORD                       
         USING GLINTD,R2                                                        
*                                                                               
         L     RF,ABOX                                                          
         USING BOXD,RF             ESTABLISH BOXAREA                            
*                                                                               
         MVI   BOXYORN,C'N'        TURN OFF BOXES                               
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
*                                                                               
         L     R3,GLAP1            POINT TO PRINT LINE                          
         USING PLELINED,R3         ESTABLISH PUB ERROR TITLES                   
*                                                                               
*        FORMAT COLUMN HEADINGS                                                 
*                                                                               
         MVC   PLEMEDT,=C'MEDIA'                                                
         MVC   PLERAGYT,=C'AOR'                                                 
         MVC   PLERCLTT,=C'AOR'                                                 
         MVC   PLERPUBT,=C'AOR'                                                 
         MVC   PLEAGYT,=C'AGY'                                                  
         MVC   PLECLTT,=C'AGY'                                                  
         MVC   PLEPUBT,=C'AGY'                                                  
         MVC   PLEPUBNT(3),=C'AGY'                                              
         MVC   PLEPZNMT(3),=C'AGY'                                              
         MVC   PLEPCTYT(3),=C'AGY'                                              
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT LINE OF REPORT                         
*                                                                               
         MVC   PLERAGYT,=C'AGY'                                                 
         MVC   PLERCLTT,=C'CLT'                                                 
         MVC   PLERPUBT,=C'PUB'                                                 
         MVC   PLEAGYT,=C'AGY'                                                  
         MVC   PLECLTT,=C'CLT'                                                  
         MVC   PLEPUBT,=C'PUB'                                                  
         MVC   PLEPUBNT,=CL20'PUBLICATION NAME'                                 
         MVC   PLEPZNMT,=CL20'ZONE NAME'                                        
         MVC   PLEPCTYT,=CL16'CITY'                                             
         MVC   PLEPSTT,=CL2'ST'                                                 
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT LINE OF REPORT                         
*                                                                               
         MVC   PLEMEDT,=C'-----'                                                
         MVC   PLERAGYT,=C'---'                                                 
         MVC   PLERCLTT,=C'---'                                                 
         MVC   PLERPUBT,=C'---'                                                 
         MVC   PLEAGYT,=C'---'                                                  
         MVC   PLECLTT,=C'---'                                                  
         MVC   PLEPUBT,=C'---'                                                  
         MVC   PLEPUBNT,=20C'-'                                                 
         MVC   PLEPZNMT,=20C'-'                                                 
         MVC   PLEPCTYT,=20C'-'                                                 
         MVC   PLEPSTT,=20C'-'                                                  
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT LINE OF REPORT                         
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT SPACING LINE                           
*                                                                               
*        READ FILE UNTIL END IS REACHED                                         
*                                                                               
PLELOOP  DS    0H                                                               
*                                                                               
         GET   (RA),PLEBLK         READ RECORD FROM FILE                        
*                                                                               
         CLC   PLEBLKSV,PLEBLK     SKIP IF SAME AS LAST TIME                    
         BE    PLELPCN                                                          
*                                                                               
         MVC   PLEBLKSV,PLEBLK     SAVE INCOMING RECORD                         
*                                                                               
*        FORMAT PRINT LINE                                                      
*                                                                               
         MVC   PLEPMED,PLEBLK+PAOBLKL  MEDIA                                    
*                                                                               
         MVC   PLERAGY,PAORAGY-PAOBLK+PLEBLK DISPLAY AGENCY OF RECORD           
         MVC   PLERCLT,PAORCLT-PAOBLK+PLEBLK AOR CLIENT                         
*                                                                               
         OC    PAORPUB#-PAOBLK+PLEBLK,PAORPUB#-PAOBLK+PLEBLK  SKIP IF           
         BZ    PBELP10                NO AOR PUB ID AVAILABLE                   
*                                                                               
         LA    RF,PAORPUB-PAOBLK+PLEBLK  POINT TO AOR PUB ID                    
*                                                                               
*        PRINT PUB NUMBER/EDITION/ZONE                                          
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,(RF),(C'S',PLERPUB#)                            
*                                                                               
PBELP10  DS    0H                                                               
*                                                                               
*        DISPLAY AGENCY INFORMATION                                             
*                                                                               
         MVC   PLEAGY,PAOAGY-PAOBLK+PLEBLK DISPLAY AGENCY                       
         MVC   PLECLT,PAOCLT-PAOBLK+PLEBLK AGY CLIENT                           
*                                                                               
         OC    PAOPUB#-PAOBLK+PLEBLK,PAOPUB#-PAOBLK+PLEBLK SKIP IF              
         BZ    PLELP20               NO AGY PUB ID AVAILABLE                    
*                                                                               
         LA    RF,PAOPUB#-PAOBLK+PLEBLK   PUB AGY ID                            
*                                                                               
*        PRINT PUB NUMBER/EDITION/ZONE                                          
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,(RF),(C'S',PLEPUB#)                             
*                                                                               
PLELP20  DS    0H                                                               
*                                                                               
         MVC   PLEPUBN,PLEPUBNM    PUB NAME                                     
         MVC   PLEPZNM,PLEZNAME    ZONE NAME                                    
         MVC   PLEPCTY,PLECITY     CITY                                         
         MVC   PLEPST,PLESTATE     STATE                                        
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT LINE OF REPORT                         
*                                                                               
PLELPCN  DS    0H                                                               
*                                                                               
         B     PLELOOP                                                          
*                                                                               
PLEEOD   DS    0H                                                               
*                                                                               
         L     RA,PBPLEDCB         POINT TO PUBERR DCB                          
*                                                                               
         CLOSE ((RA))              CLOSE FILE                                   
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         FREEPOOL (RA)                                                          
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    PLEBLKSV,PLEBLKSV   INIT SAVEAREA FOR NEXT REPORT                
*                                                                               
         MVI   PAOPLESW,0          CLEAR PUB LINK ERROR SW                      
*                                                                               
         B     PUBLNKEX                                                         
*                                                                               
PUBLNKEX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
PLEBLK   DC    XL(256)'00'         INPUT AREA-PAOBLK+PUBNAME                    
         ORG   PLEBLK+PAOBLKL                                                   
PLEMED   DS    CL1                 MEDIA                                        
PLEPUBNM DS    CL20                PUB NAME                                     
PLEZNAME DS    CL20                ZONE NAME                                    
PLECITY  DS    CL16                CITY                                         
PLESTATE DS    CL2                 STATE                                        
         ORG                                                                    
PLEBLKSV DC    XL(256)'00'         SAVEAREA INPUT AREA                          
*                                                                               
PUBERR   DCB   DDNAME=PUBERR,DSORG=PS,MACRF=(PM,GM),EODAD=PLEEOD                
*                                                                               
         DROP  R1,R2,R3,RF                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'INPUT FIELDS VALIDATION'                                        
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREC     NMOD1 STACKBFL,**#VREC,CLEAR=YES                                       
*                                                                               
         ST    RC,ASTACKBF         SAVE STACK BUFFER ADDRESS                    
*                                                                               
         L     RC,0(R1)            RESTORE WORKING STORAGE                      
*                                                                               
         MVI   NEWOPTS,0           INIT NEW OPTIONS                             
*                                                                               
         GOTO1 VALAGY              AGENCY                                       
*                                                                               
         MVI   FRSTLAST,C'Y'       DEFAULT TO RUNFRST/RUNLAST L01               
*                                                                               
         CLI   RPTOVLY,0                                                        
         BE    *+8                                                              
         MVI   FRSTLAST,C'N'       TURN OFF CALLS IF NOT WRITER                 
*                                                                               
         LA    R2,WRIMEDH          MEDIA                                        
         GOTO1 VALMED                                                           
*                                                                               
         LA    R2,WRIOPTH          VALIDATE OPTIONS EARLY                       
         GOTO1 VALOPTS                                                          
*                                                                               
         CLC   =C'TRANSMIT',CONREC       IF TRANSMIT REPORT                     
         BNE   NOTRNSMT                                                         
*                                                                               
         CLI   CONOUTH+5,0         ANYTHING ENTERED                             
         BNE   TRANS10               YES                                        
*                                                                               
         FOUT CONOUTH,=CL8'DOWN',8   FORCE DOWN OPTION                          
*                                                                               
         MVI   CONOUTH+5,4                                                      
*                                                                               
         MVC   TWAOUT,CONOUT                                                    
*                                                                               
TRANS10  DS    0H                                                               
*                                                                               
         MVI   DOWNOPT,C'Y'                                                     
*                                                                               
         OI    REQRTYP,REQTDOWN                                                 
         OI    GENSTAT2,NOREQDET         ALWAYS SET NOREQDET                    
         OI    SPECOPT,GLDLACTV+GLDLNOTR+GLDLALPH+GLDLNOHD+GLDLCUT              
*                                                                               
NOTRNSMT DS    0H                                                               
*                                                                               
*        INIT PQ INDEX DATA TABLE IF NEEDED                                     
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE,                         
         BNE   VRECINXX                                                         
*                                                                               
         OI    GENSTAT7,GES7ERHK   SET FOR ERROR HOOK                           
*                                                                               
         TM    NEWOPTS,NOPTPQIX    IF PQ INDEX OPTION SELECTD                   
         BO    VRECINX1             YES - ALWAYS GIVE INDEX                     
*                                                                               
         L     RF,TWAMASTC         ALLOCATE PQINDEX TABLE?                      
         USING MASTD,RF                                                         
*                                                                               
         TM    MCOPT1,MCQ1PQIX     DDMAST OPTION MUST BE ON                     
         BZ    VRECINXX                                                         
*                                                                               
         OC    MCREMOTE,MCREMOTE     MUST BE DIRECT                             
         BNZ   *+14                                                             
         OC    MCREMPQK,MCREMPQK     OR  SOON REPORT                            
         BZ    VRECINXX                                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
VRECINX1 DS    0H                                                               
*                                                                               
         LA    R3,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS MAX ENTRIES IN TAB           
         MHI   R3,PQINDXEQ         MAX RECS * L'ENTRY                           
         AHI   R3,4                EXTRA 4 BYTES FOR TABLE LENGTH               
*                                                                               
         ST    R3,DMCB+4           AMOUNT OF STORAGE WANTED                     
         ST    R3,DMCB+8                                                        
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET'  GET STORAGE FOR TABLE                        
         ICM   RE,15,4(R1)         NO ERRORS TOLERATED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R3,0(RE)            +0 = L'TABLE                                 
*                                                                               
         AHI   R3,-4               SIZE OF TABLE                                
         AHI   RE,4                A(START OF TABLE)                            
         ST    RE,APQINDEX         SAVE START ADDRESS                           
*                                                                               
         XCEF  (RE),(R3)           INIT TABLE                                   
*                                                                               
VRECINXX DS    0H                                                               
*                                                                               
         BRAS  RE,DOWNCHK          CHECK IF DOWN OPTION                         
*                                                                               
         LA    R2,WRICLTH          CLIENT                                       
         GOTO1 VALCLT                                                           
*                                                                               
         LA    R2,WRIPRDH          PRODUCT                                      
         GOTO1 VALPRD                                                           
*                                                                               
         LA    R2,WRIESTH          ESTIMATE                                     
         GOTO1 VALEST                                                           
*                                                                               
         LA    R2,WRIPUBH          PUB                                          
         GOTO1 VALPUB                                                           
*                                                                               
*                                  CHECK FOR PMTAPE                             
         CLI   RPTSCRN,X'D7'       THIS SCREEN DOES NOT HAVE DATE               
         BE    VALREC05            TYPE OR PERIOD // BYPASS                     
*                                                                               
*                                  CHECK FOR BTTAPE                             
         CLI   RPTSCRN,X'D9'       THIS SCREEN DOES NOT HAVE DATE L01           
         BE    VALREC05            TYPE OR PERIOD // BYPASS       L01           
*                                                                               
         LA    R2,WRIDTYH          DATE TYPE                                    
         GOTO1 VALDTYP                                                          
*                                                                               
         LA    R2,WRIPERH          PERIOD                                       
         GOTO1 VALPER                                                           
*                                                                               
VALREC05 DS    0H                                                               
*                                                                               
         LA    R2,WRIFLTH          OPTIONAL FILTERS               L01           
         GOTO1 VALFILT                                            L01           
*                                                                               
         CLI   DPGFILE,0           TESTDPG FILE NUMBER PROVIDED   L01           
         BNE   VALREC08      YES-SKIP RPT DEFINITION VALIDATION   L01           
*                                  NOTE THIS IS LOADED IN BASE    L01           
*                                                                               
         LA    R2,WRIADCH          ADCODE                                       
         GOTO1 VALADC                                                           
*                                                                               
*                                                                               
         LA    R2,WRIDIVH          DIVISION                       L01           
         GOTO1 VALDIV                                             L01           
*                                                                               
         LA    R2,WRIREGH          REGION                         L01           
         GOTO1 VALREG                                             L01           
*                                                                               
         LA    R2,WRIDISH          DISTRICT                       L01           
         GOTO1 VALDST                                             L01           
*                                                                               
         MVI   PBQRP2SW,0          INIT SECOND REPORT SWITCH                    
*                                                                               
         OC    RP2ID,RP2ID         SKIP IF NO SECOND REPORT TO BE DONE          
         BZ    VRRP1X                                                           
*                                                                               
         CLI   PBQRP2OP,C'C'       SKIP IF CONTINUATION OF REPORT               
         BE    VRRP1X                                                           
*                                                                               
         XC    BLOCK(42),BLOCK                                                  
         MVC   BLOCK(2),=X'0602'   LENGTH OF SPLIT FIELD                        
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(8),=C'RECORD'                                           
         MVC   BLOCK+22(2),=C'NP'                                               
*                                                                               
         LA    R4,BLOCK            POINT TO CREATED SCANNER BLOCK               
*                                                                               
         GOTO1 VROWDRON            VALIDATE RPTID AS KEYWORD                    
*                                                                               
         CLI   DRERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    DRFLAGO,X'FF'-X'80' TURN OFF PRINT IND ON ALL HEADLINES          
         NI    DRHEAD1,X'FF'-X'80'                                              
         NI    DRHEAD2,X'FF'-X'80'                                              
         NI    DRHEAD3,X'FF'-X'80'                                              
         NI    DRHEAD4,X'FF'-X'80'                                              
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFF-LINE                         
         BNE   VRRP1X                                                           
*                                                                               
         GOTO1 GROWDRON            GENERATE ROW                                 
*                                                                               
VRRP1X   DS    0H                                                               
*                                                                               
         LA    R2,WRIHEADH         HEADERS                                      
         MVI   MAX,6                                                            
         GOTO1 VALHEAD                                                          
*                                                                               
         L     R1,PDADATES            TEMP SAVE OF IOAREA                       
         LA    R0,WEELLENT                                                      
         BAS   RE,CLRBUFF                                                       
*                                                                               
         LA    R2,WRIMIDH          MIDLINE                                      
*                                                                               
         MVI   MAX,1                                                            
         GOTO1 VALMID                                                           
*                                                                               
         LA    R2,WRIROWSH         ROWS                                         
         MVI   MAX,6               NUMBER OF ENTRIES                            
         GOTO1 VALROWS                                                          
*                                  COLUMNS                                      
         LA    R2,WRICOLSH                                                      
         MVI   MAX,14              MAX NUMBER OF ENTRIES                        
         GOTO1 VALCOLS                                                          
*                                                                               
VALREC08 CLI   RPTSCRN,0           TEST USER SCREEN               L01           
         BNE   VALREC10                                           L01           
*                                                                               
         BRAS  RE,DOWNCHK          CHECK IF DOWN OPTION                         
*                                                                               
         LA    R2,WRITITH          USER TITLES                                  
         GOTO1 VALTITS                                                          
*                                                                               
VALREC10 BAS   RE,FINALVAL         FINAL SCREEN VALIDATION        L01           
*                                                                               
*        IF A SECOND REPORT TO BE RUN, VALIDATE HERE                            
*                                                                               
         OC    RP2ID,RP2ID         SKIP IF NO SECOND REPORT TO BE DONE          
         BZ    VRRP2X                                                           
*                                                                               
         MVI   PBQRP2SW,2          INDICATE SECOND REPORT                       
*                                                                               
         MVC   DRSTBUF,DRCURBUF    RESET DRONE BUFFER START                     
*                                                                               
         CLI   PBQRP2OP,C'C'       SKIP IF CONTINUATION OF A REPORT             
         BE    VRRP210                                                          
*                                                                               
         GOTO1 INITDRON            RE-INITIALIZE DRONE                          
*                                                                               
         XC    BLOCK(42),BLOCK                                                  
         MVC   BLOCK(2),=X'0602'   LENGTH OF SPLIT FIELD                        
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(8),=C'RECORD'                                           
         MVC   BLOCK+22(2),=C'NP'                                               
*                                                                               
         LA    R4,BLOCK            POINT TO CREATED SCANNER BLOCK               
*                                                                               
         GOTO1 VROWDRON            VALIDATE RPTID AS KEYWORD                    
*                                                                               
         CLI   DRERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    DRFLAGO,X'FF'-X'80' TURN OFF PRINT IND ON ALL HEADLINES          
         NI    DRHEAD1,X'FF'-X'80'                                              
         NI    DRHEAD2,X'FF'-X'80'                                              
         NI    DRHEAD3,X'FF'-X'80'                                              
         NI    DRHEAD4,X'FF'-X'80'                                              
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFF-LINE                         
         BNE   VRRP210                                                          
*                                                                               
         GOTO1 GROWDRON            GENERATE ROW                                 
*                                                                               
VRRP210  DS    0H                                                               
*                                                                               
         GOTOR RP2GET,DMCB,(RC)    READ IN SECOND REPORT                        
*                                                                               
         CLI   PBQRP2OP,C'C'       SKIP IF CONTINUATION OF A REPORT             
         BE    VRRP211                                                          
*                                                                               
         LA    R2,WRIHEADH         HEADERS                                      
         MVI   MAX,6                                                            
         GOTO1 VALHEAD                                                          
*                                                                               
         LA    R2,WRIMIDH          MIDLINE                                      
*                                                                               
         MVI   MAX,1                                                            
         GOTO1 VALMID                                                           
*                                                                               
         LA    R2,WRIROWSH         ROWS                                         
         MVI   MAX,6               NUMBER OF ENTRIES                            
         GOTO1 VALROWS                                                          
*                                  COLUMNS                                      
VRRP211  DS    0H                                                               
*                                                                               
         LA    R2,WRICOLSH                                                      
         MVI   MAX,14              MAX NUMBER OF ENTRIES                        
         GOTO1 VALCOLS                                                          
*                                                                               
         CLI   PBQRP2OP,C'C'       SKIP IF CONTINUATION OF A REPORT             
         BE    VRRP218             TESTING                                      
*                                                                               
         LA    R2,WRITITH          SAVE SECOND REPORT TITLE                     
*                                                                               
         LH    R3,=Y(RP2TITLE-SYSD) POINT TO RPT2 TITLE SAVE                    
         LA    R3,SYSD(R3)                                                      
*                                                                               
         MVC   0(L'RP2TITLE,R3),SPACES     INIT TITLE                           
*                                                                               
         CLI   5(R2),0                                                          
         BE    VRRP215             NO TITLE AVAILABLE                           
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVC   0(L'RP2TITLE,R3),WORK                                            
         OC    0(L'RP2TITLE,R3),SPACES                                          
*                                                                               
VRRP215  DS    0H                                                               
         LA    R1,L'RP2TITLE                                                    
         ST    R1,DMCB+4                                                        
*                                                                               
         GOTO1 CENTER,DMCB,(R3)                                                 
*                                                                               
*        RESTORE CURRENT TWA SAVED AT BACK OF TWA                               
*                                                                               
VRRP218  DS    0H                                                               
*                                                                               
         LH    RE,=Y(L'RP2TWASV)                                                
         LA    RE,CONHEADH-64(RE)  A(TWA SAVEAREA)                              
         LH    RF,=Y(TWAENDLQ)     SAVEAREA LENGTH                              
         LR    R1,RF               COPY LENGTH                                  
         LA    R0,CONHEADH-64      SAVE TWA FROM HERE                           
*                                                                               
         MVCL  R0,RE                                                            
*                                                                               
VRRP2X   DS    0H                                                               
*                                                                               
         GOTO1 WRAPDRON            WRAP UP DRONE                                
*                                                                               
VALRECX  XIT1                                                                   
*                                                                               
*==================================================                             
* SUBROUTINE CLEARS BUFFER AT R1 FOR LENGTH IN R0 *                             
*==================================================                             
         SPACE 1                                                                
CLRBUFF  LA    RF,256                                                           
         CR    R0,RF                                                            
         BNH   CLRBUFF2                                                         
         XC    0(256,R1),0(R1)                                                  
         AR    R1,RF                                                            
         SR    R0,RF                                                            
         BP    CLRBUFF                                                          
*                                                                               
CLRBUFF2 LTR   RF,R0                                                            
         BZR   RE                                                               
         BCTR  RF,0                                                             
         EX    RF,*+6                                                           
         BR    RE                                                               
         XC    0(0,R1),0(R1) *EXECUTED*                                         
*                                                                               
         EJECT                                                                  
*                                                                               
* FINAL SCREEN VALIDATION                                         L01           
*                                                                               
         DS    0D                                                               
FINALVAL NTR1                                                     L01           
*                                                                               
         CLI   RPTOVLY,0           TEST USER REPORT OVERLAY       L01           
         BE    FV02                                               L01           
*                                                                               
         LA    R4,ARPTNTRY         YES-LOAD IT                    L01           
         ICM   R4,8,RPTOVLY                                       L01           
         BAS   RE,MYCALLOV                                        L01           
         BNE   FV99                                               L01           
*                                                                               
FV02     CLI   DPGFILE,0           TEST DPG FILE NUMBER           L01           
         BE    FV05                                               L01           
*                                                                               
         CLI   OFFLINE,C'Y'        AND OFFLINE                    L01           
         BNE   FV03                                               L01           
*                                                                               
         LA    R4,ADPGPROG                                        L01           
         ICM   R4,8,DPGFILE                                       L01           
         BAS   RE,MYCALLOV         YES-GET IT'S ADDRESS           L01           
         BNE   FV99                                               L01           
*                                                                               
FV03     CLI   RPTOVLY,0           CHECK FOR REPORT OVERLAY       L01           
         BE    FV99                                               L01           
         GOTO1 =A(RPT),DMCB,('RPINIT',(RC)),RR=RELO01 CALL USER OVLY            
*                                                                               
FV05     GOTO1 =A(RPT),DMCB,('RPVAL',(RC)),RR=RELO01 CALL USER OVLY             
         XIT1                                                     L01           
*                                                                               
FV99     LA    R2,WRIOPTH                                         L01           
         L     R1,=A(EROPTM)                                      L01           
         B     MYCURSOR                                           L01           
*                                                                               
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                              L01           
*                                                                               
MYCURSOR A     R1,RELO01                                          L01           
         MVC   CONHEAD(30),0(R1)                                  L01           
         MVI   ERROR,X'FE'                                        L01           
         GOTO1 CURSERR                                            L01           
*                                                                               
*************************************                                           
EROPTM   DC    CL30'ERROR - INVALID REPORT OPTION'                L01           
*                                                                               
* ROUTINE TO LOAD A PHASE                                         L01           
* INPUT  : R4 HIGH ORDER BYTE = PHASE NUMBER                      L01           
*          R4 = A(AREA TO RECEIVE PHASE ADDRESS)                  L01           
*                                                                               
MYCALLOV LR    R0,RE                                              L01           
         XC    DMCB(4),DMCB                                       L01           
         STCM  R4,8,DMCB                                          L01           
         GOTO1 CALLOV,DMCB,,0,0                                   L01           
         CLI   4(R1),X'FF'                                        L01           
         BNE   *+8                                                L01           
         LTR   RE,R0                                              L01           
         BR    RE                                                 L01           
         MVC   0(4,R4),0(R1)       SAVE A(PHASE)                  L01           
         LR    RE,R0                                              L01           
         CR    RE,RE                                              L01           
         BR    RE                                                 L01           
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI01 - READ SECOND REPORT INTO TWA - DOWNCHK'                
***********************************************************************         
*                                                                     *         
*        PUT 'DOWN' IN OUTPUT FIELD IF DOWNOPT ON                     *         
*              RLP CAN SCREW THIS UP                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DOWNCHK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   DOWNOPT,C'N'        SKIP IF NOT DOWNLOADING                      
         BE    DOWNCHKX                                                         
*                                                                               
         CLI   DOWNOPT,0           SKIP IF NOT DOWNLOADING                      
         BE    DOWNCHKX                                                         
*                                                                               
         OC    ARFPBLK,ARFPBLK     RLP/RFP MODE?                                
         BZ    *+10                                                             
         MVC   TWAOUT,=CL8'DOWN'   MAKE SURE TWAOUT SET                         
*                                                                               
         OI    REQRTYP,REQTDOWN                                                 
*                                                                               
         CLI   CONOUT,C' '         SKIP IF OUTPUT TYPE REQUESTED                
         BH    DOWNCHKX                                                         
*                                                                               
         MVC   CONOUT(8),=CL8'DOWN'   YES-DEFAULT TO OUTPUT OF 'DOWN'           
         MVI   CONOUTH+5,4                                                      
         OI    CONOUTH+6,X'80'                                                  
         MVC   TWAOUT,CONOUT                                                    
*                                                                               
DOWNCHKX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI01 - READ SECOND REPORT INTO TWA - RP2GET'                 
***********************************************************************         
*                                                                     *         
*        SAVE CURRENT REPORT AND READ NEW ONE INTO THE TWA            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RP2GET   NMOD1 0,**#2RPT                                                        
*                                                                               
         L     RC,0(R1)            RESTORE WORKING STORAGE                      
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
*                                                                               
*        SAVE CURRENT TWA AT BACK OF TWA                                        
*                                                                               
         LH    RE,=Y(L'RP2TWASV)                                                
         LA    RE,CONHEADH-64(RE)  A(TWA SAVEAREA)                              
         LH    RF,=Y(TWAENDLQ)     SAVEAREA LENGTH                              
         LR    R1,RF               COPY LENGTH                                  
         LA    R0,CONHEADH-64      SAVE TWA FROM HERE                           
*                                                                               
         MVCL  RE,R0                                                            
*                                                                               
         TWAXC WRIHEADH            CLEAR BOTTOM OF SCREEN                       
*                                                                               
*        FIND PROGRAM RECORD FOR SECOND REPORT                                  
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'  SET FILE NAME                             
         MVI   USEIO,C'Y'                                                       
*                                                                               
         XC    KEY,KEY             ESTABLISH PROGRAM RECORD KEY                 
         LA    R4,KEY                                                           
         USING CT01RECD,R4                                                      
*                                                                               
         MVI   CT01TYPE,CT01TYPQ   SET RECORD TYPE                              
         MVC   CT01AGID,AGYALPHA   SET AGENCY ID                                
         MVI   CT01SYS,4           SET FOR PRINT SYSTEM                         
         MVI   CT01PRG,5           SET FOR PRINT WRITER                         
         MVI   CT01PHAS,1          SET FOR PHASE 1                              
         MVC   CT01NAME,RP2ID      SET SECOND REPORT ID                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 READ                READ IN PROGRAM RECORD                       
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,C'N'                                                       
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         LA    R2,WRIHEADH         POINT TO FIRST HEADLINE                      
         L     R4,AIO              POINT TO FOUND RECORD                        
         LA    R6,CT01DATA         POINT TO FIRST ELEMENT                       
*                                                                               
RP2FLD1L DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE AT END OF SCREEN                        
         BE    RP2FLD1D                                                         
*                                                                               
         TM    1(R2),X'20'         SKIP PROTECTED FIELDS                        
         BO    RP2FLD1C                                                         
*                                                                               
         TM    1(R2),X'02'         MUST HAVE EXTENDED FIELD HEADER              
         BNO   RP2FLD1C                                                         
*                                                                               
         MVI   5(R2),0             MAKE SURE IT STARTS WITH 0 LENGTH            
*                                                                               
         IC    RF,0(R2)            TWA FIELD LENGTH                             
         LA    R3,0(RF,R2)         POINT TO EXTENDED HEADER                     
         SH    R3,=H'8'                                                         
*                                                                               
RP2FLD2L DS    0H                                                               
*                                                                               
         USING CT01FCD,R6          ESTABLISH FIELD DATA ELEMENT                 
*                                                                               
         CLI   CT01FCD,0           DONE IF EOR REACHED                          
         BE    RP2FLD2D                                                         
*                                                                               
         CLI   CT01FCD,CT01FCDQ    SKIP IF NOT A FIELD ELEMENT                  
         BNE   RP2FLD2C                                                         
*                                                                               
         CLC   CT01ID,0(R3)        IF FIELD ID NUMBER                           
         BL    RP2FLD2C               LOW - KEEP LOOKING IN RECORD              
         BH    RP2FLD2D               HIGH - SKIP TO NEXT TWA FLIELD            
*                                     MATCH - MOVE TO TWA FIELD                 
         IC    RF,CT01FLEN         ELEMENT LENGTH                               
         SH    RF,=Y(CT01TEXT-CT01FLDD)   TEXT LENGTH                           
         BNP   RP2FLD21            NO TEXT TO MOVE                              
         STC   RF,5(R2)            SET INPUT LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),CT01TEXT    MOVE TEXT TO TWA FIELD                       
*                                                                               
         OI    4(R2),X'80'         FIELD INPUT THIS TIME                        
*                                                                               
RP2FLD21 DS    0H                                                               
*                                                                               
         IC    RF,CT01FLEN         ELEMENT LENGTH                               
         LA    R6,CT01FLDD(RF)     NEXT ELEMENT                                 
*                                                                               
         B     RP2FLD2D                                                         
*                                                                               
RP2FLD2C DS    0H                                                               
*                                                                               
         IC    RF,CT01FLEN         ELEMENT LENGTH                               
         LA    R6,CT01FLDD(RF)     NEXT ELEMENT                                 
*                                                                               
         B     RP2FLD2L                                                         
*                                                                               
RP2FLD2D DS    0H                                                               
*                                                                               
RP2FLD1C DS    0H                                                               
*                                                                               
         IC    RF,0(R2)            TWA FIELD LENGTH                             
         LA    R2,0(RF,R2)         NEXT ELEMENT                                 
*                                                                               
         B     RP2FLD1L                                                         
*                                                                               
RP2FLD1D DS    0H                                                               
*                                                                               
RP2GETX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI01 - ALLOCATE COST - RDSPLIT'                              
RDSPLIT  NMOD1 0,RDSPLIT                                                        
***      =====          **                                                      
         USING SYSD,R9            R9 = CONTAINS PRNTBLOCK                       
***      =====          **                                                      
*   CALLING ROUTINE WILL PASS ADDRESS OF REG/DIST ELEM TO BE PROCESSED          
*       IN PARAMETER                                                            
* === UPON RETURN IF R1+4  HAS C'Y'  THEN SUCCESSFUL RDSPLIT      ENT           
***                                                                             
         MVC   PROCTHIS,0(R1)       ADDRESS OF ELEMENT TO BE PROCESSED          
         L     RF,0(R1)                                                         
         MVI   4(R1),0                                                          
         ST    R1,R1SAVE                                                        
         MVC   RDCLTDIV,2(RF)       CLIENT DIVISION CONTROL                     
         CLI   PBQDRDCL,X'C1'         USING OTHER CLIENT DRD                    
         BL    *+10                                                             
         MVC   RDCLTDIV(3),RDCLTDIV    USE OTHER DRD CLI                        
         MVC   RDDIFS(48),PBGRS                                                 
         MVC   RDDIFS+48(16),PBBGROSS                                           
         XC    RDSUMS,RDSUMS                                                    
         XC    RDBLSUMS,RDBLSUMS                                                
         XC    FULL,FULL                                                        
**************                                                                  
*     FIRST PASS DETERMINES HOW MANY ELEMENTS THERE ARE FOR THIS CL/DIV         
*      FOR ROUNDING PURPOSES                                                    
**************                                                                  
        L      R2,AIO1                                                          
        CLC    0(14,R2),BOMBITS                                                 
        BNE    *+6                                                              
        DC     H'0'                                                             
         L     R2,PBALTLEL         POINTS TO FIRST ELEMENT FOR CLI/DIV          
         CLI   0(R2),X'71'                                                      
         BE    RDSP4                                                            
RDSP3    DS    0H                                                               
         BAS   RE,RDNXTEL                                                       
         BNE   RDSP8                                                            
RDSP4    DS    0H                                                               
***      =====          **                                                      
         USING PUBDSTEL,R2                                                      
***      =====          **                                                      
         OC    PUBDCLT,BLANKS                                                   
         CLC   PUBDCLT(6),RDCLTDIV                                              
         MVC   WORK(6),PUBDCLT                                                  
         BNE   RDSP3                                                            
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),PUBDSHR                                                
         LA    R4,PBGRS                                                         
         LA    R3,RDDIFS                                                        
         ZAP   COUNT,=P'1'                                                      
RDSP5    DS    0H                                                               
         L     R1,0(R4)                                                         
         LTR   R1,R1                                                            
         BZ    RDSP6                                                            
         M     R0,FULL                                                          
         BAS   RE,RDDIV            ROUNDED DIVISION                             
         L     R0,0(R3)                                                         
         SR    R0,R1                                                            
         ST    R0,0(R3)                                                         
RDSP6    DS    0H                                                               
         CP    COUNT,=P'16'                                                     
         BE    RDSP3                                                            
         AP    COUNT,=P'1'                                                      
RDSP6C   LA    R4,4(R4)                                                         
         LA    R3,4(R3)                                                         
RDSP6D   B     RDSP5               NEXT AMOUNT                                  
*                                                                               
BOMBITS  DC    C'JWM',X'20',C'EK RPR',X'0000107200'                             
*                                                                               
RDSP8    DS    0H                                                               
         OC    FULL,FULL                                                        
         BZ    RDSPLITX            NO SHARES                                    
*                                  END OF PASS 1                                
*                                  NOW RDDIFS HAS DIFFERENCES                   
*                                  BETWEEN ORIGINAL AMOUNTS AND                 
*                                  SUM OF ROUNDED SHARES                        
*                                                                               
*                                  PASS 2                                       
*********************************************************************           
         L     R2,PBALTLRC         ADDRESS OF LITTLE REC ELMENT TO **           
         LA    R2,33(R2) * * BE PROCESSED                                       
**                                                                              
         CLI   0(R2),X'71' * *                                                  
         BE    RDSP10 * *                                                       
* * *                                                                           
RDSP9   DS    0H * *                                                            
         BAS   RE,RDNXTEL * *                                                   
         BNE   RDSP30                       *************************           
RDSP10   DS    0H                                                               
         CLC   PUBDCLT(6),RDCLTDIV       * SAME CLIENT DIVISON                  
         BNE   RDSP9                     *                                      
************                                                                    
*     COMPARE ELEMENT TO REQUEST                                                
***********                        IS THIS A SELECTED REG/DST                   
         MVI   DUB+6,0                  NO                                      
         CLC   PBQRGN(3),=C'ALL'        ALL REGIONS                             
         BE    RDSP11                                                           
         OC    PBQRGN(3),PBQRGN         ALL REGIONS                             
         BZ    RDSP11                                                           
         L     RF,PROCTHIS          THIS IS THE ELEMENT TO BE PROCESSED         
         CLC   PBQRGN(3),8(RF)                                                  
         BH    RDSP13 *                                                         
         BL    RDSP30 *                                                         
*=====                                                                          
RDSP11   DS    0H                                                               
         CLC   PBQDST(3),=C'ALL'   ALL DISTRICTS                                
         BE    RDSP12                                                           
         OC    PBQDST(3),PBQDST    ALL DISTRICTS                                
         BZ    RDSP12                                                           
         L     RF,PROCTHIS          THIS IS THE ELEMENT TO BE PROCESSED         
         CLC   PBQDST(3),11(RF)                                                 
         BH    RDSP13 *                                                         
         BL    RDSP30 *                                                         
*=====                                                                          
RDSP12   DS    0H                                                               
         L     RF,PROCTHIS          THIS IS THE ELEMENT TO BE PROCESSED         
         CLC   PUBDCLT(12),2(RF)   ENSURE ELM IS TO BE PROCESSED                
         BNE   RDSP13                                                           
         MVI   DUB+6,1          YES SHOULD BE CHECKED INCALLING ROUTINE         
         L     R1,R1SAVE                                                        
         MVI   4(R1),C'Y'                                                       
RDSP13   DS    0H                                                               
         MVC   FULL+2(2),PUBDSHR   INSERT SHARE                                 
         LA    R4,PBGRS            GROSS COST                                   
         LA    R3,RDDIFS           DIFFERENCES                                  
         ZAP   COUNT,=P'1'                                                      
RDSP13A  DS    0H                                                               
         OC    0(4,R4),0(R4)                                                    
         BZ    RDSP20              NO AMOUNT                                    
         OC    0(4,R3),0(R3)                                                    
         BNZ   RDSP14                                                           
*                                  NO DIFF                                      
         CLI   DUB+6,0             TEST IF A SELECTED REG/DST                   
         BE    RDSP20              NO                                           
         L     R1,0(R4)                                                         
         M     R0,FULL             YES COMPUTE SHARE                            
         BAS   RE,RDDIV                                                         
*                                  AND ADD TO RDSUMS                            
RDSP13B  DS    0H                                                               
         A     R1,64(R3)                                                        
         ST    R1,64(R3)                                                        
         B     RDSP20                                                           
RDSP13D  DS    0H                                                               
         CLI   DUB+6,0             TEST IF A SELECTED REG/DST                   
         BE    RDSP20              NO                                           
         B     RDSP13B             YES                                          
*                                  DIFFERENCE NO-ZERO                           
RDSP14   DS    0H                                                               
         L     R1,0(R4)                                                         
         M     R0,FULL             SHARE                                        
         D     R0,=F'10000'        TRUNCATED DIVISION                           
         LTR   R0,R0               TEST REMAINDER                               
         BZ    RDSP13D             ZERO                                         
         BM    RDSP17              NEG                                          
*                                                                               
*                                  AMOUNT POS                                   
         TM    0(R3),X'80'         TEST DIFF                                    
         BNZ   RDSP15                                                           
*                                  AMOUNT POS, DIFF POS                         
         C     R0,=F'5000'         TEST IF ROUNDED DOWN                         
         BNL   RDSP13D             NO                                           
         L     RF,0(R3)            YES - SUBTRACT 1 FROM DIFF                   
         BCTR  RF,R0                                                            
         ST    RF,0(R3)                                                         
         A     R1,=F'1'            ADD 1 TO AMOUNT                              
         B     RDSP13D                                                          
*                                  AMOUNT POS, DIFF NEG                         
RDSP15   DS    0H                                                               
         C     R0,=F'5000'         TEST IF ROUNDED UP                           
         BL    RDSP13D             NO                                           
         L     RF,0(R3)            YES - ADD 1 TO DIFF                          
         A     RF,=F'1'                                                         
         ST    RF,0(R3)                                                         
         B     RDSP13D                                                          
*                                                                               
*                                  AMOUNT NEG                                   
RDSP17   DS    0H                                                               
         TM    0(R3),X'80'         TEST DIFF                                    
         BZ    RDSP18                                                           
*                                  AMOUNT NEG, DIFF NEG                         
         C     R0,=F'-5000'       TEST IF ROUNDED UP                            
         BL    RDSP13D             NO                                           
         L     RF,0(R3)            YES - ADD 1 TO DIFF                          
         A     RF,=F'1'                                                         
         ST    RF,0(R3)                                                         
         BCTR  R1,R0               SUBTRACT 1 FROM AMT                          
         B     RDSP13D                                                          
*                                  AMOUNT NEG, DIFF POS                         
RDSP18   DS    0H                                                               
         C     R0,=F'-5000'        TEST IF ROUNDED DOWN                         
         BNL   RDSP13D             NO                                           
         L     RF,0(R3)            YES - SUB 1 FROM DIFF                        
         BCTR  RF,R0                                                            
         ST    RF,0(R3)                                                         
         B     RDSP13D                                                          
*                                                                               
*                                                                               
RDSP20   DS    0H                                                               
         CP    COUNT,=P'16'                                                     
         BE    RDSP9        *                                                   
         AP    COUNT,=P'1'                                                      
RDSP23   LA    R4,4(R4)                                                         
         LA    R3,4(R3)                                                         
RDSP24   B     RDSP13A             NEXT AMOUNT                                  
*                                                                               
RDSP30   DS    0H                                                               
         MVC   PBGRS(48),RDSUMS                                                 
         MVC   PBBGROSS(16),RDBLSUMS                                            
*==========================                                                     
*== LOOK UP REGION NAME   =                                                     
*==========================                                                     
         L     R2,PROCTHIS                                                      
         MVC   PBRGN,PUBDREG        PASS REGION                                 
         MVC   PBDST,PUBDDST        PASS DISTRICT                               
         LA    RF,PUBDDIV                                                       
         ICM   R2,15,PBADRDBF      ADDRESS OF DRD BUFFER                        
         BZ    DRDNOBUF            NO BUFFER SET UP                             
         LA    R5,DRDLEN                                                        
DRDRGLK  CLC   0(6,RF),0(R2)                                   L01              
         BE    DRDFNDRG            FOUND REGION                                 
         AR    R2,R5                                                            
         CLI   0(R2),0                                                          
         BNE   DRDRGLK                                                          
         B     DRDUNASS                                                         
DRDNOBUF DC    H'0'                FORCE USER TO USE BUFFER                     
         SPACE 2                                                                
DRDFNDRG DS    0H                                                               
*                                                                               
***      =====          **                                                      
         USING DRDBUFFD,R2                                                      
***      =====          **                                                      
         OC    DRDDST(3),DRDDST             DIST M/B ZERO                       
         BNZ   DRDUNASS                                                         
         MVC   PBREGNM,DRDNAME                                                  
         DROP  R2                                                               
DRDDIVRR DS    0H                                                               
*==========================                                                     
*== LOOK UP DISTRCT NAME  =                                                     
*==========================                                                     
         ICM   R2,15,PBADRDBF      ADDRESS OF DRD BUFFER                        
         BZ    DRDNOBUF            NO BUFFER SET UP-- FORCE USER                
         LA    R5,DRDLEN                                                        
         OC    6(3,RF),6(RF)                                                    
         BZ    DRDUNDIS            NO DISTRICT                                  
*                                                                               
DRDRGLKA CLC   0(9,RF),0(R2)                                   L01              
         BE    DRDFNDDS            FOUND DISTRICT                               
         AR    R2,R5                                                            
         CLI   0(R2),0                                                          
         BNE   DRDRGLKA                                                         
         B     DRDUNASS                                                         
         SPACE 2                                                                
DRDFNDDS DS    0H                                                               
*                                                                               
***      =====          **                                                      
         USING DRDBUFFD,R2                                                      
***      =====          **                                                      
         MVC   PBDSTNM,DRDNAME                                                  
         DROP  R2                                                               
*                                                                               
*==================================================================             
RDSPLITX DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
DRDUNASS XC    PBREGNM,PBREGNM                                                  
         XC    PBRGN,PBRGN                                                      
DRDUNDIS XC    PBDST,PBDST                                                      
         XC    PBDSTNM,PBDSTNM                                                  
         B     RDSPLITX                                                         
*                                                                               
RDNXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    RDNXTEL2                                                         
         CLI   0(R2),X'71'                                                      
         BER   RE                                                               
         B     RDNXTEL+2                                                        
RDNXTEL2 LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 3                                                                
RDDIV    DS    0H                                                               
         SLDA  R0,1                                                             
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
         DS    0F                                                               
GROSSAVE DS    16F                                                              
PROCTHIS DS    F                                                                
R1SAVE   DS    F                                                                
RDDIFS   DS    CL64                                                             
RDSUMS   DS    CL48                                                             
RDBLSUMS DS    CL16                                                             
RDCLTDIV DS    CL6                                                              
COUNT    DC    PL2'0'                                                           
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
BLANKS   DC    CL10' '                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*=================================================================*             
* MQRPTERR - ERROR HANDLER WHEN WRITING TO FILE                   *             
*                                                                 *             
* THIS CODE WILL INTERCEPT ERROR EXITS AND TEST WHETHER OUTPUT IS *             
* BEING WRITTEN TO A FILE.  IF NOT, IT WILL GO TO ERREX.  ELSE IT *             
* WILL SEND INFORMATION TO MQ ABOUT WHERE THE REPORT CAN BE FOUND *             
* AND THEN CONTINUE ON TO ERREX.                                  *             
*                                                                 *             
*=================================================================*             
         SPACE 1                                                                
         USING CONHEADH-64,R7                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
MQE      USING MQERRD,ELEM                                                      
*                                                                               
MQRPTERR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R7,ATWA                                                          
*                                                                               
         TM    PBQFLOPT,PBQFLOUT   SEND OUTPUT DATA TO FILE?                    
         BNO   MQRPX                NO - JUST GO ON TO ERREX                    
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQEERRLQ-1),ELEM                                          
         MVC   MQE.MQEHID,=CL6'ERNOT1'                                          
         MVC   MQE.MQESYS,=CL3'PRT' SYSTEM                                      
*                                                                               
         L     RF,TWAMASTC                                                      
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   MQE.MQEAGYID,MCAGYCOD-MCEXTRA(RF)                                
*                                                                               
         MVC   MQE.MQEQUAL,WRITIT+16                                            
         OC    MQE.MQEQUAL,=CL32' '                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(X'20',MQE.MQEDATE)                       
*                                                                               
         THMS                                                                   
*                                                                               
         ST    R1,FULL                                                          
         EDIT  (P4,FULL),(6,MQE.MQETIME),FILL=0                                 
*                                                                               
         MVC   MQE.MQEDATA1(L'WRIDESC),WRIDESC  MEDIACOM REQ ID                 
         OC    MQE.MQEDATA1,=CL32' '                                            
*                                                                               
         L     RF,TWAMASTC         ALLOCATE PQINDEX TABLE                       
         USING MASTD,RF                                                         
*                                                                               
         MVC   MQE.MQECOD(3),MCREMPQK+2                                         
         MVI   MQE.MQECOD+3,C','                                                
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,MCREMPQK+5                                                  
         EDIT  (RE),(4,MQE.MQECOD+4),ALIGN=LEFT                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   MQE.MQEMSG(L'CONHEAD),CONHEAD                                    
*                                                                               
         DROP  MQE                                                              
*                                                                               
         GOTO1 PBAMQRPT,DMCB,(0,=C'PUT'),ELEM,MQEERRLQ,0                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 PBAMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                
*                                                                               
MQRPX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
MQERRD   DSECT                                                                  
MQEHID   DS    CL6                 HUB RECORD ID                                
MQESYS   DS    CL3                 SYSTEM=PRT                                   
MQEAGYID DS    CL4                 AGENCY ID                                    
MQEQUAL  DS    CL16                QUALIFIER                                    
MQEDATE  DS    CL6                 FILE DATE                                    
MQETIME  DS    CL6                 FILE TIME HHMMSS                             
MQEDATA1 DS    CL32                                                             
MQEDATA2 DS    CL32                                                             
MQECOD   DS    CL32                ERROR CODE (OPT)                             
MQEMSG   DS    CL80                ERROR MESSAGE (OPT)                          
MQEERRLQ EQU   *-MQERRD                                                         
*                                                                               
T40501   CSECT                                                                  
*                                                                               
MQOPEN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RF,15,TWADCONS                                                   
         BNZ   *+6                                                              
         DCHO                                                                   
*                                                                               
         ICM   RF,15,TMQRPT-TWADCOND(RF)                                        
         BNZ   *+6                                                              
         DCHO                                                                   
*                                                                               
         ST    RF,PBAMQRPT                                                      
*                                                                               
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
*                                                                               
* IF WE'RE RUNNING A TEST, SEND TO TEST MQ BROKER                               
*                                                                               
         BRAS  RE,TESTRUN          IS THIS A TEST RUN?                          
         BNE   *+8                  NO                                          
         OI    DMCB+8,X'01'         YES -PUT TO TEST MQ BROKER                  
*                                                                               
*NOTE: THE MQ 'HEADER' IS USED AS THE ROUTE                                     
*                                                                               
         TM    PBQFLOPT,PBQFLSCJ   SC JOHNSON ESTIMATE FILE?                    
         BZ    MQOPN10             NO                                           
         GOTO1 PBAMQRPT,DMCB,(0,=C'OPEN'),(0,SCJTIT),,0                         
         B     MQOPN20                                                          
*                                                                               
MQOPN10  GOTO1 PBAMQRPT,DMCB,(0,=C'OPEN'),(0,WRITIT),,0                         
MQOPN20  CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DCHO                                                                   
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
SPCLSTUP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,TWAMASTC         RF = A(MASTC)                                
         USING MASTD,RF            MASTD DSECT                                  
         L     RE,MCAEXTRA         RE = A(EXTRA DATA AREA)                      
         USING MCEXTRA,RE          MCEXTRA DSECT                                
         CLC   MCAGYCOD,=C'AINY'   AGENCY LABEL (CTAGCCOD) = AINY?              
         BNE   SPCLNEQ             NO - SET CC NEQ                              
         DROP  RE,RF               DROP USINGS                                  
*                                                                               
         USING FILNAMD,R2          FILNAMD DSECT                                
         LA    RE,FILNAMX-FNPRG-1  SPACE PAD FOR THIS LENGTH                    
         EX    RE,*+8              EXECUTE THE MVC                              
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   FNPRG(0),SPACES     INIT FNPRG TO SPACES                         
         LA    R3,FNPRG            FILE NAME WILL GO HERE                       
         MVC   0(8,R3),WRITIT+32   1 BYTE PAST EDIHUBFT********BILLING          
         LA    R3,7(R3)            POSSIBLY LAST CHAR OF FILE NAME              
         CLI   0(R3),X'40'         LAST CHARACTER OF FILENAME?                  
         BH    *+8                 YES                                          
         BCT   R3,*-8              NO - BACK UP 1 CHARACTER                     
*                                                                               
         LA    RE,FNPRG            RE = A(START OF FILE NAME)                   
         LR    RF,R3               RF = LAST CHAR OF FILE NAME                  
*                                                                               
SPCL10   CR    RF,RE               CHECKED EVERY CHAR OF FILE NAME?             
         BL    SPCL20              YES                                          
         CLI   0(RF),X'40'         CHARACTER DATA?                              
         BH    *+6                 YES                                          
         DC    H'0'                NO - CANNOT HAVE IMBEDDED SPACES!            
         BCT   RF,SPCL10           CHECK PREVIOUS CHARACTER                     
*                                                                               
SPCL20   MVC   1(2,R3),=C'.D'      MOVE .D INTO FILE NAME                       
         LA    R3,3(R3)            MOVE DATE STAMP HERE                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(X'20',0(R3))                             
*                                                                               
         MVC   6(2,R3),=C'.T'      MOVE .T INTO FILE NAME                       
         LA    R3,8(R3)            MOVE TIME STAMP HERE                         
*                                                                               
         TIME  DEC                 R0=HHMMSSHH                                  
         ST    R0,FULL             FULL = HHMMSSHH                              
         GOTO1 HEXOUT,DMCB,FULL,0(R3),4                                         
         MVI   7(R3),C' '          GET RID OF HUNDREDTHS                        
*                                                                               
SPCLEQU  CR    RB,RB               SET CC EQU                                   
         J     SPCLXIT             RETURN                                       
*                                                                               
SPCLNEQ  LTR   RB,RB               SET CC NEQ                                   
*                                                                               
SPCLXIT  XIT1                      RETURN                                       
*                                                                               
         DROP  R2                  DROP FILNAMD USING                           
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
         USING CONHEADH-64,R7                                                   
*                                                                               
TESTRUN  DS    0H                                                               
*                                                                               
         ICM   RF,15,TWAMASTC                                                   
         JNZ   *+6                                                              
         DCHO                                                                   
*                                                                               
         ICM   RF,15,MCSSB-MASTD(RF)                                            
         JNZ   *+6                                                              
         DCHO                                                                   
*                                                                               
         CLI   SSOXTND-SSOOFF(RF),X'FF'                                         
         JNE   NOTEST                                                           
         CLI   SSODSPAC-SSOOFF(RF),C'A'  IS THIS A TEST REQUEST?                
         JE    NOTEST                     NO                                    
         CLI   SSODSPAC-SSOOFF(RF),C' '  IS THIS A TEST REQUEST?                
         JNH   NOTEST                     NO                                    
*                                                                               
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
NOTEST   CR    RE,RB                                                            
         BR    RE                                                               
*                                                                               
         TITLE 'PRWRI01 - WORKAREAS'                                            
       ++INCLUDE PRWRIWORKD                                                     
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*DDTWADCOND                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDTWADCOND                                                     
         PRINT ON                                                               
*DRGLOBAL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
         PRINT ON                                                               
*DRIVETABLE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DRIVETABLE                                                     
         PRINT ON                                                               
*DRINTRECD2                                                                     
         PRINT OFF                                                              
       ++INCLUDE DRINTRECD2                                                     
         PRINT ON                                                               
*PRWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE PRWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE PRWRIF1D                                                       
         ORG   WRIWORK                                                          
FILTER   DS    CL4                                                              
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRWRIEAD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG                                                                    
RP2TWASV DS    XL(TWAENDLQ)        TWA SAVEAREA FOR SECOND REPORT               
*                                                                               
         EJECT                                                                  
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
*PRGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGLOBEQUS                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*FASSBOFF                                                                       
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
PPEDICTD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPEDICT                                                        
         PRINT ON                                                               
         SPACE 1                                                                
CONTTABL DSECT                                                                  
CONTNUMB DS    XL4                 CONTRACT PUB NUMBER                          
CONTNED  DS    XL1                 EDITION                                      
CONTNZO  DS    XL1                 ZONE                                         
CONTNST  DS    XL3                 CONT START DATE                              
CONTNEN  DS    XL3                 CONT END DATE                                
CONTNADD DS    XL4                 DISK ADDR                                    
CONTNLEN EQU   *-CONTTABL                                                       
DATELSTD DS   0H                                                                
WEEKLIST DS   18XL105                                                           
MNTHLIST DS   18XL25                                                            
QURTLIST DS   18XL32                                                            
DAYSLIST DS   6XL56                                                             
EXPLIST  DS   XL105                                                             
         DS   0F                                                                
WEELLENT EQU  *-DATELSTD                                                        
         TITLE 'PRNTIO - DSECT FOR PUB LINK ERROR REPORT LINE'                  
***********************************************************************         
*                                                                     *         
*        LINE OF MISSING PUB LINKS ERROR REPORT                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PLELINED DSECT                                                                  
PLELINE  DS    0X                                                               
         DS    CL3                 SPACING                                      
PLEPMED  DS    CL1                 MEDIA                                        
         DS    CL3                 SPACING                                      
PLERAGY  DS    CL2                 AOR AGENCY                                   
         DS    CL2                                                              
PLERCLT  DS    CL3                 AOR CLIENT                                   
         DS    CL2                                                              
PLERPUB  DS    0CL12               AOR PUB ID                                   
PLERPUB# DS    CL8                 AOR PUB NUMBER                               
PLERZONC DS    CL1                 ZONE COMMA                                   
PLERZON  DS    CL1                 ZONE                                         
PLEREDC  DS    CL1                 EDITION COMMA                                
PLERED   DS    CL1                 EDITION                                      
         DS    CL10                                                             
PLEAGY   DS    CL2                 AGY AGENCY                                   
         DS    CL2                                                              
PLECLT   DS    CL3                 AGY CLIENT                                   
         DS    CL2                                                              
PLEPUB   DS    0CL12               AGY PUB ID                                   
PLEPUB#  DS    CL8                 AGY PUB NUMBER                               
PLEZONC  DS    CL1                 ZONE COMMA                                   
PLEZON   DS    CL1                 ZONE                                         
PLEEDC   DS    CL1                 EDITION COMMA                                
PLEED    DS    CL1                 EDITION                                      
         DS    CL2                                                              
PLEPUBN  DS    CL20                PUBLICATION NAME                             
         DS    CL1                                                              
PLEPZNM  DS    CL20                AGY ZONE NAME                                
         DS    CL1                                                              
PLEPCTY  DS    CL16                AGY CITY                                     
         DS    CL1                                                              
PLEPST   DS    CL2                 AGY STATE                                    
*                                                                               
*        TITLES                                                                 
*                                                                               
         ORG   PLELINE                                                          
         DS    CL1                 SPACING                                      
PLEMEDT  DS    CL5                 MEDIA                                        
         DS    CL1                 SPACING                                      
PLERAGYT DS    CL3                 AOR AGENCY                                   
         DS    CL1                                                              
PLERCLTT DS    CL3                 AOR CLIENT                                   
         DS    CL4                                                              
PLERPUBT DS    CL3                 AOR PUB ID                                   
         DS    CL17                                                             
PLEAGYT  DS    CL3                 AGY AGENCY                                   
         DS    CL1                                                              
PLECLTT  DS    CL3                 AGY CLIENT                                   
         DS    CL4                                                              
PLEPUBT  DS    CL3                 AGY PUB ID                                   
         DS    CL9                                                              
PLEPUBNT DS    CL20                AGY PUB  NAME                                
         DS    CL1                                                              
PLEPZNMT DS    CL20                AGY ZONE NAME                                
         DS    CL1                                                              
PLEPCTYT DS    CL16                AGY CITY                                     
         DS    CL1                                                              
PLEPSTT  DS    CL2                 AGY STATE                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'128PRWRI01   10/09/20'                                      
         END                                                                    
